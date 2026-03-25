module TPSDK.GenerativeInterfacesTests

#nowarn "760" // IDisposable needs new

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting


[<TypeProvider>]
type GenerativeInterfacesProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "Interfaces.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Contracts", Some typeof<obj>, isErased = false)

    let createInterface name (members: (string * (string * Type) list * Type) list) =
        let t = ProvidedTypeDefinition(name, None, isErased = false, isInterface = true)

        members
        |> List.map (fun (name, parameters, retType) ->
            let ps = parameters |> List.map (fun (name, ty) -> ProvidedParameter(name, ty))
            let m = ProvidedMethod(name, ps, retType)
            //m.SetMethodAttrs (MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig ||| MethodAttributes.VtableLayoutMask ||| MethodAttributes.Abstract)
            m.AddMethodAttrs (MethodAttributes.Virtual ||| MethodAttributes.Abstract)
            m)
        |> t.AddMembers
        
        t

    do
        let marker = createInterface "IMarker" []
        container.AddMember marker

        let members = [ "GetString", [], typeof<string>
                        "Sum", [("x", typeof<int>); ("y", typeof<int>)], typeof<int> ]
        let contract = createInterface "IContract" members
        container.AddMember contract

        // IExtended inherits from IContract and adds an extra method
        let extended = createInterface "IExtended" [ "GetLength", [("s", typeof<string>)], typeof<int> ]
        extended.AddInterfaceImplementation(contract)
        container.AddMember extended

        // Impl is a concrete class that implements both IMarker and IContract
        let impl = ProvidedTypeDefinition("Impl", Some typeof<obj>, isErased = false)
        let getString = ProvidedMethod("GetString", [], typeof<string>, invokeCode = fun _args -> <@@ "hello" @@>)
        getString.AddMethodAttrs(MethodAttributes.Virtual)
        let sum = ProvidedMethod("Sum", [ProvidedParameter("x", typeof<int>); ProvidedParameter("y", typeof<int>)], typeof<int>,
                                 invokeCode = fun args -> <@@ (%%args.[1] : int) + (%%args.[2] : int) @@>)
        sum.AddMethodAttrs(MethodAttributes.Virtual)
        impl.AddMember getString
        impl.AddMember sum
        impl.AddInterfaceImplementation(marker)
        impl.AddInterfaceImplementation(contract)
        container.AddMember impl

        tempAssembly.AddTypes [container]
        this.AddNamespace(container.Namespace, [container])

let testProvidedAssembly test =
        let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
        let runtimeAssembly = runtimeAssemblyRefs.[0]
        let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
        let tp = GenerativeInterfacesProvider(cfg) :> TypeProviderForNamespaces
        let providedNamespace = tp.Namespaces.[0] 
        let providedTypes  = providedNamespace.GetTypes()
        let providedType = providedTypes.[0] 
        let providedTypeDefinition = providedType :?> ProvidedTypeDefinition
        Assert.Equal("Contracts", providedTypeDefinition.Name)

        let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedTypeDefinition.Assembly)
        let assembly = Assembly.Load assemContents
        assembly.ExportedTypes |> Seq.find (fun ty -> ty.Name = "Contracts") |> test

let runningOnMono = try Type.GetType("Mono.Runtime") <> null with _ -> false 

[<Fact>]
let ``Marker interfaces are generated correctly``() =
  // // See tracking bug https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/211 
  // if not runningOnMono then 
    testProvidedAssembly <| fun container -> 
        let marker = container.GetNestedType "IMarker"
        Assert.NotNull marker
        Assert.True(marker.IsInterface, "Expected IMarker to be an interface")

[<Fact>]
let ``Interfaces with methods are generated correctly``() =
  // // See tracking bug https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/211 
  // if not runningOnMono then 
    testProvidedAssembly <| fun container -> 
        let contract = container.GetNestedType "IContract"
        Assert.NotNull contract
        Assert.True(contract.IsInterface, "Expected IContract to be an interface")

        let contractGetString = contract.GetMethod("GetString")
        Assert.NotNull contractGetString
        Assert.True(contractGetString.IsAbstract, "Expected GetString method to be abstract")
        Assert.True(contractGetString.IsVirtual, "Expected GetString method to be virtual")

        let contractSum = contract.GetMethod("Sum")
        Assert.NotNull contractSum
        Assert.True(contractSum.IsAbstract, "Expected Sum method to be abstract")
        Assert.True(contractSum.IsVirtual, "Expected Sum method to be virtual")

[<Fact>]
let ``Interface inheritance: IExtended extends IContract``() =
    testProvidedAssembly <| fun container ->
        let extended = container.GetNestedType "IExtended"
        Assert.NotNull(extended)
        Assert.True(extended.IsInterface, "Expected IExtended to be an interface")

        // IExtended declares its own method
        let getLength = extended.GetMethod("GetLength")
        Assert.NotNull(getLength)
        Assert.True(getLength.IsAbstract, "Expected GetLength to be abstract")

        // IExtended inherits IContract
        let ifaces = extended.GetInterfaces()
        let hasIContract = ifaces |> Array.exists (fun i -> i.Name = "IContract")
        Assert.True(hasIContract, "Expected IExtended to implement IContract")

[<Fact>]
let ``Concrete class implementing interfaces is generated correctly``() =
    testProvidedAssembly <| fun container ->
        let impl = container.GetNestedType "Impl"
        Assert.NotNull(impl)
        Assert.False(impl.IsInterface, "Impl should not be an interface")
        Assert.False(impl.IsAbstract, "Impl should not be abstract")

        // Impl implements both IMarker and IContract
        let ifaces = impl.GetInterfaces()
        let hasIMarker  = ifaces |> Array.exists (fun i -> i.Name = "IMarker")
        let hasIContract = ifaces |> Array.exists (fun i -> i.Name = "IContract")
        Assert.True(hasIMarker,   "Expected Impl to implement IMarker")
        Assert.True(hasIContract, "Expected Impl to implement IContract")

[<Fact>]
let ``Concrete class methods satisfy interface contract``() =
    testProvidedAssembly <| fun container ->
        let impl = container.GetNestedType "Impl"
        Assert.NotNull(impl)

        let getString = impl.GetMethod("GetString")
        Assert.NotNull(getString)
        Assert.False(getString.IsAbstract, "GetString on Impl should not be abstract")

        let sum = impl.GetMethod("Sum")
        Assert.NotNull(sum)
        let ps = sum.GetParameters()
        Assert.Equal(2, ps.Length)
        Assert.Equal(typeof<int>, ps.[0].ParameterType)
        Assert.Equal(typeof<int>, ps.[1].ParameterType)
        Assert.Equal(typeof<int>, sum.ReturnType)

