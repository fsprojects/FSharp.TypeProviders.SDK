module TPSDK.GenerativeAbstractClassesTests

#nowarn "760" // IDisposable needs new

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting

[<TypeProvider>]
type GenerativeAbstractClassesProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "AbstractClasses.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Contracts", Some typeof<obj>, isErased = false)

    let createAbstractClass name (members: (string * (string * Type) list * Type * bool) list) =
        let t = ProvidedTypeDefinition(name, Some typeof<System.MarshalByRefObject>, hideObjectMethods = true, isErased = false, isAbstract = true)
        
        members
        |> List.map (fun (name, parameters, retType, isVirtual) ->
            let ps =
                parameters
                |> List.map (fun (name, ty) ->
                    ProvidedParameter(name, ty))
            if isVirtual then
                let m = ProvidedMethod(name, ps, retType, invokeCode = fun args ->
                    <@ raise (NotImplementedException(name + " is not implemented")) @>.Raw
                    )
                m.AddMethodAttrs (MethodAttributes.Virtual ||| MethodAttributes.HasSecurity)
                m
            else
                let m = ProvidedMethod(name, ps, retType)
                //m.AddMethodAttrs (MethodAttributes.Virtual ||| MethodAttributes.Abstract)
                m
            )
        |> t.AddMembers
        
        t

    do
        let members = [ "GetString", [], typeof<string>, false
                        "Sum", [("x", typeof<int>); ("y", typeof<int>)], typeof<int>, false ]
        let contract = createAbstractClass "Contract" members
        container.AddMember contract

        let members = [ "GetString", [], typeof<string>, true
                        "Sum", [("x", typeof<int>); ("y", typeof<int>)], typeof<int>, true ]
        let virtualContract = createAbstractClass "VirtualContract" members
        container.AddMember virtualContract

        tempAssembly.AddTypes [container]
        this.AddNamespace(container.Namespace, [container])

let testProvidedAssembly test =
        let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
        let runtimeAssembly = runtimeAssemblyRefs.[0]
        let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
        let tp = GenerativeAbstractClassesProvider(cfg) :> TypeProviderForNamespaces
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
let ``Abstract classes with abstract members are generated correctly``() =
  // // See tracking bug https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/211 
  // if not runningOnMono then 
    testProvidedAssembly <| fun container -> 
        let contract = container.GetNestedType "Contract"
        Assert.NotNull contract
        Assert.True(contract.IsAbstract, "Expected Contract to be an abstract type")

        let contractGetString = contract.GetMethod("GetString")
        Assert.NotNull contractGetString
        Assert.True(contractGetString.IsAbstract, "Expected GetString method to be abstract")
        Assert.True(contractGetString.IsVirtual, "Expected GetString method to be virtual")

        let contractSum = contract.GetMethod("Sum")
        Assert.NotNull contractSum
        Assert.True(contractSum.IsAbstract, "Expected Sum method to be abstract")
        Assert.True(contractSum.IsVirtual, "Expected Sum method to be virtual")

[<Fact>]
let ``Abstract classes with virtual members are generated correctly``() =
  // // See tracking bug https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/211 
  // if not runningOnMono then 
    testProvidedAssembly <| fun container -> 
        let contract = container.GetNestedType "VirtualContract"
        Assert.NotNull contract
        Assert.True(contract.IsAbstract, "Expected VirtualContract to be an abstract type")

        let contractGetString = contract.GetMethod("GetString")
        Assert.NotNull contractGetString
        Assert.False(contractGetString.IsAbstract, "Expected GetString method to not be abstract")
        Assert.True(contractGetString.IsVirtual, "Expected GetString method to be virtual")

        let contractSum = contract.GetMethod("Sum")
        Assert.NotNull contractSum
        Assert.False(contractSum.IsAbstract, "Expected Sum method to not be abstract")
        Assert.True(contractSum.IsVirtual, "Expected Sum method to be virtual")

