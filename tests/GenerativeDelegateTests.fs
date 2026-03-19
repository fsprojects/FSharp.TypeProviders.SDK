module TPSDK.GenerativeDelegateTests

#nowarn "760" // IDisposable needs new

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting

/// Type provider that creates a container type with two custom delegate types:
///   - SimpleHandler : delegate void SimpleHandler(object sender, EventArgs e)
///   - ValueHandler  : delegate int ValueHandler(int x, int y)
[<TypeProvider>]
type GenerativeDelegatesProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "Delegates.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    do
        // --- SimpleHandler: void(object, EventArgs) ---
        let simpleHandler = ProvidedTypeDefinition("SimpleHandler", Some typeof<System.MulticastDelegate>, isErased = false)
        simpleHandler.AddMember(
            ProvidedConstructor(
                [ ProvidedParameter("object", typeof<obj>)
                  ProvidedParameter("method", typeof<nativeint>) ],
                invokeCode = fun _ -> <@@ () @@>))
        let invokeSimple = ProvidedMethod("Invoke",
            [ ProvidedParameter("sender", typeof<obj>)
              ProvidedParameter("e", typeof<EventArgs>) ],
            typeof<Void>)
        simpleHandler.AddMember invokeSimple
        container.AddMember simpleHandler

        // --- ValueHandler: int(int, int) ---
        let valueHandler = ProvidedTypeDefinition("ValueHandler", Some typeof<System.MulticastDelegate>, isErased = false)
        valueHandler.AddMember(
            ProvidedConstructor(
                [ ProvidedParameter("object", typeof<obj>)
                  ProvidedParameter("method", typeof<nativeint>) ],
                invokeCode = fun _ -> <@@ () @@>))
        let invokeValue = ProvidedMethod("Invoke",
            [ ProvidedParameter("x", typeof<int>)
              ProvidedParameter("y", typeof<int>) ],
            typeof<int>)
        valueHandler.AddMember invokeValue
        container.AddMember valueHandler

        tempAssembly.AddTypes [container]
        this.AddNamespace(ns, [container])

let loadTestAssembly () =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeDelegatesProvider(cfg) :> TypeProviderForNamespaces
    let providedNamespace = tp.Namespaces.[0]
    let providedType = providedNamespace.GetTypes().[0] :?> ProvidedTypeDefinition
    Assert.Equal("Container", providedType.Name)
    let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    Assembly.Load bytes

[<Fact>]
let ``Generative delegate type is present in generated assembly``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let delegateType = containerType.GetNestedType("SimpleHandler")
    Assert.NotNull(delegateType)
    Assert.True(delegateType.IsClass, "SimpleHandler should be a class")
    Assert.Equal("System.MulticastDelegate", delegateType.BaseType.FullName)

[<Fact>]
let ``Generative delegate type has correct constructor``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let delegateType = containerType.GetNestedType("SimpleHandler")
    Assert.NotNull(delegateType)
    let ctor = delegateType.GetConstructor([| typeof<obj>; typeof<nativeint> |])
    Assert.NotNull(ctor)
    let ps = ctor.GetParameters()
    Assert.Equal(2, ps.Length)
    Assert.Equal(typeof<obj>, ps.[0].ParameterType)
    Assert.Equal(typeof<nativeint>, ps.[1].ParameterType)

[<Fact>]
let ``Generative delegate Invoke method has correct signature``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let delegateType = containerType.GetNestedType("SimpleHandler")
    Assert.NotNull(delegateType)
    let invoke = delegateType.GetMethod("Invoke")
    Assert.NotNull(invoke)
    let ps = invoke.GetParameters()
    Assert.Equal(2, ps.Length)
    Assert.Equal("sender", ps.[0].Name)
    Assert.Equal(typeof<obj>, ps.[0].ParameterType)
    Assert.Equal("e", ps.[1].Name)
    Assert.Equal(typeof<EventArgs>, ps.[1].ParameterType)
    Assert.Equal(typeof<Void>, invoke.ReturnType)

[<Fact>]
let ``Generative delegate with value return type has correct Invoke signature``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let delegateType = containerType.GetNestedType("ValueHandler")
    Assert.NotNull(delegateType)
    Assert.Equal("System.MulticastDelegate", delegateType.BaseType.FullName)
    let invoke = delegateType.GetMethod("Invoke")
    Assert.NotNull(invoke)
    let ps = invoke.GetParameters()
    Assert.Equal(2, ps.Length)
    Assert.Equal(typeof<int>, ps.[0].ParameterType)
    Assert.Equal(typeof<int>, ps.[1].ParameterType)
    Assert.Equal(typeof<int>, invoke.ReturnType)

[<Fact>]
let ``Multiple delegate types can coexist in one container``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let nested = containerType.GetNestedTypes()
    let names = nested |> Array.map (fun t -> t.Name) |> Array.sort
    Assert.Contains("SimpleHandler", names)
    Assert.Contains("ValueHandler", names)
    for t in nested do
        Assert.Equal("System.MulticastDelegate", t.BaseType.FullName)
