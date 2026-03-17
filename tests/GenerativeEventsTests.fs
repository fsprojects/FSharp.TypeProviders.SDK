module TPSDK.GenerativeEventsTests

#nowarn "760" // IDisposable needs new

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting

/// Type provider with a generative type that has both an instance and a static event.
[<TypeProvider>]
type GenerativeEventsProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "Events.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    do
        let eventType = ProvidedTypeDefinition("Widget", Some typeof<obj>, isErased = false)

        // Instance event: fired when widget changes
        let adderCode   (args: Expr list) = <@@ ignore (%%(args.[1]) : EventHandler) @@>
        let removerCode (args: Expr list) = <@@ ignore (%%(args.[1]) : EventHandler) @@>
        let instanceEvent = ProvidedEvent("Changed", typeof<EventHandler>, adderCode = adderCode, removerCode = removerCode, isStatic = false)
        eventType.AddMember instanceEvent

        // Static event: fired when any widget is created
        let staticAdder   (args: Expr list) = <@@ ignore (%%(args.[0]) : EventHandler) @@>
        let staticRemover (args: Expr list) = <@@ ignore (%%(args.[0]) : EventHandler) @@>
        let staticEvent = ProvidedEvent("AnyCreated", typeof<EventHandler>, adderCode = staticAdder, removerCode = staticRemover, isStatic = true)
        eventType.AddMember staticEvent

        eventType.AddMember (ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        container.AddMember eventType

        tempAssembly.AddTypes [container]
        this.AddNamespace(ns, [container])

let loadTestAssembly () =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeEventsProvider(cfg) :> TypeProviderForNamespaces
    let providedNamespace = tp.Namespaces.[0]
    let providedType = providedNamespace.GetTypes().[0] :?> ProvidedTypeDefinition
    Assert.Equal("Container", providedType.Name)
    let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    Assembly.Load bytes

[<Fact>]
let ``Generative instance event is present in generated assembly``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let evt = widgetType.GetEvent("Changed", BindingFlags.Instance ||| BindingFlags.Public)
    Assert.NotNull(evt)
    Assert.Equal(typeof<EventHandler>, evt.EventHandlerType)
    Assert.False(evt.GetAddMethod().IsStatic, "add_Changed should be an instance method")
    Assert.False(evt.GetRemoveMethod().IsStatic, "remove_Changed should be an instance method")

[<Fact>]
let ``Generative instance event add/remove methods have correct signatures``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let evt = widgetType.GetEvent("Changed", BindingFlags.Instance ||| BindingFlags.Public)
    Assert.NotNull(evt)

    let addMethod = evt.GetAddMethod()
    Assert.NotNull(addMethod)
    Assert.Equal("add_Changed", addMethod.Name)
    let addParams = addMethod.GetParameters()
    Assert.Equal(1, addParams.Length)
    Assert.Equal(typeof<EventHandler>, addParams.[0].ParameterType)

    let removeMethod = evt.GetRemoveMethod()
    Assert.NotNull(removeMethod)
    Assert.Equal("remove_Changed", removeMethod.Name)
    let removeParams = removeMethod.GetParameters()
    Assert.Equal(1, removeParams.Length)
    Assert.Equal(typeof<EventHandler>, removeParams.[0].ParameterType)

[<Fact>]
let ``Generative static event is present in generated assembly``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let evt = widgetType.GetEvent("AnyCreated", BindingFlags.Static ||| BindingFlags.Public)
    Assert.NotNull(evt)
    Assert.Equal(typeof<EventHandler>, evt.EventHandlerType)
    Assert.True(evt.GetAddMethod().IsStatic, "add_AnyCreated should be a static method")
    Assert.True(evt.GetRemoveMethod().IsStatic, "remove_AnyCreated should be a static method")

[<Fact>]
let ``Generative type has expected event count``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let allEvents = widgetType.GetEvents(BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public)
    Assert.Equal(2, allEvents.Length)
