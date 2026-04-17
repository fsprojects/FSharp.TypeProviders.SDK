module TPSDK.GenerativePropertiesTests

#nowarn "760" // IDisposable needs new

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting
open UncheckedQuotations

/// Generative type provider with a variety of property kinds:
///   - instance read-only property
///   - instance read-write property
///   - static read-only property
///   - multiple properties on the same nested type (exercises ILPropertyDefs lazy name-lookup cache)
[<TypeProvider>]
type GenerativePropertiesProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "Properties.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    do
        let widgetType = ProvidedTypeDefinition("Widget", Some typeof<obj>, isErased = false)

        // A backing field for the read-write property
        let labelField = ProvidedField("_label", typeof<string>)
        widgetType.AddMember labelField

        // Instance read-only property: returns a constant
        let readOnlyProp =
            ProvidedProperty(
                "Kind", typeof<string>,
                isStatic = false,
                getterCode = fun _args -> <@@ "widget" @@>)
        widgetType.AddMember readOnlyProp

        // Instance read-write property backed by _label field
        let readWriteProp =
            ProvidedProperty(
                "Label", typeof<string>,
                isStatic = false,
                getterCode = (fun args -> Expr.FieldGetUnchecked(args.[0], labelField)),
                setterCode = (fun args -> Expr.FieldSetUnchecked(args.[0], labelField, args.[1])))
        widgetType.AddMember readWriteProp

        // Static read-only property
        let staticProp =
            ProvidedProperty(
                "DefaultKind", typeof<string>,
                isStatic = true,
                getterCode = fun _args -> <@@ "default-widget" @@>)
        widgetType.AddMember staticProp

        // A second property to verify the name-lookup dictionary handles multiple entries
        let countProp =
            ProvidedProperty(
                "Index", typeof<int>,
                isStatic = false,
                getterCode = fun _args -> <@@ 42 @@>)
        widgetType.AddMember countProp

        widgetType.AddMember (ProvidedConstructor([], invokeCode = fun _args -> <@@ () @@>))
        container.AddMember widgetType
        tempAssembly.AddTypes [container]
        this.AddNamespace(ns, [container])

let loadTestAssembly () =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativePropertiesProvider(cfg) :> TypeProviderForNamespaces
    let providedNamespace = tp.Namespaces.[0]
    let providedType = providedNamespace.GetTypes().[0] :?> ProvidedTypeDefinition
    Assert.Equal("Container", providedType.Name)
    let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    Assembly.Load bytes

[<Fact>]
let ``Instance read-only property is present in generated assembly``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let prop = widgetType.GetProperty("Kind", BindingFlags.Instance ||| BindingFlags.Public)
    Assert.NotNull(prop)
    Assert.Equal(typeof<string>, prop.PropertyType)
    Assert.True(prop.CanRead, "Kind should be readable")
    Assert.False(prop.CanWrite, "Kind should not be writable")
    Assert.False(prop.GetMethod.IsStatic, "Kind getter should be an instance method")

[<Fact>]
let ``Instance read-write property is present and has getter and setter``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let prop = widgetType.GetProperty("Label", BindingFlags.Instance ||| BindingFlags.Public)
    Assert.NotNull(prop)
    Assert.Equal(typeof<string>, prop.PropertyType)
    Assert.True(prop.CanRead,  "Label should be readable")
    Assert.True(prop.CanWrite, "Label should be writable")
    Assert.Equal("get_Label", prop.GetMethod.Name)
    Assert.Equal("set_Label", prop.SetMethod.Name)

[<Fact>]
let ``Static read-only property is present in generated assembly``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let prop = widgetType.GetProperty("DefaultKind", BindingFlags.Static ||| BindingFlags.Public)
    Assert.NotNull(prop)
    Assert.Equal(typeof<string>, prop.PropertyType)
    Assert.True(prop.CanRead, "DefaultKind should be readable")
    Assert.True(prop.GetMethod.IsStatic, "DefaultKind getter should be static")

[<Fact>]
let ``All expected properties are found by name (exercises ILPropertyDefs lazy name-lookup cache)``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let allFlags = BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public
    // Each property must be findable by exact name (uses FindByName dictionary)
    for name in ["Kind"; "Label"; "DefaultKind"; "Index"] do
        let prop = widgetType.GetProperty(name, allFlags)
        Assert.NotNull(prop)
        Assert.Equal(name, prop.Name)

[<Fact>]
let ``Widget type has expected number of properties``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let allProps = widgetType.GetProperties(BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public)
    Assert.Equal(4, allProps.Length)
