module TPSDK.GenerativeCustomAttributeTests

#nowarn "760" // IDisposable needs new

open System
open System.ComponentModel
open System.Diagnostics
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting

// ---------------------------------------------------------------------------
// Provider: custom attributes on the type, method, and property.
//
// Covers attribute targets not addressed by the property-focused tests in
// BasicGenerativeProvisionTests.fs:
//   - ObsoleteAttribute(string, bool) on the TYPE ITSELF
//   - DescriptionAttribute(string)    on an instance METHOD
//   - Two attrs on the SAME method    (multiple-attribute path)
//   - DebuggerBrowsableAttribute(DebuggerBrowsableState)  on a PROPERTY  (enum arg)
// ---------------------------------------------------------------------------

[<TypeProvider>]
type GenerativeCustomAttrProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "CustomAttrTargets.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    do
        let obsoleteCtor2   = typeof<ObsoleteAttribute>.GetConstructor([| typeof<string>; typeof<bool> |])
        let descriptionCtor = typeof<DescriptionAttribute>.GetConstructor([| typeof<string> |])
        let browsableCtor   = typeof<DebuggerBrowsableAttribute>.GetConstructor([| typeof<DebuggerBrowsableState> |])

        let widgetType = ProvidedTypeDefinition("Widget", Some typeof<obj>, isErased = false)

        // Attribute on the TYPE ITSELF: ObsoleteAttribute("old api", false)
        widgetType.AddCustomAttribute {
            new CustomAttributeData() with
                member _.Constructor = obsoleteCtor2
                member _.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<string>, box "old api"); CustomAttributeTypedArgument(typeof<bool>, box false) |]
                member _.NamedArguments = upcast [||] }

        // Method with TWO attributes: DescriptionAttribute and ObsoleteAttribute
        let runCode (_args: Expr list) = <@@ "ok" @@>
        let meth = ProvidedMethod("Run", [], typeof<string>, invokeCode = runCode, isStatic = false)
        meth.AddCustomAttribute {
            new CustomAttributeData() with
                member _.Constructor = descriptionCtor
                member _.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<string>, box "my method") |]
                member _.NamedArguments = upcast [||] }
        meth.AddCustomAttribute {
            new CustomAttributeData() with
                member _.Constructor = obsoleteCtor2
                member _.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<string>, box "old method"); CustomAttributeTypedArgument(typeof<bool>, box false) |]
                member _.NamedArguments = upcast [||] }
        widgetType.AddMember meth

        // Property with DebuggerBrowsableAttribute(Never) — tests enum-valued argument
        let hiddenCode (_args: Expr list) = <@@ 0 @@>
        let prop = ProvidedProperty("Hidden", typeof<int>, isStatic = false, getterCode = hiddenCode)
        prop.AddCustomAttribute {
            new CustomAttributeData() with
                member _.Constructor = browsableCtor
                member _.ConstructorArguments =
                    upcast [| CustomAttributeTypedArgument(typeof<DebuggerBrowsableState>, box DebuggerBrowsableState.Never) |]
                member _.NamedArguments = upcast [||] }
        widgetType.AddMember prop

        widgetType.AddMember (ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        container.AddMember widgetType
        tempAssembly.AddTypes [container]
        this.AddNamespace(ns, [container])

let loadAttrTestAssembly () =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeCustomAttrProvider(cfg) :> TypeProviderForNamespaces
    let providedType = tp.Namespaces.[0].GetTypes().[0]
    let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    let assem = Assembly.Load bytes
    assem.GetType("CustomAttrTargets.Provided.Container").GetNestedType("Widget")

[<Fact>]
let ``Custom attribute on a generative type round-trips correctly``() =
    // Verifies that ObsoleteAttribute placed on the type definition itself survives the
    // IL write/read round-trip inside the generative assembly writer.
    let widgetType = loadAttrTestAssembly ()
    Assert.NotNull(widgetType)
    let attrs = widgetType.GetCustomAttributesData()
    let obsolete = attrs |> Seq.tryFind (fun a -> a.Constructor.DeclaringType = typeof<ObsoleteAttribute>)
    Assert.True(obsolete.IsSome, "ObsoleteAttribute should be present on Widget type")
    let attr = obsolete.Value
    Assert.Equal(2, attr.ConstructorArguments.Count)
    Assert.Equal("old api", attr.ConstructorArguments.[0].Value :?> string)
    Assert.Equal(false, attr.ConstructorArguments.[1].Value :?> bool)

[<Fact>]
let ``Custom attribute with bool constructor argument round-trips correctly``() =
    // Verifies that bool values survive the encode/decode path (ECMA-335 §II.23.3).
    let widgetType = loadAttrTestAssembly ()
    Assert.NotNull(widgetType)
    // isError = false survives
    let typeAttrs = widgetType.GetCustomAttributesData()
    let obs = typeAttrs |> Seq.find (fun a -> a.Constructor.DeclaringType = typeof<ObsoleteAttribute>)
    Assert.Equal(false, obs.ConstructorArguments.[1].Value :?> bool)

[<Fact>]
let ``Custom attribute with enum constructor argument round-trips correctly``() =
    // DebuggerBrowsableAttribute takes a DebuggerBrowsableState enum value.
    // Enum values are encoded as their underlying int in custom attribute blobs (ECMA-335 §II.23.3).
    let widgetType = loadAttrTestAssembly ()
    Assert.NotNull(widgetType)
    let prop = widgetType.GetProperty("Hidden")
    Assert.NotNull(prop)
    let attrs = prop.GetCustomAttributesData()
    let browsable = attrs |> Seq.tryFind (fun a -> a.Constructor.DeclaringType = typeof<DebuggerBrowsableAttribute>)
    Assert.True(browsable.IsSome, "DebuggerBrowsableAttribute should be present on Hidden property")
    let attr = browsable.Value
    Assert.Equal(1, attr.ConstructorArguments.Count)
    // The binary reader decodes enum args as their underlying integral value.
    // DebuggerBrowsableState.Never == 0; verify the round-tripped value equals 0.
    let rawValue = Convert.ToInt32(attr.ConstructorArguments.[0].Value)
    Assert.Equal(int DebuggerBrowsableState.Never, rawValue)

[<Fact>]
let ``Multiple custom attributes on a single generative method are all preserved``() =
    // Tests that defineCustomAttrs writes all attributes for a method, not just the first.
    let widgetType = loadAttrTestAssembly ()
    Assert.NotNull(widgetType)
    let meth = widgetType.GetMethod("Run", BindingFlags.Instance ||| BindingFlags.Public)
    Assert.NotNull(meth)
    let attrs = meth.GetCustomAttributesData()
    let desc = attrs |> Seq.tryFind (fun a -> a.Constructor.DeclaringType = typeof<DescriptionAttribute>)
    let obs  = attrs |> Seq.tryFind (fun a -> a.Constructor.DeclaringType = typeof<ObsoleteAttribute>)
    Assert.True(desc.IsSome, "DescriptionAttribute should be present on Run method")
    Assert.True(obs.IsSome,  "ObsoleteAttribute should be present on Run method")
    Assert.Equal("my method",  desc.Value.ConstructorArguments.[0].Value :?> string)
    Assert.Equal("old method", obs.Value.ConstructorArguments.[0].Value :?> string)

[<Fact>]
let ``Custom attribute on a generative method has correct string argument``() =
    let widgetType = loadAttrTestAssembly ()
    Assert.NotNull(widgetType)
    let meth = widgetType.GetMethod("Run", BindingFlags.Instance ||| BindingFlags.Public)
    let attrs = meth.GetCustomAttributesData()
    let desc = attrs |> Seq.find (fun a -> a.Constructor.DeclaringType = typeof<DescriptionAttribute>)
    Assert.Equal(1, desc.ConstructorArguments.Count)
    Assert.Equal("my method", desc.ConstructorArguments.[0].Value :?> string)

// ---------------------------------------------------------------------------
// System.Type constructor argument round-trip
//
// Regression test for the 8.4.0 fix: decodeILCustomAttribData now resolves
// System.Type custom attribute arguments (previously always returned null).
// DebuggerTypeProxyAttribute(Type proxyType) takes a System.Type, making it
// ideal for testing the typeof<T> encode/decode path.
// ---------------------------------------------------------------------------

[<TypeProvider>]
type GenerativeTypeArgAttrProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "TypeArgAttr.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    do
        // DebuggerTypeProxyAttribute(Type) accepts a System.Type argument.
        let typeProxyCtor = typeof<DebuggerTypeProxyAttribute>.GetConstructor([| typeof<Type> |])

        let widgetType = ProvidedTypeDefinition("Widget", Some typeof<obj>, isErased = false)

        // Attach DebuggerTypeProxyAttribute(typeof<int>) — the typeof<int> is a System.Type value
        widgetType.AddCustomAttribute {
            new CustomAttributeData() with
                member _.Constructor = typeProxyCtor
                member _.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<Type>, box typeof<int>) |]
                member _.NamedArguments = upcast [||] }

        widgetType.AddMember (ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        container.AddMember widgetType
        tempAssembly.AddTypes [container]
        this.AddNamespace(ns, [container])

let loadTypeArgAttrTestAssembly () =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeTypeArgAttrProvider(cfg) :> TypeProviderForNamespaces
    let providedType = tp.Namespaces.[0].GetTypes().[0]
    let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    let assem = Assembly.Load bytes
    assem.GetType("TypeArgAttr.Provided.Container").GetNestedType("Widget")

[<Fact>]
let ``Custom attribute with System.Type constructor argument round-trips correctly``() =
    // Regression test: before the 8.4.0 fix, decodeILCustomAttribData returned null
    // for System.Type-typed constructor arguments instead of resolving the type.
    let widgetType = loadTypeArgAttrTestAssembly ()
    Assert.NotNull(widgetType)
    let attrs = widgetType.GetCustomAttributesData()
    let proxyAttr = attrs |> Seq.tryFind (fun a -> a.Constructor.DeclaringType = typeof<DebuggerTypeProxyAttribute>)
    Assert.True(proxyAttr.IsSome, "DebuggerTypeProxyAttribute should be present on Widget type")
    let attr = proxyAttr.Value
    Assert.Equal(1, attr.ConstructorArguments.Count)
    // The decoded value should be the System.Type for System.Int32.
    let argValue = attr.ConstructorArguments.[0].Value
    Assert.NotNull(argValue)
    let resolvedType = argValue :?> Type
    Assert.NotNull(resolvedType)
    Assert.Equal("System.Int32", resolvedType.FullName)
