# Creating F# Type Providers: A Complete Guide

This guide covers everything you need to know to build a production-quality F# type provider using the Type Provider SDK (TPSDK).

## What Is a Type Provider?

A type provider is a compile-time component that supplies types, properties, and methods to F# programs. The consumer sees
real types with IntelliSense, auto-completion, and type-safe access. Under the hood, the provider generates those types
on demand from an external data source, schema, or configuration.

Classic examples:
- **FSharp.Data.JsonProvider** – given a sample JSON string, provides F# record types matching the structure.
- **SQLProvider** – connects to a database at design time and provides typed tables, columns, and queries.

## Erased vs Generative Type Providers

There are two fundamentally different kinds of type providers:

| | Erased | Generative |
|---|---|---|
| Types exist at runtime? | No – erased to a base type | Yes – emitted as real .NET IL |
| Can be inherited or serialized? | No | Yes |
| Performance overhead | Lower | Higher (IL generation) |
| Use case | Schema-driven data access | Code generation scenarios |

**Erased** providers are by far the most common. The provided types exist only at compile time; at runtime every erased
type is replaced by a designated *erased base type* (typically `obj`). All method bodies are given as F# quotations that
are spliced in at the call site.

**Generative** providers emit real IL into a `ProvidedAssembly` whose bytes are injected into the compilation. The result
is a genuine .NET type that persists at runtime. Because of the additional complexity, generative providers should only be
used when inheritance, serialization, or other runtime reflection is required.

---

## Setting Up a Type Provider Project

### Using the Template (Recommended)

```text
dotnet new -i FSharp.TypeProviders.Templates
dotnet new typeprovider -n MyProvider -lang F#
cd MyProvider
dotnet tool restore
dotnet paket update
dotnet build -c Release
dotnet test -c Release
```

This creates a project with a design-time component, a runtime component, and a test project, all wired together correctly.

### Project Structure

A typical type provider solution has three projects:

```text
MyProvider.sln
  MyProvider.Runtime/          ← TPRTC: the runtime component
  MyProvider.DesignTime/       ← TPDTC: the design-time component (includes ProvidedTypes.fs)
  MyProvider.Tests/            ← unit tests for the design-time logic
```

The **runtime component** (`MyProvider.Runtime`) is the DLL that end-users reference. It carries the
`[<TypeProviderAssembly("MyProvider.DesignTime.dll")>]` attribute to tell the F# compiler where to find the
design-time component.

The **design-time component** (`MyProvider.DesignTime`) contains your type provider implementation plus the
source-included files `ProvidedTypes.fs` and `ProvidedTypes.fsi` from this SDK. It is loaded into the compiler
(or IDE) at compile time.

---

## Your First Erased Type Provider

### Minimal Provider

```fsharp
namespace MyProvider.DesignTime

open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

[<TypeProvider>]
type MyErasingProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(
        config,
        assemblyReplacementMap = [("MyProvider.DesignTime", "MyProvider.Runtime")],
        addDefaultProbingLocation = true)

    let ns  = "MyProvider.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        [myType]

    do
        this.AddNamespace(ns, createTypes())
```

Key points:
- `[<TypeProvider>]` registers the class with the F# compiler.
- Inherit `TypeProviderForNamespaces` (not `ITypeProvider` directly).
- `assemblyReplacementMap` rewrites assembly names in quotation code so that runtime references land in the
  runtime component, not the design-time component.
- `addDefaultProbingLocation = true` adds the design-time DLL's directory to the assembly probing path.
- `Some typeof<obj>` is the *erased base type*. All values of `MyType` are represented as `obj` at runtime.

### Adding a Property

```fsharp
let myProp =
    ProvidedProperty(
        "Greeting",
        typeof<string>,
        isStatic = true,
        getterCode = fun _args -> <@@ "Hello, world!" @@>)
myType.AddMember myProp
```

The `getterCode` function receives a list of `Expr` arguments (`args.[0]` is `this` for instance members).
The returned quotation is *spliced* into the caller's code at compile time.

> **Test coverage**: `ErasingProvider` in `tests/BasicErasedProvisionTests.fs` demonstrates static and instance
> properties with getter and setter code.

### Adding an Instance Property with State

An erased type stores its state as the *erased base type* at runtime. The common pattern is to box state into
the base type in the constructor and unbox it in properties:

```fsharp
let myType =
    ProvidedTypeDefinition(asm, ns, "Connection", Some typeof<obj>, hideObjectMethods = true)

// Default constructor – stores the connection string as the erased representation
let ctor =
    ProvidedConstructor(
        [ ProvidedParameter("connectionString", typeof<string>) ],
        invokeCode = fun args -> <@@ (%%(args.[0]) : string) :> obj @@>)
myType.AddMember ctor

// Property reads back the stored value
let connStrProp =
    ProvidedProperty(
        "ConnectionString",
        typeof<string>,
        getterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
myType.AddMember connStrProp
```

`hideObjectMethods = true` hides `Equals`, `GetHashCode`, and `ToString` from IntelliSense – useful for
types that represent an opaque handle.

> **Test coverage**: `ErasingConstructorProvider` in `tests/BasicErasedProvisionTests.fs`.

### Adding a Method

```fsharp
let greetMethod =
    ProvidedMethod(
        "Greet",
        [ ProvidedParameter("name", typeof<string>) ],
        typeof<string>,
        invokeCode = fun args ->
            <@@ sprintf "Hello, %s!" (%%(args.[1]) : string) @@>)
greetMethod.AddXmlDoc "Returns a greeting for the given name."
myType.AddMember greetMethod
```

`args.[0]` is `this`; `args.[1]` is the first explicit parameter.

For static methods, use `isStatic = true` and note that `args.[0]` is the first explicit parameter.

---

## Static Parameters

Static parameters let consumers specialise a type at compile time:

```fsharp
type MyParameterisedProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config)

    let ns  = "MyProvider.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createType typeName (count: int) =
        let t = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)
        for i in 1 .. count do
            let prop =
                ProvidedProperty(
                    "Property" + string i,
                    typeof<int>,
                    isStatic = true,
                    getterCode = fun _args -> <@@ i @@>)
            t.AddMember prop
        t

    do
        let containerType = ProvidedTypeDefinition(asm, ns, "Schema", Some typeof<obj>)
        containerType.DefineStaticParameters(
            [ ProvidedStaticParameter("Count", typeof<int>) ],
            fun typeName args -> createType typeName (args.[0] :?> int))
        this.AddNamespace(ns, [containerType])
```

Usage in user code:

```fsharp
type MySchema = MyProvider.Provided.Schema<Count = 5>
printfn "%d" MySchema.Property3   // 3
```

### Optional Static Parameters with Defaults

```fsharp
containerType.DefineStaticParameters(
    [ ProvidedStaticParameter("Count", typeof<int>)
      ProvidedStaticParameter("Prefix", typeof<string>, parameterDefaultValue = "Col") ],
    fun typeName args ->
        let count  = args.[0] :?> int
        let prefix = args.[1] :?> string
        createType typeName count prefix)
```

> **Warning**: if *all* static parameters have defaults the SDK emits a compiler warning to alert you that
> the unapplied base type would be the same as the applied type with defaults. This is intentional behaviour.

> **Test coverage**: `ErasingProviderWithStaticParams` in `tests/BasicErasedProvisionTests.fs`;
> `GenerativePropertyProviderWithStaticParams` in `tests/BasicGenerativeProvisionTests.fs`.

---

## Quotations in Type Providers

### Quotation Literals

Quotation literals (`<@@ ... @@>`) are the primary way to specify the runtime behaviour of erased members.
The F# compiler splices the quotation body at the call site.

```fsharp
// Access the 'this' parameter of an instance getter
let prop =
    ProvidedProperty("Count", typeof<int>,
        getterCode = fun args ->
            // args.[0] is 'this', typed as the erased base type
            <@@ (%%(args.[0]) :> obj :?> System.Collections.Generic.List<int>).Count @@>)
```

### Cross-Targeting Considerations

Type providers normally run against a different version of the target framework than the design-time host.
The SDK handles translation automatically, but there is one important rule:

**Always use `ProvidedTypeBuilder.MakeGenericType` and `ProvidedTypeBuilder.MakeGenericMethod`** instead of
the standard reflection methods when constructing generic types or methods for use in quotation code:

```fsharp
// Correct – works in both hosted and cross-targeting modes
let seqType = ProvidedTypeBuilder.MakeGenericType(typedefof<seq<_>>, [typeof<string>])
let mapMethod =
    ProvidedTypeBuilder.MakeGenericMethod(
        typedefof<list<_>>.GetMethod("Map"),   // example
        [typeof<string>; typeof<int>])

// Wrong – may fail with type-identity mismatches in cross-targeting mode
let seqType = typedefof<seq<_>>.MakeGenericType([| typeof<string> |])  // ❌
```

### UncheckedQuotations

When you need to call methods or construct objects that involve types not known at design time, use
`UncheckedQuotations`:

```fsharp
open ProviderImplementation.ProvidedTypes.UncheckedQuotations

let meth = someType.GetMethod("Foo")
let callExpr = Expr.CallUnchecked(meth, [Expr.Value 42])
```

`Expr.CallUnchecked` bypasses the standard quotation type-check and is safe to use in type provider
implementations. See `tests/BasicGenerativeProvisionTests.fs` for practical examples.

---

## Your First Generative Type Provider

```fsharp
type MyGenerativeProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(
        config,
        assemblyReplacementMap = [("MyProvider.DesignTime", "MyProvider.Runtime")])

    let ns  = "MyProvider.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createType typeName (count: int) =
        let provAsm = ProvidedAssembly()
        let myType  = ProvidedTypeDefinition(provAsm, ns, typeName, Some typeof<obj>, isErased = false)

        let ctor = ProvidedConstructor([], invokeCode = fun _args -> <@@ () @@>)
        myType.AddMember ctor

        for i in 1 .. count do
            let prop =
                ProvidedProperty(
                    "Property" + string i, typeof<int>,
                    getterCode = fun _args -> <@@ i @@>)
            myType.AddMember prop

        provAsm.AddTypes [myType]
        myType

    do
        let containerType = ProvidedTypeDefinition(asm, ns, "GenerativeSchema", Some typeof<obj>, isErased = false)
        containerType.DefineStaticParameters(
            [ ProvidedStaticParameter("Count", typeof<int>) ],
            fun typeName args -> createType typeName (args.[0] :?> int))
        this.AddNamespace(ns, [containerType])
```

Key differences from erased providers:

- `isErased = false` on `ProvidedTypeDefinition`.
- Must create a `ProvidedAssembly()` and call `provAsm.AddTypes [myType]`.
- The **constructor is mandatory** in a generative type – every `ProvidedTypeDefinition` must have at least
  one `ProvidedConstructor`.

> **Test coverage**: `GenerativePropertyProviderWithStaticParams` in `tests/BasicGenerativeProvisionTests.fs`.

---

## Providing Members in Depth

### Constructors

```fsharp
// Default constructor
let ctor0 = ProvidedConstructor([], invokeCode = fun _args -> <@@ () @@>)

// Constructor with parameters (generative: args.[0] is 'this', args.[1] is first param)
let ctor1 =
    ProvidedConstructor(
        [ ProvidedParameter("value", typeof<int>) ],
        invokeCode = fun args -> <@@ ignore (%%(args.[1]) : int) @@>)

// Static constructor (type initialiser)
let cctor =
    ProvidedConstructor(
        [], invokeCode = fun _args -> <@@ () @@>,
        IsTypeInitializer = true)
```

For **erased** providers, `args.[0]` is the first explicit parameter (there is no `this` expression).
For **generative** providers, `args.[0]` is `this`.

### Events

```fsharp
let myEvent =
    ProvidedEvent(
        "DataChanged",
        typeof<System.EventHandler>,
        adderCode   = fun args -> <@@ ignore (%%(args.[1]) : System.EventHandler) @@>,
        removerCode = fun args -> <@@ ignore (%%(args.[1]) : System.EventHandler) @@>)
myType.AddMember myEvent
```

> **Test coverage**: `GenerativePropertyProviderWithStaticParams` in `tests/BasicGenerativeProvisionTests.fs`.

### Fields (Generative Only)

```fsharp
// Regular field
let field = ProvidedField("_count", typeof<int>)
myType.AddMember field

// Literal / enum constant
let litField = ProvidedField.Literal("MaxSize", typeof<int>, 100)
myType.AddMember litField
```

> **Test coverage**: `GenerativeEnumsProvisionTests.fs`.

### Delayed Member Generation

For large schemas it can be expensive to generate all members eagerly. Use `AddMembersDelayed` to defer
generation until the compiler actually needs them:

```fsharp
myType.AddMembersDelayed(fun () ->
    [ for col in schema.Columns ->
        ProvidedProperty(col.Name, col.Type,
            getterCode = fun args -> <@@ fetchColumn (%%(args.[0]) : obj) col.Name @@>) ])
```

---

## Interfaces (Generative)

```fsharp
// Declare the interface
let iface =
    ProvidedTypeDefinition("IContract", None, isErased = false, isInterface = true)
let meth = ProvidedMethod("Execute", [], typeof<unit>)
meth.AddMethodAttrs(MethodAttributes.Virtual ||| MethodAttributes.Abstract)
iface.AddMember meth

// Implement the interface on a generative type
myType.AddInterfaceImplementation iface
let impl = ProvidedMethod("Execute", [], typeof<unit>,
               invokeCode = fun _args -> <@@ () @@>)
myType.AddMember impl
myType.DefineMethodOverride(impl, iface.GetMethod("Execute"))

provAsm.AddTypes [iface; myType]
```

> **Test coverage**: `GenerativeInterfacesTests.fs`.

---

## Enumerations (Generative)

```fsharp
let enumType = ProvidedTypeDefinition("Status", Some typeof<Enum>, isErased = false)
enumType.SetEnumUnderlyingType(typeof<int>)

for (name, value) in [ "Active", 1; "Inactive", 2; "Pending", 3 ] do
    let field = ProvidedField.Literal(name, enumType, value)
    enumType.AddMember field

provAsm.AddTypes [enumType]
```

> **Test coverage**: `GenerativeEnumsProvisionTests.fs`.

---

## Nested Types

Both erased and generative providers support nested types:

```fsharp
// Erased nested type
let outerType = ProvidedTypeDefinition(asm, ns, "Outer", Some typeof<obj>)
let innerType = ProvidedTypeDefinition("Inner", Some typeof<obj>)
innerType.AddMember(ProvidedProperty("Value", typeof<int>, isStatic = true,
    getterCode = fun _args -> <@@ 42 @@>))
outerType.AddMember innerType
```

For generative providers, nested types must also be registered with the `ProvidedAssembly` using
`AddNestedTypes`:

```fsharp
provAsm.AddNestedTypes([innerType], ["Outer"])
```

---

## XML Documentation

Use `AddXmlDoc` to supply IntelliSense documentation for any provided member:

```fsharp
let myProp = ProvidedProperty("Status", typeof<string>, ...)
myProp.AddXmlDoc "Gets the current connection status."

// Delayed – evaluated only when IntelliSense requests it
myProp.AddXmlDocDelayed(fun () ->
    sprintf "Gets the '%s' column value." columnName)
```

---

## Custom Attributes

Attach arbitrary custom attributes to provided members using `AddCustomAttribute`:

```fsharp
open System.Reflection

// Mark a type as [<Obsolete("use NewType instead")>]
myType.AddCustomAttribute {
    new CustomAttributeData() with
        member _.Constructor = typeof<System.ObsoleteAttribute>.GetConstructor([| typeof<string> |])
        member _.ConstructorArguments =
            [| CustomAttributeTypedArgument(typeof<string>, "use NewType instead" :> obj) |] :> _
        member _.NamedArguments = [||] :> _
}

// Mark a parameter as [<ReflectedDefinition>]
let param = ProvidedParameter("expr", typeof<Microsoft.FSharp.Quotations.Expr<int>>)
param.AddCustomAttribute {
    new CustomAttributeData() with
        member _.Constructor = typeof<ReflectedDefinitionAttribute>.GetConstructor([||])
        member _.ConstructorArguments = [||] :> _
        member _.NamedArguments = [||] :> _
}
```

`AddCustomAttribute` is available on `ProvidedTypeDefinition`, `ProvidedMethod`, `ProvidedProperty`,
and `ProvidedParameter`. `ProvidedConstructor` does not support it; use `AddObsoleteAttribute` instead.

> **Test coverage**: `GenerativeEnumsProvisionTests.fs` (enum field attributes);
> `tests/BasicErasedProvisionTests.fs` (parameter custom attributes in the `NameOf` example).

---

## Units of Measure

Use `ProvidedMeasureBuilder` to annotate numeric types with F# units of measure:

```fsharp
let kg = ProvidedMeasureBuilder.SI "kg"
let m  = ProvidedMeasureBuilder.SI "m"
let s  = ProvidedMeasureBuilder.SI "s"

// float<kg>
let floatKg = ProvidedMeasureBuilder.AnnotateType(typeof<float>, [ kg ])

// m/s²  (acceleration)
let accel = ProvidedMeasureBuilder.Ratio(m, ProvidedMeasureBuilder.Square(s))
let floatAccel = ProvidedMeasureBuilder.AnnotateType(typeof<float>, [ accel ])

let weightProp =
    ProvidedProperty("WeightKg", floatKg, isStatic = true,
        getterCode = fun _args -> <@@ 70.0 @@>)
myType.AddMember weightProp
```

For a complete reference including custom units and compound units, see [Technical Notes](technical-notes.html).

---

## Type Provider Non-Nullability and HideObjectMethods

### `nonNullable`

Set `nonNullable = true` to prevent the F# `null` literal from being used with the type:

```fsharp
let myType = ProvidedTypeDefinition(asm, ns, "Row", Some typeof<obj>, nonNullable = true)
```

This adds `[<AllowNullLiteral(false)>]` to the provided type.

### `hideObjectMethods`

Set `hideObjectMethods = true` to remove `Equals`, `GetHashCode`, and `ToString` from IntelliSense:

```fsharp
let myType =
    ProvidedTypeDefinition(asm, ns, "Connection", Some typeof<obj>, hideObjectMethods = true)
```

---

## Abstract Classes and Base Constructors (Generative)

```fsharp
let baseType =
    ProvidedTypeDefinition(provAsm, ns, "AnimalBase", Some typeof<obj>,
        isErased = false, isAbstract = true, isSealed = false)

// Implicit constructor (fields become accessible with Expr.GlobalVar)
let baseCtor =
    ProvidedConstructor(
        [ ProvidedParameter("name", typeof<string>) ],
        invokeCode = fun _args -> <@@ () @@>,
        IsImplicitConstructor = true)
baseType.AddMember baseCtor

// Derived type
let derivedType =
    ProvidedTypeDefinition(provAsm, ns, "Dog", Some (baseType :> System.Type), isErased = false)
let derivedCtor =
    ProvidedConstructor(
        [ ProvidedParameter("name", typeof<string>) ],
        invokeCode = fun args -> <@@ () @@>)
derivedCtor.BaseConstructorCall <- fun args -> (baseCtor :> System.Reflection.ConstructorInfo), [args.[1]]
derivedType.AddMember derivedCtor

provAsm.AddTypes [baseType; derivedType]
```

> **Test coverage**: `GenerativeAbstractClassesTests.fs`.

---

## Testing Your Type Provider

The SDK ships a `ProvidedTypesTesting` module (in `tests/ProvidedTypesTesting.fs`) that provides utilities
for unit-testing type providers without invoking the full F# compiler:

```fsharp
open ProviderImplementation.ProvidedTypesTesting

// Create a simulated TypeProviderConfig that points at .NET Standard 2.0 references
let refs = Targets.DotNetStandard20FSharpRefs()
let cfg  = Testing.MakeSimulatedTypeProviderConfig(__SOURCE_DIRECTORY__, refs.[0], refs)

// Instantiate your provider
let tp = MyProvider(cfg) :> TypeProviderForNamespaces

// Navigate the namespace and types
let providedNs   = tp.Namespaces.[0]
let providedType = providedNs.GetTypes().[0]

// Apply static parameters
let instantiated =
    (tp :> Microsoft.FSharp.Core.CompilerServices.ITypeProvider)
        .ApplyStaticArguments(providedType, [| "MyType,\"5\"" |], [| box 5 |])

// For generative providers – get the IL bytes and load the assembly
let bytes = (tp :> Microsoft.FSharp.Core.CompilerServices.ITypeProvider)
                .GetGeneratedAssemblyContents(instantiated.Assembly)
let asm = System.Reflection.Assembly.Load(bytes)
let t   = asm.GetType("MyProvider.Provided.MyType,\"5\"")
```

### Cross-Targeting Snapshot Tests

The `Testing.FormatProvidedType` helper renders a type's members as text, which you can use in
approval-style snapshot tests:

```fsharp
let tp, t = Testing.GenerateProvidedTypeInstantiation(
                __SOURCE_DIRECTORY__, refs.[0], refs,
                (fun cfg -> MyProvider(cfg) :> _), [| box 3 |])
let snapshot = Testing.FormatProvidedType(tp, t, useQualifiedNames = true)
Assert.Equal(expectedSnapshot, snapshot.Trim())
```

These tests catch accidental API changes in your provided types.

---

## Debugging

See [Debugging Type Providers](debugging.html) for techniques to debug your provider when it is loaded by
`fsc.exe`, `dotnet fsi`, Visual Studio, or Ionide.

---

## Packaging

### NuGet Package Layout

```text
lib/netstandard2.0/
    MyProvider.dll               ← TPRTC (carries TypeProviderAssembly attribute)

typeproviders/fsharp41/netstandard2.0/
    MyProvider.DesignTime.dll    ← TPDTC
    MyDependency.dll             ← all design-time dependencies bundled here
```

The TPRTC goes in `lib/`; the TPDTC and all its dependencies go in `typeproviders/fsharp41/`.

### Bundling Dependencies

All dependencies required by the design-time component **must** be bundled alongside
`MyProvider.DesignTime.dll`. They must **not** be listed as regular NuGet package dependencies.

```xml
<!-- In MyProvider.DesignTime.fsproj – copy a dependency to the output folder -->
<Content Include="..\..\packages\Newtonsoft.Json\lib\netstandard2.0\Newtonsoft.Json.dll"
         CopyToOutputDirectory="PreserveNewest" />
```

Make sure these bundled DLLs end up in `typeproviders/fsharp41/netstandard2.0/` in your NuGet package.

### Assembly Replacement Map

When your quotation code references types from the runtime component, the SDK must rewrite the assembly
reference from the design-time component name to the runtime component name. Provide the map in the
`TypeProviderForNamespaces` constructor:

```fsharp
inherit TypeProviderForNamespaces(
    config,
    assemblyReplacementMap = [("MyProvider.DesignTime", "MyProvider.Runtime")])
```

---

## Summary: Common Pitfalls

| Pitfall | Fix |
|---|---|
| Using `type.MakeGenericType(...)` in quotations | Use `ProvidedTypeBuilder.MakeGenericType(...)` |
| Forgetting `provAsm.AddTypes [myType]` | Required for all generative types |
| Missing constructor on generative type | Add at least one `ProvidedConstructor` |
| Design-time dependency not found at runtime | Bundle all design-time DLLs next to the TPDTC |
| `args.[0]` confusion (erased vs generative) | Erased: first param; Generative: `this` |
| All static params have defaults | Intentional SDK warning – expected behaviour |
