---
index: 2
---
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

## Quick Start

See [Quick Start: Your First F# Type Provider](quick-start.html) for a step-by-step walkthrough: installing
the template, understanding the project structure (including how `IsFSharpDesignTimeProvider` wires the
runtime and design-time components together), and building a minimal erased provider with a constructor,
property, and method.

---

## Providing Types, Members, and Other Features

See [Providing Types, Members, and Features](providing-types.html) for technical details and examples of providing static parameters, constructors, events, fields, delayed members, interfaces, enumerations, nested types, XML documentation, custom attributes, units of measure, non-nullability, and abstract classes.

---

## Testing Your Type Provider

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

See [Units of Measure in F# Type Providers](units-of-measure.html) for SI units, compound units, custom units, and multi-argument annotation.

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

See [Packaging F# Type Providers](packaging.html) for NuGet package layout, how `IsFSharpDesignTimeProvider` automates packaging and build-time tool collection, bundling design-time dependencies, and the assembly replacement map.

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
