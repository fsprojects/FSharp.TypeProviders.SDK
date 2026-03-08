---
title: Quick Start
category: Documentation
categoryindex: 1
index: 1
---
# Quick Start: Your First F# Type Provider

This guide takes you from zero to a working, testable F# type provider in a few minutes using the
official project template. By the end you will have an erased provider that exposes a custom type with
a constructor, a property, and a method.

---

## Prerequisites

- .NET SDK 6.0 or later (`dotnet --version`)
- The F# compiler included with the .NET SDK

---

## Step 1 — Install and Run the Template

```text
dotnet new install FSharp.TypeProviders.Templates
dotnet new typeprovider -n MyProvider -lang F#
cd MyProvider
dotnet tool restore
dotnet paket update
```

`dotnet paket update` downloads `ProvidedTypes.fs` and `ProvidedTypes.fsi` from this SDK — those are the
source files that get compiled directly into your design-time project.

Verify everything builds and the generated tests pass:

```text
dotnet build -c Release
dotnet test -c Release
```

---

## Step 2 — Understand the Project Structure

The template creates three projects:

```text
MyProvider.sln
  src/
    MyProvider.Runtime/      ← the DLL users reference (TPRTC)
    MyProvider.DesignTime/   ← loaded by the compiler at design time (TPDTC)
  tests/
    MyProvider.Tests/        ← tests against the runtime DLL
```

### How the projects relate

**`MyProvider.Runtime`** is the NuGet-published DLL. It contains any runtime helpers (types, functions the
generated/erased code calls at runtime) and the attribute that tells the F# compiler which DLL holds the
provider implementation:

```fsharp
// MyProvider.Runtime.fs — included in both projects; attribute only emitted in the runtime DLL
#if !IS_DESIGNTIME
[<assembly: CompilerServices.TypeProviderAssembly("MyProvider.DesignTime.dll")>]
do ()
#endif
```

**`MyProvider.DesignTime`** contains your `[<TypeProvider>]` class, the `ProvidedTypes.fs` implementation
from this SDK, and a copy of the runtime helpers (so it can reason about runtime types when building the
code model). It is **never shipped to users** — it is loaded by `fsc`, `dotnet fsi`, and the IDE at
design time only.

**`MyProvider.Tests`** references only `MyProvider.Runtime`. It tests both erased and generative providers
via normal F# code.

### What wires the two DLLs together at build time

In `MyProvider.Runtime.fsproj`:

```xml
<PropertyGroup>
  <!-- Tell MSBuild/NuGet that this is a type provider runtime component -->
  <FSharpToolsDirectory>typeproviders</FSharpToolsDirectory>
</PropertyGroup>

<ItemGroup>
  <!-- IsFSharpDesignTimeProvider copies the DesignTime DLL into
       bin/typeproviders/fsharp41/<tfm>/ next to the runtime DLL,
       so the compiler can find it. -->
  <ProjectReference Include="..\MyProvider.DesignTime\MyProvider.DesignTime.fsproj">
    <IsFSharpDesignTimeProvider>true</IsFSharpDesignTimeProvider>
    <PrivateAssets>all</PrivateAssets>
  </ProjectReference>
</ItemGroup>
```

`IsFSharpDesignTimeProvider` is a built-in F# SDK MSBuild property: when set to `true` on a project
reference, the .NET SDK automatically copies the referenced DLL (and its dependencies) into the
`typeproviders/fsharp41/<tfm>/` subdirectory of the runtime project's output, exactly where the F#
compiler looks for design-time tools. See [Packaging](packaging.html) for the full MSBuild details.

---

## Step 3 — The Minimal Provider

Open `src/MyProvider.DesignTime/MyProvider.DesignTime.fs`. The template already contains a working
`BasicErasingProvider`. Here is the essential skeleton:

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
- Inherit `TypeProviderForNamespaces`, not `ITypeProvider` directly.
- `assemblyReplacementMap` rewrites assembly names in quotation bodies so that runtime references resolve
  to `MyProvider.Runtime` rather than `MyProvider.DesignTime`.
- `addDefaultProbingLocation = true` adds the design-time DLL's directory to the probing path so
  bundled dependencies are found.
- `Some typeof<obj>` is the *erased base type*. All values of `MyType` are represented as plain `obj`
  at runtime — no IL is emitted for `MyType` itself.

---

## Step 4 — Add a Static Property

Inside `createTypes`, add a static string property to `myType`:

```fsharp
let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)

let greetingProp =
    ProvidedProperty(
        "Greeting",
        typeof<string>,
        isStatic = true,
        getterCode = fun _args -> <@@ "Hello from MyType!" @@>)
greetingProp.AddXmlDoc "Returns a fixed greeting string."
myType.AddMember greetingProp
```

The `getterCode` closure receives the argument list at the call site. For static properties the list is
empty. The returned quotation is spliced into the caller's compiled output — there is no actual
`MyType` object; the value `"Hello from MyType!"` is embedded directly.

Consumer code would look like:

```fsharp
open MyProvider.Provided
printfn "%s" MyType.Greeting    // Hello from MyType!
```

---

## Step 5 — Add a Constructor and an Instance Property

Erased types carry their runtime state as the *erased base type* (here `obj`). The usual pattern is to
box your state in the constructor and unbox it in each property:

```fsharp
let myType =
    ProvidedTypeDefinition(asm, ns, "Connection", Some typeof<obj>, hideObjectMethods = true)

// Constructor – stores the connection string as a boxed obj
let ctor =
    ProvidedConstructor(
        [ ProvidedParameter("connectionString", typeof<string>) ],
        invokeCode = fun args -> <@@ (%%(args.[0]) : string) :> obj @@>)
ctor.AddXmlDoc "Opens a connection to the given endpoint."
myType.AddMember ctor

// Property – unboxes the stored string
let connStrProp =
    ProvidedProperty(
        "ConnectionString",
        typeof<string>,
        getterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
connStrProp.AddXmlDoc "The connection string this instance was created with."
myType.AddMember connStrProp
```

`args.[0]` in an instance getter is `this` — the boxed `obj` that was returned by the constructor.
`hideObjectMethods = true` suppresses `Equals`, `GetHashCode`, and `ToString` from IntelliSense.

Consumer usage:

```fsharp
let conn = MyProvider.Provided.Connection("Server=localhost;Database=mydb")
printfn "%s" conn.ConnectionString    // Server=localhost;Database=mydb
```

---

## Step 6 — Add a Method

```fsharp
let greetMethod =
    ProvidedMethod(
        "Greet",
        [ ProvidedParameter("name", typeof<string>) ],
        typeof<string>,
        invokeCode = fun args ->
            <@@ sprintf "Hello, %s!" (%%(args.[1]) : string) @@>)
greetMethod.AddXmlDoc "Returns a personalised greeting."
myType.AddMember greetMethod
```

For instance methods, `args.[0]` is `this` (the boxed state) and `args.[1]` is the first explicit
parameter. For static methods (`isStatic = true`), `args.[0]` is the first explicit parameter.

---

## Step 7 — Build and Test

```text
dotnet build -c Release
dotnet test -c Release
```

The template's test project already exercises the erased and generative providers in
`tests/MyProvider.Tests/MyProvider.Tests.fs`. Add your own assertions there or alongside the existing
ones.

For a richer testing approach — including snapshot tests that catch accidental API changes — see
[Testing](guide.html#testing-your-type-provider) in the complete guide.

---

## Step 8 — Pack as a NuGet Package

The template includes a `paket.template` in `src/MyProvider.Runtime/` that defines the NuGet package
layout. At its core, it maps:

```text
bin/Release/netstandard2.0/MyProvider.Runtime.*
    → lib/netstandard2.0/                          (the DLL users reference)

bin/Release/typeproviders/fsharp41/netstandard2.0/*.dll
    → typeproviders/fsharp41/netstandard2.0/       (the design-time DLL the compiler loads)
```

The `typeproviders/fsharp41/…` path is populated automatically during `dotnet build` by the
`IsFSharpDesignTimeProvider` MSBuild plumbing in `MyProvider.Runtime.fsproj`. You do not need to
copy any files manually.

To produce the package file, run:

```text
dotnet build -c Release
dotnet paket pack output/
```

Before publishing, edit `src/MyProvider.Runtime/paket.template` to set your own `id`, `authors`,
`projectUrl`, and `licenseUrl`. The `dependencies` line uses `LOCKEDVERSION`, which Paket replaces
with the exact FSharp.Core version from `paket.lock` — the result is a pinned minimum version in
the emitted `.nuspec`.

> If you prefer SDK-style packing (`dotnet pack`) instead of Paket, set `<GeneratePackageOnBuild>` or
> run `dotnet pack -c Release` from the Runtime project directory. Because `IsFSharpDesignTimeProvider`
> hooks into `TargetsForTfmSpecificContentInPackage`, the design-time DLL is included automatically in
> the resulting `.nupkg` without any additional configuration.

See [Packaging](packaging.html) for the full NuGet layout reference, how to bundle extra
design-time dependencies, the assembly replacement map, and common pitfalls.

---

## Removing Paket

The template uses [Paket](https://fsprojects.github.io/Paket/) to fetch `ProvidedTypes.fs` and
`ProvidedTypes.fsi` from this SDK's GitHub repository. This is the recommended approach because it
always picks up the latest published source. If you prefer plain `PackageReference` you can switch as
follows.

**1. In `MyProvider.DesignTime.fsproj`**, replace the two `<Compile>` items that reference paket-files
with a package reference:

```xml
<!-- Remove these: -->
<!-- <Compile Include="..\..\paket-files\fsprojects\FSharp.TypeProviders.SDK\src\ProvidedTypes.fsi" /> -->
<!-- <Compile Include="..\..\paket-files\fsprojects\FSharp.TypeProviders.SDK\src\ProvidedTypes.fs" /> -->

<!-- Add this instead: -->
<PackageReference Include="FSharp.TypeProviders.SDK" Version="*">
  <PrivateAssets>all</PrivateAssets>
</PackageReference>
```

And change the compile items to refer to the NuGet cache paths, or — more simply — tick the
`<GeneratePathProperty>true</GeneratePathProperty>` flag and use:

```xml
<Compile Include="$(PkgFSharp_TypeProviders_SDK)\src\ProvidedTypes.fsi" />
<Compile Include="$(PkgFSharp_TypeProviders_SDK)\src\ProvidedTypes.fs" />
```

**2. Remove Paket artefacts** — delete `paket.dependencies`, `paket.lock`, `paket-files/`, the
`.config/dotnet-tools.json` Paket entry, and the `<Import>` of `Paket.Restore.targets` from each
`.fsproj`.

**3. Remove the paket.template** and use `dotnet pack` for packaging instead (it works out of the box
because `IsFSharpDesignTimeProvider` hooks directly into `TargetsForTfmSpecificContentInPackage`).

---

## What's Next?

| Topic | Where |
|---|---|
| Static parameters, generative providers, events, interfaces, enumerations | [Providing Types, Members, and Features](providing-types.html) |
| NuGet packaging, `IsFSharpDesignTimeProvider` MSBuild details | [Packaging](packaging.html) |
| Units of measure | [Units of Measure](units-of-measure.html) |
| Debugging the provider inside `fsc` / Ionide | [Debugging](debugging.html) |
| SDK internals, cross-targeting, UncheckedQuotations | [Technical Notes](technical-notes.html) |
