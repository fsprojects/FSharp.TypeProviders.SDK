---
title: Packaging
category: Documentation
categoryindex: 1
index: 4
---

## NuGet Package Layout

```text
lib/netstandard2.0/
    MyProvider.dll               ← TPRTC (carries TypeProviderAssembly attribute)

typeproviders/fsharp41/netstandard2.0/
    MyProvider.DesignTime.dll    ← TPDTC
    MyDependency.dll             ← all design-time dependencies bundled here
```

The TPRTC goes in `lib/`; the TPDTC and all its dependencies go in `typeproviders/fsharp41/`.

## `IsFSharpDesignTimeProvider`: How Packaging Is Automated

The NuGet package layout above is generated automatically by the F# SDK MSBuild targets shipped with the .NET SDK. The mechanism is controlled by the `IsFSharpDesignTimeProvider` MSBuild property and item metadata. This is a built-in feature of the .NET SDK's F# support but is almost entirely undocumented in official sources.

### The Three Roles of `IsFSharpDesignTimeProvider`

`IsFSharpDesignTimeProvider` plays three distinct roles depending on where it appears in your project files.

**Role 1: `ProjectReference` metadata (the standard split-project pattern)**

The most common pattern is a runtime project that references the design-time project via `ProjectReference`
and marks it with `IsFSharpDesignTimeProvider` metadata:

```xml
<!-- MyProvider.Runtime.fsproj -->
<PropertyGroup>
  <TargetFrameworks>netstandard2.0;net8.0</TargetFrameworks>
  <FSharpToolsDirectory>typeproviders</FSharpToolsDirectory>
</PropertyGroup>

<ItemGroup>
  <ProjectReference Include="..\MyProvider.DesignTime\MyProvider.DesignTime.fsproj">
    <IsFSharpDesignTimeProvider>true</IsFSharpDesignTimeProvider>
    <PrivateAssets>all</PrivateAssets>
  </ProjectReference>
</ItemGroup>
```

This metadata activates two separate MSBuild targets:

- **`CollectFSharpDesignTimeTools`** (runs `BeforeTargets="BeforeCompile"`): Resolves the design-time DLL
  path and adds it to the `FscCompilerTools` item group, which is passed to the F# compiler via the
  `--compilertools:<path>` flag. This is what allows type providers to load during compilation of consumer
  projects without any special configuration — the build system wires it up automatically.

- **`PackageFSharpDesignTimeTools`** (runs as part of `TargetsForTfmSpecificContentInPackage`, hooked in
  during `GenerateNuspec`): Collects the entire build output folder of the referenced design-time project
  and places it in the NuGet package under
  `$(FSharpToolsDirectory)/$(FSharpDesignTimeProtocol)/$(TargetFramework)/`, one sub-folder per target
  framework. `FSharp.Core.dll` and `System.ValueTuple.dll` are always excluded — they are expected to be
  available in the host tooling environment.

`<PrivateAssets>all</PrivateAssets>` is essential: it prevents the design-time project from being emitted
as a `<dependency>` element in the `.nuspec`. The design-time DLL is bundled directly into the package
(under `typeproviders/` or `tools/`), not declared as a package dependency.

**Role 2: Project-level property (a project that is its own design-time provider)**

When `<IsFSharpDesignTimeProvider>true</IsFSharpDesignTimeProvider>` appears as a top-level
`<PropertyGroup>` property, the project's own build output is packed as a design-time provider. This is
used when the runtime and design-time components are a single combined DLL:

```xml
<!-- MyProvider.fsproj – a combined TPRTC + TPDTC in one DLL -->
<PropertyGroup>
  <IsFSharpDesignTimeProvider>true</IsFSharpDesignTimeProvider>
  <FSharpToolsDirectory>typeproviders</FSharpToolsDirectory>
</PropertyGroup>
```

In `PackageFSharpDesignTimeTools`, when the project property is set, the target collects
`@(BuiltProjectOutputGroupKeyOutput)` (the current project's outputs) rather than the outputs of a
referenced project, and places them under the `typeproviders/` or `tools/` folder.

**Role 3: `PackageReference` metadata (consuming a separately published design-time package)**

When the design-time DLL is shipped as a distinct NuGet package, consumers reference it on a
`PackageReference`:

```xml
<PackageReference Include="MyProvider.DesignTime" Version="1.0.0">
  <IsFSharpDesignTimeProvider>true</IsFSharpDesignTimeProvider>
</PackageReference>
```

`CollectFSharpDesignTimeTools` then uses the auto-generated `Pkg<PackageName>` MSBuild property —
set by NuGet restore to the package's content root — to locate the DLL and add it to `FscCompilerTools`.
The property name is derived by replacing `.` with `_` in the package file name, e.g.
`PkgMyProvider_DesignTime`.

### `FSharpToolsDirectory` and `FSharpDesignTimeProtocol`

Two MSBuild properties control the exact path inside the NuGet package:

| Property | Default | Valid values | Purpose |
|---|---|---|---|
| `FSharpToolsDirectory` | `tools` | `tools`, `typeproviders` | Top-level folder in the package |
| `FSharpDesignTimeProtocol` | `fsharp41` | `fsharp41` (only) | Protocol sub-folder |

The combined path for design-time DLLs is:

```
$(FSharpToolsDirectory)/$(FSharpDesignTimeProtocol)/$(TargetFramework)/
```

For example, with `FSharpToolsDirectory=typeproviders` and `TargetFramework=netstandard2.0`:

```
typeproviders/fsharp41/netstandard2.0/MyProvider.DesignTime.dll
```

**`typeproviders/` vs `tools/`**: Both locations are probed by the F# SDK when resolving design-time
providers. `typeproviders/` is intended specifically for F# type providers and is the **recommended**
choice. Set `<FSharpToolsDirectory>typeproviders</FSharpToolsDirectory>` on your runtime project. The
`tools/` folder is a more generic location also used by other F# tools; use it only if you have a specific
reason.

The `fsharp41` protocol identifier reflects the provider protocol version introduced with F# 4.1. It is
currently the only valid value; the build targets treat any other value as an error.

### What the `_CheckForDesignTimeProviderReferences` Target Does

The F# SDK includes a `_CheckForDesignTimeProviderReferences` target that runs
`BeforeTargets="GenerateNuspec"`. It scans all `ProjectReference` and `PackageReference` items for
`IsFSharpDesignTimeProvider` metadata. If any are found and the project itself does not already have
`<IsFSharpDesignTimeProvider>true</IsFSharpDesignTimeProvider>` as a project property, it appends
`PackageFSharpDesignTimeTools` to `TargetsForTfmSpecificContentInPackage`.

The practical consequence: **you do not need to set `<IsFSharpDesignTimeProvider>` as a project
property on the runtime project**. Setting it only as metadata on the `ProjectReference` is sufficient
to trigger both build-time tool collection and pack-time layout.

### Complete Example for a Split-Project Provider

`MyProvider.Runtime.fsproj` (generates the package):

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Library</OutputType>
    <TargetFrameworks>netstandard2.0;net8.0</TargetFrameworks>
    <!-- Place design-time DLL under typeproviders/fsharp41/ in the package -->
    <FSharpToolsDirectory>typeproviders</FSharpToolsDirectory>
  </PropertyGroup>

  <ItemGroup>
    <Compile Include="MyProvider.Runtime.fs" />
  </ItemGroup>

  <ItemGroup>
    <!-- Mark the design-time project: bundles it into the package and wires it up during build -->
    <ProjectReference Include="..\MyProvider.DesignTime\MyProvider.DesignTime.fsproj">
      <IsFSharpDesignTimeProvider>true</IsFSharpDesignTimeProvider>
      <PrivateAssets>all</PrivateAssets>
    </ProjectReference>
  </ItemGroup>
</Project>
```

`MyProvider.DesignTime.fsproj` (built and bundled — **not** set as a package project itself):

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Library</OutputType>
    <TargetFrameworks>netstandard2.0;net8.0</TargetFrameworks>
  </PropertyGroup>

  <ItemGroup>
    <!-- Include a copy of the runtime helpers for quotation code generation -->
    <Compile Include="..\MyProvider.Runtime\MyProvider.Runtime.fs" />
    <Compile Include="MyProvider.DesignTime.fs" />
  </ItemGroup>

  <ItemGroup>
    <ProjectReference Include="..\..\src\FSharp.TypeProviders.SDK.fsproj" />
  </ItemGroup>
</Project>
```

Running `dotnet pack` on the runtime project produces a package with this layout:

```text
lib/netstandard2.0/
    MyProvider.Runtime.dll             ← TPRTC; carries [<TypeProviderAssembly("MyProvider.DesignTime")>]
lib/net8.0/
    MyProvider.Runtime.dll

typeproviders/fsharp41/netstandard2.0/
    MyProvider.DesignTime.dll          ← TPDTC; loaded by the F# compiler / IDE
    SomeDesignTimeDependency.dll       ← any other files from the design-time build output
typeproviders/fsharp41/net8.0/
    MyProvider.DesignTime.dll
    SomeDesignTimeDependency.dll
```

## Bundling Dependencies

All dependencies required by the design-time component **must** be bundled alongside
`MyProvider.DesignTime.dll`. They must **not** be listed as regular NuGet package dependencies.

```xml
<!-- In MyProvider.DesignTime.fsproj – copy a dependency to the output folder -->
<Content Include="..\..\packages\Newtonsoft.Json\lib\netstandard2.0\Newtonsoft.Json.dll"
         CopyToOutputDirectory="PreserveNewest" />
```

Make sure these bundled DLLs end up in `typeproviders/fsharp41/netstandard2.0/` in your NuGet package.

## Assembly Replacement Map

When your quotation code references types from the runtime component, the SDK must rewrite the assembly
reference from the design-time component name to the runtime component name. Provide the map in the
`TypeProviderForNamespaces` constructor:

```fsharp
inherit TypeProviderForNamespaces(
    config,
    assemblyReplacementMap = [("MyProvider.DesignTime", "MyProvider.Runtime")])
```

## Runtime vs Design-Time Dependencies

Runtime dependencies are often the same as design-time dependencies for simple type providers. For more complex providers these can differ:

- **Runtime dependencies** are the dependencies of everything in your quotations in your type provider implementation. These are normal NuGet package dependencies, just like any normal .NET library. For example, if your type provider has Newtonsoft.Json as a runtime dependency, your NuGet package should list it as a normal `<dependency>` in the nuspec.

- **Design-time dependencies** are the dependencies of everything outside the quotations — the code that decides and generates the provided types. These must all be bundled alongside your design-time DLL. The design-time component is loaded into a tool like Ionide or Visual Studio and must be loadable without referencing any other packages.

## Common Pitfalls

| Pitfall | Fix |
|---|---|
| Design-time DLL missing from NuGet package | Set `IsFSharpDesignTimeProvider` metadata on the `ProjectReference` and `<FSharpToolsDirectory>typeproviders</FSharpToolsDirectory>` on the runtime project |
| Design-time DLL ends up in `tools/` not `typeproviders/` | Set `<FSharpToolsDirectory>typeproviders</FSharpToolsDirectory>` on the runtime project |
| Design-time project appears as a NuGet dependency | Add `<PrivateAssets>all</PrivateAssets>` to the `IsFSharpDesignTimeProvider` `ProjectReference` |
| Design-time dependency DLL not found at runtime | Bundle all design-time DLLs into the design-time build output folder; they are automatically swept into the package |
