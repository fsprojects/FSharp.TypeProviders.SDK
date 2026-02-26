# The F# Type Provider SDK

The F# Type Provider SDK (TPSDK) is the standard library for building F# type providers. It provides
`ProvidedTypes.fs`, a single source file that implements the full Type Provider API, including erased and
generative type definitions, static parameters, quotation support, and cross-targeting infrastructure.

## Quick Start

Create a new type provider from the template:

```text
dotnet new -i FSharp.TypeProviders.Templates
dotnet new typeprovider -n MyProvider -lang F#
cd MyProvider
dotnet tool restore
dotnet paket update
dotnet build -c Release
dotnet test -c Release
```

## Documentation

| | |
|---|---|
| [**Complete Guide**](guide.html) | Step-by-step guide covering erased and generative providers, static parameters, quotations, interfaces, enumerations, units of measure, testing, and packaging. |
| [**Technical Notes**](technical-notes.html) | Design-time vs runtime components, naming conventions, NuGet package layout, lifetime of type provider instances, quotation construction, and API reference for key features. |
| [**Debugging**](debugging.html) | How to debug a type provider running inside `fsc.exe`, `dotnet fsi`, Visual Studio, or Ionide. |

## Examples

See the [`examples`](https://github.com/fsprojects/FSharp.TypeProviders.SDK/tree/master/examples) directory
for `BasicProvider` (both erased and generative variants) and `StressProvider`.

## Using the SDK via NuGet

Instead of source-including `ProvidedTypes.fs`, you can reference the SDK as a NuGet package in the
design-time component:

```xml
<PackageReference Include="FSharp.TypeProviders.SDK" Version="8.0.0">
  <PrivateAssets>all</PrivateAssets>
</PackageReference>
<PackageReference Update="FSharp.Core" Version="8.0.0">
  <ExcludeAssets>all</ExcludeAssets>
</PackageReference>
```

The runtime component should **not** reference `FSharp.TypeProviders.SDK`.

## External Resources

- [Type Providers from the ground up](https://blog.mavnn.co.uk/type-providers-from-the-ground-up/) and
  the [follow up posts](https://blog.mavnn.co.uk/blog/categories/typeprovider/)
- [F# RFC FS-1034 – Provided Types](https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1034-fsharp-type-providers.md)
