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
| [**Quick Start**](quick-start.html) | Install the template, understand the project structure, and build your first erased type provider with a constructor, property, and method. |
| [**Complete Guide**](guide.html) | Erased vs generative providers, testing, debugging, and packaging overview. |
| [**Providing Types, Members, and Features**](providing-types.html) | Static parameters, quotations, generative providers, all member kinds, interfaces, enumerations, nested types, XML docs, custom attributes, units of measure, non-nullability, and abstract classes. |
| [**Packaging**](packaging.html) | NuGet package layout, how `IsFSharpDesignTimeProvider` automates packaging and build-time tool collection, bundling dependencies, and the assembly replacement map. |
| [**Units of Measure**](units-of-measure.html) | SI units, compound units, custom units, and annotating numeric types with `ProvidedMeasureBuilder`. |
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
