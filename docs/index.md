# The F# Type Provider SDK

## Getting Started

To create a type provider use:

```text
dotnet new -i FSharp.TypeProviders.Templates
dotnet new typeprovider -n LemonadeProvider -lang F#
```

The template uses paket to acquire the files of the latest published type provider SDK.

```text
cd LemonadeProvider

dotnet tool restore
dotnet paket update
dotnet build -c release

dotnet test -c release
```

The type provider also contains the logic necessary to package the type provider:

```text
dotnet paket pack nuget --version 0.0.1
```

## Examples

See examples the [`examples`](https://github.com/fsprojects/FSharp.TypeProviders.SDK/tree/master/examples) directory.

## Debugging Type Providers

See [Debugging Type Providers](debugging.html)

## Technical notes

See [Technical Notes](technical-notes.html)

## Experimental: Using the NuGet Package

You can use the TypeProvider SDK via a nuget package instead of by source inclusion. To enable this, use settings like these in the design-time component:

```xml
    <PackageReference Include="FSharp.TypeProviders.SDK" Version="7.0.3">
      <PrivateAssets>all</PrivateAssets>
    </PackageReference>
    <PackageReference Update="FSharp.Core" Version="6.0.5">
      <ExcludeAssets>all</ExcludeAssets>
    </PackageReference>
```

The runtime-component should not have a reference to FSharp.TypeProviders.SDK.

## Resources

- [Debugging Type Providers](debugging.html)
- [Technical Notes](technical-notes.html)
- [Type Providers from the ground up](https://blog.mavnn.co.uk/type-providers-from-the-ground-up/) and the [follow up posts](https://blog.mavnn.co.uk/blog/categories/typeprovider/)
