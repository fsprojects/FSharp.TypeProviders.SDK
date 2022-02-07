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

## Resources

For advice on how to get started building a type provider, check out:

 - [Debugging Type Providers](debugging.html)
 - [Technical Notes](technical-notes.html)
 - [Type Providers from the ground up](https://blog.mavnn.co.uk/type-providers-from-the-ground-up/) and the [follow up posts](https://blog.mavnn.co.uk/blog/categories/typeprovider/)


