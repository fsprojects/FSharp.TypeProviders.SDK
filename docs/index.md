# The F# Type Provider SDK

## Getting Started

To create a type provider use:

    dotnet new -i FSharp.TypeProviders.Templates
    dotnet new typeprovider -n LemonadeProvider -lang F#

The template uses paket to acquire the files of the latest published type provider SDK.

    cd LemonadeProvider

    dotnet tool restore
    dotnet paket update
    dotnet build -c release

    dotnet test -c release

The type provider also contains the logic necessary to package the type provider:

    dotnet paket pack nuget --version 0.0.1

## Examples

See examples the [`examples`](https://github.com/fsprojects/FSharp.TypeProviders.SDK/tree/master/examples) directory.

## Type Provider Naming Conventions

A type provider is simultaneously a tool and a library.  The existing guidance is as follows, from https://fsharp.github.io/2014/09/19/fsharp-libraries.html.

> Likewise, a type provider for a data source or schema format XYZ should normally be placed in “FSharp.Data”, e.g. “FSharp.Data.XYZ”.

Good type provider naming examples:

* `FSharp.Text.RegexProvider`
* `FSharp.Azure.StorageTypeProvider`

Existing type providers that don't quite follow the guidelines but which are ok:

* `FSharp.Data`

Here are some examples of existing type providers that aren't too bad (they are clear) but could be renamed to follow the guidelines:

* `ExcelProvider` (better would be `FSharp.Interop.ExcelProvider`)
* `RProvider` (better would be `FSharp.Interop.RProvider`)
* `ApiaryProvider` (better would be `FSharp.Data.ApiaryProvider`)
* `SQLProvider`  (better would be `FSharp.Data.SQLProvider`)
* `DynamicsNAVProvider`  (better would be `FSharp.Interop.DynamicsNAVProvider`)
* `DynamicsCRMProvider`  (better would be `FSharp.Interop.DynamicsCRMProvider`)

## Resources

For advice on how to get started building a type provider, check out:

 - [Tehcnical Notes](technical-notes.html)
 - [FAQ](faq.html)
 - [Type Providers from the ground up](http://blog.mavnn.co.uk/type-providers-from-the-ground-up/)
 - (and the [follow up posts](http://blog.mavnn.co.uk/blog/categories/typeprovider/))
 - [The MSDN Tutorial](http://msdn.microsoft.com/en-us/library/hh361034.aspx). The code in this package replaces the code from the sample pack it mentions.


