[![Issue Stats](http://issuestats.com/github/fsprojects/FSharp.TypeProviders.StarterPack/badge/issue)](http://issuestats.com/github/fsprojects/FSharp.TypeProviders.StarterPack)
[![Issue Stats](http://issuestats.com/github/fsprojects/FSharp.TypeProviders.StarterPack/badge/pr)](http://issuestats.com/github/fsprojects/FSharp.TypeProviders.StarterPack)

# F# Type Provider Starter Pack [![NuGet Status](http://img.shields.io/nuget/v/FSharp.TypeProviders.StarterPack.svg?style=flat)](https://www.nuget.org/packages/FSharp.TypeProviders.StarterPack/)

The F# Type Provider Starter Pack is two things: 

1. The ``ProvidedTypes.fs`` API files you need to author type providers

2. Documentation and samples on type provider creation

This package is actively seeking contributions.   We are aiming for documentation
(the plan is a GitHub page setup similar to [FSharp.Data](http://fsharp.github.io/FSharp.Data)) with
examples of all the most common features of Type Providers that people will want to use (basic erased type
generation, parameterized providers, full generated types, seperating design and runtime how's and why's, etc). 
It will also become the main place for improvements and additions to the ProvidedTypes code.

## Build status


[![Build status (Windows)](https://ci.appveyor.com/api/projects/status/y1c6gs2r0ihog1re)](https://ci.appveyor.com/project/mavnn/fsharp-typeproviders-starterpack)  [![Build Status (MacOS, mono)](https://travis-ci.org/fsprojects/FSharp.TypeProviders.StarterPack.svg?branch=master)](https://travis-ci.org/fsprojects/FSharp.TypeProviders.StarterPack)

## The ProvidedTypes API - Adding the Files

Building a type provider nearly always starts with adding these files to your project:

* ProvidedTypes.fsi
* ProvidedTypes.fs

The [Starter Pack NuGet package](https://www.nuget.org/packages/FSharp.TypeProviders.StarterPack) contains both these files as well as a set of debugging helpers, and when you install
it, it should add them all to your F# project. It's probably best not to modify the files after adding them as
upgrades to the package will ask to replace the previous versions - either submit changes back to this project
or shadow the relevant functions in a seperate file.

If using Paket, you can also add code files by direct GitHub references.


## The ProvidedTypes API - Cross-Targeting Type Providers

Type providers may be used in projects that generate portable code or target other .NET Frameworks than
that being used by the F# compiler. To convert an erasing
type provider to a cross-targeting erasing type provider, add the following source files to your project:

* AssemblyReader.fs
* AssemblyReaderReflection.fs
* ProvidedTypesContext.fs

Then add 

```fsharp
let ctxt = ProvidedTypesContext.Create(config)
```

to your code and always create provided entities using this ``ctxt`` object:

```fsharp
let myType = ctxt.ProvidedTypeDefinition(asm, ns, "MyType", typeof<obj>)
```

This is shown in the example below.

## The ProvidedTypes API - A Basic Type Provider

Here is a basic erasing type provider using the Provided Types API:

```fsharp
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection

[<TypeProvider>]
type BasicProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "StaticProperty.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let ctxt = ProvidedTypesContext.Create(config)

    let createTypes () =
        let myType = ctxt.ProvidedTypeDefinition(asm, ns, "MyType", typeof<obj>)
        let myProp = ctxt.ProvidedProperty("MyProperty", typeof<string>, IsStatic = true, getterCode = (fun args -> <@@ "Hello world" @@>))
        myType.AddMember(myProp)
        [myType]

    do
        this.AddNamespace(ns, createTypes())

[<assembly:TypeProviderAssembly>]
do ()
```

## Resources

For advice on how to get started building a type provider, check out:

 - [Type Providers from the ground up](http://blog.mavnn.co.uk/type-providers-from-the-ground-up/)
 - (and the [follow up posts](http://blog.mavnn.co.uk/blog/categories/typeprovider/))
 - [The MSDN Tutorial](http://msdn.microsoft.com/en-us/library/hh361034.aspx). The code in this package replaces the code from the sample pack it mentions.


## Support and community

 - If you have a question about `FSharp`, ask at StackOverflow and [mark your question with the `f#` tag](http://stackoverflow.com/questions/tagged/f%23). 
 - If you want to submit a bug, a feature request or help with fixing bugs then look at [issues](https://github.com/mavnn/FSharp.TypeProviders.StarterPack/issues).
 - To discuss more general issues about F# Type Providers Starter Pack, its goals and other open-source F# projects, join the [fsharp-opensource mailing list](http://groups.google.com/group/fsharp-opensource)

## Building

Use

```shell
build.sh RunTests
```

or

```shell
build.cmd RunTests
```

## Library license

The library is available under Apache 2.0. For more information see the [License file][1] in the GitHub repository.

 [1]: https://github.com/mavnn/FSharp.TypeProviders.StarterPack/blob/master/LICENSE.md


## Maintainer(s)

- [@mavnn](https://github.com/mavnn)
- [@ovatsus](https://github.com/ovatsus)
- [@dsyme](https://github.com/dsyme)

The default maintainer account for projects under "fsprojects" is [@fsprojectsgit](https://github.com/fsprojectsgit) - F# Community Project Incubation Space (repo management)
