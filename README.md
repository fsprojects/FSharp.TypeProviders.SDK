[![Issue Stats](http://issuestats.com/github/fsprojects/FSharp.TypeProviders.StarterPack/badge/issue)](http://issuestats.com/github/fsprojects/FSharp.TypeProviders.StarterPack)
[![Issue Stats](http://issuestats.com/github/fsprojects/FSharp.TypeProviders.StarterPack/badge/pr)](http://issuestats.com/github/fsprojects/FSharp.TypeProviders.StarterPack)

# F# Type Provider Starter Pack [![NuGet Status](http://img.shields.io/nuget/v/FSharp.TypeProviders.StarterPack.svg?style=flat)](https://www.nuget.org/packages/FSharp.TypeProviders.StarterPack/)

The F# Type Provider Starter Pack is two things: a code only NuGet package with the code
files you'll need to get you started in type provider creation, and a repository of tutorials
that will (hopefully, over time) answer all your questions about how to build type providers,
best practices and general hints and tips.

This package is still currently pre-release, and actively seeking contributions towards documentation
(the plan is a GitHub page setup similar to [FSharp.Data](http://fsharp.github.io/FSharp.Data)) with
examples of all the most common features of Type Providers that people will want to use (basic erased type
generation, parameterized providers, full generated types, seperating design and runtime how's and why's, etc). 
It will also become the main place for improvements and additions to the ProvidedTypes code.

Once it has reached an acceptable standard of documentation/helpfulness ownership will be transferred to the
F# Foundation.

## Build status

### Windows (.net)

[![Build status](https://ci.appveyor.com/api/projects/status/y1c6gs2r0ihog1re)](https://ci.appveyor.com/project/mavnn/fsharp-typeproviders-starterpack)

### MacOS (mono)

[![Build Status](https://travis-ci.org/fsprojects/FSharp.TypeProviders.StarterPack.svg?branch=master)](https://travis-ci.org/fsprojects/FSharp.TypeProviders.StarterPack)

## Documentation 

Building a type provider nearly always starts with adding the `ProvidedTypes.fs` and `ProvidedTypes.fsi`. The
[Starter Pack NuGet package](https://www.nuget.org/packages/FSharp.TypeProviders.StarterPack) contains both these files as well as a set of debugging helpers, and when you install
it, it should add them all to your F# project. It's probably best not to modify the files after adding them as
upgrades to the package will ask to replace the previous versions - either submit changes back to this project
or shadow the relevant functions in a seperate file.

For advice on how to get started building a type provider, check out:

 - [Type Providers from the ground up](http://blog.mavnn.co.uk/type-providers-from-the-ground-up/)
 - (and the [follow up posts](http://blog.mavnn.co.uk/blog/categories/typeprovider/))
 - [The MSDN Tutorial](http://msdn.microsoft.com/en-us/library/hh361034.aspx). The code in this package replaces the code from the sample pack it mentions.


## Support and community

 - If you have a question about `FSharp`, ask at StackOverflow and [mark your question with the `f#` tag](http://stackoverflow.com/questions/tagged/f%23). 
 - If you want to submit a bug, a feature request or help with fixing bugs then look at [issues](https://github.com/mavnn/FSharp.TypeProviders.StarterPack/issues).
 - To discuss more general issues about F# Type Providers Starter Pack, its goals and other open-source F# projects, join the [fsharp-opensource mailing list](http://groups.google.com/group/fsharp-opensource)

## Building

- This repository contains no compiled code.

## Library license

The library is available under Apache 2.0. For more information see the [License file][1] in the GitHub repository.

 [1]: https://github.com/mavnn/FSharp.TypeProviders.StarterPack/blob/master/LICENSE.md


## Maintainer(s)

- [@mavnn](https://github.com/mavnn)
- [@ovatsus](https://github.com/ovatsus)

The default maintainer account for projects under "fsprojects" is [@fsprojectsgit](https://github.com/fsprojectsgit) - F# Community Project Incubation Space (repo management)
