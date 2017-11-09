[![Issue Stats](http://issuestats.com/github/fsprojects/FSharp.TypeProviders.SDK/badge/issue)](http://issuestats.com/github/fsprojects/FSharp.TypeProviders.SDK)
[![Issue Stats](http://issuestats.com/github/fsprojects/FSharp.TypeProviders.SDK/badge/pr)](http://issuestats.com/github/fsprojects/FSharp.TypeProviders.SDK)

# The F# Type Provider SDK 

The F# Type Provider SDK is two things: 

1. The ``ProvidedTypes.fs`` API files you need to author type providers

2. Documentation and samples on type provider creation

This package is actively seeking contributions.

## Build status


[![Build status (Windows)](https://ci.appveyor.com/api/projects/status/y1c6gs2r0ihog1re)](https://ci.appveyor.com/project/mavnn/fsharp-typeproviders-sdk)  [![Build Status (MacOS, mono)](https://travis-ci.org/fsprojects/FSharp.TypeProviders.SDK.svg?branch=master)](https://travis-ci.org/fsprojects/FSharp.TypeProviders.SDK)

## The ProvidedTypes API - Adding the Files

Building a type provider nearly always starts with adding these files to your project:

* ProvidedTypes.fsi
* ProvidedTypes.fs

If using Paket, you can add code files by direct GitHub references like [this](https://github.com/dsyme/FSharp.Data/blob/e841dde62091a82225b91b2f38b76513dafbfc05/paket.dependencies#L20-L22) and reference the files in a project file like [this](https://github.com/dsyme/FSharp.Data/blob/e841dde62091a82225b91b2f38b76513dafbfc05/src/FSharp.Data.Source.fsproj#L54-L59).


Type providers may be used in projects that generate .NET Standard code or target other .NET Frameworks than that being used to execute the F# compiler. 

## The ProvidedTypes API - A Basic Type Provider

Here is a basic erasing type provider using the Provided Types API:

```fsharp
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection

[<TypeProvider>]
type BasicProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "StaticProperty.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        let myProp = ProvidedProperty("MyProperty", typeof<string>, isStatic = true, getterCode = (fun args -> <@@ "Hello world" @@>))
        myType.AddMember(myProp)
        [myType]

    do
        this.AddNamespace(ns, createTypes())

[<assembly:TypeProviderAssembly>]
do ()
```

## Some unit testing helpers

The SDK includes a file

* ProvidedTypesTesting.fs

which is sometimes incorporated into a type provider to help enable unit testing. For examples of how this is used, see uses of the helpers in the FSharp.Data library such as  
* [``Testing.GenerateProvidedTypeInstantiation``](https://github.com/fsharp/FSharp.Data/blob/f5df4554938138c60af2ec886d5a132883633351/src/TypeProviderInstantiation.fs#L127)
* ``Targets.DotNet45FSharp40Refs()`` to get a suitable set of references for .NET 4.5, F# 4.0 target on a typical Mono/.NET Framework installation 
* [``Testing.FormatProvidedType``](https://github.com/fsharp/FSharp.Data/blob/f5df4554938138c60af2ec886d5a132883633351/src/TypeProviderInstantiation.fs#L171) to get a textual representation of a provided type, used to "snapshot" the full description of expected type generation

Sometimes unit test DLLs incorporate the entire type provider implementation, and sometimes they use InternalsVisibleTo.

The unit testing helpers aren't really an official, documented part of the DK - caveat emptor.

## Technical Notes

### Using Type Providers with .NET SDK 2.0 Projects and ``dotnet build``

See [How to enable type providers with new-style .NET SDK project files, ``dotnet build``, .NET Standard and .NET Core programming](https://github.com/Microsoft/visualfsharp/issues/3303)

### Some Type Provider terminology

* TPRTC - Type Provider Referenced Component, e.g. ``FSharp.Data.dll``. 

  * This is the component referenced by ``#r`` or ``-r:`` on the command line or other confugration of a host tool
  
  * May be the same physical file as the TPDTC.
  
  * Contains either a [``TypeProviderAssembly()``](https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/compilerservices.typeproviderassemblyattribute-class-%5Bfsharp%5D) attribute indicating that  this component is also a TPDTC, or ``TypeProviderAssembly("MyDesignTime.dll")`` attribute indicating that the name of the design time component.
  
  * A type provider package may have multiple such DLLs for different target platforms, e.g. 
  
        lib\net45\FSharp.Data.dll
        lib\netstandard2.0\FSharp.Data.dll

  * TPRTCs are normally .NET Framework 4.x, .NET Standard 2.0 or some portable profile component.  

* TPDTC - Type Provider Design Time Component, e.g. ``FSharp.Data.DesignTime.dll``.  

  * The DLL that gets loaded into host tools.
  
  * May be the same physical file as the TPRTC.
  
  * This component includes the ProvidedTypes.fs/fsi files from the type provider SDK.

  * TPDTCs are currently .NET Framework 4.x.  They can also be .NET Standard 2.0 components, see below

* Host tool - Either ``fsc.exe``, ``fsi.exe`` or some tool hosting ``FSharp.Compiler.Service.dll`` such as ``devenv.exe`` or ``FsAutoComplete.exe``


### How the TPDTC is found and loaded

Currently, host tools look for TPDTC DLLs alongside the TPRTC DLL. For simple type providers, these DLLs are the same. When executing using .NET Framework, the host tool uses ``Assembly.LoadFrom`` to load this component.

See [Type provider design-time DLLs should be chosen more appropriately](https://github.com/Microsoft/visualfsharp/issues/3736) for a proposal to change the rules to allow TPDTC components to be found more usefully, and in particular for different TPDTC components to be loaded depending on the execution environment of the host tooling.

### Making a .NET Standard 2.0 TPDTC

It will be increasingly common to make type providers where the TPDTC is a .NET Standard 2.0 component. In the very simplest case,  there will just be one  happy .NET Standard 2.0 component ``MyTypeProvider.dll`` acting as both the TPDTC and TPRTC.  Such a type provider will eventually be loadable into all F# tooling.

However, today, for a TPDTC to be .NET Standard 2.0, it must be loadable into host tools using .NET Framework 4.6.1 or Mono 5.x, the most common platforms for execution of F# tooling. Because .NET Framework 4.6.1 doesn't _fully_ support .NET Standard 2.0, this can only be done if the TPDTC ships alongside some facade DLLs.  Currently the following facade DLLs are needed alongside the TPDTC:

```
    <!-- These files are the facades necessary to run .NET Standard 2.0 components on .NET Framweork 4.6.1 (.NET Framework 4.7 will -->
    <!-- come with these facades included). Because the type provider is a .NET Standard 2.0 component, the deployment of the type -->
    <!--  provider must include these facade DLLs if it is to run hosted inside an F# compiler executing using  .NET Framework 4.6.1 or Mono 5.0. -->
    <!-- -->
    <!-- We are not yet sure if the presence of these files will prevent an otherwise .NET Standard 2.0 type provider running inside a -->
    <!-- F# compiler executing using .NET CoreApp 2.0, as until recently F# compilers running using .NET CoreApp 2.0 do not load type providers correctly. -->
    <None Include="..\..\packages\NETStandard.Library.NETFramework\build\net461\lib\netstandard.dll">
        <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
    </None>
    <None Include="..\..\packages\NETStandard.Library.NETFramework\build\net461\lib\System.Reflection.dll">
        <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
    </None>
    <None Include="..\..\packages\NETStandard.Library.NETFramework\build\net461\lib\System.Runtime.dll">
        <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
    </None>
```



## Resources

For advice on how to get started building a type provider, check out:

 - [Type Providers from the ground up](http://blog.mavnn.co.uk/type-providers-from-the-ground-up/)
 - (and the [follow up posts](http://blog.mavnn.co.uk/blog/categories/typeprovider/))
 - [The MSDN Tutorial](http://msdn.microsoft.com/en-us/library/hh361034.aspx). The code in this package replaces the code from the sample pack it mentions.


## Support and community

 - If you have a question about `FSharp`, ask at StackOverflow and [mark your question with the `f#` tag](http://stackoverflow.com/questions/tagged/f%23). 
 - If you want to submit a bug, a feature request or help with fixing bugs then look at [issues](https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues).
 - To discuss more general issues about F# Type Providers SDK, its goals and other open-source F# projects, join the [fsharp-opensource mailing list](http://groups.google.com/group/fsharp-opensource)

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

 [1]: https://github.com/fsprojects/FSharp.TypeProviders.SDK/blob/master/LICENSE.md


## Maintainer(s)

- [@mavnn](https://github.com/mavnn)
- [@ovatsus](https://github.com/ovatsus)
- [@dsyme](https://github.com/dsyme)

The default maintainer account for projects under "fsprojects" is [@fsprojectsgit](https://github.com/fsprojectsgit) - F# Community Project Incubation Space (repo management)
