
# The F# Type Provider SDK

The F# Type Provider SDK is two things:

1. The ``ProvidedTypes.fs`` API files you need to author type providers

2. Documentation and samples on type provider creation

[![Build Status](https://github.com/fsprojects/FSharp.TypeProviders.SDK/workflows/Build%20and%20Test/badge.svg?branch=master)](https://github.com/fsprojects/FSharp.TypeProviders.SDK/actions?query=branch%3Amaster)


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

## The ProvidedTypes API - A Basic Type Provider

When you instantiate the template above you get basic erasing and geneative type providers.

## Some unit testing helpers

The SDK includes a file

* ProvidedTypesTesting.fs

which is sometimes incorporated into a type provider to help enable unit testing. For examples of how this is used, see uses of the helpers in the FSharp.Data library such as  
* [``Testing.GenerateProvidedTypeInstantiation``](https://github.com/fsharp/FSharp.Data/blob/f5df4554938138c60af2ec886d5a132883633351/src/TypeProviderInstantiation.fs#L127)
* ``Targets.DotNet45FSharp40Refs()`` to get a suitable set of references for .NET 4.5, F# 4.0 target on a typical Mono/.NET Framework installation 
* [``Testing.FormatProvidedType``](https://github.com/fsharp/FSharp.Data/blob/f5df4554938138c60af2ec886d5a132883633351/src/TypeProviderInstantiation.fs#L171) to get a textual representation of a provided type, used to "snapshot" the full description of expected type generation

Sometimes unit test DLLs incorporate the entire type provider implementation, and sometimes they use InternalsVisibleTo.

The unit testing helpers aren't really an official, documented part of the DK - caveat emptor.


## Examples

See examples the [`examples`](examples) directory.

* ComboProvider: the TPDTC and TPRTC are combined together in one assembly which is a single `netstandard2.0` component 
* BasicProvider: the TPDTC and TPRTC are each available as both `netstandard2.0` and `.NET 4.x` components


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

## Technical Notes

### Using Type Providers with ``dotnet build``

Correctly updated type providers can be used with either the `dotnet` toolchain (.NET SDK tools which executes using .NET Core) or `msbuild` (traditional .NET Framework/Mono) toolchain. 

* For .NET SDK 2.1.4 and before, see [How to enable type providers with new-style .NET SDK project files, ``dotnet build``, .NET Standard and .NET Core programming](https://github.com/Microsoft/visualfsharp/issues/3303)

* For .NET SDK 2.1.100 and above, you can either use type providers specifically updated to work with the .NET SDK, or use the same [workaround](https://github.com/Microsoft/visualfsharp/issues/3303).

### Updating a Type Provider to be suitable for use with the .NET SDK

This short guide assumes 
1. You have a type provider with separate TPDTC and TPRTC components (see below if you don't know what those are)
2. Some of your code might have dependencies on .NET Framework functionality
3. You want your type provider to be usable with both the `dotnet` toolchain (.NET SDK tools which executes using .NET Core) or `msbuild` (traditional .NET Framework/Mono) toolchain.
4. You want your type provider to be usable for all of .NET Standard, .NET Core and .NET Framework programming (if possible)


Here is a guide to the steps to perform:

1. Use .NET SDK 2.1.100-preview-007363 or above.  Forget .NET SDK 2.1.4 and before.

1. If using Visual Studio, then use Visual Studio 2017 15.6 and above.  Your type provider will still be usable with previous versionss, we'll get to that, but for now assume 15.6

2. First switch to use .NET SDK project files, compiling them with `msbuild` 

3. Update to the latest ProvidedTypes.fs/fsi from this project

   * If making a generative type provider, check your `isErased` flags and use of `ProvidedAssembly` fragments, see [this example](https://github.com/dsyme/FSharp.TypeProviders.SDK/blob/36b9f59c8f25d93adc11851affbcf71fcf671ef1/examples/BasicProvider.DesignTime/BasicProvider.Provider.fs#L68)
   
   * If your TPDTC contains a copy of your TPRTC implementation then use [`assemblyReplacementMap`](https://github.com/dsyme/FSharp.TypeProviders.SDK/blob/36b9f59c8f25d93adc11851affbcf71fcf671ef1/examples/BasicProvider.DesignTime/BasicProvider.Provider.fs#L12)

4. Work out how much your TPRTC (runtime component) depends on .NET Framework by trying to target `netstandard2.0`.  You may need to use different package references to try this.

   * If your TPRTC **fundamentally** depends on .NET Framework, then you will not be able to use your type provider within projects targeting .NET Core or .NET Standard. Keep targeting your TPRTC at .NET Framework.
   
   * If your TPRTC **partially** depends on .NET Framework, then multi-target the TPRTC to `net45;netstandard2.0` and use `#if NETSTANDARD2_0`
   
   * If your TPRTC **doesn't** depend on .NET Framework, then target the TPRTC to `netstandard2.0`

5. Work out how much of a dependency your TPDTC has on .NET Framework:

   * If the compile-time computations performed by your TPDTC **fundamentally** depend on .NET Framework, then your type provider will not be usable with the .NET SDK toolchain without using [the workaround](https://github.com/Microsoft/visualfsharp/issues/3303))
   
   * If the TPDTC **partially** depends on .NET Framework, then multi-target the TPDTC to `net45;netcoreapp2.0` and use `#if NETCOREAPP3_1`
   
   * If the TPDTC **doesn't** depend on .NET Framework, then target the TPDTC to `netstandard2.0`
   
   Beware that your TPDTC might have a **false** dependency induced by including a copy of the TPRTC source code into the TPDTC (which is generally a good technique). It is likely such a dependency can be removed by selectively stubbing out runtime code using a `IS_DESIGNTIME` define.  The TPDTC only needs access to an "API" that has the same logical shape as the TPRTC in order to generate code and types.  That "API" is then translated to match the target references assemblies in an actual compilation.

7. Modify your project to copy the design-time DLLs into the right place, e.g. see [this example](https://github.com/dsyme/FSharp.TypeProviders.SDK/blob/36b9f59c8f25d93adc11851affbcf71fcf671ef1/examples/BasicProvider/BasicProvider.fsproj#L16)

8. Have your test projects multi-target to `netcoreapp2.0; net471`

9. Use  `dotnet build` to build instead of `msbuild`

   * If any of your projects targeting .NET 4.x so they will compile with `dotnet` on Linux/OSX when Mono is installed, then include [netfx.props](https://github.com/dsyme/FSharp.TypeProviders.SDK/blob/36b9f59c8f25d93adc11851affbcf71fcf671ef1/examples/BasicProvider/BasicProvider.fsproj#L4) in the project and project file

10. Modify your nuget package layout as described below.


### Nuget package layouts you should use

The typical nuget package layout for a provider that has **combined** design-time and runtime components is:

    lib/netstandard2.0
        MyProvider.dll // TPRTC and TPDTC
        netstandard.dll // Extra facade, see below
        System.Runtime.dll // Extra facade, see below
        System.Reflection.dll // Extra facade, see below

The typical nuget package layout for a provider that has separate design-time and runtime components is:

    lib/net45/
        MyProvider.dll // TPRTC
        MyProvider.DesignTime.dll // .NET 4.x TPDTC alongside TPRTC (only needed for legacy loading: VS2015, Mono 5.12, VS2017 before 15.6)
    
    typeproviders/fsharp41/
        net45/
            MyProvider.DesignTime.dll // .NET 4.x TPDTC
    
        netcoreapp2.0/
            MyProvider.DesignTime.dll // .NET Core App 2.0 TPDTC

It is important that the design-time assemblies you use (if any) are not loaded at runtime. To ensure this does not happen, when you distribute a Nuget package for your Type Provider you _must_ provide an explicit list of project references for consumers to include. If you do not, every assembly you publish in the package will be included, which can lead to design-type only references being loaded at runtime.  To reference only a subset of assemblies, see the [Nuget documentation](https://docs.microsoft.com/en-us/nuget/reference/nuspec#explicit-assembly-references) or the [Paket documentation](https://fsprojects.github.io/Paket/template-files.html#References).

That is, an explicit `.nuspec` file will be needed with an explicit `<references>` node (so that only the TPRTC gets added as a reference), see [this example](https://github.com/baronfel/FSharp.Data/blob/e0f133e6e79b4a41365776da51332227dccff9ab/nuget/FSharp.Data.nuspec).

### Lifetime of type provider instantiations

F# type providers are hosted by applications using FSharp.Compiler.Service.
These notes describe the lifetime and typical resource usage of type provider instances for applications that incorporate 
FSharp.Compiler.Service (the host).  Most documentation on the compiler as a service can be found at http://github.com/fsharp/FSharp.Compiler.Service.

Each time the host application (e.g. devenv.exe) checks a file using type providers (e.g. containing `JsonProvider<"...">`), one or more new TP instantiations may be created, along with subsequent calls to `ApplyStaticArguments`.   

* The F# compiler service doesn't try to cache these (except where it caches the TAST structures that results of checking a file or project).

* Individual type providers may use caching of some kind, returning previous provided types when the type provider is instantiated the same way.  Care should be taken that these caches do not permanently occupy resources

* Under the hood, the majority of resources used by a TP instantiation are those required to "map" the generated types to the referenced assemblies.  To support this, each TP Instantiation creates one ILModuleReader for each referenced assembly. When the compiler is used as a service, the natural (minimal)  lifetime of a ILModuleReader is the same as its TP Instanatiation.  The TPSDK may share these resources.

* The natural (i.e. minimal) lifetime of a TP Instantiation and its related objects (ProvidedType ProvidedMethodInfo etc. etc. ) is the same as the TAST structures which refer these things (TProvidedTypeInfo, MethInfo, FieldInfo from infos.fs).

The lifetime of TAST structures is as long as they are held in the IncrementalBuilder, or you hold on to FSharpCheckFileResults, or FSharpCheckProjectResults, or FSharpAssemblyContents.  




### FAQ

#### How do I debug execution of a type provider when using .NET Framework tools on Windows?

1. Capture output of `msbuild -v:n` in `args.txt` and trim out the rubbish leaving just the command line arguments to the F# compiler, usually starting with `-o:...`

2. Run an explicit invocation of the compiler using this, checking that your failures still happen

       fsc.exe @args.txt

   Then debug that invocation using

       devenv /debugexe fsc.exe @args.txt

   If your failures only happen in the IDE then use `devenv /debugexe devenv.exe MyProj.fsproj`, set debug type to  ".NET Framework 4.0" and launch F5. Likewise if your failures only happen in F# Interactive then use `devenv /debugexe fsi.exe MyProj.fsproj`.

   Set first-catch exception handling (Ctrl-Alt-E, select all CLR exceptions) and set Just My Code off

#### How do I debug execution of a type provider when using .NET Core tools on Windows?

One approach:

1. Capture output of `dotnet build -v:n` in `args.txt` and trim out the rubbish leaving just the command line arguments to the F# compiler, usually starting with `-o:...`

2. Run an explicit invocation of the compiler using:

       "c:\Program Files\dotnet\dotnet.exe" "C:\Program Files\dotnet\sdk\2.1.403\FSharp\fsc.exe" @args.txt

   Then debug that invocation using

       devenv /debugexe "c:\Program Files\dotnet\dotnet.exe" "C:\Program Files\dotnet\sdk\2.1.403\FSharp\fsc.exe" @args.txt

   Be careful to make sure Visual Studio debugging type is set to ".NET Core" (right click properties on dotnet and set debug type)

   Set first-catch exception handling (Ctrl-Alt-E, select all CLR exceptions) and set Just My Code off.

### Some Type Provider terminology

* TPRTC - Type Provider Run Time Component, e.g. ``FSharp.Data.dll``. 

  * This is the component referenced by ``#r`` or ``-r:`` on the command line or other configration of a host tool
  
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
    <!-- These files are the facades necessary to run .NET Standard 2.0 components on .NET Framework 4.6.1 (.NET Framework 4.7 will -->
    <!-- come with these facades included). Because the type provider is a .NET Standard 2.0 component, the deployment of the type -->
    <!--  provider must include these facade DLLs if it is to run hosted inside an F# compiler executing using  .NET Framework 4.6.1 or Mono 5.0. -->
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

### Explicit construction of code: MakeGenericType, MakeGenericMethod and UncheckedQuotations

Some type providers need to build code via explicit calls to `FSharp.Quotations.Expr.*` rather than via quotation
literals. Frequently, this is needed when code must instantiate generic methods or types.  However, in some cases limitations
of the F# quotations API are reached. 

In these cases, follow these rules

1. Always use `ProvidedTypeBuilder.MakeGenericType(type, typeArguments)` rather than `type.MakeGenericType(typeArguments)`
1. Always use `ProvidedTypeBuilder.MakeGenericMethod(methInfo, methTypeArguments)` rather than `methInfo.MakeGenericType(methTypeArguments)`
1. Where necessary open `open ProviderImplementation.ProvidedTypes.UncheckedQuotations` and make quotation nodes representing calls and other operations using `Expr.CallUnchecked`.

If you don't do this you may get errors like

    The type provider 'FSharp.Configuration.ConfigTypeProvider+FSharpConfigurationProvider' reported an error: Type mismatch when building 'args': invalid parameter for a method or indexer property. Expected 'System.Collections.Generic.IEnumerable`1[System.String]', but received type 'System.Collections.Generic.IEnumerable`1[System.String]'.�Parameter name: receivedType

or 

    System.InvalidOperationException: the operation is not valid due to the current state of the object. at System.Reflection.MemberInfo.get_MetadataToken() in f:\dd\ndp\clr\src\BCL\system\reflection\memberinfo.cs:line 65

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

The library is available under the MIT License. For more information see the [License file][1] in the GitHub repository.

 [1]: https://github.com/fsprojects/FSharp.TypeProviders.SDK/blob/master/LICENSE.md


## Maintainer(s)

- [@mavnn](https://github.com/mavnn)
- [@ovatsus](https://github.com/ovatsus)
- [@dsyme](https://github.com/dsyme)

The default maintainer account for projects under "fsprojects" is [@fsprojectsgit](https://github.com/fsprojectsgit) - F# Community Project Incubation Space (repo management)
