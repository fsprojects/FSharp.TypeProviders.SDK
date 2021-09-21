# Technical Notes

## Using Type Providers with ``dotnet build``

Correctly updated type providers can be used with either the `dotnet` toolchain (.NET SDK tools which executes using .NET Core) or `msbuild` (traditional .NET Framework/Mono) toolchain.

## Nuget package layouts you should use

The typical nuget package layout for a very simple provider that has **combined** design-time and runtime components and **no dependencies** is:

    lib/netstandard2.0
        MyProvider.dll // acts as both TPRTC and TPDTC

The typical nuget package layout for a provider that has separate design-time and runtime components is like this.  You should also likely use this if your type provider has any extra dependencies.

    lib/netstandard2.0
        MyProvider.dll // TPRTC
    
    typeproviders/fsharp41/
        netstandard2.0/
            MyProvider.DesignTime.dll // TPDTC
            MyDesignTimeDependency.dll // bundled dependencies of TPDTC

Runtime dependencies are often the same as design time dependencies for simple type providers.  For more complex providers these can be different

* The runtime dependencies are the dependencies of everything in your quotations in your type provider implementation.

* The design dependencies are the dependencies of everything outside the quotations to decide and generate the provided types.

These dependencies are packaged and managed differently 

* The runtime dependencies are normal nuget package dependencies just like any normal .NET library. For example, if your type provider has Newtonsoft.Json as a runtime
  dependency then your nuget package should list this a normal nuget dependency.

* The design dependencies must all be bundled alongside your design-time DLL.  The design-time component is a component loaded into a tool like Ionide or
  Visual Studio and must be loadable without referencing any other packages.
 

## Lifetime of type provider instantiations

F# type providers are hosted by applications using FSharp.Compiler.Service.
These notes describe the lifetime and typical resource usage of type provider instances for applications that incorporate 
FSharp.Compiler.Service (the host).  

Each time the host application (e.g. devenv.exe) checks a file using type providers (e.g. containing `JsonProvider<"...">`), one or more new TP instantiations may be created, along with subsequent calls to `ApplyStaticArguments`.   

* The F# compiler service doesn't try to cache these (except where it caches the TAST structures that results of checking a file or project).

* Individual type providers may use caching of some kind, returning previous provided types when the type provider is instantiated the same way.  Care should be taken that these caches do not permanently occupy resources

* Under the hood, the majority of resources used by a TP instantiation are those required to "map" the generated types to the referenced assemblies.  To support this, each TP Instantiation creates one ILModuleReader for each referenced assembly. When the compiler is used as a service, the natural (minimal)  lifetime of a ILModuleReader is the same as its TP Instanatiation.  The TPSDK may share these resources.

* The natural (i.e. minimal) lifetime of a TP Instantiation and its related objects (ProvidedType ProvidedMethodInfo etc. etc. ) is the same as the TAST structures which refer these things (TProvidedTypeInfo, MethInfo, FieldInfo from infos.fs).

The lifetime of TAST structures is as long as they are held in the IncrementalBuilder, or you hold on to FSharpCheckFileResults, or FSharpCheckProjectResults, or FSharpAssemblyContents.  

