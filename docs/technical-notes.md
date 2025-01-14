# Technical Notes

A type provider is simultaneously a tool and a library. There is a component that runs at compile-time (also called "design-time") and a component
that runs at runtime. F# type providers are hosted by applications using FSharp.Compiler.Service.

First, some terminology: 

* The **Type Provider Run Time Component (TPRTC)** is the component referenced by ``#r`` or ``-r:`` on the command line or other configuration of a host tool, e.g. ``FSharp.Data.dll``.

* The **Type Provider Design Time Component (TPDTC)**, e.g. ``FSharp.Data.DesignTime.dll`` is the DLL that gets loaded into host tools.

* The **host tool** is, for example ``fsc.exe`` or ``fsi.exe``, or some tool hosting ``FSharp.Compiler.Service.dll`` such as ``devenv.exe`` or ``FsAutoComplete.exe``.

## The Type Provider Runtime Component (TPRTC)
  
This contains either a [``TypeProviderAssembly``](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-core-compilerservices-typeproviderassemblyattribute.html) attribute indicating that  this component is also a TPDTC, or ``TypeProviderAssembly("MyDesignTime.dll")`` attribute indicating that the name of the design time component.
  
TPRTCs are normally `netstandard2.0` or above.

## The Type Provider Design Time Component (TPDTC)

The Type Provider Design Time Component (TPDTC) is, for example, ``FSharp.Data.DesignTime.dll``.  

This is the DLL that gets loaded into host tools, and may be the same physical file as the TPRTC. This component includes the ProvidedTypes.fs/fsi files from the type provider SDK.

TPDTC are generally netstandard2.0 or netstandard2.1 components.

See [Loading type providers](https://github.com/Microsoft/visualfsharp/issues/3736) for the rules to find TPDTC components.

## Naming Conventions

The following guidance extends https://fsharp.github.io/2014/09/19/fsharp-libraries.html.

* A type provider for a data source or schema format XYZ can often be placed in `FSharp.Data`, e.g. “FSharp.Data.XYZ”.

* A type provider for interoperability can often be placed in`FSharp.Interop`, e.g. “FSharp.Interop.XYZ”.

Good type provider naming examples:

* `FSharp.Text.RegexProvider`
* `FSharp.Azure.StorageTypeProvider`

Here are some examples of existing type providers that aren't too bad (they are clear) but could be renamed to follow the guidelines:

* `ExcelProvider` (better would be `FSharp.Interop.ExcelProvider`)
* `RProvider` (better would be `FSharp.Interop.RProvider`)
* `ApiaryProvider` (better would be `FSharp.Data.ApiaryProvider`)
* `SQLProvider`  (better would be `FSharp.Data.SQLProvider`)
* `DynamicsNAVProvider`  (better would be `FSharp.Interop.DynamicsNAVProvider`)
* `DynamicsCRMProvider`  (better would be `FSharp.Interop.DynamicsCRMProvider`)

## Nuget package layout

The Nuget package layout of a type provider follows these rules:

1. The TPRTCs go under `lib`
2. The TPDTC goes under `typeproviders/fsharp41`
3. ALl dependencies of the TPDTC are bundled with the TPDTC (except the framework and FSharp.Core)

For examples:

```text
lib/net45
    FSharp.Data.dll
lib/netstandard2.0
    MyProvider.dll

typeproviders/fsharp41/netstandard2.0/
    MyProvider.DesignTime.dll // TPDTC
    MyDesignTimeDependency.dll // bundled dependencies of TPDTC
```

## Dependencies

Runtime dependencies are often the same as design time dependencies for simple type providers.  For more complex providers these can be different

* The runtime dependencies are the dependencies of everything in your quotations in your type provider implementation.

* The design dependencies are the dependencies of everything outside the quotations to decide and generate the provided types.

These dependencies are packaged and managed differently 

* The runtime dependencies are normal Nuget package dependencies just like any normal .NET library. For example, if your type provider has Newtonsoft.Json as a runtime
  dependency then your nuget package should list this a normal nuget dependency.

* The design dependencies must all be bundled alongside your design-time DLL.  The design-time component is a component loaded into a tool like Ionide or
  Visual Studio and must be loadable without referencing any other packages.
 
## Lifetime of type provider instantiations

F# type providers are hosted by applications using FSharp.Compiler.Service.
These notes describe the lifetime and typical resource usage of type provider instances for applications that incorporate 
FSharp.Compiler.Service (the host).  

Each time the host application (e.g. devenv.exe) checks a file using type providers (e.g. containing `JsonProvider<"...">`), one or more new TP instantiations may be created, along with subsequent calls to `ApplyStaticArguments`.   

* The F# compiler service doesn't try to cache these (except where it caches the TAST structures that results of checking a file or project).

* Individual type providers may use caching of some kind, returning previously provided types when the type provider is instantiated the same way.  Care should be taken that these caches do not permanently occupy resources

* Under the hood, the majority of resources used by a TP instantiation are those required to "map" the generated types to the referenced assemblies.  To support this, each TP Instantiation creates one ILModuleReader for each referenced assembly. When the compiler is used as a service, the natural (minimal)  lifetime of an ILModuleReader is the same as its TP Instantiation.  The TPSDK may share these resources.

* The natural (i.e. minimal) lifetime of a TP Instantiation and its related objects (ProvidedType ProvidedMethodInfo etc. etc. ) is the same as the TAST structures which refer to these things (TProvidedTypeInfo, MethInfo, FieldInfo from infos.fs).

The lifetime of TAST structures is as long as they are held in the IncrementalBuilder, or you hold on to FSharpCheckFileResults, or FSharpCheckProjectResults, or FSharpAssemblyContents.  

## Explicit construction of code: MakeGenericType, MakeGenericMethod and UncheckedQuotations

Some type providers need to build code via explicit calls to `FSharp.Quotations.Expr.*` rather than via quotation
literals. Frequently, this is needed when code must instantiate generic methods or types.  However, in some cases limitations
of the F# quotations API are reached. 

In these cases, follow these rules

1. Always use `ProvidedTypeBuilder.MakeGenericType(type, typeArguments)` rather than `type.MakeGenericType(typeArguments)`
1. Always use `ProvidedTypeBuilder.MakeGenericMethod(methInfo, methTypeArguments)` rather than `methInfo.MakeGenericType(methTypeArguments)`
1. Where necessary open `open ProviderImplementation.ProvidedTypes.UncheckedQuotations` and make quotation nodes representing calls and other operations using `Expr.CallUnchecked`.

If you don't do this you may get errors like

```text
    The type provider 'FSharp.Configuration.ConfigTypeProvider+FSharpConfigurationProvider' reported an error: Type mismatch when building 'args': invalid parameter for a method or indexer property. Expected 'System.Collections.Generic.IEnumerable`1[System.String]', but received type 'System.Collections.Generic.IEnumerable`1[System.String]'.�Parameter name: receivedType
```

or 

```text
    System.InvalidOperationException: the operation is not valid due to the current state of the object. at System.Reflection.MemberInfo.get_MetadataToken() in f:\dd\ndp\clr\src\BCL\system\reflection\memberinfo.cs:line 65
```

