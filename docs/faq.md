# FAQ

## How do I debug execution of a type provider when using .NET Framework tools on Windows?

1. Capture output of `msbuild -v:n` in `args.txt` and trim out the rubbish leaving just the command line arguments to the F# compiler, usually starting with `-o:...`

2. Run an explicit invocation of the compiler using this, checking that your failures still happen

       fsc.exe @args.txt

   Then debug that invocation using

       devenv /debugexe fsc.exe @args.txt

   If your failures only happen in the IDE then use `devenv /debugexe devenv.exe MyProj.fsproj`, set debug type to  ".NET Framework 4.0" and launch F5. Likewise if your failures only happen in F# Interactive then use `devenv /debugexe fsi.exe MyProj.fsproj`.

Set first-catch exception handling (Ctrl-Alt-E, select all CLR exceptions) and set Just My Code off

## A dependency of my type provider is not loading, what do I do?

For example, let's say you have this error in your test project:

```text
2>E:\GitHub\admin\joe-provider\test\Joe.Test\ProviderTests.fs(8,10): error FS3033: The type provider 'Joe.Provider.JoeProvider' reported an error: Could not load file or assembly 'Newtonsoft.Json, Version=12.0.0.0, Culture=neutral, PublicKeyToken=30ad4fe6b2a6aeed'. The system cannot find the file specified. [E:\GitHub\dsyme\joe-provider\test\Joe.Test\Joe.Test.fsproj]
```

Here your test project is referencing your provider project, and your type provider has a dependency on `Newtonsoft.Json.dll`. To see what's going on, run

```text
dotnet build -v:n
```

In the compilation of your test project you will see something like this:

```text
    C:\Program Files\dotnet\dotnet.exe "C:\Program Files\dotnet\sdk\3.1.401\FSharp\fsc.exe"
         -o:obj\Debug\net5.0\Joe.Test.dll
         ...
         -r:E:\GitHub\admin\joe-provider\src\Joe.Provider\bin\Debug\netstandard2.0\Joe.Provider.dll
         ...
```

1. The tool `fsc.exe` is trying to load the type provider but a dependency is not found.  As mentioned above, all dependencies must be packaged
   alongside your design time component.  For example, adding

   ```text
   <Content Include="..\..\packages\Newtonsoft.Json\lib\netstandard2.0\Newtonsoft.Json.dll" CopyToOutputDirectory="PreserveNewest" />
   ```

   will include the component and unblock you.  However, you will need to be careful to make sure this component is laid down in the right place in your nuget
   package, see the instructions above for what the final layout of the nuget package should be.

2. When making type providers whose design-time components have dependencies, you should always use a "split" type provider that separates the design-time and runtime components.

## How do I debug execution of a type provider when using .NET Core tools on Windows?

One approach:

1. Capture output of `dotnet build -v:n` in `args.txt` and trim out the rubbish leaving just the command line arguments to the F# compiler, usually starting with `-o:...`

2. Run an explicit invocation of the compiler using:

   ```text
   "c:\Program Files\dotnet\dotnet.exe" "C:\Program Files\dotnet\sdk\2.1.403\FSharp\fsc.exe" @args.txt
   ```

   Then debug that invocation using

   ```text
   devenv /debugexe "c:\Program Files\dotnet\dotnet.exe" "C:\Program Files\dotnet\sdk\2.1.403\FSharp\fsc.exe" @args.txt
   ```

   Be careful to make sure Visual Studio debugging type is set to ".NET Core" (right click properties on dotnet and set debug type)

   Set first-catch exception handling (Ctrl-Alt-E, select all CLR exceptions) and set Just My Code off.

## Some Type Provider terminology

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

## How the TPDTC is found and loaded

Currently, host tools look for TPDTC DLLs alongside the TPRTC DLL. For simple type providers, these DLLs are the same. When executing using .NET Framework, the host tool uses ``Assembly.LoadFrom`` to load this component.

See [Type provider design-time DLLs should be chosen more appropriately](https://github.com/Microsoft/visualfsharp/issues/3736) for a proposal to change the rules to allow TPDTC components to be found more usefully, and in particular for different TPDTC components to be loaded depending on the execution environment of the host tooling.

## Explicit construction of code: MakeGenericType, MakeGenericMethod and UncheckedQuotations

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

## Some unit testing helpers

The SDK includes a file

* ProvidedTypesTesting.fs

which is sometimes incorporated into a type provider to help enable unit testing. For examples of how this is used, see uses of the helpers in the FSharp.Data library such as:

* [``Testing.GenerateProvidedTypeInstantiation``](https://github.com/fsharp/FSharp.Data/blob/f5df4554938138c60af2ec886d5a132883633351/src/TypeProviderInstantiation.fs#L127)
* ``Targets.DotNetStandard20FSharpRefs`` to get a suitable set of references for .NET Standard plus the running FSharp.Core
* [``Testing.FormatProvidedType``](https://github.com/fsharp/FSharp.Data/blob/f5df4554938138c60af2ec886d5a132883633351/src/TypeProviderInstantiation.fs#L171) to get a textual representation of a provided type, used to "snapshot" the full description of expected type generation

Sometimes unit test DLLs incorporate the entire type provider implementation, and sometimes they use InternalsVisibleTo.

The unit testing helpers aren't really an official, documented part of the SDK - caveat emptor.
