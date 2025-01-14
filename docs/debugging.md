# Debugging a Type Provider

Debugging a type provider can be difficult because it is a program run at compile-time and editing-time in host compilation tools including
`fsc.exe`, `devenv.exe` and `FsAutoComplete.exe`.

This article discusses some techniques you can use to debug a type provider when it is run inside these different tools.

## How do I debug the execution of a type provider when using .NET SDK tools?

To debug the use of a type provider inside the `dotnet` toolchain, you should first isolate a precise invocation of the `dotnet` tool used in compilation.

1. Capture the output of `dotnet build -v:n` in `args.txt` and trim out the rubbish, leaving just the command line arguments to the F# compiler, usually starting with `-o:...`

2. Run an explicit invocation of the compiler, checking that your failures still happen, then debug that invocation.

For example, on Windows:

```text
"c:\Program Files\dotnet\dotnet.exe" "C:\Program Files\dotnet\sdk\2.1.403\FSharp\fsc.exe" @args.txt

devenv /debugexe "c:\Program Files\dotnet\dotnet.exe" "C:\Program Files\dotnet\sdk\2.1.403\FSharp\fsc.exe" @args.txt
```

Be careful to make sure Visual Studio debugging type is set to ".NET Core" (right click properties on dotnet and set debug type). Set first-catch exception handling (Ctrl-Alt-E, select all CLR exceptions) and set Just My Code off.

## How do I debug the execution of a type provider hosted in F# Interactive?

If your failures only happen in F# Interactive then use `devenv /debugexe fsi.exe MyProj.fsproj`, or a simialr .NET SDK invocation.

## How do I debug the execution of a type provider inside an IDE?

This can be quite tricky. First try to unit-test the type-provider and debug command-line invocations thoroughly.  If your failures only happen
in Visual Studio, then use `devenv /debugexe devenv.exe MyProj.fsproj`, set debug type to  ".NET Framework 4.0"
and launch F5.

## How do I debug the execution of a type provider when using .NET Framework tools?

To debug the use of a type provider inside the `msbuild` toolchain (.NET Framework), you should first isolate a precise invocation of the `dotnet` tool used in compilation.

1. Capture the output of `msbuild -v:n` in `args.txt` and trim to leave the command line arguments to the F# compiler, usually starting with `-o:...`

2. Run an explicit invocation of the compiler using this, checking that your failures still happen, then debug that invocation.

For example, on Windows:

```text
fsc.exe @args.txt

devenv /debugexe fsc.exe @args.txt
```

Set first-catch exception handling (Ctrl-Alt-E, select all CLR exceptions) and set Just My Code off.

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

TODO: give exact .fsproj/nuget instructions to get the dependency into the `typeproviders\fsharp41\netstandard2.0` directory alongside the design-time component.

