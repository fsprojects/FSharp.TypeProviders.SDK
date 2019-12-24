// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#I "packages/FAKE/tools"

#r "packages/FAKE/tools/FakeLib.dll"

open System
open System.IO
open Fake

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet
// --------------------------------------------------------------------------------------

let project = "FSharp.TypeProviders.SDK"
let authors = ["FSharp.TypeProviders.SDK contributors" ]
let summary = "Helper code and examples for getting started with Type Providers"
let description = """
  The F# Type Provider SDK provides utilities for authoring type providers."""
let tags = "F# fsharp typeprovider"

let gitHome = "https://github.com/fsprojects"
let gitName = "FSharp.TypeProviders.SDK"

let config = "Release"
let outputPath = __SOURCE_DIRECTORY__ + "/bin"

// Read release notes & version info from RELEASE_NOTES.md
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release =
    File.ReadLines "RELEASE_NOTES.md"
    |> ReleaseNotesHelper.parseReleaseNotes

let dotnetSdkVersion = "2.1.403"
let sdkPath = lazy DotNetCli.InstallDotNetSDK dotnetSdkVersion
let getSdkPath() = sdkPath.Value

printfn "Desired .NET SDK version = %s" dotnetSdkVersion
printfn "DotNetCli.isInstalled() = %b" (DotNetCli.isInstalled())
if DotNetCli.isInstalled() then printfn "DotNetCli.getVersion() = %s" (DotNetCli.getVersion())

let exec p args = 
    printfn "Executing %s %s" p args 
    Shell.Exec(p, args) |> function 0 -> () | d -> failwithf "%s %s exited with error %d" p args d

let execIn dir p args = 
    printfn "Executing %s %s in %s" p args dir
    Shell.Exec(p, args, dir=dir) |> function 0 -> () | d -> failwithf "%s %s exited with error %d" p args d

let pullRequest =
    match getBuildParamOrDefault "APPVEYOR_PULL_REQUEST_NUMBER" "" with
    | "" ->
        trace "Master build detected"
        None
    | a ->
        trace "Pull Request build detected"
        Some <| int a

let buildNumber =
    getBuildParamOrDefault "APPVEYOR_BUILD_VERSION" "0"

let version =
    match pullRequest with
    | None ->
        sprintf "%s.%s" release.AssemblyVersion buildNumber
    | Some num ->
        sprintf "%s-pull-%d-%s" release.AssemblyVersion num buildNumber
let releaseNotes = release.Notes |> String.concat "\n"
let srcDir = "src"

let sources =
    [srcDir @@ "ProvidedTypes.fsi"
     srcDir @@ "ProvidedTypes.fs"
     srcDir @@ "ProvidedTypesTesting.fs" ]

Target "Clean" (fun _ ->
    CleanDirs []
)

Target "Build" (fun _ ->
    DotNetCli.Build  (fun p -> { p with Configuration = config; Project = "src/FSharp.TypeProviders.SDK.fsproj"; ToolPath =  getSdkPath() })
    DotNetCli.Build  (fun p -> { p with Configuration = config; Project = "tests/FSharp.TypeProviders.SDK.Tests.fsproj"; ToolPath =  getSdkPath() })
)

Target "Examples" (fun _ ->
    DotNetCli.Build  (fun p -> { p with Configuration = config; Project = "examples/BasicProvider.DesignTime/BasicProvider.DesignTime.fsproj"; ToolPath =  getSdkPath() })
    DotNetCli.Build  (fun p -> { p with Configuration = config; Project = "examples/BasicProvider/BasicProvider.fsproj"; ToolPath =  getSdkPath() })
    DotNetCli.Build  (fun p -> { p with Configuration = config; Project = "examples/ComboProvider/ComboProvider.fsproj"; ToolPath =  getSdkPath() })
    DotNetCli.Build  (fun p -> { p with Configuration = config; Project = "examples/StressProvider/StressProvider.fsproj"; ToolPath =  getSdkPath() })
)
Target "RunTests" (fun _ ->
#if MONO
  // Run the netcoreapp2.0 tests with "dotnet test"
    DotNetCli.Test  (fun p -> { p with Configuration = config; Project = "tests/FSharp.TypeProviders.SDK.Tests.fsproj"; ToolPath =  getSdkPath(); AdditionalArgs=["-f"; "netcoreapp2.0"] })

  // We don't use "dotnet test" for Mono testing the test runner doesn't know how to run with Mono
  // This is a bit of a hack to find the output test DLL and run xunit using Mono directly
    let dir = "tests/bin/" + config + "/net461"
    let file = 
        (Array.append [| dir |] (System.IO.Directory.GetDirectories(dir)))
        |> Array.pick (fun subdir -> 
            let file = subdir + "/FSharp.TypeProviders.SDK.Tests.dll"
            if System.IO.File.Exists file then Some file else None)

    exec "packages/xunit.runner.console/tools/net452/xunit.console.exe" (file + " -parallel none")
            
    // This can also be used on Mono to give console output:
    // msbuild tests/FSharp.TypeProviders.SDK.Tests.fsproj /p:Configuration=Debug && mono packages/xunit.runner.console/tools/net452/xunit.console.exe tests/bin/Debug/net461/FSharp.TypeProviders.SDK.Tests.dll -parallel none
#else
    DotNetCli.Test  (fun p -> { p with Configuration = config; Project = "tests/FSharp.TypeProviders.SDK.Tests.fsproj"; ToolPath =  getSdkPath(); Framework="net461" })
    DotNetCli.Test  (fun p -> { p with Configuration = config; Project = "examples/BasicProvider.Tests/BasicProvider.Tests.fsproj"; ToolPath =  getSdkPath(); Framework="net461" })
    DotNetCli.Test  (fun p -> { p with Configuration = config; Project = "examples/ComboProvider.Tests/ComboProvider.Tests.fsproj"; ToolPath =  getSdkPath(); Framework="net461" })
    DotNetCli.Test  (fun p -> { p with Configuration = config; Project = "examples/StressProvider.Tests/StressProvider.Tests.fsproj"; ToolPath =  getSdkPath(); Framework="net461" })
    DotNetCli.Test  (fun p -> { p with Configuration = config; Project = "tests/FSharp.TypeProviders.SDK.Tests.fsproj"; ToolPath =  getSdkPath(); Framework="netcoreapp2.0" })
    DotNetCli.Test  (fun p -> { p with Configuration = config; Project = "examples/BasicProvider.Tests/BasicProvider.Tests.fsproj"; ToolPath =  getSdkPath(); Framework="netcoreapp2.0" })
    DotNetCli.Test  (fun p -> { p with Configuration = config; Project = "examples/StressProvider.Tests/StressProvider.Tests.fsproj"; ToolPath =  getSdkPath(); Framework="netcoreapp2.0" })

    // This can also be used to give console output:
    // dotnet build tests\FSharp.TypeProviders.SDK.Tests.fsproj -c Debug -f net461 && packages\xunit.runner.console\tools\net452\xunit.console.exe tests\bin\Debug\net461\FSharp.TypeProviders.SDK.Tests.dll -parallel none

#endif
)

Target "Pack" (fun _ ->
    // TODO: This is using an awkward mix of Paket and dotnet to do packaging. We should just use paket.
    DotNetCli.Pack  (fun p -> { p with Configuration = config; 
                                       Project = "src/FSharp.TypeProviders.SDK.fsproj"; 
                                       ToolPath =  getSdkPath(); OutputPath = outputPath; 
                                       AdditionalArgs= [ sprintf "/p:PackageVersion=%s" release.NugetVersion;
                                                         sprintf "/p:ReleaseNotes=\"%s\"" (String.concat " " release.Notes)  ] })
    DotNetCli.Pack   (fun p -> { p with Configuration = config; Project = "examples/BasicProvider/BasicProvider.fsproj"; ToolPath =  getSdkPath(); OutputPath = outputPath })
    DotNetCli.Pack   (fun p -> { p with Configuration = config; Project = "examples/ComboProvider/ComboProvider.fsproj"; ToolPath =  getSdkPath(); OutputPath = outputPath })
    DotNetCli.Pack   (fun p -> { p with Configuration = config; Project = "examples/StressProvider/StressProvider.fsproj"; ToolPath =  getSdkPath(); OutputPath = outputPath })
    NuGetHelper.NuGetPack (fun p -> { p with WorkingDir = "templates"; OutputPath = outputPath; Version = release.NugetVersion; ReleaseNotes = toLines release.Notes}) @"templates/FSharp.TypeProviders.Templates.nuspec"
)

let dotnetSdkVersion3 = "3.1.100"
let sdkPath3 = lazy DotNetCli.InstallDotNetSDK dotnetSdkVersion3
let getSdkPath3() = sdkPath3.Value

Target "TestTemplatesNuGet" (fun _ ->
    let wd = Path.Combine(__SOURCE_DIRECTORY__, "temp")
    // Globally install the templates from the template nuget package we just built
    DotNetCli.RunCommand (fun p -> { p with ToolPath =  getSdkPath3(); WorkingDir=wd }) ("new -i " + outputPath + "/FSharp.TypeProviders.Templates." + release.NugetVersion + ".nupkg")

    // Instantiate the template into a randomly generated name
    let testAppName = "tp2" + string (abs (hash System.DateTime.Now.Ticks) % 100)
    let testAppDir = Path.Combine(wd, testAppName)
    CleanDir testAppDir
    DotNetCli.RunCommand (fun p -> { p with ToolPath =  getSdkPath3(); WorkingDir=wd }) (sprintf "new typeprovider -n %s -lang F#" testAppName)

    let pkgs = Path.GetFullPath(outputPath)

    let setCommandParams (p:DotNetCli.CommandParams) = { p with  ToolPath =  getSdkPath3(); WorkingDir=testAppDir }
    DotNetCli.RunCommand setCommandParams "tool restore"
    DotNetCli.RunCommand setCommandParams "paket restore"
    DotNetCli.RunCommand setCommandParams "build -c debug"
    DotNetCli.RunCommand setCommandParams "test -c debug"

    (* Manual steps without building nupkg
        dotnet pack src\FSharp.TypeProviders.SDK.fsproj /p:PackageVersion=0.0.0.99 --output bin -c release
        .nuget\nuget.exe pack -OutputDirectory bin -Version 0.0.0.99 templates/FSharp.TypeProviders.Templates.nuspec
        dotnet new -i  bin/FSharp.TypeProviders.Templates.0.0.0.99.nupkg
        dotnet new typeprovider -n tp3 -lang:F#
        *)

)

Target "All" id

"Clean" ==> "Pack"
"Build" ==> "Examples" ==> "RunTests" ==> "Pack" ==> "TestTemplatesNuGet" ==> "All"

RunTargetOrDefault "All"
