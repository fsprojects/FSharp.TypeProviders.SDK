#r @"paket:
source https://nuget.org/api/v2
framework netstandard2.0
nuget Fake.Core.Target
nuget Fake.Core.Process
nuget Fake.Core.ReleaseNotes 
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Cli
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.Paket
nuget Fake.DotNet.NuGet //"

#if !FAKE
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard" // Temp fix for https://github.com/fsharp/FAKE/issues/1985
#endif

open Fake 
open Fake.Core.TargetOperators
open Fake.Core 
open Fake.IO
open Fake.DotNet
open System
open System.IO

Target.initEnvironment()

let config = DotNet.BuildConfiguration.Release
let setParams (p:DotNet.BuildOptions) = {p with Configuration = config}

let outputPath = __SOURCE_DIRECTORY__ + "/bin"

// Read release notes & version info from RELEASE_NOTES.md
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = ReleaseNotes.load "RELEASE_NOTES.md"

let exec p args = 
    printfn "Executing %s %s" p args 
    Shell.Exec(p, args) |> function 0 -> () | d -> failwithf "%s %s exited with error %d" p args d


Target.create "Clean" (fun _ ->
    Shell.cleanDirs ["temp"]
)

Target.create "Build" (fun _ ->
    DotNet.build setParams "src/FSharp.TypeProviders.SDK.fsproj"
    DotNet.build setParams "tests/FSharp.TypeProviders.SDK.Tests.fsproj"
)

Target.create "Examples" (fun _ ->
    DotNet.build setParams "examples/BasicProvider.DesignTime/BasicProvider.DesignTime.fsproj"
    DotNet.build setParams "examples/BasicProvider/BasicProvider.fsproj"
    DotNet.build setParams "examples/ComboProvider/ComboProvider.fsproj"
    DotNet.build setParams "examples/StressProvider/StressProvider.fsproj"
)

Target.create "RunTests" (fun _ ->
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
    let setTestOptions framework (p:DotNet.TestOptions) =
        { p with Configuration = config; Framework= Some framework }
    DotNet.test (setTestOptions "net461") "tests/FSharp.TypeProviders.SDK.Tests.fsproj"
    DotNet.test (setTestOptions "net461") "examples/BasicProvider.Tests/BasicProvider.Tests.fsproj"
    DotNet.test (setTestOptions "net461") "examples/ComboProvider.Tests/ComboProvider.Tests.fsproj"
    DotNet.test (setTestOptions "net461") "examples/StressProvider.Tests/StressProvider.Tests.fsproj"
    
    DotNet.test (setTestOptions "netcoreapp2.0") "tests/FSharp.TypeProviders.SDK.Tests.fsproj"
    DotNet.test (setTestOptions "netcoreapp2.0") "examples/BasicProvider.Tests/BasicProvider.Tests.fsproj"
    // TODO: Where is ComboProvider?
    DotNet.test (setTestOptions "netcoreapp2.0") "examples/StressProvider.Tests/StressProvider.Tests.fsproj"
    
    // This can also be used to give console output:
    // dotnet build tests\FSharp.TypeProviders.SDK.Tests.fsproj -c Debug -f net461 && packages\xunit.runner.console\tools\net452\xunit.console.exe tests\bin\Debug\net461\FSharp.TypeProviders.SDK.Tests.dll -parallel none

#endif
)

Target.create "Pack" (fun _ ->
    let releaseNotes = String.toLines release.Notes
    // TODO: This is using an awkward mix of Paket and dotnet to do packaging. We should just use paket.
    let setParams (p:DotNet.PackOptions) = { p with OutputPath = Some outputPath; Configuration = config }
    DotNet.pack  (fun p -> { 
        setParams p with 
            MSBuildParams= { 
                MSBuild.CliArguments.Create() with
                    Properties = [
                        "PackageVersion", release.NugetVersion
                        "ReleaseNotes", releaseNotes
                    ] 
            } 
        }) "src/FSharp.TypeProviders.SDK.fsproj"
    DotNet.pack setParams "examples/BasicProvider/BasicProvider.fsproj"
    DotNet.pack setParams "examples/ComboProvider/ComboProvider.fsproj"
    DotNet.pack setParams "examples/StressProvider/StressProvider.fsproj"

    NuGet.NuGet.NuGetPack (fun p -> {
        p with 
            WorkingDir="templates"
            OutputPath=outputPath
            Version=release.NugetVersion
            ReleaseNotes = releaseNotes
    }) "templates/FSharp.TypeProviders.Templates.nuspec"
)

Target.create "TestTemplatesNuGet" (fun _ ->
    let wd = Path.Combine(__SOURCE_DIRECTORY__, "temp")
    let runInTempDir (p:DotNet.Options) = { p with WorkingDirectory = wd }
    // Globally install the templates from the template nuget package we just built
    DotNet.exec runInTempDir "new" ("-i " + outputPath + "/FSharp.TypeProviders.Templates." + release.NugetVersion + ".nupkg") |> ignore

    // Instantiate the template into a randomly generated name
    let testAppName = "tp2" + string (abs (hash DateTime.Now.Ticks) % 100)
    let testAppDir = Path.Combine(wd, testAppName)
    Shell.cleanDir testAppDir
    DotNet.exec runInTempDir "new" (sprintf "typeprovider -n %s -lang F#" testAppName) |> ignore

    let runInTestAppDir (p:DotNet.Options) = { p with WorkingDirectory = testAppDir }
    DotNet.exec runInTestAppDir "tool" "restore" |> ignore
    DotNet.exec runInTestAppDir "paket" "restore" |> ignore
    DotNet.exec runInTestAppDir "build" "-c debug" |> ignore
    DotNet.exec runInTestAppDir "test" "-c debug" |> ignore

    (* Manual steps without building nupkg
        dotnet pack src\FSharp.TypeProviders.SDK.fsproj /p:PackageVersion=0.0.0.99 --output bin -c release
        .nuget\nuget.exe pack -OutputDirectory bin -Version 0.0.0.99 templates/FSharp.TypeProviders.Templates.nuspec
        dotnet new -i  bin/FSharp.TypeProviders.Templates.0.0.0.99.nupkg
        dotnet new typeprovider -n tp3 -lang:F#
        *)
)

Target.create "All" ignore

"Clean" ==> "Pack"
"Build" ==> "Examples" ==> "RunTests" ==> "Pack" ==> "TestTemplatesNuGet" ==> "All"

Target.runOrDefault "All"
