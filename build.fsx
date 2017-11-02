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
let authors = ["Tomas Petricek"; "Gustavo Guerra"; "Michael Newton"; "Don Syme" ]
let summary = "Helper code and examples for getting started with Type Providers"
let description = """
  The F# Type Provider SDK provides utilities for authoring type providers."""
let tags = "F# fsharp typeprovider"

let gitHome = "https://github.com/fsprojects"
let gitName = "FSharp.TypeProviders.SDK"

let config = "Release"

// Read release notes & version info from RELEASE_NOTES.md
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release =
    File.ReadLines "RELEASE_NOTES.md"
    |> ReleaseNotesHelper.parseReleaseNotes


let exec p args = 
    printfn "Executing %s %s" p args 
    Shell.Exec(p, args) |> function 0 -> () | d -> failwithf "%s %s exited with error %d" p args d

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

Target "Restore" (fun _ ->
    exec "dotnet" "restore src/FSharp.TypeProviders.SDK.fsproj"
    exec "dotnet" "restore tests/FSharp.TypeProviders.SDK.Tests.fsproj"
    exec "dotnet" "restore examples/BasicProvider/BasicProvider.fsproj"
    exec "dotnet" "restore examples/BasicProvider.Tests/BasicProvider.Tests.fsproj"
)
Target "Build" (fun _ ->
    exec "dotnet" ("build -c "+config+" src/FSharp.TypeProviders.SDK.fsproj")
    exec "dotnet" ("build -c "+config+" tests/FSharp.TypeProviders.SDK.Tests.fsproj")
)

Target "Examples" (fun _ ->
    exec "dotnet" ("build -c "+config+" examples/BasicProvider/BasicProvider.fsproj")
    exec "dotnet" ("build -c "+config+" examples/BasicProvider.Tests/BasicProvider.Tests.fsproj")
)
Target "RunTests" (fun _ ->
#if MONO
  // We don't use dotnet test because of https://github.com/dotnet/sdk/issues/335
  // This is a bit of a hack to find the output tesat DLL and run xunit using Mono directly
    let dir = "tests/bin/" + config + "/net461"
    let file = 
        System.IO.Directory.GetDirectories(dir)
        |> Array.pick (fun subdir -> 
            let file = subdir + "/FSharp.TypeProviders.SDK.Tests.dll"
            if System.IO.File.Exists file then Some file else None)
    exec "packages/xunit.runner.console/tools/net452/xunit.console.exe" (file + " -parallel none")
            
#else
    exec "dotnet" ("test tests/FSharp.TypeProviders.SDK.Tests.fsproj -c " + config)
    // This also gives console output:
    // msbuild tests\FSharp.TypeProviders.SDK.Tests.fsproj /p:Configuration=Debug && packages\xunit.runner.console\tools\net452\xunit.console.exe tests\bin\Debug\net461\FSharp.TypeProviders.SDK.Tests.dll -parallel none

    // msbuild tests/FSharp.TypeProviders.SDK.Tests.fsproj /p:Configuration=Debug && mono packages/xunit.runner.console/tools/net452/xunit.console.exe tests/bin/Debug/net461/FSharp.TypeProviders.SDK.Tests.dll -parallel none
#endif
    ()
)

Target "NuGet" (fun _ ->
#if !MONO
  // We don't do this on Linux/OSX because of https://github.com/dotnet/sdk/issues/335
    exec "dotnet" ("pack src/FSharp.TypeProviders.SDK.fsproj -c " + config)
#endif
    ()
)

"Clean"
    ==> "NuGet"

"Restore"
    ==> "Build"
    ==> "RunTests"
    ==> "Examples"
    ==> "NuGet"

RunTargetOrDefault "RunTests"
