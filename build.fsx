// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/FAKE/tools"
#r "packages/FAKE/tools/FakeLib.dll"

open System
open System.IO
open Fake 
open Fake.AssemblyInfoFile
open Fake.Git
open Fake.FscHelper

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet
// --------------------------------------------------------------------------------------

let project = "FSharp.TypeProviders.StarterPack"
let authors = ["Tomas Petricek"; "Gustavo Guerra"; "Michael Newton"]
let summary = "Helper code and examples for getting started with Type Providers"
let description = """
  The F# Type Provider Starter Pack contains everything you need to start building your own
  type providers."""
let tags = "F# fsharp typeprovider"

let gitHome = "https://github.com/mavnn"
let gitName = "FSharp.TypeProviders.StarterPack"

// Read release notes & version info from RELEASE_NOTES.md
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = 
    File.ReadLines "RELEASE_NOTES.md" 
    |> ReleaseNotesHelper.parseReleaseNotes

let PullRequest =
    match getBuildParamOrDefault "APPVEYOR_PULL_REQUEST_NUMBER" "" with
    | "" -> 
        trace "Master build detected"
        None
    | a -> 
        trace "Pull Request build detected"
        Some <| int a

let buildNumber =
    int (getBuildParamOrDefault "APPVEYOR_BUILD_VERSION" "0")

let version =
    match PullRequest with
    | None ->
        sprintf "%s.%d" release.AssemblyVersion buildNumber
    | Some num ->
        sprintf "%s-pull-%d-%05d" release.AssemblyVersion num buildNumber
let releaseNotes = release.Notes |> String.concat "\n"
let outputPath = "./output/"
let srcPath = "src"

// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" (fun _ ->
    CleanDirs [outputPath]
)

// --------------------------------------------------------------------------------------
// Compile ProvidedTypes as a smoke test
Target "Compile" (fun _ ->
    Fsc id [srcPath @@ "ProvidedTypes.fsi";srcPath @@ "ProvidedTypes.fs"]
)


// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->
    // Format the description to fit on a single line (remove \r\n and double-spaces)
    let description = description.Replace("\r", "").Replace("\n", "").Replace("  ", " ")
    NuGet (fun p -> 
        { p with   
            Authors = authors
            Project = project
            Summary = summary
            Description = description
            Version = version
            ReleaseNotes = releaseNotes
            Tags = tags
            OutputPath = outputPath
            WorkingDir = outputPath
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish =
                hasBuildParam "nugetkey"
            Dependencies = [] })
        "nuget/FSharp.TypeProviders.StarterPack.nuspec"
)

// --------------------------------------------------------------------------------------
// Help

Target "Help" (fun _ ->
    printfn ""
    printfn "  Please specify the target by calling 'build <Target>'"
    printfn ""
    printfn "  * NuGet (creates package only, doesn't publish unless api key provided)"
    printfn "  * Compile (attempts to compile ProvidedTypes.fs)"
    printfn "")

"Clean"
    ==> "Compile"
    ==> "NuGet"

RunTargetOrDefault "Help"
