// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#I "packages/FAKE/tools"
#r "FakeLib.dll"

open System
open System.IO
open Fake.AssemblyInfoFile
open Fake.Git
open Fake.Testing
open Fake.FscHelper
open Fake


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

let pullRequest =
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
    match pullRequest with
    | None ->
        sprintf "%s.%d" release.AssemblyVersion buildNumber
    | Some num ->
        sprintf "%s-pull-%d-%05d" release.AssemblyVersion num buildNumber
let releaseNotes = release.Notes |> String.concat "\n"
let outputPath = "./output/"
let workingDir = "./temp/"
let srcDir = "src"
let exampleDir =  "examples"
let testDir =  "test"
let nunitDir = "packages/NUnit/lib/net45"

let sources =
    [srcDir @@ "ProvidedTypes.fsi"
     srcDir @@ "ProvidedTypes.fs"
     srcDir @@ "AssemblyReader.fs"
     srcDir @@ "AssemblyReaderReflection.fs"
     srcDir @@ "ProvidedTypesContext.fs"
     srcDir @@ "ProvidedTypesTesting.fs" ]


// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" (fun _ ->
    CleanDirs [outputPath; workingDir;testDir]
)

// --------------------------------------------------------------------------------------
// Compile ProvidedTypes as a smoke test
Target "Compile" (fun _ ->
    sources
    |> Compile [
        FscHelper.Target TargetType.Library
        Platform PlatformType.AnyCpu
        Reference "System.Reflection.Metadata.dll"
    ]

    !! "FSharp.TypeProviders.StarterPack.sln"
    |> MSBuildRelease "" "Build"
    |> ignore
)

type ExampleWithTests =
    { Name : string
      ProviderSourceFiles : string list
      TestSourceFiles : string list }


// --------------------------------------------------------------------------------------
// Compile example providers and accompanying test dlls
Target "Examples" (fun _ ->
    let examples =
        [
            { Name = "StaticProperty"; ProviderSourceFiles = ["StaticProperty.fsx"]; TestSourceFiles = ["StaticProperty.Tests.fsx"]}
            { Name = "ErasedWithConstructor"; ProviderSourceFiles = ["ErasedWithConstructor.fsx"]; TestSourceFiles = ["ErasedWithConstructor.Tests.fsx"]}
        ]

    if not (Directory.Exists testDir) then
        Directory.CreateDirectory testDir |> ignore

    let testNunitDll = testDir @@ "nunit.framework.dll"

    if File.Exists testNunitDll then
        File.Delete testNunitDll

    File.Copy (nunitDir @@ "nunit.framework.dll", testNunitDll)

    let fromExampleDir filenames =
        filenames
        |> List.map (fun filename -> exampleDir @@ filename)

    examples
    |> List.iter (fun example ->
            // Compile type provider
            let output = testDir @@ example.Name + ".dll"
            (List.concat [sources;fromExampleDir example.ProviderSourceFiles])
            |> Compile [
                Out output
                FscHelper.Target TargetType.Library
            ]

            // Compile test dll
            (fromExampleDir example.TestSourceFiles)
            |> Compile [
                Out (testDir @@ example.Name + ".Tests.dll")
                FscHelper.Target TargetType.Library
                References [output;nunitDir @@ "nunit.framework.dll"]
            ]
        )
)

Target "RunTests" (fun _ ->
    !! ("tests/bin/Release/FSharp.TypeProviders.StarterPack.Tests.dll")
    |> NUnit3 id
    !! (testDir @@ "*.Tests.dll")
    |> NUnit3 id
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->
    sources |> CopyTo (workingDir @@ "content")

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
            WorkingDir = workingDir
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey"
            Files = [(workingDir, None, None)]
            Dependencies = [] })
        "nuget/FSharp.TypeProviders.StarterPack.nuspec"
)

// --------------------------------------------------------------------------------------
// Help

"Clean"
    ==> "NuGet"

"Compile"
    ==> "Examples"
    ==> "RunTests"
    ==> "NuGet"

RunTargetOrDefault "RunTests"
