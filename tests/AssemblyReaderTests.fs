#if INTERACTIVE
#r "../packages/NUnit/lib/net45/nunit.framework.dll"
#load "../src/AssemblyReader.fs" 
//#load "../src/AssemblyReaderReflection.fs"  (strangely fails to bind)
#load "FsUnit.fs"
#else
module FSharp.TypeProviders.StarterPack.Tests.AssemblyReaderTests
#endif

open System
open System.IO
open NUnit.Framework
open FsUnit


[<Test>]
let ``AssemblyReader reads assemblies containing Reflected Definitions``() : unit  = 
    let file = Path.Combine [| __SOURCE_DIRECTORY__; ".."; "test"; "ConsoleApplication4.exe" |]
    let assemblyReader = ProviderImplementation.AssemblyReader.ILModuleReaderAfterReadingAllBytes(file, ProviderImplementation.AssemblyReader.mkILGlobals ProviderImplementation.AssemblyReader.EcmaMscorlibScopeRef)
    let someExtractedValue =
        [for inp in assemblyReader.ILModuleDef.ManifestOfAssembly.CustomAttrs.Elements do 
             match ProviderImplementation.AssemblyReader.decodeILCustomAttribData assemblyReader.ILGlobals inp with
             | [] -> ()
             | args -> yield (inp.Method.EnclosingType.BasicQualifiedName, Seq.head [ for (_,arg) in args -> if arg = null then "" else arg.ToString()]) ]
    ()
