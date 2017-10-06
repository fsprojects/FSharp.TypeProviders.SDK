#if INTERACTIVE
#r "../packages/NUnit/lib/net45/nunit.framework.dll"
#load "../src/ProvidedTypes.fsi" "../src/ProvidedTypes.fs" "../src/AssemblyReader.fs" "../src/AssemblyReaderReflection.fs" "../src/ProvidedTypesContext.fs" 
#load "../src/ProvidedTypesTesting.fs"
#load "FsUnit.fs"
#else
module FSharp.TypeProviders.SDK.Tests.BasicGenerativeTests
#endif

open System.Reflection
open System.IO
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting
open Microsoft.FSharp.Core.CompilerServices
open NUnit.Framework
open FsUnit

#nowarn "760" // IDisposable needs new

#if !NO_GENERATIVE

[<TypeProvider>]
type GenerativePropertyProviderWithStaticParams (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "StaticProperty.Provided"
    let asm = Assembly.GetExecutingAssembly()
    //let ctxt = ProvidedTypesContext.Create(config) 

    let createType (typeName, n:int) =
        let tmp = Path.ChangeExtension(Path.GetTempFileName(), "dll")
        let myAssem = ProvidedAssembly(tmp)
        let myType = ProvidedTypes.ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, IsErased=false)
        let myProp = ProvidedTypes.ProvidedProperty("MyProperty", typeof<string list>, IsStatic = true, GetterCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>))
        myType.AddMember(myProp)
        myAssem.AddTypes [myType]
        myType

    do
        let myType = ProvidedTypes.ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        let parameters = [ ProvidedTypes.ProvidedStaticParameter("Count", typeof<int>) 
                           ProvidedTypes.ProvidedStaticParameter("Count2", typeof<int>, 3) ]
        myType.DefineStaticParameters(parameters, (fun typeName args -> createType(typeName, (args.[0] :?> int) + (args.[1] :?> int))))

        this.AddNamespace(ns, [myType])



[<Test>]
let ``GenerativePropertyProviderWithStaticParams generates for .NET 4.5 F# 4.0 correctly``() : unit  = 
  if Targets.supportsFSharp40 then 
    let args = [|  box 3; box 4  |] 
    let runtimeAssembly = Targets.DotNet45FSharp40Refs.[0]
    let runtimeAssemblyRefs = Targets.DotNet45FSharp40Refs
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
    let typeProviderForNamespaces = GenerativePropertyProviderWithStaticParams cfg :> TypeProviderForNamespaces
    let providedTypeDefinition = typeProviderForNamespaces.Namespaces |> Seq.last |> snd |> Seq.last
    let typeName = providedTypeDefinition.Name + (args |> Seq.map (fun s -> ",\"" + (if s = null then "" else s.ToString()) + "\"") |> Seq.reduce (+))

    let t = providedTypeDefinition.MakeParametricType(typeName, args)
    Assert.True(t.Assembly.FullName.Contains("tmp"))
    let assemContents = (typeProviderForNamespaces :> ITypeProvider).GetGeneratedAssemblyContents(t.Assembly)
    Assert.AreNotEqual(assemContents.Length, 0)

#endif
