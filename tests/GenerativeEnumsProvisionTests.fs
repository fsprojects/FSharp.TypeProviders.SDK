#if INTERACTIVE
#r "../packages/NUnit/lib/net45/nunit.framework.dll"
#load "../src/ProvidedTypes.fsi" "../src/ProvidedTypes.fs" "../src/AssemblyReader.fs" "../src/AssemblyReaderReflection.fs" "../src/ProvidedTypesContext.fs" 
#load "../src/ProvidedTypesTesting.fs"
#load "FsUnit.fs"
#else
module FSharp.TypeProviders.StarterPack.Tests.GenerativeEnumsProvisionTests
#endif

#nowarn "760" // IDisposable needs new

#if NO_GENERATIVE
#else

open System
open System.Reflection
open System.IO
open FSharp.Core.CompilerServices
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting
open NUnit.Framework

let createEnum name (values: list<string*int>) =
    let enumType = ProvidedTypeDefinition(name, Some typeof<Enum>, IsErased = false)
    enumType.SetEnumUnderlyingType(typeof<int>)
    
    values
    |> List.map (fun (name, value) -> ProvidedLiteralField(name, enumType, value))
    |> enumType.AddMembers
    
    enumType

[<TypeProvider>]
type SimpleGenerativeEnumProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "Enums.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let tmp = Path.ChangeExtension(Path.GetTempFileName(), "dll")
    let myAssem = ProvidedAssembly(tmp)
    let container = ProvidedTypeDefinition(asm, ns, "Container", Some typeof<obj>, IsErased = false)

    do
        container.AddMember <| createEnum "Enum1" ["One", 1; "Two", 2]
        myAssem.AddTypes [container]
        this.AddNamespace(ns, [container])

[<TypeProvider>]
type UsedNestedGenerativeEnumProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "Enums.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let tmp = Path.ChangeExtension(Path.GetTempFileName(), "dll")
    let myAssem = ProvidedAssembly(tmp)

    do
        let container = ProvidedTypeDefinition(asm, ns, "Container", Some typeof<obj>, IsErased = false)
        let someType = ProvidedTypeDefinition("SomeClass", Some typeof<obj>, IsErased = false)
        let enum = createEnum "NestedEnum" ["Foo", 1; "Bar", 2]
        someType.AddMember enum
        someType.AddMember <| ProvidedField("enumField", enum)
        container.AddMember someType
        myAssem.AddTypes [container]
        this.AddNamespace(ns, [container])
    

let testTypeProvider test = 
    if Targets.supportsFSharp40 then
        let runtimeAssemblyRefs = Targets.DotNet45FSharp40Refs
        let runtimeAssembly = runtimeAssemblyRefs.[0]
        let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
        test cfg

[<Test>]
let ``GenerativeEnumsProvider generates simple enum correctly``()  = 
    testTypeProvider <| fun cfg -> 
        let typeProviderForNamespaces = SimpleGenerativeEnumProvider cfg :> TypeProviderForNamespaces
        let providedTypeDefinition = typeProviderForNamespaces.Namespaces |> Seq.last |> snd |> Seq.last
        Assert.AreEqual("Container", providedTypeDefinition.Name)

        let assemContents = (typeProviderForNamespaces :> ITypeProvider).GetGeneratedAssemblyContents(providedTypeDefinition.Assembly)
        Assert.AreNotEqual(assemContents.Length, 0)
        
        let assembly = Assembly.Load assemContents
        Assert.IsNotEmpty assembly.ExportedTypes

[<Test>]
let ``Nested enum used by the enclosing type is generated correctly``() =
    testTypeProvider <| fun cfg -> 
        let typeProviderForNamespaces = UsedNestedGenerativeEnumProvider cfg :> TypeProviderForNamespaces
        let providedTypeDefinition = typeProviderForNamespaces.Namespaces |> Seq.last |> snd |> Seq.last
        Assert.AreEqual("Container", providedTypeDefinition.Name)

        let assemContents = (typeProviderForNamespaces :> ITypeProvider).GetGeneratedAssemblyContents(providedTypeDefinition.Assembly)
        Assert.AreNotEqual(assemContents.Length, 0)
        
        let assembly = Assembly.Load assemContents
        Assert.IsNotEmpty assembly.ExportedTypes

#endif