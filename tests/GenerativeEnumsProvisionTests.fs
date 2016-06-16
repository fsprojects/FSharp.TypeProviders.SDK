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
type UsedNestedGenerativeEnumProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "Enums.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let tempAssembly = Path.ChangeExtension(Path.GetTempFileName(), "dll") |> ProvidedAssembly
    let container = ProvidedTypeDefinition(asm, ns, "Container", Some typeof<obj>, IsErased = false)

    do
        let enumContainer = ProvidedTypeDefinition("EnumContainer", Some typeof<obj>, IsErased = false)
        let enum = createEnum "NestedEnum" ["Foo", 1; "Bar", 2]
        enumContainer.AddMember enum
        enumContainer.AddMember <| ProvidedField("enumField", enum)
        container.AddMember <| enumContainer
    
        tempAssembly.AddTypes [container]
        this.AddNamespace(container.Namespace, [container])

let testProvidedAssembly (typeProvider: TypeProviderConfig -> #TypeProviderForNamespaces) test = 
    if Targets.supportsFSharp40 then
        let runtimeAssemblyRefs = Targets.DotNet45FSharp40Refs
        let runtimeAssembly = runtimeAssemblyRefs.[0]
        let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
        let tp = typeProvider cfg
        let providedTypeDefinition = tp.Namespaces |> Seq.last |> snd |> Seq.last
        Assert.AreEqual("Container", providedTypeDefinition.Name)

        let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedTypeDefinition.Assembly)
        let assembly = Assembly.Load assemContents
        assembly.ExportedTypes |> Seq.find (fun ty -> ty.Name = "Container") |> test

[<Test>]
let ``Nested enum used by the enclosing type is generated correctly``() =
    testProvidedAssembly UsedNestedGenerativeEnumProvider <| fun container -> 
        let enumContainer = container.GetNestedType "EnumContainer"
        Assert.IsNotNull enumContainer

        let enum = enumContainer.GetNestedType "NestedEnum"
        Assert.IsNotNull enum

        let field = enumContainer.GetField("enumField", BindingFlags.Instance ||| BindingFlags.NonPublic)
        Assert.IsNotNull field
        Assert.AreEqual(enum, field.FieldType)

        let enumValues = Enum.GetValues(enum) |> Seq.cast<int> |> Seq.zip (Enum.GetNames(enum)) 
        CollectionAssert.AreEquivalent(["Foo", 1; "Bar", 2], enumValues)

#endif