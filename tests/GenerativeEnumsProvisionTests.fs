#if INTERACTIVE
#load "../src/ProvidedTypes.fsi" "../src/ProvidedTypes.fs" "../src/AssemblyReader.fs" "../src/AssemblyReaderReflection.fs" "../src/ProvidedTypesContext.fs" 
#load "../src/ProvidedTypesTesting.fs"

#else

module FSharp.TypeProviders.SDK.Tests.GenerativeEnumsProvisionTests
#endif

#nowarn "760" // IDisposable needs new

#if !NO_GENERATIVE

open System
open System.Reflection
open System.IO
open Microsoft.FSharp.Core.CompilerServices
open Xunit
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting


[<TypeProvider>]
type GenerativeEnumsProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "Enums.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let ctxt = ProvidedTypesContext.Create(config, isForGenerated=true)
    let tempAssembly = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), "dll"))
    let container = ctxt.ProvidedTypeDefinition(asm, ns, "Container", Some typeof<obj>, IsErased = false)

    let createEnum name (values: list<string*int>) =
        let enumType = ctxt.ProvidedTypeDefinition(name, Some typeof<Enum>, IsErased = false)
        enumType.SetEnumUnderlyingType(typeof<int>)
        
        values
        |> List.map (fun (name, value) -> ctxt.ProvidedLiteralField(name, enumType, value))
        |> enumType.AddMembers
        
        enumType

    do
        let enumContainer = ctxt.ProvidedTypeDefinition("EnumContainer", Some typeof<obj>, IsErased = false)
        let enum = createEnum "NestedEnum" ["Foo", 1; "Bar", 2]
        enumContainer.AddMember enum
        enumContainer.AddMember <| ctxt.ProvidedField("nestedEnumField", enum)
        container.AddMember <| enumContainer

        let topLevelEnum = createEnum "TopLevelEnum" ["One", 1; "Two", 2]
        enumContainer.AddMember <| ctxt.ProvidedField("topLevelEnumField", topLevelEnum)
        container.AddMember topLevelEnum
    
        tempAssembly.AddTypes [container]
        this.AddNamespace(container.Namespace, [container])

let testProvidedAssembly test = 
    if Targets.supportsFSharp40 then
        let runtimeAssemblyRefs = Targets.DotNet45FSharp40Refs
        let runtimeAssembly = runtimeAssemblyRefs.[0]
        let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
        let tp = GenerativeEnumsProvider(cfg) :> TypeProviderForNamespaces
        let providedTypeDefinition = tp.Namespaces |> Seq.last |> snd |> Seq.last
        Assert.Equal("Container", providedTypeDefinition.Name)

        let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedTypeDefinition.Assembly)
        let assembly = Assembly.Load assemContents
        assembly.ExportedTypes |> Seq.find (fun ty -> ty.Name = "Container") |> test

let runningOnMono = try System.Type.GetType("Mono.Runtime") <> null with e -> false 

[<Fact>]
let ``Enums are generated correctly``() =
  // See tracking bug https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/123 
  if not runningOnMono then 
    testProvidedAssembly <| fun container -> 
        let enumContainer = container.GetNestedType "EnumContainer"
        Assert.NotNull enumContainer

        let nestedEnum = enumContainer.GetNestedType "NestedEnum"
        Assert.NotNull nestedEnum

        let nestedEnumField = enumContainer.GetField("nestedEnumField", BindingFlags.Instance ||| BindingFlags.NonPublic)
        Assert.NotNull nestedEnumField
        Assert.Equal(nestedEnum, nestedEnumField.FieldType)

        let enumValues = Enum.GetValues(nestedEnum) |> Seq.cast<int> |> Seq.zip (Enum.GetNames(nestedEnum))  |> Seq.toList
        Assert.True(["Foo", 1; "Bar", 2] = enumValues)

        let topLevelEnum = container.GetNestedType "TopLevelEnum"
        Assert.NotNull topLevelEnum

        let topLevelEnumField = enumContainer.GetField("topLevelEnumField", BindingFlags.Instance ||| BindingFlags.NonPublic)
        Assert.NotNull topLevelEnumField
        Assert.Equal(topLevelEnum, topLevelEnumField.FieldType)

#endif