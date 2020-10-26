#if INTERACTIVE
#load "../src/ProvidedTypes.fsi" "../src/ProvidedTypes.fs" 
#load "../src/ProvidedTypesTesting.fs"

#else

module TPSDK.GenerativeEnumsProvisionTests
#endif

#nowarn "760" // IDisposable needs new

#if !NO_GENERATIVE

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting

[<TypeProvider>]
type GenerativeEnumsProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "Enums.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    let createEnum name (values: list<string*int>) =
        let enumType = ProvidedTypeDefinition(name, Some typeof<Enum>, isErased = false)
        enumType.SetEnumUnderlyingType(typeof<int>)
        
        values
        |> List.map (fun (name, value) -> 
            
            let field = ProvidedField.Literal(name, enumType, value)
            field.AddCustomAttribute({ new CustomAttributeData() with 
                                        member _.Constructor = typeof<System.Xml.Serialization.XmlEnumAttribute>.GetConstructor([|typeof<string>|])
                                        member _.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<string>, box (string value))|]
                                        member _.NamedArguments = upcast [||] } )
            field
            )
        |> enumType.AddMembers
        
        enumType

    do
        let enumContainer = ProvidedTypeDefinition("EnumContainer", Some typeof<obj>, isErased = false)
        let enum = createEnum "NestedEnum" ["Foo", 1; "Bar", 2]
        enumContainer.AddMember enum
        enumContainer.AddMember (ProvidedField("nestedEnumField", enum))
        container.AddMember enumContainer

        let topLevelEnum = createEnum "TopLevelEnum" ["One", 1; "Two", 2]
        enumContainer.AddMember (ProvidedField("topLevelEnumField", topLevelEnum))
        container.AddMember topLevelEnum
    
        tempAssembly.AddTypes [container]
        this.AddNamespace(container.Namespace, [container])

let testProvidedAssembly test =
        let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
        let runtimeAssembly = runtimeAssemblyRefs.[0]
        let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
        let tp = GenerativeEnumsProvider(cfg) :> TypeProviderForNamespaces
        let providedNamespace = tp.Namespaces.[0] 
        let providedTypes  = providedNamespace.GetTypes()
        let providedType = providedTypes.[0] 
        let providedTypeDefinition = providedType :?> ProvidedTypeDefinition
        Assert.Equal("Container", providedTypeDefinition.Name)

        let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedTypeDefinition.Assembly)
        let assembly = Assembly.Load assemContents
        assembly.ExportedTypes |> Seq.find (fun ty -> ty.Name = "Container") |> test

let runningOnMono = try Type.GetType("Mono.Runtime") <> null with _ -> false 

[<Fact>]
let ``Enums are generated correctly``() =
  // // See tracking bug https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/123 
  // if not runningOnMono then 
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

        let attrValues = nestedEnum.GetFields() |> Seq.choose (fun f -> f.GetCustomAttribute<System.Xml.Serialization.XmlEnumAttribute>() |> Option.ofObj |> Option.map (fun a -> a.Name) ) |> Seq.toList
        Assert.True(["1"; "2"] = attrValues)

        let topLevelEnum = container.GetNestedType "TopLevelEnum"
        Assert.NotNull topLevelEnum

        let topLevelEnumField = enumContainer.GetField("topLevelEnumField", BindingFlags.Instance ||| BindingFlags.NonPublic)
        Assert.NotNull topLevelEnumField
        Assert.Equal(topLevelEnum, topLevelEnumField.FieldType)


#endif