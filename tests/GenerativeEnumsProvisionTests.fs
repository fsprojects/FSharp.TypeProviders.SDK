module TPSDK.GenerativeEnumsProvisionTests

#nowarn "760" // IDisposable needs new

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

// Providers and tests for non-Int32 enum underlying types (byte/uint8 and int64).
// These exercise the assembly compiler's encoding of the value__ field and literal values
// for enum backing types other than the default Int32.

[<TypeProvider>]
type GenerativeByteEnumsProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "ByteEnums.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    do
        let byteEnum = ProvidedTypeDefinition("ByteEnum", Some typeof<Enum>, isErased = false)
        byteEnum.SetEnumUnderlyingType(typeof<byte>)
        byteEnum.AddMembers [
            ProvidedField.Literal("Zero",    byteEnum, 0uy)
            ProvidedField.Literal("One",     byteEnum, 1uy)
            ProvidedField.Literal("MaxByte", byteEnum, 255uy) ]
        container.AddMember byteEnum
        container.AddMember (ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        tempAssembly.AddTypes [container]
        this.AddNamespace(ns, [container])

[<TypeProvider>]
type GenerativeInt64EnumsProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "Int64Enums.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    do
        let int64Enum = ProvidedTypeDefinition("Int64Enum", Some typeof<Enum>, isErased = false)
        int64Enum.SetEnumUnderlyingType(typeof<int64>)
        int64Enum.AddMembers [
            ProvidedField.Literal("Zero",    int64Enum, 0L)
            ProvidedField.Literal("One",     int64Enum, 1L)
            ProvidedField.Literal("BigVal",  int64Enum, 3000000000L) ]  // value > Int32.MaxValue
        container.AddMember int64Enum
        container.AddMember (ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        tempAssembly.AddTypes [container]
        this.AddNamespace(ns, [container])

let makeAssembly (tp: TypeProviderForNamespaces) =
    let ns = tp.Namespaces.[0]
    let providedType = ns.GetTypes().[0]
    let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    Assembly.Load assemContents

let makeTargetAssembly (tp: TypeProviderForNamespaces) =
    let ns = tp.Namespaces.[0]
    let providedType = ns.GetTypes().[0]
    let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    tp.TargetContext.ReadRelatedAssembly(assemContents)

[<Fact>]
let ``Generative enum with byte underlying type is generated correctly``() =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeByteEnumsProvider(cfg) :> TypeProviderForNamespaces
    let assembly = makeAssembly tp

    let container = assembly.GetType("ByteEnums.Provided.Container")
    Assert.NotNull(container)

    let byteEnum = container.GetNestedType("ByteEnum")
    Assert.NotNull(byteEnum)
    Assert.True(byteEnum.IsEnum, "ByteEnum should be recognised as an enum")
    Assert.Equal(typeof<byte>, byteEnum.GetEnumUnderlyingType())

    // Enum names in declaration order; GetValues returns them sorted by numeric value
    let values = Enum.GetValues(byteEnum) |> Seq.cast<byte> |> Seq.zip (Enum.GetNames(byteEnum)) |> Seq.toList
    Assert.Equal<(string * byte) list>([("Zero", 0uy); ("One", 1uy); ("MaxByte", 255uy)], values)

[<Fact>]
let ``Generative enum with int64 underlying type is generated correctly``() =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeInt64EnumsProvider(cfg) :> TypeProviderForNamespaces
    let assembly = makeAssembly tp

    let container = assembly.GetType("Int64Enums.Provided.Container")
    Assert.NotNull(container)

    let int64Enum = container.GetNestedType("Int64Enum")
    Assert.NotNull(int64Enum)
    Assert.True(int64Enum.IsEnum, "Int64Enum should be recognised as an enum")
    Assert.Equal(typeof<int64>, int64Enum.GetEnumUnderlyingType())

    // BigVal (3_000_000_000L) is > Int32.MaxValue, proving the int64 encoding path is exercised
    let values = Enum.GetValues(int64Enum) |> Seq.cast<int64> |> Seq.zip (Enum.GetNames(int64Enum)) |> Seq.toList
    Assert.Equal<(string * int64) list>([("Zero", 0L); ("One", 1L); ("BigVal", 3000000000L)], values)

// ---------------------------------------------------------------------------
// Tests that read enums back via TargetContext.ReadRelatedAssembly (the IL binary
// reader path through TargetTypeDefinition), exercising GetEnumUnderlyingType()
// on non-Int32 enum types via the target context.
// ---------------------------------------------------------------------------

[<Fact>]
let ``Byte enum underlying type is correct when read via target context (ReadRelatedAssembly)``() =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeByteEnumsProvider(cfg) :> TypeProviderForNamespaces
    let targetAssem = makeTargetAssembly tp

    let container = targetAssem.GetType("ByteEnums.Provided.Container")
    Assert.NotNull(container)

    let byteEnum = container.GetNestedType("ByteEnum", BindingFlags.Public ||| BindingFlags.NonPublic)
    Assert.NotNull(byteEnum)
    Assert.True(byteEnum.IsEnum, "ByteEnum should be recognised as an enum via target context")
    // GetEnumUnderlyingType on a TargetTypeDefinition reads the "value__" field type from IL.
    Assert.Equal("System.Byte", byteEnum.GetEnumUnderlyingType().FullName)

[<Fact>]
let ``Int64 enum underlying type is correct when read via target context (ReadRelatedAssembly)``() =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeInt64EnumsProvider(cfg) :> TypeProviderForNamespaces
    let targetAssem = makeTargetAssembly tp

    let container = targetAssem.GetType("Int64Enums.Provided.Container")
    Assert.NotNull(container)

    let int64Enum = container.GetNestedType("Int64Enum", BindingFlags.Public ||| BindingFlags.NonPublic)
    Assert.NotNull(int64Enum)
    Assert.True(int64Enum.IsEnum, "Int64Enum should be recognised as an enum via target context")
    Assert.Equal("System.Int64", int64Enum.GetEnumUnderlyingType().FullName)
