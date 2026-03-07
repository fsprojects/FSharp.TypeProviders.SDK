module TPSDK.GenerativeStructProvisionTests

#nowarn "760" // IDisposable needs new

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting
open UncheckedQuotations

[<TypeProvider>]
type GenerativeStructProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "Structs.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    do
        // Point struct with two int fields using isStruct = true convenience parameter
        let pointType = ProvidedTypeDefinition("Point", None, isStruct = true, isErased = false)
        let xField = ProvidedField("X", typeof<int>)
        let yField = ProvidedField("Y", typeof<int>)
        pointType.AddMember xField
        pointType.AddMember yField
        let ctor = ProvidedConstructor(
            [ProvidedParameter("x", typeof<int>); ProvidedParameter("y", typeof<int>)],
            invokeCode = fun args ->
                Expr.Sequential(
                    Expr.FieldSetUnchecked(args.[0], xField, args.[1]),
                    Expr.FieldSetUnchecked(args.[0], yField, args.[2])))
        pointType.AddMember ctor
        container.AddMember pointType

        // Struct defined the traditional way (explicitly passing System.ValueType as base)
        let vectorType = ProvidedTypeDefinition("Vector", Some typeof<System.ValueType>, isErased = false)
        let dxField = ProvidedField("DX", typeof<float>)
        let dyField = ProvidedField("DY", typeof<float>)
        vectorType.AddMember dxField
        vectorType.AddMember dyField
        container.AddMember vectorType

        tempAssembly.AddTypes [container]
        this.AddNamespace(container.Namespace, [container])

let testProvidedAssembly test =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeStructProvider(cfg) :> TypeProviderForNamespaces
    let providedNamespace = tp.Namespaces.[0]
    let providedTypes = providedNamespace.GetTypes()
    let providedType = providedTypes.[0]
    let providedTypeDefinition = providedType :?> ProvidedTypeDefinition
    Assert.Equal("Container", providedTypeDefinition.Name)
    let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedTypeDefinition.Assembly)
    let assembly = Assembly.Load assemContents
    assembly.ExportedTypes |> Seq.find (fun ty -> ty.Name = "Container") |> test

[<Fact>]
let ``Struct defined with isStruct=true is a value type``() =
    testProvidedAssembly <| fun container ->
        let pointType = container.GetNestedType("Point")
        Assert.NotNull(pointType)
        Assert.True(pointType.IsValueType, "Point should be a value type")
        Assert.True(pointType.IsSealed, "Structs should be sealed")

[<Fact>]
let ``Struct defined with isStruct=true has SequentialLayout``() =
    testProvidedAssembly <| fun container ->
        let pointType = container.GetNestedType("Point")
        Assert.NotNull(pointType)
        let hasSequentialLayout = (int pointType.Attributes &&& int TypeAttributes.SequentialLayout) <> 0
        Assert.True(hasSequentialLayout, "Struct defined with isStruct=true should have SequentialLayout")

[<Fact>]
let ``Struct defined with isStruct=true has expected fields``() =
    testProvidedAssembly <| fun container ->
        let pointType = container.GetNestedType("Point")
        Assert.NotNull(pointType)
        let fields = pointType.GetFields(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        Assert.True(fields.Length >= 2, "Point should have at least 2 fields")
        let names = fields |> Array.map (fun f -> f.Name) |> Array.sort
        Assert.Contains("X", names)
        Assert.Contains("Y", names)

[<Fact>]
let ``Struct defined the traditional way (baseType = ValueType) is also a value type``() =
    testProvidedAssembly <| fun container ->
        let vectorType = container.GetNestedType("Vector")
        Assert.NotNull(vectorType)
        Assert.True(vectorType.IsValueType, "Vector should be a value type")
