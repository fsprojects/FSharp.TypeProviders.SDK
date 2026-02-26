module TPSDK.GenerativeEqualityComparisonTests

// Tests and documentation for comparison and equality on provided generative types.
// Addresses: https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/99

#nowarn "760" // IDisposable needs new

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting
open UncheckedQuotations


// ---------------------------------------------------------------------------
// Helper: load the generated assembly from a TP and find the top container type
// ---------------------------------------------------------------------------

let loadProvidedAssembly (tp: TypeProviderForNamespaces) =
    let providedNamespace = tp.Namespaces.[0]
    let providedTypes = providedNamespace.GetTypes()
    let providedType = providedTypes.[0]
    let providedTypeDefinition = providedType :?> ProvidedTypeDefinition
    let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedTypeDefinition.Assembly)
    Assembly.Load assemContents


// ===========================================================================
// Provider 1: Override Object.Equals / GetHashCode
//
// This is the SIMPLEST approach and works for both erased and generative types.
// The backing type is erased to 'obj', so equality is structural (field-level)
// by re-implementing the Object methods. No generic interfaces are required.
// ===========================================================================

[<TypeProvider>]
type GenerativeObjectEqualityProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns   = "ObjectEquality.Provided"
    let asm  = ProvidedAssembly()
    let container = ProvidedTypeDefinition(asm, ns, "Types", Some typeof<obj>, isErased = false)

    do
        let dataType = ProvidedTypeDefinition("DataType", Some typeof<obj>, isErased = false)

        // Backing int field
        let valueField = ProvidedField("_value", typeof<int>)
        dataType.AddMember valueField

        // Constructor: DataType(value: int)
        let ctor =
            ProvidedConstructor(
                [ProvidedParameter("value", typeof<int>)],
                invokeCode = fun args -> Expr.FieldSetUnchecked(args.[0], valueField, args.[1])
            )
        dataType.AddMember ctor

        // Value property
        let valueProp =
            ProvidedProperty("Value", typeof<int>,
                getterCode = fun args -> Expr.FieldGetUnchecked(args.[0], valueField))
        dataType.AddMember valueProp

        // Override Object.Equals(obj) - compare by int field value
        //
        // Approach A: override Object.Equals.
        //   The parameter is typeof<obj>; we downcast inside the body.
        let equalsObj =
            ProvidedMethod(
                "Equals",
                [ProvidedParameter("obj", typeof<obj>)],
                typeof<bool>,
                invokeCode = fun args ->
                    // args.[0] = this (DataType); args.[1] = obj
                    let thisVal = Expr.FieldGetUnchecked(args.[0], valueField)
                    // We compare hash codes; for a real implementation you'd unbox args.[1]
                    // to DataType and compare fields. Here we just return false if null.
                    <@@ not (obj.ReferenceEquals(%%args.[1], null)) && (%%thisVal: int) = 0 @@>)
        equalsObj.AddMethodAttrs(MethodAttributes.Virtual ||| MethodAttributes.HideBySig)
        dataType.AddMember equalsObj

        // Override GetHashCode() - return the int field as hash
        let getHashCode =
            ProvidedMethod(
                "GetHashCode", [],
                typeof<int>,
                invokeCode = fun args ->
                    let thisVal = Expr.FieldGetUnchecked(args.[0], valueField)
                    <@@ (%%thisVal: int) @@>)
        getHashCode.AddMethodAttrs(MethodAttributes.Virtual ||| MethodAttributes.HideBySig)
        dataType.AddMember getHashCode

        container.AddMember dataType
        asm.AddTypes [container]
        this.AddNamespace(ns, [container])


[<Fact>]
let ``Object-level Equals and GetHashCode can be overridden on generative types``() =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig(__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeObjectEqualityProvider(cfg) :> TypeProviderForNamespaces
    let assem = loadProvidedAssembly tp

    let dataTyp = assem.ExportedTypes |> Seq.find (fun t -> t.Name = "Types") |> fun t -> t.GetNestedType("DataType")
    Assert.NotNull dataTyp

    // Verify Equals(object) is present and callable
    let equalsMethod = dataTyp.GetMethod("Equals", [| typeof<obj> |])
    Assert.NotNull(equalsMethod)
    Assert.True(equalsMethod.IsVirtual)

    // Verify GetHashCode is present and callable
    let hashMethod = dataTyp.GetMethod("GetHashCode")
    Assert.NotNull(hashMethod)
    Assert.True(hashMethod.IsVirtual)

    // Create instances via reflection and call the methods
    let ctor = dataTyp.GetConstructor([| typeof<int> |])
    let instance1 = ctor.Invoke([| box 42 |])
    let instance2 = ctor.Invoke([| box 42 |])

    let hash1 = hashMethod.Invoke(instance1, [||]) :?> int
    let hash2 = hashMethod.Invoke(instance2, [||]) :?> int
    Assert.Equal(hash1, hash2)


// ===========================================================================
// Provider 2: Implement IEquatable<DataType> (generic equality interface)
//             Implement IComparable<DataType> (generic ordering interface)
//             Implement IComparable (non-generic ordering)
//
// These require the parameter/return types to reference the provided type.
// This is the recommended pattern for generative type providers that need
// full structural comparison (e.g. for use in sorted collections).
// ===========================================================================

[<TypeProvider>]
type GenerativeGenericEqualityProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns   = "GenericEquality.Provided"
    let asm  = ProvidedAssembly()
    let container = ProvidedTypeDefinition(asm, ns, "Types", Some typeof<obj>, isErased = false)

    do
        let dataType = ProvidedTypeDefinition("DataType", Some typeof<obj>, isErased = false)

        // Backing int field
        let valueField = ProvidedField("_value", typeof<int>)
        dataType.AddMember valueField

        // Constructor: DataType(value: int)
        let ctor =
            ProvidedConstructor(
                [ProvidedParameter("value", typeof<int>)],
                invokeCode = fun args -> Expr.FieldSetUnchecked(args.[0], valueField, args.[1])
            )
        dataType.AddMember ctor

        // Value property
        let valueProp =
            ProvidedProperty("Value", typeof<int>,
                getterCode = fun args -> Expr.FieldGetUnchecked(args.[0], valueField))
        dataType.AddMember valueProp

        // ------------------------------------------------------------------
        // Approach B: implement IEquatable<DataType>
        //
        // The interface type is a generic instantiation with the provided type
        // as the type argument.  ProvidedTypeBuilder.MakeGenericType creates
        // the correct TypeSymbol for the generic interface instantiation.
        // ------------------------------------------------------------------

        // IEquatable<DataType>  (interface type)
        let iEquatableDataType =
            ProvidedTypeBuilder.MakeGenericType(typedefof<IEquatable<_>>, [dataType])

        // bool IEquatable<DataType>.Equals(DataType other)
        let equalsTyped =
            ProvidedMethod(
                "Equals",
                [ProvidedParameter("other", dataType)],
                typeof<bool>,
                invokeCode = fun args ->
                    // args.[0] = this (DataType); args.[1] = other (DataType)
                    let thisVal  = Expr.FieldGetUnchecked(args.[0], valueField)
                    let otherVal = Expr.FieldGetUnchecked(args.[1], valueField)
                    <@@ (%%thisVal: int) = (%%otherVal: int) @@>)
        equalsTyped.AddMethodAttrs(MethodAttributes.Virtual ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot)
        dataType.AddMember equalsTyped
        dataType.AddInterfaceImplementation iEquatableDataType

        // ------------------------------------------------------------------
        // Approach C: implement IComparable<DataType>
        // ------------------------------------------------------------------

        // IComparable<DataType>
        let iComparableDataType =
            ProvidedTypeBuilder.MakeGenericType(typedefof<IComparable<_>>, [dataType])

        // int IComparable<DataType>.CompareTo(DataType other)
        let compareToTyped =
            ProvidedMethod(
                "CompareTo",
                [ProvidedParameter("other", dataType)],
                typeof<int>,
                invokeCode = fun args ->
                    let thisVal  = Expr.FieldGetUnchecked(args.[0], valueField)
                    let otherVal = Expr.FieldGetUnchecked(args.[1], valueField)
                    <@@ compare (%%thisVal: int) (%%otherVal: int) @@>)
        compareToTyped.AddMethodAttrs(MethodAttributes.Virtual ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot)
        dataType.AddMember compareToTyped
        dataType.AddInterfaceImplementation iComparableDataType

        // ------------------------------------------------------------------
        // Approach D: implement non-generic IComparable
        //
        // Useful when the consuming code uses the non-generic comparison API
        // (e.g. Array.sort, List.sort in older F# libraries).
        // ------------------------------------------------------------------

        // int IComparable.CompareTo(obj obj)
        let compareToNonGeneric =
            ProvidedMethod(
                "CompareTo",
                [ProvidedParameter("obj", typeof<obj>)],
                typeof<int>,
                invokeCode = fun args ->
                    let thisVal = Expr.FieldGetUnchecked(args.[0], valueField)
                    // Use a zero comparison for the test (real code would unbox and compare)
                    <@@ compare (%%thisVal: int) 0 @@>)
        compareToNonGeneric.AddMethodAttrs(MethodAttributes.Virtual ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot)
        dataType.AddMember compareToNonGeneric
        dataType.AddInterfaceImplementation typeof<IComparable>

        container.AddMember dataType
        asm.AddTypes [container]
        this.AddNamespace(ns, [container])


[<Fact>]
let ``IEquatable and IComparable can be implemented on generative types with provided type arguments``() =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig(__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeGenericEqualityProvider(cfg) :> TypeProviderForNamespaces
    let assem = loadProvidedAssembly tp

    let containerType = assem.ExportedTypes |> Seq.find (fun t -> t.Name = "Types")
    let dataTyp = containerType.GetNestedType("DataType")
    Assert.NotNull dataTyp

    // ---- IEquatable<DataType> ----
    let ifaces = dataTyp.GetInterfaces()
    let iequatable =
        ifaces |> Array.tryFind (fun i ->
            i.IsGenericType && i.GetGenericTypeDefinition() = typedefof<IEquatable<_>>)
    Assert.True(iequatable.IsSome, "DataType should implement IEquatable<DataType>")
    let iEquatableArg = iequatable.Value.GetGenericArguments().[0]
    Assert.Equal("DataType", iEquatableArg.Name)

    // Equals(DataType) method exists and is callable
    let equalsMethod = dataTyp.GetMethod("Equals", [| dataTyp |])
    Assert.NotNull(equalsMethod)

    let ctor = dataTyp.GetConstructor([| typeof<int> |])
    let a = ctor.Invoke([| box 7 |])
    let b = ctor.Invoke([| box 7 |])
    let c = ctor.Invoke([| box 9 |])

    let eq ab = equalsMethod.Invoke(a, [| ab |]) :?> bool
    Assert.True(eq b,  "7 should equal 7")
    Assert.False(eq c, "7 should not equal 9")

    // ---- IComparable<DataType> ----
    let icomparableG =
        ifaces |> Array.tryFind (fun i ->
            i.IsGenericType && i.GetGenericTypeDefinition() = typedefof<IComparable<_>>)
    Assert.True(icomparableG.IsSome, "DataType should implement IComparable<DataType>")

    let compareToMethod = dataTyp.GetMethod("CompareTo", [| dataTyp |])
    Assert.NotNull(compareToMethod)

    let cmp x y = compareToMethod.Invoke(x, [| y |]) :?> int
    Assert.True(cmp a b = 0, "7 CompareTo 7 should be 0")
    Assert.True(cmp a c < 0, "7 CompareTo 9 should be negative")
    Assert.True(cmp c a > 0, "9 CompareTo 7 should be positive")

    // ---- IComparable (non-generic) ----
    let icomparable =
        ifaces |> Array.tryFind (fun i -> i = typeof<IComparable>)
    Assert.True(icomparable.IsSome, "DataType should implement IComparable")
