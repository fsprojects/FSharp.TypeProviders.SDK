namespace FSharp.TypeProviders.SDK.Benchmarks

open System
open System.Collections.Generic
open System.Reflection
open BenchmarkDotNet.Attributes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes

// ---------------------------------------------------------------------------
// Shared helpers (module required because namespaces cannot contain let-bindings)
// ---------------------------------------------------------------------------

[<AutoOpen>]
module internal BenchmarkHelpers =

    // Minimal simulation of the F# compiler host required by the SDK's
    // cross-targeting assembly resolver (mirrors ProvidedTypesTesting.fs).

    [<AllowNullLiteral>]
    type private DllInfo(path: string) =
        member _.FileName = path

    // The type must be named TcImports and expose Base + DllInfos.
    type private TcImports(bas: TcImports option, dllInfosInitial: DllInfo list) =
        let mutable dllInfos = dllInfosInitial
        member _.Base    = bas
        member _.DllInfos = dllInfos

    let private setProp (cfg: TypeProviderConfig) (prop: string) (value: obj) =
        let ty = cfg.GetType()
        match ty.GetProperty(prop, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic) with
        | null ->
            let fld = ty.GetField(prop, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
            if isNull fld then failwith ("expected TypeProviderConfig to have a property or field " + prop)
            fld.SetValue(cfg, value)
        | p -> p.GetSetMethod(nonPublic = true).Invoke(cfg, [| value |]) |> ignore

    /// Build the list of referenced assemblies from the currently-loaded AppDomain.
    let buildRefs () =
        let asms =
            AppDomain.CurrentDomain.GetAssemblies()
            |> Array.filter (fun x -> not x.IsDynamic && x.Location <> "")
            |> Array.map (fun x -> x.Location)
        let runtimeAssembly =
            asms
            |> Array.find (fun x ->
                match IO.Path.GetFileNameWithoutExtension(x).ToLower() with
                | "mscorlib" | "system.runtime" -> true
                | _ -> false)
        runtimeAssembly, Array.toList asms

    /// Create a TypeProviderConfig that properly simulates the F# compiler host.
    let makeConfig () =
        let runtimeAssembly, runtimeAssemblyRefs = buildRefs ()
        let cfg = TypeProviderConfig(fun _ -> false)
        cfg.IsHostedExecution <- false
        cfg.IsInvalidationSupported <- true
        cfg.ResolutionFolder <- __SOURCE_DIRECTORY__
        cfg.RuntimeAssembly <- runtimeAssembly
        cfg.ReferencedAssemblies <- Array.ofList runtimeAssemblyRefs

        // Set up the systemRuntimeContainsType closure with the shape expected by
        // AssemblyResolver.fs (must close over a value named `tcImports`).
        let dllInfos = [ yield DllInfo(runtimeAssembly); for r in runtimeAssemblyRefs do yield DllInfo(r) ]
        let tcImports = TcImports(Some(TcImports(None, [])), dllInfos)
        let systemRuntimeContainsType = (fun (_s: string) -> if tcImports.DllInfos.Length = 1 then true else true)
        setProp cfg "systemRuntimeContainsType" systemRuntimeContainsType
        cfg

    // ---------------------------------------------------------------------------
    // Stress scenario: many methods with repeated common types
    // ---------------------------------------------------------------------------

    /// Build a generative assembly with `typeCount` types, each having `methodsPerType`
    /// static methods whose parameters and return types are drawn from a small set of
    /// commonly-used .NET types.  This maximises cache hits for `transType`.
    let buildLargeGenerativeAssembly (typeCount: int) (methodsPerType: int) =
        let cfg = makeConfig ()
        let tp = new TypeProviderForNamespaces(cfg)

        let ns = "BenchmarkNamespace"
        let tempAssembly = ProvidedAssembly()

        // The common types that appear repeatedly in real-world schemas.
        let commonTypes =
            [| typeof<string>; typeof<int>; typeof<bool>; typeof<float>; typeof<int64>
               typeof<DateTime>; typeof<Guid>; typeof<decimal>; typeof<byte[]>
               typeof<string option>; typeof<int option>; typeof<bool option> |]

        for i in 0 .. typeCount - 1 do
            let container =
                ProvidedTypeDefinition(
                    tempAssembly, ns,
                    sprintf "GeneratedType%d" i,
                    Some typeof<obj>, isErased = false)

            for m in 0 .. methodsPerType - 1 do
                // Cycle through common types for parameters and return types so that
                // transType is called many times with the same Type objects.
                let retTy    = commonTypes.[m % commonTypes.Length]
                let param1Ty = commonTypes.[(m + 1) % commonTypes.Length]
                let param2Ty = commonTypes.[(m + 2) % commonTypes.Length]
                let meth =
                    ProvidedMethod(
                        sprintf "Method%d" m,
                        [ ProvidedParameter("a", param1Ty)
                          ProvidedParameter("b", param2Ty) ],
                        retTy,
                        invokeCode = (fun _ -> Expr.Value(null, retTy)),
                        isStatic = true)
                container.AddMember meth

            tempAssembly.AddTypes [container]
            tp.AddNamespace(ns, [container])

        // Return the tp and the first type so the caller can trigger compilation.
        let providedType =
            tp.Namespaces.[0].GetTypes().[0] :?> ProvidedTypeDefinition
        tp, providedType

    // ---------------------------------------------------------------------------
    // Property-heavy scenario: many backing fields + properties with real bodies
    // ---------------------------------------------------------------------------

    /// Build a generative assembly with `typeCount` types each having `propsPerType`
    /// properties.  Each property is backed by a ProvidedField; the getter and setter
    /// bodies use Expr.FieldGetUnchecked / Expr.FieldSetUnchecked so that
    /// transFieldSpec is exercised on the hot path.
    let buildProviderWithProperties (typeCount: int) (propsPerType: int) =
        let cfg = makeConfig ()
        let tp = new TypeProviderForNamespaces(cfg)
        let ns = "BenchmarkNamespace"
        let tempAssembly = ProvidedAssembly()

        // Cycle through primitive types for field types (no generic options here
        // because nested generic types exercise a different path).
        let fieldTypes =
            [| typeof<string>; typeof<int>; typeof<bool>; typeof<float>; typeof<int64>
               typeof<DateTime>; typeof<Guid>; typeof<decimal> |]

        for i in 0 .. typeCount - 1 do
            let container =
                ProvidedTypeDefinition(
                    tempAssembly, ns,
                    sprintf "ResourceType%d" i,
                    Some typeof<obj>, isErased = false)

            let fields = Array.init propsPerType (fun p ->
                let fty = fieldTypes.[p % fieldTypes.Length]
                let f = ProvidedField(sprintf "_field%d" p, fty)
                container.AddMember f
                f)

            for p in 0 .. propsPerType - 1 do
                let field = fields.[p]
                let fty = field.FieldType
                let prop =
                    ProvidedProperty(
                        sprintf "Property%d" p,
                        fty,
                        getterCode = (fun args -> Expr.FieldGet(Expr.Coerce(args.[0], field.DeclaringType), field)),
                        setterCode = (fun args -> Expr.FieldSet(Expr.Coerce(args.[0], field.DeclaringType), field, args.[1])))
                container.AddMember prop

            tempAssembly.AddTypes [container]
            tp.AddNamespace(ns, [container])

        let providedType =
            tp.Namespaces.[0].GetTypes().[0] :?> ProvidedTypeDefinition
        tp, providedType

    // ---------------------------------------------------------------------------
    // Complex-body scenario: method bodies that call real BCL methods
    // (exercises transMeth / transMethRef caching)
    // ---------------------------------------------------------------------------

    /// Build a generative assembly where every method body calls a real BCL method
    /// (String.IsNullOrEmpty).  All methods share the same BCL call target, so
    /// transMethRef is invoked many times with identical inputs — the cache should
    /// eliminate redundant ILMethodRef allocations.
    let buildProviderWithComplexBodies (typeCount: int) (methodsPerType: int) =
        let cfg = makeConfig ()
        let tp = new TypeProviderForNamespaces(cfg)
        let ns = "BenchmarkNamespace"
        let tempAssembly = ProvidedAssembly()

        // These BCL methods will be referenced from every generated method body.
        let isNullOrEmpty  = typeof<string>.GetMethod("IsNullOrEmpty", [| typeof<string> |])
        let objToString    = typeof<obj>.GetMethod("ToString", [||])

        for i in 0 .. typeCount - 1 do
            let container =
                ProvidedTypeDefinition(
                    tempAssembly, ns,
                    sprintf "ServiceType%d" i,
                    Some typeof<obj>, isErased = false)

            for m in 0 .. methodsPerType - 1 do
                // Alternate between two different BCL call sites to avoid trivially
                // hitting the memoization cache for the result type.
                let callSite, retTy =
                    if m % 2 = 0 then isNullOrEmpty, typeof<bool>
                    else objToString,   typeof<string>

                let meth =
                    ProvidedMethod(
                        sprintf "Op%d" m,
                        [ ProvidedParameter("s", typeof<string>) ],
                        retTy,
                        invokeCode =
                            (if m % 2 = 0 then
                                fun args -> Expr.Call(callSite, [ args.[0] ])
                             else
                                fun args -> Expr.Call(args.[0], callSite, [])),
                        isStatic = (m % 2 = 0))
                container.AddMember meth

            tempAssembly.AddTypes [container]
            tp.AddNamespace(ns, [container])

        let providedType =
            tp.Namespaces.[0].GetTypes().[0] :?> ProvidedTypeDefinition
        tp, providedType

    // ---------------------------------------------------------------------------
    // Nested-types scenario: types containing nested type definitions
    // ---------------------------------------------------------------------------

    /// Build a generative assembly with `outerCount` top-level types, each
    /// containing `nestingDepth` levels of nested types with `methodsPerLevel`
    /// methods.  This exercises the recursive type-definition walk in Compile().
    let buildProviderWithNestedTypes (outerCount: int) (nestingDepth: int) (methodsPerLevel: int) =
        let cfg = makeConfig ()
        let tp = new TypeProviderForNamespaces(cfg)
        let ns = "BenchmarkNamespace"
        let tempAssembly = ProvidedAssembly()

        let retTy = typeof<string>

        let rec buildNested (depth: int) (name: string) : ProvidedTypeDefinition =
            let td = ProvidedTypeDefinition(name, Some typeof<obj>, isErased = false)
            for m in 0 .. methodsPerLevel - 1 do
                let meth =
                    ProvidedMethod(
                        sprintf "Method%d" m,
                        [ ProvidedParameter("x", typeof<int>) ],
                        retTy,
                        invokeCode = (fun _ -> Expr.Value("", retTy)),
                        isStatic = true)
                td.AddMember meth
            if depth > 1 then
                let nested = buildNested (depth - 1) (sprintf "%s_Nested" name)
                td.AddMember nested
            td

        for i in 0 .. outerCount - 1 do
            let outer =
                ProvidedTypeDefinition(
                    tempAssembly, ns,
                    sprintf "OuterType%d" i,
                    Some typeof<obj>, isErased = false)
            for m in 0 .. methodsPerLevel - 1 do
                let meth =
                    ProvidedMethod(
                        sprintf "OuterMethod%d" m,
                        [ ProvidedParameter("x", typeof<int>) ],
                        retTy,
                        invokeCode = (fun _ -> Expr.Value("", retTy)),
                        isStatic = true)
                outer.AddMember meth
            let nested = buildNested nestingDepth (sprintf "OuterType%d_Nested" i)
            outer.AddMember nested
            tempAssembly.AddTypes [outer]
            tp.AddNamespace(ns, [outer])

        let providedType =
            tp.Namespaces.[0].GetTypes().[0] :?> ProvidedTypeDefinition
        tp, providedType

// ---------------------------------------------------------------------------
// Benchmark classes
// ---------------------------------------------------------------------------

/// Benchmarks the compilation of a large generative type provider.
/// Setup (creating ProvidedTypeDefinitions) happens in [IterationSetup]; only
/// GetGeneratedAssemblyContents (which calls transType internally) is timed.
[<MemoryDiagnoser>]
[<SimpleJob(launchCount = 1, warmupCount = 3, iterationCount = 10)>]
type GenerativeCompilationBenchmark() =

    // Pre-built providers ready for compilation in each iteration.
    // We keep a queue because GetGeneratedAssemblyContents consumes the assembly.
    let mutable pool: (TypeProviderForNamespaces * ProvidedTypeDefinition) Queue = Queue()

    [<Params(50, 200)>]
    member val TypeCount = 50 with get, set

    [<Params(20)>]
    member val MethodsPerType = 20 with get, set

    /// Fill the pool before each iteration starts.
    [<IterationSetup>]
    member this.Setup() =
        // Ensure at least 2 providers are ready (warmup + iteration).
        while pool.Count < 2 do
            pool.Enqueue(buildLargeGenerativeAssembly this.TypeCount this.MethodsPerType)

    [<Benchmark(Description = "CompileGenerativeAssembly")>]
    member _.CompileGenerativeAssembly() =
        let tp, providedType = pool.Dequeue()
        let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
        (tp :> IDisposable).Dispose()
        bytes.Length   // return value to prevent dead-code elimination


/// Stress scenario: simulate a large schema provider (like SwaggerProvider over
/// a big OpenAPI spec) by generating 500 types with 10 methods each, all using
/// the same handful of primitive types.  This is the scenario most improved by
/// the transType memoization fix.
[<MemoryDiagnoser>]
[<SimpleJob(launchCount = 1, warmupCount = 2, iterationCount = 5)>]
type LargeSchemaProviderBenchmark() =

    let mutable pool: (TypeProviderForNamespaces * ProvidedTypeDefinition) Queue = Queue()

    [<IterationSetup>]
    member _.Setup() =
        while pool.Count < 2 do
            pool.Enqueue(buildLargeGenerativeAssembly 500 10)

    [<Benchmark(Description = "CompileLargeSchema_500types_10methods")>]
    member _.CompileLargeSchema() =
        let tp, providedType = pool.Dequeue()
        let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
        (tp :> IDisposable).Dispose()
        bytes.Length


/// Property-heavy benchmark: types with many backing fields and properties whose
/// getter/setter bodies use Expr.FieldGetUnchecked / FieldSetUnchecked.
/// This exercises the ProvidedField → fieldMap → ILFieldBuilder path and simulates
/// data-transfer-object schemas (OpenAPI "components/schemas").
[<MemoryDiagnoser>]
[<SimpleJob(launchCount = 1, warmupCount = 2, iterationCount = 5)>]
type PropertyHeavyBenchmark() =

    let mutable pool: (TypeProviderForNamespaces * ProvidedTypeDefinition) Queue = Queue()

    [<Params(50, 200)>]
    member val TypeCount = 50 with get, set

    [<Params(10)>]
    member val PropsPerType = 10 with get, set

    [<IterationSetup>]
    member this.Setup() =
        while pool.Count < 2 do
            pool.Enqueue(buildProviderWithProperties this.TypeCount this.PropsPerType)

    [<Benchmark(Description = "CompilePropertyHeavy")>]
    member _.CompilePropertyHeavy() =
        let tp, providedType = pool.Dequeue()
        let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
        (tp :> IDisposable).Dispose()
        bytes.Length


/// Complex-body benchmark: method bodies that call real BCL methods, exercising
/// the transMeth / transMethRef caching path.  All methods call one of two BCL
/// methods (String.IsNullOrEmpty / Object.ToString), so without the
/// transMethRefCache many identical ILMethodRef objects would be allocated.
[<MemoryDiagnoser>]
[<SimpleJob(launchCount = 1, warmupCount = 2, iterationCount = 5)>]
type ComplexBodyBenchmark() =

    let mutable pool: (TypeProviderForNamespaces * ProvidedTypeDefinition) Queue = Queue()

    [<Params(100, 300)>]
    member val TypeCount = 100 with get, set

    [<Params(10)>]
    member val MethodsPerType = 10 with get, set

    [<IterationSetup>]
    member this.Setup() =
        while pool.Count < 2 do
            pool.Enqueue(buildProviderWithComplexBodies this.TypeCount this.MethodsPerType)

    [<Benchmark(Description = "CompileComplexBodies")>]
    member _.CompileComplexBodies() =
        let tp, providedType = pool.Dequeue()
        let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
        (tp :> IDisposable).Dispose()
        bytes.Length


/// Nested-types benchmark: outer types each contain several levels of nested types.
/// Exercises the recursive defineNestedTypes / typeMembers walk in Compile().
[<MemoryDiagnoser>]
[<SimpleJob(launchCount = 1, warmupCount = 2, iterationCount = 5)>]
type NestedTypesBenchmark() =

    let mutable pool: (TypeProviderForNamespaces * ProvidedTypeDefinition) Queue = Queue()

    [<Params(30, 100)>]
    member val OuterCount = 30 with get, set

    [<Params(4)>]
    member val NestingDepth = 4 with get, set

    [<IterationSetup>]
    member this.Setup() =
        while pool.Count < 2 do
            pool.Enqueue(buildProviderWithNestedTypes this.OuterCount this.NestingDepth 5)

    [<Benchmark(Description = "CompileNestedTypes")>]
    member _.CompileNestedTypes() =
        let tp, providedType = pool.Dequeue()
        let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
        (tp :> IDisposable).Dispose()
        bytes.Length


/// Extra-large stress benchmark: 1 000 types × 10 methods.  Designed to amplify
/// any O(N²) behaviour that slipped through the smaller scenarios.
[<MemoryDiagnoser>]
[<SimpleJob(launchCount = 1, warmupCount = 1, iterationCount = 3)>]
type XLargeStressBenchmark() =

    let mutable pool: (TypeProviderForNamespaces * ProvidedTypeDefinition) Queue = Queue()

    [<IterationSetup>]
    member _.Setup() =
        while pool.Count < 2 do
            pool.Enqueue(buildLargeGenerativeAssembly 1000 10)

    [<Benchmark(Description = "CompileXLarge_1000types_10methods")>]
    member _.CompileXLarge() =
        let tp, providedType = pool.Dequeue()
        let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
        (tp :> IDisposable).Dispose()
        bytes.Length

