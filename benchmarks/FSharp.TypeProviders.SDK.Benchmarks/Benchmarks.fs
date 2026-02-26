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

