module FSharp.TypeProviders.SDK.Benchmarks.Main

open System
open System.Diagnostics
open BenchmarkDotNet.Running
open FSharp.TypeProviders.SDK.Benchmarks
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

/// Quick (non-BenchmarkDotNet) timing for before/after comparison.
/// Separates setup (creating ProvidedTypeDefinitions) from compilation
/// (GetGeneratedAssemblyContents) to isolate the transType-heavy path.
/// Usage: dotnet run -c Release -- --quick
let runQuickMeasurement () =
    let measure label (build: unit -> TypeProviderForNamespaces * ProvidedTypeDefinition) iterations =
        // Pre-build all instances so setup doesn't contaminate the timing.
        let instances = Array.init (iterations + 3) (fun _ -> build ())
        // Warmup
        for i in 0..2 do
            let tp, pt = instances.[i]
            (tp :> ITypeProvider).GetGeneratedAssemblyContents(pt.Assembly) |> ignore
            (tp :> IDisposable).Dispose()
        // Timed
        let sw = Stopwatch.StartNew()
        for i in 3 .. iterations + 2 do
            let tp, pt = instances.[i]
            (tp :> ITypeProvider).GetGeneratedAssemblyContents(pt.Assembly) |> ignore
            (tp :> IDisposable).Dispose()
        sw.Stop()
        printfn "%-55s  %6.1f ms/iter  (total %d ms, %d iters)"
            label
            (float sw.ElapsedMilliseconds / float iterations)
            sw.ElapsedMilliseconds iterations

    printfn ""
    printfn "Quick timing — compilation only (setup excluded), warmup=3"
    printfn "Note: not a substitute for a full BenchmarkDotNet run."
    printfn "%-55s  %13s" "Scenario" "Mean"
    printfn "%s" (String.replicate 75 "-")
    // Method-heavy (original scenarios)
    measure "Methods  50 types × 20 methods"  (fun () -> buildLargeGenerativeAssembly 50  20) 10
    measure "Methods 200 types × 20 methods"  (fun () -> buildLargeGenerativeAssembly 200 20)  5
    measure "Methods 500 types × 10 methods"  (fun () -> buildLargeGenerativeAssembly 500 10)  3
    measure "Methods 1000 types × 10 methods" (fun () -> buildLargeGenerativeAssembly 1000 10) 2
    printfn ""
    // Property-heavy scenarios (field backing + FieldGet/FieldSet bodies)
    measure "Props    50 types × 10 props"    (fun () -> buildProviderWithProperties 50  10) 10
    measure "Props   200 types × 10 props"    (fun () -> buildProviderWithProperties 200 10)  5
    measure "Props   500 types × 10 props"    (fun () -> buildProviderWithProperties 500 10)  3
    printfn ""
    // Complex-body scenarios (BCL method calls in every body → transMethRef cache)
    measure "ComplexBody 100 types × 10 methods" (fun () -> buildProviderWithComplexBodies 100 10) 5
    measure "ComplexBody 300 types × 10 methods" (fun () -> buildProviderWithComplexBodies 300 10) 3
    printfn ""
    // Nested-type scenarios
    measure "Nested  30 outer × depth 4 × 5 methods" (fun () -> buildProviderWithNestedTypes 30  4 5) 5
    measure "Nested 100 outer × depth 4 × 5 methods" (fun () -> buildProviderWithNestedTypes 100 4 5) 3

[<EntryPoint>]
let main args =
    if args |> Array.contains "--quick" then
        runQuickMeasurement ()
        0
    else
        BenchmarkSwitcher
            .FromAssembly(typeof<GenerativeCompilationBenchmark>.Assembly)
            .Run(args)
        |> ignore
        0
