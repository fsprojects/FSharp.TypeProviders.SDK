module FSharp.TypeProviders.SDK.Benchmarks.Main

open System
open System.Diagnostics
open BenchmarkDotNet.Running
open FSharp.TypeProviders.SDK.Benchmarks
open Microsoft.FSharp.Core.CompilerServices

/// Quick (non-BenchmarkDotNet) timing for before/after comparison.
/// Separates setup (creating ProvidedTypeDefinitions) from compilation
/// (GetGeneratedAssemblyContents) to isolate the transType-heavy path.
/// Usage: dotnet run -c Release -- --quick
let runQuickMeasurement () =
    let measure label typeCount methodsPerType iterations =
        // Pre-build all instances so setup doesn't contaminate the timing.
        let instances =
            Array.init (iterations + 3) (fun _ -> buildLargeGenerativeAssembly typeCount methodsPerType)
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
        printfn "%-45s  %6.1f ms/iter  (total %d ms, %d iters)"
            (sprintf "%s  %d types × %d methods" label typeCount methodsPerType)
            (float sw.ElapsedMilliseconds / float iterations)
            sw.ElapsedMilliseconds iterations

    printfn ""
    printfn "Quick timing — compilation only (setup excluded), warmup=3"
    printfn "Note: not a substitute for a full BenchmarkDotNet run."
    printfn "%-45s  %13s" "Scenario" "Mean"
    printfn "%s" (String.replicate 65 "-")
    measure "Moderate" 50  20 10
    measure "Heavy"    200 20  5
    measure "Stress"   500 10  3

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
