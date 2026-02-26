# FSharp.TypeProviders.SDK Benchmarks

Performance benchmarks for the FSharp.TypeProviders.SDK using [BenchmarkDotNet](https://benchmarkdotnet.org/).

## Overview

The benchmark suite measures **generative type provider compilation** — specifically the time spent in `GetGeneratedAssemblyContents`, which is where `transType` (and its memoization caches) run.

| Benchmark | Scenario | Description |
|-----------|----------|-------------|
| `GenerativeCompilationBenchmark` | Moderate / Heavy | 50 or 200 types, 20 methods each (static methods, primitive types) |
| `LargeSchemaProviderBenchmark` | Stress | 500 types, 10 methods each (simulates SwaggerProvider over a large OpenAPI spec) |
| `XLargeStressBenchmark` | Extra-large stress | 1 000 types, 10 methods each (amplifies O(N²) hotspots) |
| `PropertyHeavyBenchmark` | Property-heavy | 50 or 200 types, 10 properties each with ProvidedField backing + getter/setter bodies |
| `ComplexBodyBenchmark` | BCL method calls | 100 or 300 types, 10 methods each with bodies that call real BCL methods (`String.IsNullOrEmpty`, `Object.ToString`) |
| `NestedTypesBenchmark` | Nested types | 30 or 100 outer types, 4 levels of nesting, 5 methods per level |

All method-heavy scenarios use 12 common .NET types (`string`, `int`, `bool`, `float`, `int64`, `DateTime`, `Guid`, `decimal`, `byte[]`, `string option`, `int option`, `bool option`) cycling through parameter and return types to maximise cache hit rates.

## Running

### Full BenchmarkDotNet run (recommended — requires Release mode)

```bash
dotnet run -c Release --project benchmarks/FSharp.TypeProviders.SDK.Benchmarks -- --filter '*'
```

To run a specific benchmark class:

```bash
dotnet run -c Release --project benchmarks/FSharp.TypeProviders.SDK.Benchmarks -- --filter '*Large*'
dotnet run -c Release --project benchmarks/FSharp.TypeProviders.SDK.Benchmarks -- --filter '*Property*'
dotnet run -c Release --project benchmarks/FSharp.TypeProviders.SDK.Benchmarks -- --filter '*Complex*'
dotnet run -c Release --project benchmarks/FSharp.TypeProviders.SDK.Benchmarks -- --filter '*Nested*'
```

### Quick timing mode (for before/after comparison)

```bash
dotnet run -c Release --project benchmarks/FSharp.TypeProviders.SDK.Benchmarks -- --quick
```

This pre-builds all type provider instances (excluding setup from timing) and measures compilation only, covering all scenarios.

## Optimisations Tracked

### `transType` memoization (PR #443)

Caches `ILType` per `System.Type` to avoid repeated IL-type translation for the same .NET type.

### `transTypeRef` caching

Caches `ILTypeRef` per `System.Type` (generic type definitions share the same entry). Avoids allocating duplicate `ILTypeRef` objects when the same BCL type appears as a field type, method parameter, or declaring type in multiple schema members.

### `transMethRef` caching

Caches `ILMethodRef` per `MethodInfo` (using `GetDefinition()` as the key to treat all instantiations of a generic method as one entry). Eliminates redundant `ILMethodRef` allocations when the same BCL method is referenced from many generated method bodies — the main scenario for the `ComplexBodyBenchmark`.
