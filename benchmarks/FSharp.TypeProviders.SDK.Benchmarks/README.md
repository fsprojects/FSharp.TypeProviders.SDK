# FSharp.TypeProviders.SDK Benchmarks

Performance benchmarks for the FSharp.TypeProviders.SDK using [BenchmarkDotNet](https://benchmarkdotnet.org/).

## Overview

The benchmark suite measures **generative type provider compilation** — specifically the time spent in `GetGeneratedAssemblyContents`, which is where `transType` (and its new memoization cache) runs.

| Benchmark | Scenario | Description |
|-----------|----------|-------------|
| `GenerativeCompilationBenchmark` | Moderate / Heavy | 50 or 200 types, 20 methods each |
| `LargeSchemaProviderBenchmark` | Stress | 500 types, 10 methods each (simulates SwaggerProvider over a large OpenAPI spec) |

All scenarios use 12 common .NET types (`string`, `int`, `bool`, `float`, `int64`, `DateTime`, `Guid`, `decimal`, `byte[]`, `string option`, `int option`, `bool option`) cycling through parameter and return types to maximise `transType` cache hits.

## Running

### Full BenchmarkDotNet run (recommended — requires Release mode)

```bash
dotnet run -c Release --project benchmarks/FSharp.TypeProviders.SDK.Benchmarks -- --filter '*'
```

To run a specific benchmark class:

```bash
dotnet run -c Release --project benchmarks/FSharp.TypeProviders.SDK.Benchmarks -- --filter '*Large*'
```

### Quick timing mode (for before/after comparison)

```bash
dotnet run -c Release --project benchmarks/FSharp.TypeProviders.SDK.Benchmarks -- --quick
```

This pre-builds all type provider instances (excluding setup from timing) and measures compilation only.

## Before/After Results — `transType` Memoization (PR #443)

Measured with `--quick` on an Ubuntu 22.04 runner (GitHub Actions, 4-core, `net8.0`).
Setup time (creating `ProvidedTypeDefinition` instances) is excluded — only `GetGeneratedAssemblyContents` is timed.

| Scenario | Before (no cache) | After (cache) | Improvement |
|----------|----------------:|-------------:|------------:|
| 50 types × 20 methods | 910 ms | 893 ms | ~2% |
| 200 types × 20 methods | 3689 ms | 3521 ms | ~5% |
| 500 types × 10 methods | 4564 ms | 4392 ms | ~4% |

**Note:** The improvement is modest here because the 500-type scenario only calls `transType` with 12 distinct types, and each individual call is cheap (a few .NET reflection property checks). Real-world gains are larger when:

- The schema contains many *distinct* complex types (deeply nested generics, arrays of value types, etc.)
- `transType` is called via other paths in `Compile()` (field types, local variable types) in addition to method signatures
- The type provider is invalidated and re-compiled multiple times in the same IDE session

The cache itself has near-zero overhead: one `Dictionary.TryGetValue` per `transType` call (~30 ns on a modern processor).
