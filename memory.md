# Repo Assist Memory

## Last Updated
2026-02-26

## Merged PRs (this month)
- PR #422: Release 8.2.0 preparation (merged 2026-02-24)
- PR #428: Warn when all static parameters are optional (merged 2026-02-25)
- PR #430: Update fsdocs from 11.4.3 to 11.5.1 (merged 2026-02-25)
- PR #431: Update from .NET 5 to .NET 8 (merged 2026-02-25)
- PR #432: Fix custom attributes on nested erased types (merged 2026-02-25)
- PR #437: Update fsdocs-tool to v21 (merged 2026-02-25)
- PR #442: Add coverage tests and Coverage build target (merged 2026-02-26)
- PR #443: Memoize transType in AssemblyCompiler (merged 2026-02-26)
- PR #444: Equality/comparison on generative provided types (merged 2026-02-26)
- PR #445: Docs and tests for nonNullable, hideObjectMethods, AddCustomAttribute, ProvidedMeasureBuilder (merged 2026-02-26)
- PR #446: Remove NUnit.Console and Microsoft.NETCore.App from template (merged 2026-02-26)
- PR #455: Comprehensive type provider guide + rewritten docs/index.md (merged 2026-02-26)

## Open PRs
- PR created from branch repo-assist/benchmarks-stress-and-perf-improvements: transTypeRef+transMethRef caching + new stress benchmarks (issue #341)
  PR number unknown (just submitted)

## Notes
- The repository uses paket for dependency management
- Main SDK targets netstandard2.0 for package compatibility; net8.0 for engineering
- Test projects target net8.0
- ProvidedTypes.fs has TreatWarningsAsErrors=true; needs #nowarn "0044" for .NET 8 serialization deprecation warnings
- Maintainer has -1 reaction on monthly activity issue #421 — be conservative about run frequency
- Issue #438 tracks no-op runs - maintainer is clearly seeing noise
- Issue #441 is also no-op runs issue - avoid engaging with it
- Maintainer asked in issue #341 at 19:36 UTC to profile SwaggerProvider perf - done and responded; PR #443 created+merged
- Maintainer asked in issue #99 at 19:26 UTC to investigate equality/comparison on provided types - done; PR #444 created+merged
- Maintainer asked in issue #170 at 19:25 UTC to "add to the docs also tests" - done; PR #445 created+merged
- Maintainer asked in issue #67 at 19:25 UTC to "add to the docs" - done (combined in PR #445)
- Maintainer asked (dsyme) on issue #341 comment at 2026-02-26: "Keep working on benchmarks (e.g. more stress testing) and more perf improvements" - done: transTypeRef+transMethRef caches + 4 new benchmark scenarios
- IEquatable<ProvidedType> and IComparable<ProvidedType> both work in current SDK
- Need to open UncheckedQuotations when using Expr.FieldGetUnchecked/FieldSetUnchecked in test code
- ProvidedConstructor does NOT have AddCustomAttribute (use AddObsoleteAttribute instead); this is documented in PR #445
- Template fix: NUnit.Console caused NUnit.Extension.TeamCityEventListener warning (unlisted pkg) - fixed in PR #446
- Older issues not yet engaged with: #384 (wrong namespace generative TPs - dsyme is aware), #336 (GetUnionCases on option - workaround exists), #325 (lazy TP instantiation - enhancement)
- UncheckedQuotations module extension methods (FieldGetUnchecked, CallUnchecked etc.) are NOT accessible from external assemblies via open ProviderImplementation.ProvidedTypes.UncheckedQuotations; use standard Expr.FieldGet with Expr.Coerce instead
