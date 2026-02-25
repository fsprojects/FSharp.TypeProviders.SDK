# Repo Assist Memory

## Last Updated
2026-02-25

## Merged PRs (this month)
- PR #422: Release 8.2.0 preparation (merged 2026-02-24)
- PR #428: Warn when all static parameters are optional (merged 2026-02-25)
- PR #430: Update fsdocs from 11.4.3 to 11.5.1 (merged 2026-02-25)
- PR #431: Update from .NET 5 to .NET 8 (merged 2026-02-25)
- PR #432: Fix custom attributes on nested erased types (merged 2026-02-25)
- PR #437: Update fsdocs-tool to v21 (merged 2026-02-25)

## Open PRs
- PR #442: Add coverage tests and Coverage build target (issue #424)
- PR #443: Memoize transType in AssemblyCompiler to reduce redundant type translation (issue #341)
- PR #444: Add tests and docs for equality/comparison on generative provided types (issue #99)
- PR #445: Add docs and tests for nonNullable, hideObjectMethods, AddCustomAttribute, ProvidedMeasureBuilder (issues #170 and #67)
- PR #446: Remove NUnit.Console and Microsoft.NETCore.App from template (issue #387)

## Notes
- The repository uses paket for dependency management
- Main SDK targets netstandard2.0 for package compatibility; net8.0 for engineering
- Test projects target net8.0
- ProvidedTypes.fs has TreatWarningsAsErrors=true; needs #nowarn "0044" for .NET 8 serialization deprecation warnings
- Maintainer has -1 reaction on monthly activity issue #421 — be conservative about run frequency
- Issue #438 tracks no-op runs - maintainer is clearly seeing noise
- Issue #441 is also no-op runs issue - avoid engaging with it
- Maintainer asked in issue #341 at 19:36 UTC to profile SwaggerProvider perf - done and responded; PR #443 created
- Maintainer asked in issue #99 at 19:26 UTC to investigate equality/comparison on provided types - done; PR #444 created
- Maintainer asked in issue #170 at 19:25 UTC to "add to the docs also tests" - done; PR #445 created
- Maintainer asked in issue #67 at 19:25 UTC to "add to the docs" - done (combined in PR #445)
- IEquatable<ProvidedType> and IComparable<ProvidedType> both work in current SDK
- Need to open UncheckedQuotations when using Expr.FieldGetUnchecked/FieldSetUnchecked in test code
- ProvidedConstructor does NOT have AddCustomAttribute (use AddObsoleteAttribute instead); this is documented in PR #445
- Template fix: NUnit.Console caused NUnit.Extension.TeamCityEventListener warning (unlisted pkg) - fixed in PR #446
- Older issues not yet engaged with: #384 (wrong namespace generative TPs - dsyme is aware), #336 (GetUnionCases on option - workaround exists), #325 (lazy TP instantiation - enhancement)
