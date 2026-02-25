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

## Notes
- The repository uses paket for dependency management
- Main SDK targets netstandard2.0 for package compatibility; net8.0 for engineering
- Test projects target net8.0
- ProvidedTypes.fs has TreatWarningsAsErrors=true; needs #nowarn "0044" for .NET 8 serialization deprecation warnings
- Maintainer has -1 reaction on monthly activity issue #421 — be conservative about run frequency
- Issue #438 tracks no-op runs - maintainer is clearly seeing noise
- Maintainer asked in issue #341 at 19:36 UTC to profile SwaggerProvider perf - done and responded
- transType memoization PR #443 targets the identified bottleneck (no caching of repeated type translations)
