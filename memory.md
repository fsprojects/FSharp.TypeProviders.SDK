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
- PR #440 (or next): Add coverage tests and Coverage build target (closes #424)

## Notes
- The repository uses paket for dependency management
- Main SDK targets netstandard2.0 for package compatibility; net8.0 for engineering (after PR #431 merged)
- Test projects now target net8.0
- ProvidedTypes.fs has TreatWarningsAsErrors=true; needs #nowarn "0044" for .NET 8 serialization deprecation warnings
- Maintainer has -1 reaction on monthly activity issue #421 — be conservative about run frequency
- Issue #438 tracks no-op runs - maintainer is clearly seeing noise
- Issue #439 is an auto-generated failure report
- Monthly activity issue should only be updated when something was actually done
- Current line coverage: 72.2%, branch coverage: 56.8% (above 60% target already)
- coverlet.collector is already in paket.dependencies for coverage collection
- Tried to fix issue #424: added 6 new tests + Coverage FAKE target
