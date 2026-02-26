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
- PR #444: Tests and docs for equality/comparison on generative provided types (merged 2026-02-26)
- PR #445: Add docs/tests for nonNullable, hideObjectMethods, AddCustomAttribute, ProvidedMeasureBuilder (merged 2026-02-26)
- PR #446: Remove NUnit.Console and Microsoft.NETCore.App from template (merged 2026-02-26)
- PR #455: Docs overhaul guide (merged 2026-02-26)
- PR #457: transTypeRef/transMethRef caching (merged 2026-02-26)
- PR #458: Fix GetNestedType on TypeSymbol/ProvidedTypeSymbol (merged 2026-02-26)
- PR #459: Fix mutable variable captures in QuotationSimplifier (merged 2026-02-26)

## Open PRs
- PR #462: Release 8.3.0 preparation (repo-assist, created 2026-02-26, draft) - CI passing
- PR #463: Fix custom attribute encoding (repo-assist, created 2026-02-26, draft) - CI passing

## Open Issues (as of 2026-02-26)
- #298: Binary reader removal - Repo Assist commented 2026-02-26 (SRM migration approach)
- #299: Binary writer removal - Repo Assist commented 2026-02-26 (PersistedAssemblyBuilder)
- #300: Quotation reflection hack removal - Repo Assist commented 2026-02-26 (needs FSharp.Core fix)
- #384: Wrong namespace for generative TPs - Repo Assist commented 2026-02-26 (compiler-side, attribute blob root cause)
- #421: Monthly Activity 2026-02 (Repo Assist tracking issue)
- #460: No-Op Runs tracker - do NOT engage
- #461: Repo Assist failed tracker - do NOT engage

## All Open Issues Now Have Repo Assist Comments

## Notes
- The repository uses paket for dependency management
- Main SDK targets netstandard2.0 for package compatibility; net8.0 for engineering
- Test projects target net8.0
- ProvidedTypes.fs has TreatWarningsAsErrors=true; needs #nowarn "0044" for .NET 8 serialization deprecation warnings
- Schedule is DAILY at 08:08 UTC (changed by dsyme 2026-02-26 from --repeat)
- Maintainer has -1 reaction on monthly activity issue #421 — be conservative about run frequency
- Issues #460, #461 are automated failure/no-op trackers - do NOT engage
- All currently open issues have Repo Assist comments
- UncheckedQuotations module (lines 332-517) uses mkFE0/1/2/3/N internal FSharp.Core methods - stable since F# 2.0
- Binary reader in AssemblyReader module starts at ~line 1941 in ProvidedTypes.fs
- Issue #384 is a compiler-side bug (dotnet/fsharp) - attribute blobs store type names as strings, not rewritten in generative TP compile
- Release 8.3.0 prepared 2026-02-26 with RELEASE_NOTES.md update; PR #462 created
- Custom attr encoding fix: implemented obj[] support in encodeCustomAttrElemTypeForObject (was failwith "TODO"); applied transValue to constructorArgs and namedProps/namedFields in defineCustomAttrs (was dead code). 104 tests pass. PR #463 created.
- Issues #342, #294, #325 are now closed (removed from Suggested Actions in Monthly Activity)
- Last run at 16:08 UTC: status check — no new work; noted struct type support gap; updated Monthly Activity issue
- ProvidedTypeDefinition has no isStruct parameter - SDK cannot generate struct value types (potential future work)
- TODO at line 6735/6740: decodeILCustomAttribData returns null for System.Type custom attr args (low priority)
