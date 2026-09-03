# Repo Assist Memory

## Last Updated
2026-09-03

## Merged PRs since 8.11.0 (documented in 8.12.0 release notes this run)
- PR #520: perf - stop re-deserializing CodeGenerator quotation patterns per member body (issue #341)
- PR #525/#526: Protect varTable/assemblyTable with typeTablesLock for thread safety
- PR #523: Fix invalid IL for generative methods with System.Void return type
- PR #532: Document agentic contribution workflow (CONTRIBUTING.md)

## Open PRs (as of 2026-09-03)
- eng: update .NET SDK to 8.0.130; prepare release 8.12.0 (repo-assist, created 2026-09-03, draft) - branch repo-assist/eng-sdk-8.0.130-release-8.12.0-20260903

## Open Issues (as of 2026-09-03)
- #298: Binary reader removal - Repo Assist commented 2026-02-26 (SRM migration approach), no new activity
- #299: Binary writer removal - Repo Assist commented 2026-02-26 (PersistedAssemblyBuilder), no new activity
- #300: Quotation reflection hack removal - Repo Assist commented 2026-02-26 (needs FSharp.Core fix), no new activity
- #384: Wrong namespace for generative TPs - Repo Assist commented 2026-02-26 (compiler-side, attribute blob root cause); last human comment/update 2026-03-07 (a label/reference change, no new substantive discussion) - no re-engagement needed
- New Monthly Activity issue for 2026-09 (number assigned post-workflow); closed #534 (August, no new activity)

## Notes
- The repository uses paket for dependency management
- Main SDK targets netstandard2.0 for package compatibility; net8.0 for engineering
- Test projects target net8.0
- ProvidedTypes.fs has TreatWarningsAsErrors=true; needs #nowarn "0044" for .NET 8 serialization deprecation warnings
- CONTRIBUTING.md (added via #532) now requires: every PR must have a matching discussed issue; Repo Assist may be invoked via /repo-assist <instructions>
- `./build.sh RunTests` (FAKE) fails in this sandbox: FAKE tries to resolve SDK runtime pack list over network and gets blocked by proxy (403), falling back to only finding 6.0.x NETCore.App.Ref packs, which don't match. Use `dotnet test tests/FSharp.TypeProviders.SDK.Tests.fsproj -c Release` directly instead as a reliable substitute in this sandboxed environment - confirmed 165/165 tests pass.
- Version numbers come from RELEASE_NOTES.md's newest heading (read via FAKE's release.NugetVersion) - "preparing a release" = adding a new dated entry at the top of RELEASE_NOTES.md summarizing merged, undocumented changes.
- global.json SDK pin: 8.0.125 -> 8.0.130 (latest 8.0.1xx per dotnet/core releases.json as of 2026-09-03)
- Remaining TODOs in ProvidedTypes.fs (lines 1341, 7084, 7087, 9361, 13952, 14130, 14443) reviewed this run: all are either intentional/stable design notes (e.g. eqILScopeRef always true is fine because name-based binding suffices) or low-value/out-of-scope (hardcoded unit abbreviations without FCS dependency) - not actionable without deeper design discussion; left alone per "no breaking changes without approval" guideline
- Issues #298, #299, #300, #384 all have Repo Assist comments with no new human activity since last comment - anti-spam rule: do not re-engage
- Only 4 "real" open issues remain (298, 299, 300, 384) plus the rolling Monthly Activity tracking issue
- Issue #384 is a compiler-side bug (dotnet/fsharp) - attribute blobs store type names as strings, not rewritten in generative TP compile; no upstream issue filed yet - potential future task
