# Repo Assist Memory

## Last Updated
2026-09-10

## Open PRs (as of 2026-09-10)
- #535 eng: update .NET SDK to 8.0.130; prepare release 8.12.0 (repo-assist, draft, created 2026-09-03) - global.json edit was blocked (protected file); PR body includes manual-apply instructions
- eng: align CI dotnet matrix with global.json SDK pin; add 8.12.0 RELEASE_NOTES entry (repo-assist, created 2026-09-10, draft) - branch repo-assist/eng-ci-dotnet-version-release-notes-20260910 - does NOT touch global.json, so it pushed cleanly. Content overlaps with #535's RELEASE_NOTES.md section; maintainers may want to close #535 and take the global.json bump manually, or merge this one first.

## Open Issues (as of 2026-09-10)
- #298, #299, #300, #384: unchanged from prior runs - Repo Assist commented 2026-02-26, no new human activity since - do not re-engage (anti-spam)
- #536: Monthly Activity 2026-09 (updated this run)

## Notes
- The repository uses paket for dependency management
- Main SDK targets netstandard2.0 for package compatibility; net8.0 for engineering
- Test projects target net8.0
- `./build.sh RunTests` (FAKE) fails in this sandbox: FAKE tries to resolve SDK runtime pack list over network and gets blocked by proxy (403). Use `dotnet tool restore && dotnet paket restore && dotnet test tests/FSharp.TypeProviders.SDK.Tests.fsproj -c Release` directly instead - confirmed 165/165 tests pass (2026-09-10 run).
- global.json is in the safe-outputs `protected_files` list (protected_files_policy: fallback-to-issue) - any PR/push that edits it cannot be auto-pushed; the create_pull_request/push_to_pull_request_branch tool will fall back to posting manual-apply instructions as an issue comment instead of creating a real PR branch. Avoid bundling global.json edits with other changes if you want the PR to go through cleanly - do the global.json-free changes in a separate PR when possible.
- CI workflow `dotnet` matrix version had drifted from `global.json`'s SDK pin (matrix said 8.0.124, global.json said 8.0.125) - fixed 2026-09-10 in both pr.yml and push.yml.
- .github/workflows/{pr,push}.yml still contain a "Setup .NET Core 6" step pinned to 6.0.425, seemingly vestigial since no .fsproj in the repo targets net6.0 (all target netstandard2.0 or net8.0). Not removed this run since the reason it was added isn't fully clear from git history - worth investigating in a future run before removing (git blame shows it's been present since early CI setup, survived several refactors).
- Issue #384 (wrong namespace for generative TPs) - compiler-side bug in dotnet/fsharp's rewrite phase for generated assemblies (per dsyme's 2022 comments); searched dotnet/fsharp issues this run for an existing upstream tracking issue and found none obviously matching - filing one could be valuable future work but requires careful reproduction steps to be actionable upstream; deferred (not done this run, no maintainer request to file cross-repo issues yet).
- Only 4 "real" open issues remain (298, 299, 300, 384) plus the rolling Monthly Activity tracking issue - Task 2/3 candidates are exhausted until new human activity appears.
