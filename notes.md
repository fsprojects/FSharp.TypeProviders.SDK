# Repo Assist Memory

## Last Updated
2026-02-24

## Fix Attempts
- Issue #425 (Update from .NET 5 to .NET 8): PR created from branch `repo-assist/fix-issue-425-net8-update`. All tests pass.

## Notes
- The repository uses paket for dependency management
- Main SDK targets netstandard2.0 for package compatibility; net8.0 for engineering
- Test projects target net8.0
- ProvidedTypes.fs has TreatWarningsAsErrors=true; needed #nowarn "0044" for .NET 8 serialization deprecation warnings
