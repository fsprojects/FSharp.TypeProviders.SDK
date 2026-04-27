#### 8.10.0 - April 27, 2026

- Bug fix: `defineCustomAttrs` in the generative assembly compiler now correctly handles array-typed constructor arguments whose values are `IReadOnlyList<CustomAttributeTypedArgument>` — the format that `GetCustomAttributesData()` returns for array-typed constructor parameters. Previously, propagating real reflected `CustomAttributeData` with array constructor args to a provided type would crash the assembly compiler.
- Tests: Add regression test for `IReadOnlyList<CustomAttributeTypedArgument>` array constructor arg round-trip

#### 8.9.0 - April 23, 2026

- Tests: Add `GenerativeNestedTypesTests` — 5 tests for generative nested types: constructors, instance/static properties, instance methods inside nested classes
- Refactor: `GetNestedTypes`/`GetMembers` on `ProvidedTypeDefinition` now use `canBindNestedType` for consistent BindingFlags-aware filtering of nested types

#### 8.8.0 - April 21, 2026

- Tests: Add `GenerativeInheritanceTests` — 5 tests for generative type inheritance: abstract base class, concrete derived classes, virtual method override dispatch verified at runtime

#### 8.7.0 - April 19, 2026

- Tests: Add `GenerativeMethodsTests` covering instance/static methods and method-count in generative types #505
- Tests: Add `GenerativeCustomAttributeTests` — 5 regression tests for custom attribute encoding on types, methods, and properties (string, bool, enum arguments; multiple attributes per member)

#### 8.6.0 - April 15, 2026

- Bug fix: Fix `ProvidedTypeDefinition.Logger` creating a new delegate reference on each call #501
- Refactor/Performance: Convert `ILFieldDefs`/`ILEventDefs`/`ILPropertyDefs` to concrete classes with lazy `O(1)` name-lookup caches #502
- Tests: Add `GenerativePropertiesTests` covering instance/static/read-write properties and name-lookup

#### 8.5.0 - April 7, 2026

- Performance: O(1) assembly-name dictionary lookup in `convTypeRef` #493
- Performance: Avoid O(n²) allocations in `ILMethodDefs` name index construction; use lazy caches in `TargetTypeDefinition` for `GetField`/`GetPropertyImpl`/`GetEvent` #497
- Refactor: Add `save`-based caching to `GetField`/`GetEvent`/`GetNestedType` on `ProvidedTypeDefinition`; use `Dictionary` in `ILNestedExportedTypesAndForwarders` #498
- CI: Add NuGet and FAKE build caching to CI workflows #495

#### 8.4.0 - March 31, 2026

- Bug fix: Fix `GetEnumUnderlyingType()` to correctly handle non-Int32 enum underlying types #470
- Bug fix: Fix `decodeILCustomAttribData` to read correct byte-width for non-Int32 enum fixed arguments (ECMA-335) #475
- Bug fix: Fix generative delegate type support; implement `GetInterface` on `ProvidedTypeDefinition` and `TargetTypeDefinition` #479
- Bug fix: Fix thread-safety races in `TargetTypeDefinition` member-wrapper caches #482
- Bug fix: Fix `decodeILCustomAttribData` to resolve `System.Type` custom attribute arguments (previously always returned null)
- Performance: Cache member wrapper objects in `TargetTypeDefinition` to avoid repeated allocations #471
- Performance: Cache `FullName`, `BaseType` and `GetInterfaces()` in `TargetTypeDefinition` #485
- Refactor: Simplify `mkCacheInt32`/`mkCacheGeneric` to use `ConcurrentDictionary` #486
- CI: Update GitHub Actions from v1 to v4 #476

#### 8.3.0 - February 26, 2026

- Performance: Memoize `transType` in `AssemblyCompiler` to reduce redundant type translation #443
- Performance: Cache `transTypeRef` and `transMethRef` in assembly compiler #457
- Bug fix: Fix custom attributes on nested erased types #432
- Bug fix: Fix `GetNestedType` on `TypeSymbol`/`ProvidedTypeSymbol` for generic provided types #458
- Bug fix: Fix mutable variable captures in `QuotationSimplifier` — promote to ref cells #459
- New warning when all static parameters in a type provider are optional #428
- Documentation guide overhaul #455
- Add coverage tests and Coverage build target #442
- Update to .NET 8 SDK and toolchain #431

#### 8.2.0 - February 24, 2026

- Performance optimizations for design-time loading: lazy ILAssemblyRefs, optimize convTypeRef, inline isNull #417
- Reference assembly loading fixes: don't reload already-loaded assemblies
- Build fix for FX_NO_LOCAL_FILESYSTEM #404

#### 8.1.0 - January 21, 2025

- Performance optimizations #406, #411
- Fix typing corrections #414, #410
- Remove AutoOpen from ILTableName #412
- Remove warning about patter discard not allowed for union case that takes no data #401
- Update hardcoded units of measure #394
- Fix MakeGenericType fallback #393
- Remove unnecessary pattern discards #392
- Fixes to allow TPs to have references, also notes on using the nuget package #389
- Building with .NET 8 compiler and new FAKE and Paket #409
- FSharpLint as .NET tool #398
- Fix Ubuntu builds #413

#### 8.0.2 - August 17, 2022

- [Fixes to allow TPs to have references, also notes on using the nuget package](https://github.com/fsprojects/FSharp.TypeProviders.SDK/pull/389)

#### 8.0.1 - August 17, 2022

- [Workaround for target assemblies where packages with transitive references contain type providers](https://github.com/fsprojects/FSharp.TypeProviders.SDK/pull/388)

#### 7.0.3 - September 22, 2021

- Fix template .NET version in global.json

#### 7.0.2 - September 22, 2021

- Fix template paket version

#### 7.0.1 - September 21, 2021

- Allow both netstandard2.0 and netstandard2.1

#### 7.0.0 - September 21, 2021

- Updated packages
- Moved to netstandard2.1
- Nuget includes ProvidedTypes.fs/fsi source in "src/"

#### 6.0.0 - December 24, 2019

- Template migrated to .NET Core 3.1

#### 5.0.0 - September 17, 2018

- Addition of template pack
- Many bug fixes

#### 4.1.0 - October 24 2017

- Many improvemnts to the implementation of type mapping
- Remove Default on ProvidedMeasureBuilder and make members static

#### 4.0.0 - October 17 2017

- Config parameter new passed to `TypeProviderForNamespaces`, i.e. `inherit TypeProviderForNamespaces(config)`
- ProvidedTypesContext now internal to `TypeProviderForNamespaces`. You can access it using `this.TargetContext` in the `TypeProviderForNamespaces` object
- Calls such as `ctxt.ProvidedTypeDefinition(...)` become just `ProvidedTypeDefinition` once again
- `ProvidedLiteralField` --> `ProvidedField.Literal`
- Bug fixes to context translation based on further testing
- `ProvidedTypesContext.MakeGenericType` --> `ProvidedTypeBuilder.MakeGenericType`
- `ProvidedTypesContext.MakeGenericMethod` --> `ProvidedTypeBuilder.MakeGenericMethod`
- More internal consistency checking of translation, better diagnostics
- Some getters removed from public API.
- `IsImplicitCtor` --> `IsImplicitConstructor`
- `ProvidedTypeDefinition.Logger` removed
- `TryBindAssembly*` --> `TryBindTargetAssembly*`
- `RegisterGenerated` --> `RegisterGeneratedTargetAssembly`
- `tp.TargetContext` added to access the target context of referenced assemblies

#### 3.0.1 - October 1 2017

- Cross-targeting for generative type providers, and reimplement binary reader

#### 3.0.0 - October 9 2017

- All type providers now use a ProvidedTypesContext, e.g.
  let ctxt = ProvidedTypesContext.Create(config)
  ...
  let myType = ctxt.ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
  ...
  There are no more direct constructors for ProvidedTypeDefinition etc.

- ProvidedTypesContext.Create now takes a flag isForGenerated. It should be set to true for generative type providers

- `IsStaticMethod=true` becomes `isStatic=true` and some other similar naming changes

- Direct setters such as prop.GetterCode <- ... are removed in favour of optional parameters `getterCode=...`. You must specify getterCode as a parameter

- Enables use as part of .NET Core execution of the F# compiler by extending TypeDelegator instead of Type. This needs to be more fully tested but repo itself now compiles as both .NET Standard 2.0 and .NET Framework, and passes tests as both .NET CoreApp 2.0 and .NET Framework 4.6.1

- Puts everything into one file ProvidedTypes.fs and ProvidedTypes.fsi. This file is large but this approach makes it very easy for people writing existing type providers to update (after accounting for changes in the ProvidedTypes API)

#### 2.1.0 - August 25 2017

- Mono 5 support
- Parameter names unification

#### 2.0.0 - 02/02/2016

- Updates for cross-targeting of type providers

#### 1.1.3 - July 30 2014

- Remove folders

#### 1.1.2 - July 30 2014

- Fix nuget package

#### 1.1.1 - July 17 2014

- Fix build on some versions of .net

#### 1.1.0 - July 11 2014

- Fixed utterly broken indentation in ProvidedTypes.md
- Add compile step to build as sanity check

#### 1.0.1 - July 7 2014

- Add targets file to help PCL builds

#### 1.0 - July 7 2014

- Latest updates merged in, first FsProjects based release

#### 0.9.1 - December 16 2013

- Make sure files are added in the right order by putting them in a sub folder.

#### 0.9 - December 16 2013

- Initial release
