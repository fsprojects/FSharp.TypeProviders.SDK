#### 7.0.0 - September 21, 2021
* Updated packages
* Moved to netstandard2.1
* Nuget includes ProvidedTypes.fs/fsi source in "src/"

#### 6.0.0 - December 24, 2019
* Template migrated to .NET Core 3.1 

#### 5.0.0 - September 17, 2018
* Addition of template pack
* Many bug fixes

#### 4.1.0 - October 24 2017
* Many improvemnts to the implementation of type mapping
* Remove Default on ProvidedMeasureBuilder and make members static

#### 4.0.0 - October 17 2017
* Config parameter new passed to ``TypeProviderForNamespaces``, i.e. ``inherit TypeProviderForNamespaces(config)``
* ProvidedTypesContext now internal to ``TypeProviderForNamespaces``.  You can access it using ``this.TargetContext`` in the ``TypeProviderForNamespaces`` object
* Calls such as ``ctxt.ProvidedTypeDefinition(...)`` become just ``ProvidedTypeDefinition`` once again
* ``ProvidedLiteralField`` --> ``ProvidedField.Literal``
* Bug fixes to context translation based on further testing
* ``ProvidedTypesContext.MakeGenericType`` --> ``ProvidedTypeBuilder.MakeGenericType``
* ``ProvidedTypesContext.MakeGenericMethod`` --> ``ProvidedTypeBuilder.MakeGenericMethod``
* More internal consistency checking of translation, better diagnostics
* Some getters removed from public API.
* ``IsImplicitCtor`` --> ``IsImplicitConstructor``
* ``ProvidedTypeDefinition.Logger`` removed
* ``TryBindAssembly*`` --> ``TryBindTargetAssembly*``
* ``RegisterGenerated`` --> ``RegisterGeneratedTargetAssembly``
* ``tp.TargetContext`` added to access the target context of referenced assemblies


#### 3.0.1 - October 1 2017
* Cross-targeting for generative type providers, and reimplement binary reader 

#### 3.0.0 - October 9 2017
* All type providers now use a ProvidedTypesContext, e.g.
     let ctxt = ProvidedTypesContext.Create(config)
     ...
     let myType = ctxt.ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
    ...
  There are no more direct constructors for ProvidedTypeDefinition etc.

* ProvidedTypesContext.Create now takes a flag isForGenerated. It should be set to true for generative type providers

* ``IsStaticMethod=true`` becomes ``isStatic=true`` and some other similar naming changes

* Direct setters such as prop.GetterCode <- ... are removed in favour of optional parameters ``getterCode=...``. You must specify getterCode as a parameter

* Enables use as part of .NET Core execution of the F# compiler by extending TypeDelegator instead of Type. This needs to be more fully tested but repo itself now compiles as both .NET Standard 2.0 and .NET Framework, and passes tests as both .NET CoreApp 2.0 and .NET Framework 4.6.1

* Puts everything into one file ProvidedTypes.fs and ProvidedTypes.fsi. This file is large but this approach makes it very easy for people writing existing type providers to update (after accounting for changes in the ProvidedTypes API)

#### 2.1.0 - August 25 2017
* Mono 5 support
* Parameter names unification

#### 2.0.0 - 02/02/2016
* Updates for cross-targeting of type providers

#### 1.1.3 - July 30 2014
* Remove folders

#### 1.1.2 - July 30 2014
* Fix nuget package

#### 1.1.1 - July 17 2014
* Fix build on some versions of .net

#### 1.1.0 - July 11 2014
* Fixed utterly broken indentation in ProvidedTypes.md
* Add compile step to build as sanity check

#### 1.0.1 - July 7 2014
* Add targets file to help PCL builds

#### 1.0 - July 7 2014
* Latest updates merged in, first FsProjects based release

#### 0.9.1 - December 16 2013
* Make sure files are added in the right order by putting them in a sub folder.

#### 0.9 - December 16 2013
* Initial release
