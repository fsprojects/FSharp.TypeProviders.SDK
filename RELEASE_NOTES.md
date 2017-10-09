#### 3.0.0 - Ovtober 9 2017
* All type providers now use a ProvidedTypesContext, e.g.
     let ctxt = ProvidedTypesContext.Create(config, isForGenerated=false)
     ...
     let myType = ctxt.ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
    ...
  There are no more direct constructors for ProvidedTypeDefinition etc.

* ProvidedTypesContext.Create now takes a flag isForGenerated. It should be set to true for generative type providers

* IsStaticMethod becomes IsStatic and some other similar naming changes

* Direct setters such as prop.GetterCode <- ... are removed in favour of optional parameters. You must specify GetterCode as a parameter

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
