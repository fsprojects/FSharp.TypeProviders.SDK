#if INTERACTIVE
#r "../packages/NUnit/lib/net45/nunit.framework.dll"
#load "../src/ProvidedTypes.fsi" "../src/ProvidedTypes.fs" "../src/AssemblyReader.fs" "../src/AssemblyReaderReflection.fs" "../src/ProvidedTypesContext.fs" 
#load "../src/ProvidedTypesTesting.fs"
#load "FsUnit.fs"
#else
module FSharp.TypeProviders.StarterPack.Tests.StaticProperty
#endif

open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection
open NUnit.Framework
open FsUnit

#nowarn "760" // IDisposable needs new

[<TypeProvider>]
type ErasingProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "StaticProperty.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let ctxt = ProvidedTypesContext.Create(config)

    let createTypes () =
        let myType = ctxt.ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        let myStaticGetterProp = ctxt.ProvidedProperty("MyStaticGetterProperty", typeof<string list>, IsStatic = true, getterCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>))
        let myStaticSetterProp = ctxt.ProvidedProperty("MyStaticSetterProperty", typeof<string list>, IsStatic = true, getterCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>), setterCode = (fun args -> <@@ () @@>))
        let myStaticMethod = ctxt.ProvidedMethod("MyStaticMethod", [ ctxt.ProvidedParameter("paramName",typeof<string list>) ], typeof<string list>, IsStaticMethod = true, invokeCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>))
        let myGetterProp = ctxt.ProvidedProperty("MyGetterProperty", typeof<string list>, getterCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>))
        let mySetterProp = ctxt.ProvidedProperty("MySetterProperty", typeof<string list>, getterCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>), setterCode = (fun args -> <@@ () @@>))
        let myMethod = ctxt.ProvidedMethod("MyMethod", [ ctxt.ProvidedParameter("paramName",typeof<string list>) ], typeof<string list>, invokeCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>))
        myType.AddMembers [myStaticGetterProp; myStaticSetterProp; myGetterProp; mySetterProp]
        myType.AddMembers [myStaticMethod; myMethod ]

        [myType]

    do
        this.AddNamespace(ns, createTypes())


[<TypeProvider>]
type ErasingConstructorProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "ErasedWithConstructor.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let ctxt = ProvidedTypesContext.Create(config)

    let createTypes () =
        let myType = ctxt.ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)

        let ctor = ctxt.ProvidedConstructor([], invokeCode = fun args -> <@@ ["My internal state"] :> obj @@>)
        myType.AddMember(ctor)

        let ctor2 = ctxt.ProvidedConstructor([ctxt.ProvidedParameter("InnerState", typeof<string list>)], invokeCode = fun args -> <@@ (%%(args.[0]):string list) :> obj @@>)
        myType.AddMember(ctor2)

        let innerState = ctxt.ProvidedProperty("InnerState", typeof<string list>, getterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string list @@>)
        myType.AddMember(innerState)

        [myType]

    do
        this.AddNamespace(ns, createTypes())

[<TypeProvider>]
type ErasingProviderWithStaticParams (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "StaticProperty.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let ctxt = ProvidedTypesContext.Create(config)

    let createType (typeName, n:int) =
        let myType = ctxt.ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)
        let myProp = ctxt.ProvidedProperty("MyGetterProperty", typeof<string list>, IsStatic = true, getterCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>))
        myType.AddMember(myProp)
        myType

    do
        let myType = ctxt.ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        let parameters = [ ctxt.ProvidedStaticParameter("Count", typeof<int>) ]
        myType.DefineStaticParameters(parameters, (fun typeName args -> createType(typeName, args.[0] :?> int)))

        this.AddNamespace(ns, [myType])

let testCrossTargeting (refs: string list) provider args = 
    Testing.GenerateProvidedTypeInstantiation (__SOURCE_DIRECTORY__, refs.[0], refs, provider, args ) 
    |> (fun t -> Testing.FormatProvidedType(t,useQualifiedNames=true))
    |> fun s -> s.Trim()
    |> fun s -> s.Replace("\r\n","\n")

[<Test>]
let ``ErasingProvider generates for .NET 4.5 F# 3.1 correctly``() : unit  = 
    let res = testCrossTargeting Targets.DotNet45FSharp31Refs ErasingProvider [| |]
    Assert.False(res.Contains "[FSharp.Core, Version=3.259.3.1")
    Assert.True(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.0.0")

[<Test>]
let ``ErasingProvider generates for .NET 4.5 F# 4.0 correctly``() : unit  = 
    let res = testCrossTargeting Targets.DotNet45FSharp40Refs ErasingProvider [| |]
    Assert.False(res.Contains "[FSharp.Core, Version=3.259.3.1")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.True(res.Contains "[FSharp.Core, Version=4.4.0.0")



[<Test>]
let ``ErasingProvider generates for Portable Profile 259 F# 3.1 correctly``() : unit = 
    let res = testCrossTargeting Targets.Portable259FSharp31Refs ErasingProvider [| |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.259.3.1")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")

[<Test>]
let ``ErasingProvider generates for Portable Profile 259 F# 4.0 correctly``() : unit = 
    let res = testCrossTargeting Targets.Portable259FSharp40Refs ErasingProvider [| |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.259.4.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")


[<Test>]
let ``ErasingProvider generates for Portable Profile 7 F# 4.0 correctly``() : unit = 
    let res = testCrossTargeting Targets.Portable7FSharp40Refs ErasingProvider [| |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.7.4.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")

[<Test>]
let ``ErasingProviderWithStaticParams generates for Portable Profile 7 F# 4.0 correctly``() : unit = 
    let res = testCrossTargeting Targets.Portable7FSharp40Refs ErasingProviderWithStaticParams [| box 3 |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.7.4.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")

[<Test>]
let ``ErasingConstructorProvider generates for .NET 4.5 F# 3.1 correctly``() : unit  = 
    let res = testCrossTargeting Targets.DotNet45FSharp31Refs ErasingConstructorProvider [| |]
    Assert.False(res.Contains "[FSharp.Core, Version=3.259.3.1")
    Assert.True(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.0.0")

[<Test>]
let ``ErasingConstructorProvider generates for .NET 4.5 F# 4.0 correctly``() : unit  = 
    let res = testCrossTargeting Targets.DotNet45FSharp40Refs ErasingConstructorProvider [| |]
    Assert.False(res.Contains "[FSharp.Core, Version=3.259.3.1")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.True(res.Contains "[FSharp.Core, Version=4.4.0.0")



[<Test>]
let ``ErasingConstructorProvider generates for Portable Profile 259 F# 3.1 correctly``() : unit = 
    let res = testCrossTargeting Targets.Portable259FSharp31Refs ErasingConstructorProvider [| |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.259.3.1")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")

[<Test>]
let ``ErasingConstructorProvider generates for Portable Profile 259 F# 4.0 correctly``() : unit = 
    let res = testCrossTargeting Targets.Portable259FSharp40Refs ErasingConstructorProvider [| |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.259.4.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")


[<Test>]
let ``ErasingConstructorProvider generates for Portable Profile 7 F# 4.0 correctly``() : unit = 
    let res = testCrossTargeting Targets.Portable7FSharp40Refs ErasingConstructorProvider [| |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.7.4.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")


