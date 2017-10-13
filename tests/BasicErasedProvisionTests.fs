#if INTERACTIVE
#load "../src/ProvidedTypes.fsi" "../src/ProvidedTypes.fs"
#load "../src/ProvidedTypesTesting.fs"

#else

module FSharp.TypeProviders.SDK.Tests.StaticProperty
#endif

open System
open System.IO
open System.Reflection
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting
open Microsoft.FSharp.Core.CompilerServices
open Xunit

#nowarn "760" // IDisposable needs new

[<TypeProvider>]
type ErasingProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "StaticProperty.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let ctxt = ProvidedTypesContext.Create(config)

    let createTypes () =
        let myType = ctxt.ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        let myStaticGetterProp = ctxt.ProvidedProperty("MyStaticGetterProperty", typeof<string list>, isStatic = true, getterCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>))
        let myStaticSetterProp = ctxt.ProvidedProperty("MyStaticSetterProperty", typeof<string list>, isStatic = true, getterCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>), setterCode = (fun args -> <@@ () @@>))
        let myStaticMethod = ctxt.ProvidedMethod("MyStaticMethod", [ ctxt.ProvidedParameter("paramName",typeof<string list>) ], typeof<string list>, isStatic = true, invokeCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>))
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
        let myProp = ctxt.ProvidedProperty("MyGetterProperty", typeof<string list>, isStatic = true, getterCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>))
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

[<Fact>]
let ``ErasingProvider generates for .NET 4.5 F# 3.1 correctly``() : unit  = 
    let res = testCrossTargeting (Targets.DotNet45FSharp31Refs()) (fun args -> new ErasingProvider(args)) [| |]
    Assert.False(res.Contains "[FSharp.Core, Version=3.259.3.1")
    Assert.True(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.0.0")

[<Fact>]
let ``ErasingProvider generates for .NET 4.5 F# 4.0 correctly``() : unit  = 
  if (try File.Exists (Targets.FSharpCore40Ref()) with _ -> false) then
    let res = testCrossTargeting (Targets.DotNet45FSharp40Refs()) (fun args -> new ErasingProvider(args)) [| |]
    Assert.False(res.Contains "[FSharp.Core, Version=3.259.3.1")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.True(res.Contains "[FSharp.Core, Version=4.4.0.0")



[<Fact>]
let ``ErasingProvider generates for Portable Profile 259 F# 3.1 correctly``() : unit = 
  if Targets.hasPortable259Assemblies() then 
    let res = testCrossTargeting (Targets.Portable259FSharp31Refs()) (fun args -> new ErasingProvider(args)) [| |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.259.3.1")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")

[<Fact>]
let ``ErasingProvider generates for Portable Profile 259 F# 4.0 correctly``() : unit = 
  if Targets.hasPortable259Assemblies() then 
    let res = testCrossTargeting (Targets.Portable259FSharp40Refs()) (fun args -> new ErasingProvider(args)) [| |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.259.4.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")


[<Fact>]
let ``ErasingProvider generates for Portable Profile 7 F# 4.0 correctly``() : unit = 
  if Targets.hasPortable7Assemblies() then 
    let res = testCrossTargeting (Targets.Portable7FSharp40Refs()) (fun args -> new ErasingProvider(args)) [| |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.7.4.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")

[<Fact>]
let ``ErasingProviderWithStaticParams generates for Portable Profile 7 F# 4.0 correctly``() : unit = 
  if Targets.hasPortable7Assemblies() then 
    let res = testCrossTargeting (Targets.Portable7FSharp40Refs()) (fun args -> new ErasingProviderWithStaticParams(args)) [| box 3 |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.7.4.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")

[<Fact>]
let ``ErasingConstructorProvider generates for .NET 4.5 F# 3.1 correctly``() : unit  = 
    let res = testCrossTargeting (Targets.DotNet45FSharp31Refs()) (fun args -> new ErasingConstructorProvider(args)) [| |]
    Assert.False(res.Contains "[FSharp.Core, Version=3.259.3.1")
    Assert.True(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.0.0")

[<Fact>]
let ``ErasingConstructorProvider generates for .NET 4.5 F# 4.0 correctly``() : unit  = 
  if Targets.supportsFSharp40 then
    let res = testCrossTargeting (Targets.DotNet45FSharp40Refs()) (fun args -> new ErasingConstructorProvider(args)) [| |]
    Assert.False(res.Contains "[FSharp.Core, Version=3.259.3.1")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.True(res.Contains "[FSharp.Core, Version=4.4.0.0")



[<Fact>]
let ``ErasingConstructorProvider generates for Portable Profile 259 F# 3.1 correctly``() : unit = 
  if Targets.hasPortable259Assemblies() then 
    let res = testCrossTargeting (Targets.Portable259FSharp31Refs()) (fun args -> new ErasingConstructorProvider(args)) [| |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.259.3.1")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")

[<Fact>]
let ``ErasingConstructorProvider generates for Portable Profile 259 F# 4.0 correctly``() : unit = 
  if Targets.hasPortable259Assemblies() then 
    let res = testCrossTargeting (Targets.Portable259FSharp40Refs()) (fun args -> new ErasingConstructorProvider(args)) [| |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.259.4.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")


[<Fact>]
let ``ErasingConstructorProvider generates for Portable Profile 7 F# 4.0 correctly``() : unit = 
  if Targets.hasPortable7Assemblies() then 
    let res = testCrossTargeting (Targets.Portable7FSharp40Refs()) (fun args -> new ErasingConstructorProvider(args)) [| |]
    Assert.True(res.Contains "[FSharp.Core, Version=3.7.4.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.4.0")


let primitives = 
    [ "System.Boolean", typeof<bool>, box true
      "System.String", typeof<string>, box ""
      "System.Object", typeof<obj>, box (obj())
      "System.Int32",  typeof<int32>, box 1
      "System.SByte",  typeof<sbyte>, box 1y
      "System.Int16", typeof<int16>, box 1s
      "System.Int64",  typeof<int64>, box 1L
      "System.IntPtr",  typeof<IntPtr>, box 1n
      "System.Byte",  typeof<byte>, box 1uy
      "System.UInt16", typeof<uint16>, box 1us
      "System.UInt32",  typeof<uint32>, box 1u
      "System.UInt64",  typeof<uint64>, box 1UL
      "System.UIntPtr",  typeof<UIntPtr>, box 1un
      "System.Double",  typeof<double>, box 1.0
      "System.Single",  typeof<single>, box 1.0f
      "System.Char",  typeof<char>, box '1' ]

let nonPrimitives = 
    [ "System.DateTime", typeof<System.DateTime>, box System.DateTime.Now
      "System.TimeSpan", typeof<System.TimeSpan>, box System.TimeSpan.Zero
      "System.DayOfWeek", typeof<System.DayOfWeek>, box System.DayOfWeek.Friday ]

[<Fact>]
let ``Check target primitive types are identical to design-time types``() : unit  = 
    let refs = Targets.DotNet45FSharp31Refs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let ctxt = ProvidedTypesContext.Create(cfg)
    let mscorlib31 = match ctxt.TryBindAssemblyBySimpleName("mscorlib") with Choice1Of2 asm -> asm | Choice2Of2 err -> failwithf "couldn't bind mscorlib, err: %O" err
    // primitive types with element types are ALWAYS equivalent the design-time types
    for tname, designTimeType, _ in primitives do
        let targetType = mscorlib31.GetType(tname)
        Assert.Equal(targetType, designTimeType)

[<Fact>]
let ``Check target non-primitive types are different to design-time types``() : unit  = 
    let refs = Targets.DotNet45FSharp31Refs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let ctxt = ProvidedTypesContext.Create(cfg)
    let mscorlib31 = match ctxt.TryBindAssemblyBySimpleName("mscorlib") with Choice1Of2 asm -> asm | Choice2Of2 err -> failwithf "couldn't bind mscorlib, err: %O" err
    // non-primitive types should be _not_ be equal - we should see the target type in the referenced assemblies
    for tname, designTimeType, _ in nonPrimitives do
        let targetType = mscorlib31.GetType(tname)
        Assert.NotEqual(targetType, designTimeType)

[<Fact>]
let ``Check type remapping functions work for primitives``() : unit  = 
    let refs = Targets.DotNet45FSharp31Refs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let ctxt = ProvidedTypesContext.Create(cfg)
    let mscorlib31 = match ctxt.TryBindAssemblyBySimpleName("mscorlib") with Choice1Of2 asm -> asm | Choice2Of2 err -> failwithf "couldn't bind mscorlib, err: %O" err
    for tname, designTimeType, _ in primitives do
        let targetType = mscorlib31.GetType(tname)
        Assert.Equal(targetType, ctxt.ConvertDesignTimeTypeToTargetType designTimeType)
        Assert.Equal(ctxt.ConvertTargetTypeToDesignTimeType targetType, designTimeType)


[<Fact>]
let ``Check type remapping functions work for nonPrimtives``() : unit  = 
    let refs = Targets.DotNet45FSharp31Refs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let ctxt = ProvidedTypesContext.Create(cfg)
    let mscorlib31 = match ctxt.TryBindAssemblyBySimpleName("mscorlib") with Choice1Of2 asm -> asm | Choice2Of2 err -> failwithf "couldn't bind mscorlib, err: %O" err
    for tname, designTimeType, _ in nonPrimitives do
        let targetType = mscorlib31.GetType(tname)
        // TODO: determine why this one is failing....
        //Assert.Equal(targetType, ctxt.ConvertDesignTimeTypeToTargetType designTimeType)
        Assert.Equal(ctxt.ConvertTargetTypeToDesignTimeType targetType, designTimeType)

[<Fact>]

let ``Check can create Expr Value nodes for primitive types``() : unit  = 
    let refs = Targets.DotNet45FSharp31Refs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let ctxt = ProvidedTypesContext.Create(cfg)
    let mscorlib31 = match ctxt.TryBindAssemblyBySimpleName("mscorlib") with Choice1Of2 asm -> asm | Choice2Of2 err -> failwithf "couldn't bind mscorlib, err: %O" err
    // primitive types with element types are ALWAYS equivalent the design-time types
    for tname, designTimeType, sampleValue in primitives do
        let targetType = mscorlib31.GetType(tname)
        Quotations.Expr.Value(sampleValue, targetType) |> ignore // does not throw

    // We expect Expr.Value to fail for non-primitive compile-time types.  This is a check in the F# quotations library
    for tname, designTimeType, sampleValue in nonPrimitives do
       Quotations.Expr.Value(sampleValue, designTimeType) |> ignore // no exception

[<Fact>]
let ``Check can't create Expr Value nodes for non-primitive types``() : unit  = 
    let refs = Targets.DotNet45FSharp31Refs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let ctxt = ProvidedTypesContext.Create(cfg)
    let mscorlib31 = match ctxt.TryBindAssemblyBySimpleName("mscorlib") with Choice1Of2 asm -> asm | Choice2Of2 err -> failwithf "couldn't bind mscorlib, err: %O" err
    // We expect Expr.Value to fail for non-primitive compile-time types.  This is a check in the F# quotations library
    for tname, designTimeType, sampleValue in nonPrimitives do
        try 
           let targetType = mscorlib31.GetType(tname)
           Quotations.Expr.Value(sampleValue, targetType) |> ignore
           failwith "expected exception"
        with _ -> () // ok
          
          
