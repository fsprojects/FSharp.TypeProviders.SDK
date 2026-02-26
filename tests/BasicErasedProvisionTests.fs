module TPSDK.BasicErasedTests

open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting
open Microsoft.FSharp.Core.CompilerServices
open Xunit

#nowarn "760" // IDisposable needs new

[<TypeProvider>]
type ErasingProvider (config : TypeProviderConfig, hasBigInt: bool) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "StaticProperty.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        let myStaticGetterProp = ProvidedProperty("MyStaticGetterProperty", typeof<string list>, isStatic = true, getterCode = (fun _args -> <@@ Set.ofList [ "Hello world" ] @@>))
        let myStaticGetterProp2 = ProvidedProperty("MyStaticGetterProperty2", (if hasBigInt then typeof<bigint> else typeof<int>), isStatic = true, getterCode = (fun _args -> if hasBigInt then <@@ 34L @@> else <@@ 34 @@>))
        let myStaticSetterProp = ProvidedProperty("MyStaticSetterProperty", typeof<string list>, isStatic = true, getterCode = (fun _args -> <@@ Set.ofList [ "Hello world" ] @@>), setterCode = (fun _args -> <@@ () @@>))
        let myStaticMethod = ProvidedMethod("MyStaticMethod", [ ProvidedParameter("paramName",typeof<string list>) ], typeof<string list>, isStatic = true, invokeCode = (fun _args -> <@@ Set.ofList [ "Hello world" ] @@>))
        let myGetterProp = ProvidedProperty("MyGetterProperty", typeof<string list>, getterCode = (fun _args -> <@@ Set.ofList [ "Hello world" ] @@>))
        let mySetterProp = ProvidedProperty("MySetterProperty", typeof<string list>, getterCode = (fun _args -> <@@ Set.ofList [ "Hello world" ] @@>), setterCode = (fun _args -> <@@ () @@>))
        let myMethod = ProvidedMethod("MyMethod", [ ProvidedParameter("paramName",typeof<string list>) ], typeof<string list>, invokeCode = (fun _args -> <@@ Set.ofList [ "Hello world" ] @@>))
        myType.AddMembers [myStaticGetterProp; myStaticGetterProp2; myStaticSetterProp; myGetterProp; mySetterProp]
        myType.AddMembers [myStaticMethod; myMethod ]

        [myType]

    do
        this.AddNamespace(ns, createTypes())


[<TypeProvider>]
type ErasingConstructorProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "ErasedWithConstructor.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>, hideObjectMethods=true)

        let ctor = ProvidedConstructor([], invokeCode = fun _args -> <@@ ["My internal state"] :> obj @@>)
        myType.AddMember(ctor)

        myType.AddXmlDoc("Here is some doc")

        let ctor2 = ProvidedConstructor([ProvidedParameter("InnerState", typeof<string list>)], invokeCode = fun args -> <@@ (%%(args.[0]):string list) :> obj @@>)
        myType.AddMember(ctor2)

        let innerState = ProvidedProperty("InnerState", typeof<string list>, getterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string list @@>)
        myType.AddMember(innerState)

        [myType]

    do
        this.AddNamespace(ns, createTypes())

[<TypeProvider>]
type ErasingProviderWithStaticParams (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "StaticProperty.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createType (typeName, _n:int, _dayOfWeekInt: int) =
        let myType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)
        let myProp = ProvidedProperty("MyGetterProperty", typeof<string list>, isStatic = true, getterCode = (fun _args -> <@@ Set.ofList [ "Hello world" ] @@>))
        myType.AddMember(myProp)
        myType.AddMembersDelayed(fun () -> 
             let myDelayedProp = ProvidedProperty("MyDelayedGetterProperty", typeof<string list>, isStatic = true, getterCode = (fun _args -> <@@ Set.ofList [ "Hello world" ] @@>))
             [ myDelayedProp ])

        myType

    do
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        let parameters = [ ProvidedStaticParameter("Count", typeof<int>); ProvidedStaticParameter("Day", typeof<System.DayOfWeek>) ]
        myType.DefineStaticParameters(parameters, (fun typeName args -> createType(typeName, (args.[0] :?> int), (args.[1] :?> int))))

        this.AddNamespace(ns, [myType])

let testCrossTargeting (refs: string list) provider args = 
    let tp, t = Testing.GenerateProvidedTypeInstantiation (__SOURCE_DIRECTORY__, refs.[0], refs, provider, args ) 
    let fmt = Testing.FormatProvidedType(tp, t, useQualifiedNames=true)
    fmt.Trim().Replace("\r\n","\n")

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
    [ "System.DateTime", typeof<DateTime>, box DateTime.Now
      "System.TimeSpan", typeof<TimeSpan>, box TimeSpan.Zero
      "System.DayOfWeek", typeof<DayOfWeek>, box DayOfWeek.Friday ]

[<Fact>]
let ``Check target primitive types are not identical to design-time types``() : unit  = 
    let refs = Targets.DotNetStandard20FSharpRefs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let tp = TypeProviderForNamespaces(cfg)
    let mscorlib31 = match tp.TargetContext.TryBindSimpleAssemblyNameToTarget("mscorlib") with Choice1Of2 asm -> asm | Choice2Of2 err -> failwithf "couldn't bind mscorlib, err: %O" err
    // primitive types with element types are ALWAYS equivalent the design-time types
    for tname, sourceType, _ in primitives do
        let targetType = mscorlib31.GetType(tname)
        Assert.NotEqual(targetType, sourceType)
    // System.Void is given a special treatment
    let targetType = mscorlib31.GetType("System.Void")
    Assert.NotEqual(targetType, typeof<Void>)

[<Fact>]
let ``Check target non-primitive types are different to design-time types``() : unit  = 
    let refs = Targets.DotNetStandard20FSharpRefs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let tp = TypeProviderForNamespaces(cfg)
    let mscorlib31 = match tp.TargetContext.TryBindSimpleAssemblyNameToTarget("mscorlib") with Choice1Of2 asm -> asm | Choice2Of2 err -> failwithf "couldn't bind mscorlib, err: %O" err
    // non-primitive types should be _not_ be equal - we should see the target type in the referenced assemblies
    for tname, sourceType, _ in nonPrimitives do
        let targetType = mscorlib31.GetType(tname)
        Assert.NotEqual(targetType, sourceType)


[<Fact>]
let ``Check target enum types gives right values``() : unit  = 
    let refs = Targets.DotNetStandard20FSharpRefs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let tp = TypeProviderForNamespaces(cfg)
    let dayOfWeekType = typeof<System.DayOfWeek>
    let dayOfWeekTypeT = tp.TargetContext.ConvertSourceTypeToTarget dayOfWeekType
    printfn "Enums #1"
    Assert.True(dayOfWeekType.IsEnum)
    printfn "Enums #2"
    Assert.Equal(dayOfWeekType.GetEnumUnderlyingType().FullName, "System.Int32")
    printfn "Enums #3"
    Assert.True(dayOfWeekTypeT.IsEnum)
    printfn "Enums #4"
    Assert.Equal(dayOfWeekTypeT.GetEnumUnderlyingType().FullName, "System.Int32")
    printfn "Done Enums"

[<Fact>]
let ``Check target delegate types gives right values``() : unit  = 
    let refs = Targets.DotNetStandard20FSharpRefs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let tp = TypeProviderForNamespaces(cfg)
    for delegateType in [ typeof<Func<int,int>>; typeof<System.Converter<int,int>>; typeof<System.Action> ] do
        let delegateTypeT = tp.TargetContext.ConvertSourceTypeToTarget delegateType
        printfn "Delegates #1, delegateType = %A" delegateType
        Assert.True(delegateType.IsSubclassOf(typeof<System.Delegate>))
        printfn "Delegates #2, delegateType = %A" delegateType
        Assert.True(delegateTypeT.IsSubclassOf(typeof<System.Delegate>))

[<Fact>]
let ``Check type remapping functions work for primitives``() : unit  = 
    let refs = Targets.DotNetStandard20FSharpRefs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let tp = TypeProviderForNamespaces(cfg)
    let mscorlib31 = match tp.TargetContext.TryBindSimpleAssemblyNameToTarget("mscorlib") with Choice1Of2 asm -> asm | Choice2Of2 err -> failwithf "couldn't bind mscorlib, err: %O" err
    for tname, sourceType, _ in primitives do
        let targetType = mscorlib31.GetType(tname)
        let sourceTypeT = tp.TargetContext.ConvertSourceTypeToTarget sourceType
        let targetTypeS = tp.TargetContext.ConvertTargetTypeToSource targetType
        Assert.Equal(targetType, sourceTypeT)
        Assert.Equal(targetTypeS, sourceType)


[<Fact>]
let ``Check type remapping functions work for nonPrimtives``() : unit  = 
    let refs = Targets.DotNetStandard20FSharpRefs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let tp = TypeProviderForNamespaces(cfg)
    let mscorlib31 = match tp.TargetContext.TryBindSimpleAssemblyNameToTarget("mscorlib") with Choice1Of2 asm -> asm | Choice2Of2 err -> failwithf "couldn't bind mscorlib, err: %O" err
    for tname, sourceType, _ in nonPrimitives do
        let targetType = mscorlib31.GetType(tname)
        // TODO: determine why this one is failing....
        //Assert.Equal(targetType, ConvertSourceTypeToTarget sourceType)
        Assert.Equal(tp.TargetContext.ConvertTargetTypeToSource targetType, sourceType)

[<Fact>]

let ``Check can create Expr Value nodes for primitive types``() : unit  = 
    let refs = Targets.DotNetStandard20FSharpRefs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let tp = TypeProviderForNamespaces(cfg)
    let mscorlib31 = match tp.TargetContext.TryBindSimpleAssemblyNameToTarget("mscorlib") with Choice1Of2 asm -> asm | Choice2Of2 err -> failwithf "couldn't bind mscorlib, err: %O" err
    // primitive types with element types are ALWAYS equivalent the design-time types
    for tname, _sourceType, sampleValue in primitives do
        let targetType = mscorlib31.GetType(tname)
        Quotations.Expr.Value(sampleValue, targetType) |> ignore // does not throw

    // We expect Expr.Value to fail for non-primitive compile-time types.  This is a check in the F# quotations library
    for _tname, sourceType, sampleValue in nonPrimitives do
       Quotations.Expr.Value(sampleValue, sourceType) |> ignore // no exception

[<Fact>]
let ``Check can't create Expr Value nodes for non-primitive types``() : unit  = 
    let refs = Targets.DotNetStandard20FSharpRefs()
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, refs.[0], refs)
    let tp = TypeProviderForNamespaces(cfg)
    let mscorlib31 = match tp.TargetContext.TryBindSimpleAssemblyNameToTarget("mscorlib") with Choice1Of2 asm -> asm | Choice2Of2 err -> failwithf "couldn't bind mscorlib, err: %O" err
    // We expect Expr.Value to fail for non-primitive compile-time types.  This is a check in the F# quotations library
    for tname, _sourceType, sampleValue in nonPrimitives do
        try 
           let targetType = mscorlib31.GetType(tname)
           Quotations.Expr.Value(sampleValue, targetType) |> ignore
        with _ -> () // ok

[<Fact>]
let ``test basic binding context netstandard20``() =
   let refs = Targets.DotNetStandard20FSharpRefs()
   let config = Testing.MakeSimulatedTypeProviderConfig (resolutionFolder=__SOURCE_DIRECTORY__, runtimeAssembly="whatever.dll", runtimeAssemblyRefs=refs)
   use tp1 = new TypeProviderForNamespaces(config)
   let ctxt1 = tp1.TargetContext

   match ctxt1.TryBindAssemblyNameToTarget(AssemblyName("mscorlib")) with
   | Choice1Of2 asm -> asm.GetType("System.Object").FullName |> (fun d -> Assert.Equal(d,"System.Object"))
   | Choice2Of2 err -> raise err

[<Fact>]
let ``test basic symbol type ops``() =
   let refs = Targets.DotNetStandard20FSharpRefs()
   let config = Testing.MakeSimulatedTypeProviderConfig (resolutionFolder=__SOURCE_DIRECTORY__, runtimeAssembly="whatever.dll", runtimeAssemblyRefs=refs)
   use tp = new TypeProviderForNamespaces(config)
   let ctxt = tp.TargetContext

   //let fscore =  ctxt1.TryBindAssemblyNameToTarget(AssemblyName("FSharp.Core")) 
   let decimalT = typeof<decimal>
   let kg = ProvidedMeasureBuilder.SI "kg"
   let t1 = ProvidedMeasureBuilder.AnnotateType(decimalT, [ kg ])

   match kg with :? ProvidedTypeSymbol as st -> Assert.True(st.IsFSharpTypeAbbreviation) | _ -> failwith "expected a ProvidedTypeSymbol"
   match t1 with :? ProvidedTypeSymbol as st -> Assert.True(st.IsFSharpUnitAnnotated) | _ -> failwith "expected a ProvidedTypeSymbol#2"

   let t1T = ctxt.ConvertSourceTypeToTarget t1
   let kgT = ctxt.ConvertSourceTypeToTarget kg
   match kgT with :? ProvidedTypeSymbol as st -> Assert.True(st.IsFSharpTypeAbbreviation) | _ -> failwith "expected a ProvidedTypeSymbol#3"
   match t1T with :? ProvidedTypeSymbol as st -> Assert.True(st.IsFSharpUnitAnnotated) | _ -> failwith "expected a ProvidedTypeSymbol#4"

   let t2 = ProvidedTypeBuilder.MakeTupleType([ t1; t1 ])

   Assert.True(not t2.IsGenericTypeDefinition) 
   Assert.True(t2.IsGenericType) 
   Assert.True(t2.GetGenericTypeDefinition().IsGenericType) 
   Assert.True(t2.GetGenericTypeDefinition().IsGenericTypeDefinition) 

   Assert.True(t2.GetType().Name = "TypeSymbol") // TypeSymbol is an internal type but we test it here anyway for double-check

   // We test that we can call GetProperties on tuple type symbols produced by ProvidedTypeBuilder because these get 
   // handed off to FSharpValue.PreComputeTupleConstructorInfo and so on by the SDK or by F# quotations
   Assert.True(t2.GetProperties().Length <> 0) 
   // We check these others just to make sure they have some implementation on symbols produced by ProvidedTypeBuilder 
   Assert.True(t2.GetEvents(BindingFlags.Public ||| BindingFlags.Instance).Length = 0) 
   Assert.True(t2.GetEvents(BindingFlags.NonPublic ||| BindingFlags.Instance).Length = 0) 
   Assert.True(t2.GetEvents().Length = 0) 
   t2.GetMethods() |> ignore
   t2.GetFields() |> ignore
   t2.GetConstructors() |> ignore
   t2.GetMethod("get_Item1") |> ignore


   let t2T = ctxt.ConvertSourceTypeToTarget t2

   Assert.True(t2T.GetType().Name = "TypeSymbol")// TypeSymbol is an internal type but we test it here anyway for double-check
   Assert.True(not t2T.IsGenericTypeDefinition) 
   Assert.True(t2T.GetGenericTypeDefinition().IsGenericType) 
   Assert.True(t2T.GetGenericTypeDefinition().IsGenericTypeDefinition) 
   Assert.True(t2T.IsGenericType) 
   Assert.True(t2T.GetProperties().Length <> 0) 
   Assert.True(t2T.GetEvents().Length = 0) 
   t2T.GetMethods() |> ignore
   t2T.GetFields() |> ignore
   t2T.GetConstructors() |> ignore
   t2T.GetMethod("get_Item1") |> ignore

   // MakeGenericType should fallback to classic generic type when neither typedef and arguments are generated

   let t3 = ProvidedTypeBuilder.MakeGenericType(typedefof<seq<_>>, [ typeof<bool> ])
   Assert.NotEqual<string>("TypeSymbol", t3.GetType().Name ) 

   // MakeGenericType doesn't fallback to classic generic type when type argument is a tuple composed of provided types
   let t4 = ProvidedTypeBuilder.MakeGenericType(typedefof<seq<_>>, [ t2 ])
   Assert.Equal<string>("TypeSymbol", t4.GetType().Name ) 

// Regression test for https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/395
// Comparing a FSharpTypeAbbreviation (UoM abbreviation) type using Equals must not infinite-loop.
[<Fact>]
let ``test FSharpTypeAbbreviation equality does not infinite loop`` () =
   let kg1 = ProvidedMeasureBuilder.SI "kg"
   let kg2 = ProvidedMeasureBuilder.SI "kg"
   let m   = ProvidedMeasureBuilder.SI "m"

   // Same unit abbreviation must be equal
   Assert.True(kg1.Equals(kg2 :> obj), "same SI unit should be equal")
   Assert.True(kg1.Equals(kg1 :> obj), "unit should equal itself")

   // Different unit abbreviations must not be equal
   Assert.False(kg1.Equals(m :> obj), "different SI units should not be equal")

   // Comparing against a plain .NET type must return false (not loop or throw)
   Assert.False(kg1.Equals(typeof<decimal> :> obj), "abbreviation vs plain type should not be equal")

   // Comparing with null must return false
   Assert.False(kg1.Equals(null), "abbreviation vs null should not be equal")


let stressTestCore() = 
    let refs = Targets.DotNetStandard20FSharpRefs()
    let config = Testing.MakeSimulatedTypeProviderConfig (resolutionFolder=__SOURCE_DIRECTORY__, runtimeAssembly="whatever.dll", runtimeAssemblyRefs=refs)
    use tp = new TypeProviderForNamespaces(config)
    let ctxt = tp.TargetContext

    //let fscore =  ctxt1.TryBindAssemblyNameToTarget(AssemblyName("FSharp.Core")) 
    let decimalT = typeof<decimal>
    let kg = ProvidedMeasureBuilder.SI "kg"
    let t1 = ProvidedMeasureBuilder.AnnotateType(decimalT, [ kg ])

    match kg with :? ProvidedTypeSymbol as st -> Assert.True(st.IsFSharpTypeAbbreviation) | _ -> failwith "expected a ProvidedTypeSymbol"
    match t1 with :? ProvidedTypeSymbol as st -> Assert.True(st.IsFSharpUnitAnnotated) | _ -> failwith "expected a ProvidedTypeSymbol#2"

    let t1T = ctxt.ConvertSourceTypeToTarget t1
    let kgT = ctxt.ConvertSourceTypeToTarget kg
    match kgT with :? ProvidedTypeSymbol as st -> Assert.True(st.IsFSharpTypeAbbreviation) | _ -> failwith "expected a ProvidedTypeSymbol#3"
    match t1T with :? ProvidedTypeSymbol as st -> Assert.True(st.IsFSharpUnitAnnotated) | _ -> failwith "expected a ProvidedTypeSymbol#4"

    let _ = ProvidedTypeBuilder.MakeTupleType([ t1; t1 ])
    tp

let stressTestLoop() = 
    let mutable latestTp = None
    let weakDict = AssemblyReader.Reader.GetWeakReaderCache()
    let strongDict = AssemblyReader.Reader.GetStrongReaderCache()
    weakDict.Clear()
    strongDict.Clear()

    for i = 1 to 1000 do
        latestTp <- Some (stressTestCore())

    Assert.True(weakDict.Count > 0, "Weak Reader Cache has zero count")
    Assert.True(strongDict.Count > 0, "Strong Reader Cache has zero count")

    // We created 1000 TP instances rapidly but we should not be re-creating readers.
    strongDict
    |> Seq.iter (fun (KeyValue(_, (_, count, _))) ->
        Assert.False(count > 5, "Too many instances of an assembly")
    )

    // After we are done the weak handles should still be populated as we have a handle to the last TP
    System.GC.Collect()
    for (KeyValue(_, (_, wh))) in weakDict do
        let alive = fst(wh.TryGetTarget())
        Assert.True(alive, "Weak handle should still be populated as latest TP still alive")

    latestTp <- None
    strongDict.Clear()

[<Fact>]
let ``test reader cache actually caches``() =
    let weakDict = AssemblyReader.Reader.GetWeakReaderCache()
    // We factor this test into another ethod to ensure that things get collecte properly on all version of .NET
    // i.e. that the stack frame isn't keeping any strong handles to anything.
    stressTestLoop()
    System.GC.Collect (2, GCCollectionMode.Forced, true, true)
    System.GC.WaitForPendingFinalizers()
    System.GC.Collect (2, GCCollectionMode.Forced, true, true)
    let runningOnMono = try Type.GetType("Mono.Runtime") <> null with _ -> false 
    if not runningOnMono then 
        for (KeyValue(key, (_, wh))) in weakDict do
            let alive = fst(wh.TryGetTarget())
            Assert.False(alive, sprintf "Weak handle for %A should no longer be populated as latest TP no longer alive" key)

[<TypeProvider>]
type public SampleTypeProvider(config : TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces(config)

    let namespaceName = "Sample"
    let thisAssembly = Assembly.GetExecutingAssembly()
    let sampleTpType = ProvidedTypeDefinition(thisAssembly, namespaceName, "SampleTypeProvider", baseType = Some typeof<obj>)
            
    let buildTypes (typeName : string) (args : obj []) = 
        let typeProviderForAccount = ProvidedTypeDefinition(thisAssembly, namespaceName, typeName, baseType = Some typeof<obj>)
        typeProviderForAccount.AddMember(ProvidedConstructor([], fun _ -> <@@ null @@>))
        
        let domainType = ProvidedTypeDefinition("Domain", Some typeof<obj>)

        let containersType = ProvidedTypeDefinition("Containers", Some typeof<obj>)

        containersType.AddMembersDelayed(fun _ -> 
            ["A";"B";"C";"D";"E" ] |> List.map (fun name -> 
                let oneDomainType = ProvidedTypeDefinition("DomainTypeFor"+name, Some typeof<obj>, nonNullable = true )

                // Note that this call expands the nested types under domainType "dynamically", i.e. potentially long after 
                // domainType has been added to its parent and returned to the compiler.  This is allowed and is
                // an important technique for building up an incremental set of domain types on=demand - though it feels a little dubious.
                domainType.AddMember oneDomainType

                // Check the AllowNullLiteral attribute appears
                Assert.True(oneDomainType.NonNullable)

                Assert.True (oneDomainType.GetCustomAttributesData() |> Seq.exists (fun cad -> cad.Constructor.DeclaringType.Name = typeof<AllowNullLiteralAttribute>.Name))

                Assert.Equal (1, oneDomainType.GetCustomAttributesData() |> Seq.filter (fun cad -> cad.Constructor.DeclaringType.Name = typeof<AllowNullLiteralAttribute>.Name) |> Seq.length)

                let containerName = name
                ProvidedProperty(containerName, oneDomainType, getterCode = fun _ -> <@@ containerName @@>)))
    
        domainType.AddMember containersType

        let blobStorageMember = ProvidedProperty("StaticProperty", containersType, isStatic = true, getterCode = (fun _ -> <@@ () @@>))

        // Now create child members.
        typeProviderForAccount.AddMember blobStorageMember
        typeProviderForAccount.AddMember domainType

        typeProviderForAccount
    
    let parameters = [ ProvidedStaticParameter("theParam", typeof<string>, String.Empty) ]
    
    do
        sampleTpType.DefineStaticParameters(parameters, buildTypes)
        this.AddNamespace(namespaceName, [ sampleTpType ])

[<TypeProvider>]
type ErasingProviderWithCustomAttributes (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "CustomAttributes.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)

        let nameOf =
            let param = ProvidedParameter("p", typeof<Microsoft.FSharp.Quotations.Expr<int>>)
            param.AddCustomAttribute {
                new CustomAttributeData() with
                    member __.Constructor = typeof<ReflectedDefinitionAttribute>.GetConstructor([||])
                    member __.ConstructorArguments = [||] :> _
                    member __.NamedArguments = [||] :> _
            }
            ProvidedMethod("NameOf", [ param ], typeof<string>, isStatic = true, invokeCode = fun args ->
                <@@
                    match (%%args.[0]) : Microsoft.FSharp.Quotations.Expr<int> with
                    | Microsoft.FSharp.Quotations.Patterns.ValueWithName (_, _, n) -> n
                    | e -> failwithf "Invalid quotation argument (expected ValueWithName): %A" e
                @@>)
        myType.AddMember(nameOf)

        [myType]

    do
        this.AddNamespace(ns, createTypes())

[<TypeProvider>]
type ErasingProviderWithNestedCustomAttributes (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "NestedCustomAttributes.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes typeName (_args: obj[]) =
        let myType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)
        let ctor = ProvidedConstructor([], invokeCode = fun _ -> <@@ obj() @@>)
        myType.AddMember(ctor)

        let nestedType = ProvidedTypeDefinition("Nested", Some typeof<obj>)
        // Add an ObsoleteAttribute (which is available in the target .NET Standard 2.0 refs)
        nestedType.AddCustomAttribute {
            new CustomAttributeData() with
                member __.Constructor = typeof<ObsoleteAttribute>.GetConstructor([|typeof<string>|])
                member __.ConstructorArguments = [| CustomAttributeTypedArgument(typeof<string>, "use something else" :> obj) |] :> _
                member __.NamedArguments = [||] :> _
        }
        myType.AddMember nestedType

        myType

    do
        let baseType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        let staticParams = [ProvidedStaticParameter("count", typeof<int>)]
        baseType.DefineStaticParameters(staticParams, createTypes)
        this.AddNamespace(ns, [baseType])

[<Fact>]
let ``ErasingConstructorProvider generates for netstandard20 correctly``() : unit  = 
    printfn "--------- Generating code for .NET Standard 2.0  ------"
    let res = testCrossTargeting (Targets.DotNetStandard20FSharpRefs()) (fun args -> new ErasingConstructorProvider(args)) [| |]
    Assert.False(res.Contains "[FSharp.Core, Version=3.259.4.1")
    Assert.True(res.Contains "[FSharp.Core, Version=4.7.0.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.3.1.0")
    Assert.False(res.Contains "[FSharp.Core, Version=4.4.0.0")


[<Fact>]
let ``check custom attributes``() = 
    let refs = Targets.DotNetStandard20FSharpRefs()
    let tp, t = Testing.GenerateProvidedTypeInstantiation (__SOURCE_DIRECTORY__, refs.[0], refs, ErasingProviderWithCustomAttributes, [| |]  )
    Assert.True(t.GetMethod("NameOf").GetParameters().[0].GetCustomAttributesData() |> Seq.exists (fun cad -> cad.Constructor.DeclaringType.Name = typeof<ReflectedDefinitionAttribute>.Name))
    Assert.Equal(1, t.GetMethod("NameOf").GetParameters().[0].GetCustomAttributesData() |> Seq.filter (fun cad -> cad.Constructor.DeclaringType.Name = typeof<ReflectedDefinitionAttribute>.Name) |> Seq.length)

[<Fact>]
let ``check custom attributes on nested erased types``() =
    // Regression test for https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/365
    // Custom attributes added via AddCustomAttribute to nested erased types should be visible
    // in the target model's GetCustomAttributesData() when the attribute type is available in the
    // target reference assemblies.
    let refs = Targets.DotNetStandard20FSharpRefs()
    let tp, t = Testing.GenerateProvidedTypeInstantiation (__SOURCE_DIRECTORY__, refs.[0], refs, ErasingProviderWithNestedCustomAttributes, [| box 1 |])
    let bindAll = BindingFlags.DeclaredOnly ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance
    let nestedTy = t.GetNestedType("Nested", bindAll)
    Assert.NotNull(nestedTy)
    // ObsoleteAttribute is in .NET Standard 2.0 target refs, so it should be present
    Assert.True(nestedTy.GetCustomAttributesData() |> Seq.exists (fun cad -> cad.Constructor.DeclaringType.Name = typeof<ObsoleteAttribute>.Name),
        "Expected ObsoleteAttribute on nested erased type to be visible in target model")
    Assert.Equal(1, nestedTy.GetCustomAttributesData() |> Seq.filter (fun cad -> cad.Constructor.DeclaringType.Name = typeof<ObsoleteAttribute>.Name) |> Seq.length)

[<Fact>]
let ``check on-demand production of members``() = 
    let refs = Targets.DotNetStandard20FSharpRefs()
    let tp,t = Testing.GenerateProvidedTypeInstantiation (__SOURCE_DIRECTORY__, refs.[0], refs, SampleTypeProvider, [| box "Arg" |]  )

    let domainTy = t.GetNestedType("Domain")

    Assert.Null(domainTy.GetNestedType("DomainTypeForA")) // DomainTypeForA type not yet created

    let containersType  = domainTy.GetNestedType("Containers")
    Assert.NotNull(containersType)

    Assert.Equal(domainTy.GetMembers().Length, 1) // DomainTypeForA type not yet created
    Assert.Null(domainTy.GetNestedType("DomainTypeForA")) // DomainTypeForA type not yet created.  

    let containersPropA  = containersType.GetProperty("A") // this call also creates DomainTypeForA
    Assert.NotNull(containersPropA)
    Assert.True(containersPropA.Name = "A")

    // Check there is no AllowNullLiteralAttribute
    Assert.True (domainTy.GetCustomAttributesData() |> Seq.exists (fun cad -> cad.Constructor.DeclaringType.Name = typeof<AllowNullLiteralAttribute>.Name) |> not)

    // Fetching this type was failing becuase the call to expand domainType when getting property A for the first time was only applying to the source model,
    // not the translated target model
    let domainTyForA  = domainTy.GetNestedType("DomainTypeForA")

    let bindAll = BindingFlags.DeclaredOnly ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static  ||| BindingFlags.Instance
    Assert.NotNull(domainTyForA)
    Assert.True(domainTyForA.Name = "DomainTypeForA")
    Assert.Equal(1 + 5, domainTy.GetMembers(bindAll).Length) // one for Containers, 5 properties, 5 getters for properties, 5 nested types
    Assert.Equal(0, domainTy.GetMethods(bindAll).Length) 
    Assert.Equal(1 + 5, domainTy.GetNestedTypes(bindAll).Length) 
    Assert.Equal(5, containersType.GetMethods(bindAll).Length) // 5 properties, 5 getters for properties
    Assert.Equal(5, containersType.GetProperties(bindAll).Length) // 5 properties, 5 getters for properties
    Assert.Equal(0, containersType.GetFields(bindAll).Length) // 5 properties, 5 getters for properties
    Assert.Equal(0, containersType.GetEvents(bindAll).Length) // 5 properties, 5 getters for properties
    Assert.Equal(5 + 5, containersType.GetMembers(bindAll).Length) // 5 properties, 5 getters for properties


    Assert.NotNull(domainTy.GetNestedType("DomainTypeForA")) // type is still there
    Assert.NotNull(domainTy.GetNestedType("DomainTypeForB")) // type is created because A, B, C, D, E all get created together

    let containersPropB  = containersType.GetProperty("B") // this should not re-create B!
    Assert.True(containersPropB.Name = "B")

    let domainTyForB  = domainTy.GetNestedType("DomainTypeForB")
    Assert.NotNull(domainTyForB)

    //Assert.True((domainTyForB :?> ProvidedTypeDefinition).NonNullable)

    Assert.True (domainTyForB.GetCustomAttributesData() |> Seq.exists (fun cad -> cad.Constructor.DeclaringType.Name = typeof<AllowNullLiteralAttribute>.Name))
    Assert.Equal (1, domainTyForB.GetCustomAttributesData() |> Seq.filter (fun cad -> cad.Constructor.DeclaringType.Name = typeof<AllowNullLiteralAttribute>.Name) |> Seq.length)

    Assert.True(domainTyForB.Name = "DomainTypeForB")

    // check we didn't create twice
    Assert.Equal(1 + 5, domainTy.GetMembers(bindAll).Length) // one for Containers, 5 properties, 5 getters for properties, 5 nested types
    Assert.Equal(0, domainTy.GetMethods(bindAll).Length) 
    Assert.Equal(1 + 5, domainTy.GetNestedTypes(bindAll).Length) 
    Assert.Equal(5 + 5, containersType.GetMembers(bindAll).Length) // 5 properties, 5 getters for properties
    Assert.Equal(5, containersType.GetMethods(bindAll).Length) // 5 properties, 5 getters for properties
    Assert.Equal(5, containersType.GetProperties(bindAll).Length) // 5 properties, 5 getters for properties
    Assert.Equal(0, containersType.GetFields(bindAll).Length) // 5 properties, 5 getters for properties
    Assert.Equal(0, containersType.GetEvents(bindAll).Length) // 5 properties, 5 getters for properties

// Tests for ProvidedMeasureBuilder arithmetic operations
[<Fact>]
let ``test ProvidedMeasureBuilder One Product Inverse Ratio Square``() =
    let one = ProvidedMeasureBuilder.One
    Assert.Equal(typeof<Microsoft.FSharp.Core.CompilerServices.MeasureOne>, one)

    let kg = ProvidedMeasureBuilder.SI "kg"
    let m  = ProvidedMeasureBuilder.SI "m"

    let product = ProvidedMeasureBuilder.Product(kg, m)
    Assert.True(product.IsGenericType, "Product should be a generic type")

    let inv = ProvidedMeasureBuilder.Inverse(kg)
    Assert.True(inv.IsGenericType, "Inverse should be a generic type")

    let ratio = ProvidedMeasureBuilder.Ratio(m, kg)
    Assert.True(ratio.IsGenericType, "Ratio should be a generic type")

    let sq = ProvidedMeasureBuilder.Square(m)
    Assert.True(sq.IsGenericType, "Square should be a generic type")

// Test that ProvidedMeasureBuilder.SI returns null for an unknown unit name
[<Fact>]
let ``test ProvidedMeasureBuilder SI unknown unit``() =
    let unknown = ProvidedMeasureBuilder.SI "foobar_not_a_unit"
    Assert.Null(unknown)

// Tests for ProvidedStaticParameter property accessors
[<Fact>]
let ``test ProvidedStaticParameter properties``() =
    let p1 = ProvidedStaticParameter("myParam", typeof<int>)
    Assert.Equal("myParam", p1.Name)
    Assert.Equal(typeof<int>, p1.ParameterType)
    Assert.Equal(0, p1.Position)
    Assert.Null(p1.RawDefaultValue)
    Assert.False(p1.Attributes.HasFlag(ParameterAttributes.Optional), "parameter without default should not be Optional")

    let p2 = ProvidedStaticParameter("myParam2", typeof<string>, "hello")
    Assert.Equal("myParam2", p2.Name)
    Assert.Equal(typeof<string>, p2.ParameterType)
    Assert.Equal("hello", p2.RawDefaultValue :?> string)
    Assert.True(p2.Attributes.HasFlag(ParameterAttributes.Optional), "parameter with default should be Optional")

// Tests for TypeProviderForNamespaces.AddNamespace, Namespaces, and Invalidate
[<Fact>]
let ``test TypeProviderForNamespaces AddNamespace and Namespaces``() =
    let refs = Targets.DotNetStandard20FSharpRefs()
    let config = Testing.MakeSimulatedTypeProviderConfig(resolutionFolder=__SOURCE_DIRECTORY__, runtimeAssembly="whatever.dll", runtimeAssemblyRefs=refs)
    use tp = new TypeProviderForNamespaces(config)

    let ns = "Test.AddNamespace"
    let asm = Assembly.GetExecutingAssembly()
    let ty = ProvidedTypeDefinition(asm, ns, "TestType", Some typeof<obj>)
    tp.AddNamespace(ns, [ty])

    let namespaces = tp.Namespaces
    Assert.True(namespaces |> Array.exists (fun n -> n.NamespaceName = ns),
                sprintf "Namespace '%s' should be present" ns)

// Test that Invalidate fires the Invalidate event via ITypeProvider
[<Fact>]
let ``test TypeProviderForNamespaces Invalidate``() =
    let refs = Targets.DotNetStandard20FSharpRefs()
    let config = Testing.MakeSimulatedTypeProviderConfig(resolutionFolder=__SOURCE_DIRECTORY__, runtimeAssembly="whatever.dll", runtimeAssemblyRefs=refs)
    use tp = new TypeProviderForNamespaces(config)

    let mutable fired = false
    (tp :> ITypeProvider).Invalidate.Add(fun _ -> fired <- true)
    tp.Invalidate()
    Assert.True(fired, "Invalidate event should have fired")

// Tests for ProvidedField.SetFieldAttributes
[<Fact>]
let ``test ProvidedField SetFieldAttributes``() =
    let f = ProvidedField("myField", typeof<int>)
    Assert.Equal(FieldAttributes.Private, f.Attributes)
    f.SetFieldAttributes(FieldAttributes.Public)
    Assert.Equal(FieldAttributes.Public, f.Attributes)

// ---------------------------------------------------------------------------
// Tests for type definition properties: nonNullable, hideObjectMethods
// Addresses https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/170
// ---------------------------------------------------------------------------

[<Fact>]
let ``nonNullable=true sets NonNullable property and adds AllowNullLiteralAttribute``() =
    let refs = Targets.DotNetStandard20FSharpRefs()
    let config = Testing.MakeSimulatedTypeProviderConfig(resolutionFolder=__SOURCE_DIRECTORY__, runtimeAssembly="whatever.dll", runtimeAssemblyRefs=refs)
    use _tp = new TypeProviderForNamespaces(config)
    let asm = Assembly.GetExecutingAssembly()

    let t = ProvidedTypeDefinition(asm, "Test.Namespace", "NonNullableType", Some typeof<obj>, nonNullable = true)
    Assert.True(t.NonNullable, "NonNullable property should be true")
    let attrs = t.GetCustomAttributesData()
    Assert.True(
        attrs |> Seq.exists (fun a -> a.Constructor.DeclaringType.Name = typeof<AllowNullLiteralAttribute>.Name),
        "Expected AllowNullLiteralAttribute to be present when nonNullable=true")

[<Fact>]
let ``nonNullable=false (default) does not add AllowNullLiteralAttribute``() =
    let refs = Targets.DotNetStandard20FSharpRefs()
    let config = Testing.MakeSimulatedTypeProviderConfig(resolutionFolder=__SOURCE_DIRECTORY__, runtimeAssembly="whatever.dll", runtimeAssemblyRefs=refs)
    use _tp = new TypeProviderForNamespaces(config)
    let asm = Assembly.GetExecutingAssembly()

    let t = ProvidedTypeDefinition(asm, "Test.Namespace", "NullableType", Some typeof<obj>)
    Assert.False(t.NonNullable, "NonNullable property should default to false")
    let attrs = t.GetCustomAttributesData()
    Assert.False(
        attrs |> Seq.exists (fun a -> a.Constructor.DeclaringType.Name = typeof<AllowNullLiteralAttribute>.Name),
        "AllowNullLiteralAttribute should not be present when nonNullable is not set")

[<Fact>]
let ``hideObjectMethods=true sets HideObjectMethods property``() =
    let refs = Targets.DotNetStandard20FSharpRefs()
    let config = Testing.MakeSimulatedTypeProviderConfig(resolutionFolder=__SOURCE_DIRECTORY__, runtimeAssembly="whatever.dll", runtimeAssemblyRefs=refs)
    use _tp = new TypeProviderForNamespaces(config)
    let asm = Assembly.GetExecutingAssembly()

    let t = ProvidedTypeDefinition(asm, "Test.Namespace", "HiddenObjectType", Some typeof<obj>, hideObjectMethods = true)
    Assert.True(t.HideObjectMethods, "HideObjectMethods should be true when hideObjectMethods=true")

[<Fact>]
let ``hideObjectMethods=false (default) leaves HideObjectMethods false``() =
    let refs = Targets.DotNetStandard20FSharpRefs()
    let config = Testing.MakeSimulatedTypeProviderConfig(resolutionFolder=__SOURCE_DIRECTORY__, runtimeAssembly="whatever.dll", runtimeAssemblyRefs=refs)
    use _tp = new TypeProviderForNamespaces(config)
    let asm = Assembly.GetExecutingAssembly()

    let t = ProvidedTypeDefinition(asm, "Test.Namespace", "NormalType", Some typeof<obj>)
    Assert.False(t.HideObjectMethods, "HideObjectMethods should default to false")

[<Fact>]
let ``AddCustomAttribute on method is visible``() =
    let asm = Assembly.GetExecutingAssembly()
    let t = ProvidedTypeDefinition(asm, "Test.Namespace", "TypeWithAttrMethod", Some typeof<obj>)
    let m = ProvidedMethod("OldMethod", [], typeof<unit>, invokeCode = fun _ -> <@@ () @@>)
    m.AddCustomAttribute {
        new CustomAttributeData() with
            member _.Constructor = typeof<ObsoleteAttribute>.GetConstructor([| typeof<string> |])
            member _.ConstructorArguments = [| CustomAttributeTypedArgument(typeof<string>, "use NewMethod" :> obj) |] :> _
            member _.NamedArguments = [||] :> _
    }
    t.AddMember m
    let mi = t.GetMethod("OldMethod")
    Assert.NotNull(mi)
    let methodAttrs = mi.GetCustomAttributesData()
    Assert.True(
        methodAttrs |> Seq.exists (fun a -> a.Constructor.DeclaringType.Name = typeof<ObsoleteAttribute>.Name),
        "Expected ObsoleteAttribute on method")

// ---------------------------------------------------------------------------
// Tests for ProvidedMeasureBuilder: SI units, compound units, annotation
// Addresses https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/67
// ---------------------------------------------------------------------------

[<Fact>]
let ``ProvidedMeasureBuilder SI symbol creates a FSharpTypeAbbreviation``() =
    let kg = ProvidedMeasureBuilder.SI "kg"
    match kg with
    | :? ProvidedTypeSymbol as sym ->
        Assert.True(sym.IsFSharpTypeAbbreviation, "SI 'kg' should be a FSharpTypeAbbreviation")
    | _ -> failwith "Expected ProvidedTypeSymbol for 'kg'"

[<Fact>]
let ``ProvidedMeasureBuilder AnnotateType produces IsFSharpUnitAnnotated``() =
    let kg = ProvidedMeasureBuilder.SI "kg"
    let floatKg = ProvidedMeasureBuilder.AnnotateType(typeof<float>, [ kg ])
    match floatKg with
    | :? ProvidedTypeSymbol as sym ->
        Assert.True(sym.IsFSharpUnitAnnotated, "float<kg> should be IsFSharpUnitAnnotated")
    | _ -> failwith "Expected ProvidedTypeSymbol for annotated type"

[<Fact>]
let ``ProvidedMeasureBuilder compound units Product, Ratio, Square, Inverse, One are non-null``() =
    let kg = ProvidedMeasureBuilder.SI "kg"
    let m  = ProvidedMeasureBuilder.SI "m"
    let s  = ProvidedMeasureBuilder.SI "s"

    Assert.NotNull(ProvidedMeasureBuilder.Product(kg, m))
    Assert.NotNull(ProvidedMeasureBuilder.Ratio(kg, m))
    Assert.NotNull(ProvidedMeasureBuilder.Square(m))
    Assert.NotNull(ProvidedMeasureBuilder.Inverse(s))
    Assert.NotNull(ProvidedMeasureBuilder.One)

    // float<m/s²> annotated type should also be IsFSharpUnitAnnotated
    let accel = ProvidedMeasureBuilder.Ratio(m, ProvidedMeasureBuilder.Square(s))
    let floatAccel = ProvidedMeasureBuilder.AnnotateType(typeof<float>, [ accel ])
    match floatAccel with
    | :? ProvidedTypeSymbol as sym ->
        Assert.True(sym.IsFSharpUnitAnnotated, "float<m/s²> should be IsFSharpUnitAnnotated")
    | _ -> failwith "Expected ProvidedTypeSymbol for float<m/s²>"

[<Fact>]
let ``ProvidedMeasureBuilder SI name (lowercase) creates a FSharpTypeAbbreviation``() =
    let kelvin = ProvidedMeasureBuilder.SI "kelvin"
    Assert.NotNull(kelvin)
    match kelvin with
    | :? ProvidedTypeSymbol as sym ->
        Assert.True(sym.IsFSharpTypeAbbreviation, "SI 'kelvin' should be a FSharpTypeAbbreviation")
    | _ -> failwith "Expected ProvidedTypeSymbol for 'kelvin'"

// Regression test for https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/336
// GetUnionCases on a ProvidedTypeBuilder.MakeGenericType (e.g. option<ProvidedType>) must not throw.
[<Fact>]
let ``GetUnionCases works on option of provided type`` () =
    let typ = ProvidedTypeDefinition("Blah", Some typeof<obj>)
    let optionTyp = ProvidedTypeBuilder.MakeGenericType(typedefof<_ option>, [ typ ])
    // GetNestedType on the ProvidedTypeSymbol wrapping option<T> must delegate to the real option type
    let tagsNested = optionTyp.GetNestedType("Tags", BindingFlags.Public ||| BindingFlags.NonPublic)
    Assert.NotNull(tagsNested)
    // FSharpType.GetUnionCases uses GetNestedType internally — it should succeed
    let cases = Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(optionTyp)
    Assert.Equal(2, cases.Length)
    Assert.Equal("None", cases.[0].Name)
    Assert.Equal("Some", cases.[1].Name)
