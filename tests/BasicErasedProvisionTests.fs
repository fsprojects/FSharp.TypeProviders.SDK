#if INTERACTIVE
#load "../src/ProvidedTypes.fsi" "../src/ProvidedTypes.fs"
#load "../src/ProvidedTypesTesting.fs"

#else

module TPSDK.BasicErasedTests
#endif

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

        // See bug https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/236
#if NETCOREAPP
        let genTy = typedefof<list<_>>
        // The next line loops on netcoreapp3.1 when the ProvidedTypes.fs is compiled as netstandard2.0 (without NETCOREAPP defined)
        let instTy = genTy.MakeGenericType(myType)
        let fails = instTy.FullName
#endif

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

#if INTERNAL_FSHARP_TYPEPROVIDERS_SDK_TESTS

open ProviderImplementation.ProvidedTypes.AssemblyReader

let stressTestCore() = 
    let refs = Targets.DotNetStandard20FSharp45Refs()
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
#endif

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
