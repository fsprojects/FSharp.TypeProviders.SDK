module TPSDK.BasicGenerativeTests

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Core.CompilerServices
open UncheckedQuotations
open Xunit

#nowarn "760" // IDisposable needs new

[<TypeProvider>]
type GenerativePropertyProviderWithStaticParams (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "StaticProperty.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let createType (typeName, _) =
        let myAssem = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(myAssem, ns, typeName, Some typeof<obj>, isErased=false, hideObjectMethods=true)
        myType.AddXmlDoc("Here is some doc")
        let embedString = "test"
        // Special TPSDK support for embedding Decimal values
        let embedM = 5M
        // Special TPSDK support for embedding System.DateTime values
        let embedDT = System.DateTime.Now
        // Special TPSDK support for embedding System.DateTimeOffset values
        let embedDTO = System.DateTimeOffset.Now
        // Special TPSDK support for embedding System.Type values
        let embedType = typeof<int>
        let testCode _args = 
             <@@ // NewArray
                 let arr = [| 1;2;3;4 |]
                 // Coerce
                 let arr2 = (box arr :?> int[])
                 let s = "hello world"
                 // Literal field
                 let s2 = System.DayOfWeek.Friday
                 // NewObj on default ctor - this is not yet supported in generative
                 // let s3 = System.DateTime()
                 // NewObj on value type
                 let s4 = System.DateTime(100L)
                 // NewObj on reference type
                 let s5 = System.Object()
                 // NewObj on generic reference type
                 let s6 = System.Collections.Generic.List<int>()
                 // NewObj on generic reference type
                 let s7 = System.Collections.Generic.Dictionary<int,int>()
                 let s8 = [1] |> List.map (fun x -> x + 1) |> List.map (fun x -> x + 2) 
                 let s9 = match [1] with a :: b -> a | [] -> 5
                 let s9 = match Choice1Of2 4 with Choice1Of2 a -> a | Choice2Of2 () -> 5
                 let s10 = match Choice1Of3 4 with Choice1Of3 a -> a | Choice2Of3 ()  | Choice3Of3 () -> 5
                 let s11 = { contents = 4 }
                 let s12 x = s11.contents <- x
                 let s13 x = s11.Value <- x
                 let rec s14 x = if x = 0 then 1 else s14 (x-1) + s14 (x-1)
                 let rec s14 x = if x = 0 then 1 else s15 x + s15 x
                 and s15 x = s14 (x-1)
                 let mutable j = 3
                 for i in [ 1 .. 10 ] do 
                     j <- j + 1

                 //Arithmetic - note, operations such as + are emitted as a call to the method in the F# library, even over integers
                 let z1 = 1 + 1 - 1 * 1 / 1
                 let z2 = 1u + 1u - 1u * 1u / 1u
                 let z3 = 1L + 1L - 1L * 1L / 1L
                 //Arithmetic (decimals)
                 let z4 = 1M + 1M - 1M * 1M / 1M
                 //Lambda
                 let f1  = (fun (x:int) -> x + 1)
                 let f2  = (fun (x:int) (y:int) -> x + y + 1)
                 let f3  = (fun (x:int) (y:int) (z:int) -> x + y + z + 1)
                 // Application
                 let z5 = f1 3 + f2 3 4 + f3 4 5 6
                 // Const, Tuple
                 let q1 = (embedM, embedString)
                 let q2 = (embedM, embedString, embedDT, embedType, embedDTO, (1,2,3))
                 // WhileLoop
                 while false  do ()
                 // FastIntegerForLoop
                 for i in 0 .. 100 do 
                    ignore ()
                 // // ForLoop - the TryFinally is not yet supported
                 // for i in 0L .. 100L do 
                 //    ignore ()
                 //IfThenElse one branch
                 if s = "" then 
                     failwith "test"
                 //IfThenElse
                 if s = "" then 
                   [] 
                 else
                   Set.toList (Set.ofList [ "Hello world" ]) @@>    
        let adderCode (args: Expr list) = <@@ ignore (%%(args.[1]): System.EventHandler) @@>
        let removerCode (args: Expr list) = <@@ ignore (%%(args.[1]) : System.EventHandler) @@>
        let setterCode (args: Expr list) = <@@ ignore (%%(args.[1]) : string list) @@>
        let ctorCode (args: Expr list) = <@@  ignore 1 @@>
        let cctorCode (_args: Expr list) = <@@ ignore 2 @@>
        let ctorCode2 (args: Expr list) = 
            match args.[0] with 
            | Var v -> if v.Name <> "this" then failwithf "args.[0].Name = %s" v.Name
            | q -> failwithf "expected Var but got %A" q
            match args.[1] with 
            | Var v -> if v.Name <> "arg" then failwithf "args.[1].Name = %s" v.Name
            | q -> failwithf "expected Var but got %A" q
            <@@ ignore (%%(args.[1]) : string list)  @@>

        let myProp = ProvidedProperty("MyStaticProperty", typeof<string list>, isStatic = true, getterCode = testCode)
        let myProp2 = ProvidedProperty("MyInstaceProperty", typeof<string list>, isStatic = false, getterCode = testCode, setterCode = setterCode)
        let myMeth1 = ProvidedMethod("MyStaticMethod", [], typeof<string list>, isStatic = true, invokeCode = testCode)
        let myMeth2 = ProvidedMethod("MyInstanceMethod", [], typeof<string list>, isStatic = false, invokeCode = testCode)
        let myEvent1 = ProvidedEvent("MyEvent", typeof<System.EventHandler>, isStatic = false, adderCode = adderCode, removerCode = removerCode)
        let myCctor1 = ProvidedConstructor([], invokeCode = cctorCode, IsTypeInitializer=true)
        let myCtor1 = ProvidedConstructor([], invokeCode = ctorCode)
        let myCtor2 = ProvidedConstructor([ProvidedParameter("arg", typeof<string list>)], invokeCode = ctorCode2)
        myType.AddMembers [ (myProp :> MemberInfo); (myProp2 :> MemberInfo); (myMeth1 :> MemberInfo); (myMeth2 :> MemberInfo); (myEvent1 :> MemberInfo); (myCctor1 :> MemberInfo);  (myCtor1 :> MemberInfo);  (myCtor2 :> MemberInfo)]
        myAssem.AddTypes [myType]
        myType

    do
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        let parameters = [ ProvidedStaticParameter("Count", typeof<int>) 
                           ProvidedStaticParameter("Count2", typeof<int>, 3) ]
        myType.DefineStaticParameters(parameters, (fun typeName args -> createType(typeName, (args.[0] :?> int) + (args.[1] :?> int))))

        this.AddNamespace(ns, [myType])


let testCases() = 
    let fsCoreVersion = typeof<list<int>>.Assembly.GetName().Version.ToString()
    [ (sprintf "FSharp.Core %s .NET Standard 2.0" fsCoreVersion, fsCoreVersion, (fun _ ->  true), Targets.DotNetStandard20FSharpRefs) ]

[<Fact>]
let ``GenerativePropertyProviderWithStaticParams generates for correctly``() : unit  = 
    for (text, desc, supports, refs) in testCases() do
        if supports() then 
            printfn "" 
            printfn "----- GenerativePropertyProviderWithStaticParams hosted execution: %s ------- " desc 
            let staticArgs = [|  box 3; box 4  |] 
            let runtimeAssemblyRefs = refs()
            let runtimeAssembly = runtimeAssemblyRefs.[0]
            let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
            let tp = GenerativePropertyProviderWithStaticParams cfg :> TypeProviderForNamespaces
            let providedNamespace = tp.Namespaces.[0] 
            let providedTypes  = providedNamespace.GetTypes()
            let providedType = providedTypes.[0] 
            let typeName = providedType.Name + (staticArgs |> Seq.map (fun s -> ",\"" + (if isNull s then "" else s.ToString()) + "\"") |> Seq.reduce (+))

            let t = (tp :> ITypeProvider).ApplyStaticArguments(providedType, [| typeName |], staticArgs)

            match t.Assembly with 
            | :? ProvidedAssembly -> ()
            | _ -> failwithf "expected a ProvidedAssembly"  

            let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(t.Assembly)
            Assert.NotEqual(0, assemContents.Length)
            
            // re-read the assembly with the more complete reader to allow us to look at generated references
            let assem = tp.TargetContext.ReadRelatedAssembly(assemContents)

            printfn "typeName = %s" typeName
            let typeName2 = providedType.Namespace + "." + typeName
            printfn "typeName2 = %s" typeName2
            let t2 = assem.GetType(typeName2)
            Assert.NotNull(t2) 
            let allCtors = t2.GetConstructors(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.Instance)
            Assert.Equal(3, allCtors.Length) 
            let ctors = allCtors |> Array.filter (fun c -> not c.IsStatic)
            Assert.Equal(2, ctors.Length) 
            for ctor in ctors do 
                printfn "ctor.Name = %s" ctor.Name
                Assert.Equal(".ctor", ctor.Name) 
            let cctors = allCtors |> Array.filter (fun c -> c.IsStatic)
            Assert.Equal(1, cctors.Length) 
            printfn "cctors.[0].Name = %s" cctors.[0].Name
            Assert.Equal(".cctor", cctors.[0].Name) 
            let res = [| for r in assem.GetReferencedAssemblies() -> r.ToString() |] |> String.concat ","
            printfn "----- %s ------- " text 
            printfn "compilation references for FSharp.Core target %s = %A" text runtimeAssemblyRefs
            printfn "assembly references for FSharp.Core target %s = %s" text res
            Assert.Contains("FSharp.Core, Version="+desc, res)
                
[<Fact>]
let ``GenerativePropertyProviderWithStaticParams attributes are read correctly``() : unit  = 
    for (text, desc, supports, refs) in testCases() do
        if supports() then 
            let staticArgs = [|  box 3; box 4  |] 
            let runtimeAssemblyRefs = refs()
            let runtimeAssembly = runtimeAssemblyRefs.[0]
            let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
            let tp = GenerativePropertyProviderWithStaticParams cfg :> TypeProviderForNamespaces
            let providedNamespace = tp.Namespaces.[0] 
            let providedTypes  = providedNamespace.GetTypes()
            let providedType = providedTypes.[0] 
            let typeName = providedType.Name + (staticArgs |> Seq.map (fun s -> ",\"" + (if isNull s then "" else s.ToString()) + "\"") |> Seq.reduce (+))

            let t = (tp :> ITypeProvider).ApplyStaticArguments(providedType, [| typeName |], staticArgs)
            
            //check that attributes read using the typed overload to GetCustomAttributes<t> can be read from the provided method
            let firstMethod = t.GetMembers() |> Array.find (fun m -> m :? ProvidedMethod )
            let attrib = firstMethod.GetCustomAttributes<CompiledNameAttribute>()
            Assert.NotNull attrib
            
[<Fact>]
let ``GenerativePropertyProviderWithStaticParams reflection on MethodSymbol and ConstructorSymbols do not throw``() : unit  = 
    for (text, desc, supports, refs) in testCases() do
        if supports() then 
            let staticArgs = [|  box 3; box 4  |] 
            let runtimeAssemblyRefs = refs()
            let runtimeAssembly = runtimeAssemblyRefs.[0]
            let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
            let tp = GenerativePropertyProviderWithStaticParams cfg :> TypeProviderForNamespaces
            let providedNamespace = tp.Namespaces.[0] 
            let providedTypes  = providedNamespace.GetTypes()
            let providedType = providedTypes.[0] 
            let typeName = providedType.Name + (staticArgs |> Seq.map (fun s -> ",\"" + (if isNull s then "" else s.ToString()) + "\"") |> Seq.reduce (+))

            let t = (tp :> ITypeProvider).ApplyStaticArguments(providedType, [| typeName |], staticArgs)

            let genericContainer = ProvidedTypeBuilder.MakeGenericType(typedefof<Option<_>>, [t])
            
            let methods = genericContainer.GetMethods(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.Instance) 
            let methodCustomAttributes =
                methods |> Array.choose (fun methodSymbol -> methodSymbol.GetCustomAttributes(true) |> Array.tryHead )
            Assert.NotEmpty methodCustomAttributes
            
            let ctors = genericContainer.GetConstructors(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.Instance)
            let ctorCustomAttributes = ctors |> Array.choose (fun ctorSymbol -> ctorSymbol.GetCustomAttributes(true) |> Array.tryHead )
            Assert.NotEmpty ctorCustomAttributes
                            
[<TypeProvider>]
type GenerativeProviderWithRecursiveReferencesToGeneratedTypes (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "RecursiveReferencesToGeneratedTypes.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let createType (typeName, _) =
        let myAssem = ProvidedAssembly()
        let myBaseType = ProvidedTypeDefinition(myAssem, ns, typeName+"BaseType", Some typeof<obj>, isErased=false, isSealed=false)
        let myCtorOnBaseType = ProvidedConstructor([ProvidedParameter("implicitCtorFieldName",typeof<int>)], invokeCode = (fun _args -> <@@ () @@>), IsImplicitConstructor=true)
        // Note: myType refers to another generated type as its base class.  
        let myType = ProvidedTypeDefinition(myAssem, ns, typeName, Some (myBaseType :> Type), isErased=false)
        // Note: this method refers to another generated type as its return type
        let myMeth1 = ProvidedMethod("MyInstanceMethodOnBaseType", [], myBaseType, isStatic = false, invokeCode = (fun _args -> Expr.NewObject(myCtorOnBaseType, [Expr.Value(1)])))
        // Note: this method refers to another generated type as its return type
        let myMeth2 = ProvidedMethod("MyInstanceMethod", [], myBaseType, isStatic = false, invokeCode = (fun _args -> Expr.NewObject(myCtorOnBaseType, [Expr.Value(1)])))
        // Note: this method refers to another generated type as its return type
        let myProp1 =
            match <@@ Seq.map id [] @@> with
            | Patterns.Call(_, m, _) ->
                let method = ProvidedTypeBuilder.MakeGenericMethod(m.GetGenericMethodDefinition(), [myBaseType])
                let resultType = ProvidedTypeBuilder.MakeGenericType(typedefof<seq<_>>, [myBaseType])
                let lambda = Expr.Lambda(Var("_", typeof<int>), Expr.NewObject(myCtorOnBaseType,  [Expr.Value(1)]))
                ProvidedProperty("MyProp", resultType, getterCode = function _ -> Expr.CallUnchecked(method, [lambda ; <@@ Seq.empty @@>]))
            | x -> failwithf "unexpected pattern %A" x

        myBaseType.AddMembers [ (myCtorOnBaseType :> MemberInfo); (myMeth1 :> MemberInfo) ; (myProp1 :> MemberInfo) ]
        myType.AddMembers [ (myMeth2 :> MemberInfo) ]

        // Test a generative type with a base type from mscorlib
        let myType2 = ProvidedTypeDefinition(asm, ns, typeName+"MyTypeWithBaseType", Some (typeof<System.Collections.BitArray>), isErased=false)

        // Test a generative type implementing an interface from msvorlib
        let myType3 = ProvidedTypeDefinition(asm, ns, typeName+"MyTypeWithInterfaceType", Some (typeof<obj>), isErased=false)
        myType3.AddInterfaceImplementation typeof<System.IDisposable>
        let disposeMethImpl = ProvidedMethod("Dispose", [], typeof<Void>, isStatic = false, invokeCode = (fun _args -> Expr.Value((), typeof<Void>)))
        myType3.AddMember disposeMethImpl
        myType3.DefineMethodOverride(disposeMethImpl, typeof<IDisposable>.GetMethod("Dispose"))
        
        myAssem.AddTypes [myBaseType; myType; myType2; myType3]
        myType

    do
        let myStaticParameterizedType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        let parameters = [ ProvidedStaticParameter("Count", typeof<int>) 
                           ProvidedStaticParameter("Count2", typeof<int>, 3) ]
        myStaticParameterizedType.DefineStaticParameters(parameters, (fun typeName args -> createType(typeName, (args.[0] :?> int) + (args.[1] :?> int))))

        this.AddNamespace(ns, [myStaticParameterizedType])

[<Fact>]
let ``GenerativeProviderWithRecursiveReferencesToGeneratedTypes generates correctly``() : unit  = 
    for (text, desc, supports, refs) in testCases() do
        if supports() then 
            printfn "" 
            printfn "----- GenerativeProviderWithRecursiveReferencesToGeneratedTypes generates correctly: %s ------- " text 
            let staticArgs = [|  box 3; box 4  |] 
            let runtimeAssemblyRefs = refs()

            let runtimeAssembly = runtimeAssemblyRefs.[0]
            let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
            let tp = GenerativeProviderWithRecursiveReferencesToGeneratedTypes cfg :> TypeProviderForNamespaces
            let providedNamespace = tp.Namespaces.[0] 
            let providedTypes  = providedNamespace.GetTypes()
            let providedType = providedTypes.[0] 
            let typeName = providedType.Name + (staticArgs |> Seq.map (fun s -> ",\"" + (if isNull s then "" else s.ToString()) + "\"") |> Seq.reduce (+))

            let t = (tp :> ITypeProvider).ApplyStaticArguments(providedType, [| typeName |], staticArgs)

            match t.Assembly with 
            | :? ProvidedAssembly -> ()
            | _ -> failwithf "expected a ProvidedAssembly"  

            let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(t.Assembly)
            Assert.NotEqual(0, assemContents.Length)
            
            // re-read the assembly with the more complete reader to allow us to look at generated references
            let assem = tp.TargetContext.ReadRelatedAssembly(assemContents)
            let res = [| for r in assem.GetReferencedAssemblies() -> r.ToString() |] |> String.concat ","
            printfn "----- %s ------- " text 
            printfn "compilation references for FSharp.Core target %s = %A" text runtimeAssemblyRefs
            printfn "assembly references for FSharp.Core target %s = %s" text res
            Assert.Contains("FSharp.Core, Version="+desc, res)

let ``GenerativeProviderWithRecursiveReferencesToGeneratedTypes generates for hosted execution correctly``() : unit  = 
    for (text, desc, supports, refs) in testCases() do
        if supports() then 
            printfn "----- GenerativeProviderWithRecursiveReferencesToGeneratedTypes hosted execution: %s ------- " desc 
            let staticArgs = [|  box 3; box 4  |] 
            let runtimeAssemblyRefs = refs()
            //printfn "----- refs = %A ------- " runtimeAssemblyRefs
            let runtimeAssembly = runtimeAssemblyRefs.[0]
            let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs, isHostedExecution=true) 
            let tp = GenerativeProviderWithRecursiveReferencesToGeneratedTypes cfg :> TypeProviderForNamespaces
            let providedNamespace = tp.Namespaces.[0] 
            let providedTypes  = providedNamespace.GetTypes()
            let providedType = providedTypes.[0] 
            let typeName = providedType.Name + (staticArgs |> Seq.map (fun s -> ",\"" + (if isNull s then "" else s.ToString()) + "\"") |> Seq.reduce (+))

            let t = (tp :> ITypeProvider).ApplyStaticArguments(providedType, [| typeName |], staticArgs)

            match t.Assembly with 
            | :? ProvidedAssembly -> failwithf "did not expect a ProvidedAssembly - when used in hosted execution a type provider should return a Reflection.Load assembly"  
            | _ -> ()

            // OK, now check we can do a little bit of reflection emit against the types coming from this assembly
            let tmpFile = Path.GetTempFileName()
            let assemblyFileName = Path.ChangeExtension(tmpFile, "dll")
            File.Delete(tmpFile)
            let simpleName = Path.GetFileNameWithoutExtension(assemblyFileName)
            let asmName = AssemblyName(simpleName)
            let currentDom  = AppDomain.CurrentDomain
            let asmB = AssemblyBuilder.DefineDynamicAssembly(asmName, AssemblyBuilderAccess.Run)
            let modB = asmB.DefineDynamicModule(simpleName)
            let typB = modB.DefineType("A", TypeAttributes.Sealed ||| TypeAttributes.Class)
            let methB = typB.DefineMethod("M", MethodAttributes.Static)

            methB.SetParameters( [| |])
            methB.SetReturnType(t)

    // TESTING TODO: Register binary
    // TESTING TODO: field defs

// Provider for testing custom attribute encoding fixes:
// Enums with non-int32 underlying types used for testing decodeILCustomAttribData
type ByteBackedEnum =
    | ByteBackedEnumA = 1uy
    | ByteBackedEnumB = 200uy

type Int16BackedEnum =
    | Int16BackedEnumA = 1s
    | Int16BackedEnumB = 1000s

type Int64BackedEnum =
    | Int64BackedEnumA = 1L
    | Int64BackedEnumB = 5000000000L

[<System.AttributeUsage(System.AttributeTargets.All)>]
type ByteEnumAttribute(v: ByteBackedEnum) =
    inherit System.Attribute()
    member _.Value = v

[<System.AttributeUsage(System.AttributeTargets.All)>]
type Int16EnumAttribute(v: Int16BackedEnum) =
    inherit System.Attribute()
    member _.Value = v

[<System.AttributeUsage(System.AttributeTargets.All)>]
type Int64EnumAttribute(v: Int64BackedEnum) =
    inherit System.Attribute()
    member _.Value = v

// Fix 1: encoding obj[] in an object-typed constructor parameter slot (previously threw "TODO: can't yet emit arrays in attrs")
// Fix 2: applying transValue to constructor args and named properties so System.Type values are correctly encoded
[<TypeProvider>]
type GenerativeProviderWithCustomAttrEncoding (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "CustomAttrEncoding.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    do
        // Fix 1: property with DefaultValueAttribute(object) where value is a string[] (array in object slot)
        let defaultValueCtorObj = typeof<System.ComponentModel.DefaultValueAttribute>.GetConstructor([|typeof<obj>|])
        let propWithArrayAttr = ProvidedProperty("PropWithArrayAttr", typeof<string>, isStatic = false,
                                    getterCode = fun _ -> <@@ "hello" @@>)
        propWithArrayAttr.AddCustomAttribute {
            new CustomAttributeData() with
                member _.Constructor = defaultValueCtorObj
                member _.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<obj>, box [| "a"; "b"; "c" |]) |]
                member _.NamedArguments = upcast [||] }

        // Fix 2: property with DefaultValueAttribute(object) where value is a System.Type
        let propWithTypeAttr = ProvidedProperty("PropWithTypeAttr", typeof<string>, isStatic = false,
                                    getterCode = fun _ -> <@@ "world" @@>)
        propWithTypeAttr.AddCustomAttribute {
            new CustomAttributeData() with
                member _.Constructor = defaultValueCtorObj
                member _.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<obj>, box typeof<System.Int32>) |]
                member _.NamedArguments = upcast [||] }

        container.AddMembers [ (propWithArrayAttr :> MemberInfo); (propWithTypeAttr :> MemberInfo) ]
        container.AddMember (ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        tempAssembly.AddTypes [container]
        this.AddNamespace(ns, [container])

[<Fact>]
let ``Generative custom attribute with array argument in object slot encodes correctly``() =
    // Regression test for Fix 1: encoding obj[] in an object-typed constructor parameter slot
    // Before fix: GetGeneratedAssemblyContents would throw failwith "TODO: can't yet emit arrays in attrs"
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeProviderWithCustomAttrEncoding cfg :> TypeProviderForNamespaces
    let providedNamespace = tp.Namespaces.[0]
    let providedTypes = providedNamespace.GetTypes()
    let providedType = providedTypes.[0]

    // Must not throw (before fix, this threw on the array arg)
    let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    Assert.NotEqual(0, assemContents.Length)

    let assem = Assembly.Load assemContents
    let containerType = assem.GetType("CustomAttrEncoding.Provided.Container")
    Assert.NotNull(containerType)

    let prop = containerType.GetProperty("PropWithArrayAttr")
    Assert.NotNull(prop)

    let attrData = prop.GetCustomAttributesData()
    let defaultValueAttr = attrData |> Seq.tryFind (fun a -> a.Constructor.DeclaringType.Name = "DefaultValueAttribute")
    Assert.True(defaultValueAttr.IsSome, "DefaultValueAttribute should be present on PropWithArrayAttr")

[<Fact>]
let ``Generative custom attribute with Type argument in object slot encodes correctly``() =
    // Regression test for Fix 2: applying transValue so System.Type args are correctly encoded
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeProviderWithCustomAttrEncoding cfg :> TypeProviderForNamespaces
    let providedNamespace = tp.Namespaces.[0]
    let providedTypes = providedNamespace.GetTypes()
    let providedType = providedTypes.[0]

    // Must not throw
    let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    Assert.NotEqual(0, assemContents.Length)

    let assem = Assembly.Load assemContents
    let containerType = assem.GetType("CustomAttrEncoding.Provided.Container")
    Assert.NotNull(containerType)

    let prop = containerType.GetProperty("PropWithTypeAttr")
    Assert.NotNull(prop)

    let attrData = prop.GetCustomAttributesData()
    let defaultValueAttr = attrData |> Seq.tryFind (fun a -> a.Constructor.DeclaringType.Name = "DefaultValueAttribute")
    Assert.True(defaultValueAttr.IsSome, "DefaultValueAttribute should be present on PropWithTypeAttr")

// Fix for decodeILCustomAttribData: non-int32-backed enum fixed args were decoded using
// sigptr_get_i32 (always 4 bytes), corrupting the blob read position for any subsequent
// arguments and/or causing IndexOutOfRangeException for small-backed enums.
// Encoder path expects the value boxed as the underlying primitive type (e.g. byte, int16, int64),
// NOT as the enum type itself.
[<TypeProvider>]
type GenerativeProviderWithNonInt32EnumAttrs (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "NonInt32EnumAttr.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    do
        let byteEnumCtor = typeof<ByteEnumAttribute>.GetConstructor([| typeof<ByteBackedEnum> |])
        let typeWithByteEnum = ProvidedTypeDefinition("TypeWithByteEnumAttr", Some typeof<obj>, isErased = false)
        typeWithByteEnum.AddCustomAttribute {
            new CustomAttributeData() with
                member _.Constructor = byteEnumCtor
                // Value is boxed as the underlying type (byte), which is what encodeCustomAttrPrimValue needs
                member _.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<ByteBackedEnum>, 200uy :> obj) |]
                member _.NamedArguments = upcast [||] }
        typeWithByteEnum.AddMember (ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))

        let int16EnumCtor = typeof<Int16EnumAttribute>.GetConstructor([| typeof<Int16BackedEnum> |])
        let typeWithInt16Enum = ProvidedTypeDefinition("TypeWithInt16EnumAttr", Some typeof<obj>, isErased = false)
        typeWithInt16Enum.AddCustomAttribute {
            new CustomAttributeData() with
                member _.Constructor = int16EnumCtor
                member _.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<Int16BackedEnum>, 1000s :> obj) |]
                member _.NamedArguments = upcast [||] }
        typeWithInt16Enum.AddMember (ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))

        let int64EnumCtor = typeof<Int64EnumAttribute>.GetConstructor([| typeof<Int64BackedEnum> |])
        let typeWithInt64Enum = ProvidedTypeDefinition("TypeWithInt64EnumAttr", Some typeof<obj>, isErased = false)
        typeWithInt64Enum.AddCustomAttribute {
            new CustomAttributeData() with
                member _.Constructor = int64EnumCtor
                member _.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<Int64BackedEnum>, 5000000000L :> obj) |]
                member _.NamedArguments = upcast [||] }
        typeWithInt64Enum.AddMember (ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))

        container.AddMembers [ typeWithByteEnum; typeWithInt16Enum; typeWithInt64Enum ]
        container.AddMember (ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        tempAssembly.AddTypes [container]
        this.AddNamespace(ns, [container])

/// Create a type provider config that includes this test assembly in the reference list so that
/// txILType (inside ReadRelatedAssembly) can resolve the enum types defined in this test assembly.
let makeNonInt32EnumAttrProviderConfig () =
    let thisAssemblyPath = Assembly.GetExecutingAssembly().Location
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs() @ [thisAssemblyPath]
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)

[<Fact>]
let ``decodeILCustomAttribData decodes byte-backed enum custom attribute correctly``() =
    // Regression test: before the fix, decodeILCustomAttribData always called sigptr_get_i32 for
    // ILType.Value (enum) args, reading 4 bytes regardless of the enum's actual underlying type.
    // For a byte-backed enum (1 byte in the blob), this over-read the blob, causing
    // IndexOutOfRangeException (caught and re-thrown as a failwithf).
    let cfg = makeNonInt32EnumAttrProviderConfig()
    let tp = GenerativeProviderWithNonInt32EnumAttrs(cfg) :> TypeProviderForNamespaces
    let providedType = tp.Namespaces.[0].GetTypes().[0]

    let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    Assert.NotEqual(0, assemContents.Length)

    // ReadRelatedAssembly goes through the IL reader, which exercises decodeILCustomAttribData.
    // Before the fix this would throw "FAILED decodeILCustomAttribData ..." due to blob over-read.
    let assem = tp.TargetContext.ReadRelatedAssembly(assemContents)

    let containerType = assem.GetType("NonInt32EnumAttr.Provided.Container")
    Assert.NotNull(containerType)

    let ty = containerType.GetNestedType("TypeWithByteEnumAttr")
    Assert.NotNull(ty)

    let attrData = ty.GetCustomAttributesData()
    let attr = attrData |> Seq.tryFind (fun a -> a.Constructor.DeclaringType.Name = "ByteEnumAttribute")
    Assert.True(attr.IsSome, "ByteEnumAttribute should be present on TypeWithByteEnumAttr")
    Assert.Equal(1, attr.Value.ConstructorArguments.Count)
    // Decoded value is the raw underlying byte (200uy), matching the encoded ByteBackedEnumB
    Assert.Equal(200uy :> obj, attr.Value.ConstructorArguments.[0].Value)

[<Fact>]
let ``decodeILCustomAttribData decodes int16-backed enum custom attribute correctly``() =
    // Int16-backed enum: blob contains 2 bytes; before fix, 4 bytes were read, corrupting the stream.
    let cfg = makeNonInt32EnumAttrProviderConfig()
    let tp = GenerativeProviderWithNonInt32EnumAttrs(cfg) :> TypeProviderForNamespaces
    let providedType = tp.Namespaces.[0].GetTypes().[0]

    let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    let assem = tp.TargetContext.ReadRelatedAssembly(assemContents)

    let containerType = assem.GetType("NonInt32EnumAttr.Provided.Container")
    Assert.NotNull(containerType)

    let ty = containerType.GetNestedType("TypeWithInt16EnumAttr")
    Assert.NotNull(ty)

    let attrData = ty.GetCustomAttributesData()
    let attr = attrData |> Seq.tryFind (fun a -> a.Constructor.DeclaringType.Name = "Int16EnumAttribute")
    Assert.True(attr.IsSome, "Int16EnumAttribute should be present on TypeWithInt16EnumAttr")
    Assert.Equal(1, attr.Value.ConstructorArguments.Count)
    // Decoded value is the raw underlying int16 (1000s), matching Int16BackedEnumB
    Assert.Equal(1000s :> obj, attr.Value.ConstructorArguments.[0].Value)

[<Fact>]
let ``decodeILCustomAttribData decodes int64-backed enum custom attribute correctly``() =
    // Int64-backed enum: blob contains 8 bytes; before fix, only 4 bytes were read, giving a wrong value.
    let cfg = makeNonInt32EnumAttrProviderConfig()
    let tp = GenerativeProviderWithNonInt32EnumAttrs(cfg) :> TypeProviderForNamespaces
    let providedType = tp.Namespaces.[0].GetTypes().[0]

    let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    let assem = tp.TargetContext.ReadRelatedAssembly(assemContents)

    let containerType = assem.GetType("NonInt32EnumAttr.Provided.Container")
    Assert.NotNull(containerType)

    let ty = containerType.GetNestedType("TypeWithInt64EnumAttr")
    Assert.NotNull(ty)

    let attrData = ty.GetCustomAttributesData()
    let attr = attrData |> Seq.tryFind (fun a -> a.Constructor.DeclaringType.Name = "Int64EnumAttribute")
    Assert.True(attr.IsSome, "Int64EnumAttribute should be present on TypeWithInt64EnumAttr")
    Assert.Equal(1, attr.Value.ConstructorArguments.Count)
    // Decoded value is the raw underlying int64 (5000000000L), matching Int64BackedEnumB
    Assert.Equal(5000000000L :> obj, attr.Value.ConstructorArguments.[0].Value)
