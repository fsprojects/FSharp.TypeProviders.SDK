module TPSDK.GenerativeMethodsTests

#nowarn "760" // IDisposable needs new

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting

/// Type provider with a generative type that has instance methods, a static method, and a constructor.
/// The Widget type exposes:
///   - instance method  Greet()        : string
///   - instance method  Add(x:int, y:int) : int
///   - static  method   Create()       : obj
[<TypeProvider>]
type GenerativeMethodsProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "Methods.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    do
        let widgetType = ProvidedTypeDefinition("Widget", Some typeof<obj>, isErased = false)

        // Instance method: Greet() : string
        let greetCode (_args: Expr list) = <@@ "Hello" @@>
        let greetMethod = ProvidedMethod("Greet", [], typeof<string>, invokeCode = greetCode, isStatic = false)
        widgetType.AddMember greetMethod

        // Instance method with parameters: Add(x: int, y: int) : int
        let addCode (args: Expr list) = <@@ (%%(args.[1]) : int) + (%%(args.[2]) : int) @@>
        let addMethod =
            ProvidedMethod(
                "Add",
                [ ProvidedParameter("x", typeof<int>); ProvidedParameter("y", typeof<int>) ],
                typeof<int>,
                invokeCode = addCode,
                isStatic = false)
        widgetType.AddMember addMethod

        // Static method: Create() : obj
        let createCode (_args: Expr list) = <@@ obj() @@>
        let createMethod = ProvidedMethod("Create", [], typeof<obj>, invokeCode = createCode, isStatic = true)
        widgetType.AddMember createMethod

        widgetType.AddMember (ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        container.AddMember widgetType

        tempAssembly.AddTypes [container]
        this.AddNamespace(ns, [container])

let loadTestAssembly () =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeMethodsProvider(cfg) :> TypeProviderForNamespaces
    let providedNamespace = tp.Namespaces.[0]
    let providedType = providedNamespace.GetTypes().[0] :?> ProvidedTypeDefinition
    Assert.Equal("Container", providedType.Name)
    let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    Assembly.Load bytes

[<Fact>]
let ``Generative instance method Greet is present in generated assembly``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let meth = widgetType.GetMethod("Greet", BindingFlags.Instance ||| BindingFlags.Public)
    Assert.NotNull(meth)
    Assert.Equal("Greet", meth.Name)
    Assert.False(meth.IsStatic, "Greet should be an instance method")

[<Fact>]
let ``Generative instance method Greet has correct return type``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let meth = widgetType.GetMethod("Greet", BindingFlags.Instance ||| BindingFlags.Public)
    Assert.NotNull(meth)
    Assert.Equal(typeof<string>, meth.ReturnType)
    let parms = meth.GetParameters()
    Assert.Equal(0, parms.Length)

[<Fact>]
let ``Generative instance method Add has correct parameter types``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let meth = widgetType.GetMethod("Add", BindingFlags.Instance ||| BindingFlags.Public)
    Assert.NotNull(meth)
    Assert.Equal("Add", meth.Name)
    Assert.Equal(typeof<int>, meth.ReturnType)

    let parms = meth.GetParameters()
    Assert.Equal(2, parms.Length)
    Assert.Equal("x", parms.[0].Name)
    Assert.Equal(typeof<int>, parms.[0].ParameterType)
    Assert.Equal("y", parms.[1].Name)
    Assert.Equal(typeof<int>, parms.[1].ParameterType)

[<Fact>]
let ``Generative static method Create is present and is static``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let meth = widgetType.GetMethod("Create", BindingFlags.Static ||| BindingFlags.Public)
    Assert.NotNull(meth)
    Assert.Equal("Create", meth.Name)
    Assert.True(meth.IsStatic, "Create should be a static method")
    Assert.Equal(typeof<obj>, meth.ReturnType)

[<Fact>]
let ``Generative type methods can be looked up by name``() =
    // Directly exercises the ILMethodDefs lazy name-lookup dictionary.
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let bf = BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public
    Assert.NotNull(widgetType.GetMethod("Greet",  bf))
    Assert.NotNull(widgetType.GetMethod("Add",    bf))
    Assert.NotNull(widgetType.GetMethod("Create", bf))

[<Fact>]
let ``Generative type has expected method count``() =
    let assembly = loadTestAssembly ()
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    // 3 declared methods: Greet, Add, Create  (excludes inherited Object methods and .ctor)
    let declaredMethods =
        widgetType.GetMethods(BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly)
    Assert.Equal(3, declaredMethods.Length)

// ---------------------------------------------------------------------------
// System.Void return type
//
// A ProvidedMethod may be declared with returnType = typeof<System.Void>
// (transType maps it to ILType.Void). The method body emitter must not push
// a null on the stack before ret in that case: a void-returning method must
// have an empty stack at ret, otherwise the IL is invalid and the JIT throws
// InvalidProgramException when the method is first invoked.
// ---------------------------------------------------------------------------

[<TypeProvider>]
type GenerativeVoidReturnProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "VoidReturn.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    do
        let widgetType = ProvidedTypeDefinition("Widget", Some typeof<obj>, isErased = false)

        // Static method declared with an explicit System.Void return type
        let doNothing = ProvidedMethod("DoNothing", [], typeof<System.Void>, invokeCode = (fun _ -> <@@ () @@>), isStatic = true)
        widgetType.AddMember doNothing

        widgetType.AddMember (ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))
        container.AddMember widgetType

        tempAssembly.AddTypes [container]
        this.AddNamespace(ns, [container])

[<Fact>]
let ``Generative method declared with System.Void return type generates valid IL``() =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeVoidReturnProvider(cfg) :> TypeProviderForNamespaces
    let providedType = tp.Namespaces.[0].GetTypes().[0] :?> ProvidedTypeDefinition
    let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    let assembly = Assembly.Load bytes
    let containerType = assembly.ExportedTypes |> Seq.find (fun t -> t.Name = "Container")
    let widgetType = containerType.GetNestedType("Widget")
    Assert.NotNull(widgetType)

    let meth = widgetType.GetMethod("DoNothing", BindingFlags.Static ||| BindingFlags.Public)
    Assert.NotNull(meth)
    Assert.Equal(typeof<System.Void>, meth.ReturnType)

    // JITs the body; throws InvalidProgramException if a stray ldnull precedes ret
    meth.Invoke(null, [||]) |> ignore
