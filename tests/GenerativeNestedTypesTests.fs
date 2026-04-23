module TPSDK.GenerativeNestedTypesTests

// Tests for generative nested types: one ProvidedTypeDefinition hosting other
// ProvidedTypeDefinitions as nested (inner) types.
//
// Scenario:
//   class Library
//       nested class Book  { Title: string; Author: string; ctor(t,a); GetSummary(): string }
//       nested class Shelf { Label: string; ctor(label); MaxItems: int (static) }
//
// Tests verify that nested types are emitted correctly, that their constructors,
// fields, instance methods, and static members are all accessible at runtime.

#nowarn "760" // IDisposable needs new

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting
open UncheckedQuotations

[<TypeProvider>]
type GenerativeNestedProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "Nested.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Library", Some typeof<obj>, isErased = false)

    do
        // ── nested class: Book ───────────────────────────────────────────────
        let bookType = ProvidedTypeDefinition("Book", Some typeof<obj>, isErased = false)

        let titleField  = ProvidedField("_title",  typeof<string>)
        let authorField = ProvidedField("_author", typeof<string>)
        bookType.AddMember titleField
        bookType.AddMember authorField

        // constructor: Book(title: string, author: string)
        bookType.AddMember
            (ProvidedConstructor(
                [ ProvidedParameter("title",  typeof<string>)
                  ProvidedParameter("author", typeof<string>) ],
                invokeCode = fun args ->
                    Expr.Sequential(
                        Expr.FieldSetUnchecked(args.[0], titleField,  args.[1]),
                        Expr.FieldSetUnchecked(args.[0], authorField, args.[2]))))

        // property: Title : string
        let titleProp =
            ProvidedProperty("Title", typeof<string>,
                getterCode = fun args -> Expr.FieldGetUnchecked(args.[0], titleField))
        bookType.AddMember titleProp

        // instance method: GetSummary() : string  →  "«title» by «author»"
        let getSummaryMethod =
            ProvidedMethod("GetSummary", [], typeof<string>, isStatic = false,
                invokeCode = fun args ->
                    let t = Expr.FieldGetUnchecked(args.[0], titleField)
                    let a = Expr.FieldGetUnchecked(args.[0], authorField)
                    <@@ (%%t : string) + " by " + (%%a : string) @@>)
        bookType.AddMember getSummaryMethod

        // ── nested class: Shelf ──────────────────────────────────────────────
        let shelfType = ProvidedTypeDefinition("Shelf", Some typeof<obj>, isErased = false)

        let labelField = ProvidedField("_label", typeof<string>)
        shelfType.AddMember labelField

        // constructor: Shelf(label: string)
        shelfType.AddMember
            (ProvidedConstructor(
                [ ProvidedParameter("label", typeof<string>) ],
                invokeCode = fun args ->
                    Expr.FieldSetUnchecked(args.[0], labelField, args.[1])))

        // property: Label : string
        let labelProp =
            ProvidedProperty("Label", typeof<string>,
                getterCode = fun args -> Expr.FieldGetUnchecked(args.[0], labelField))
        shelfType.AddMember labelProp

        // static property: MaxItems : int
        let maxItemsProp =
            ProvidedProperty("MaxItems", typeof<int>, isStatic = true,
                getterCode = fun _args -> <@@ 100 @@>)
        shelfType.AddMember maxItemsProp

        // ── register both nested types ───────────────────────────────────────
        container.AddMember bookType
        container.AddMember shelfType

        tempAssembly.AddTypes [container]
        this.AddNamespace(ns, [container])

let loadTestAssembly () =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeNestedProvider(cfg) :> TypeProviderForNamespaces
    let providedType = tp.Namespaces.[0].GetTypes().[0] :?> ProvidedTypeDefinition
    Assert.Equal("Library", providedType.Name)
    let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    let assem = Assembly.Load bytes
    assem.ExportedTypes |> Seq.find (fun t -> t.Name = "Library")

[<Fact>]
let ``Nested types Book and Shelf are present in generated assembly``() =
    let library = loadTestAssembly ()
    let book  = library.GetNestedType("Book",  BindingFlags.Public)
    let shelf = library.GetNestedType("Shelf", BindingFlags.Public)
    Assert.NotNull(book)
    Assert.NotNull(shelf)
    Assert.Equal("Book",  book.Name)
    Assert.Equal("Shelf", shelf.Name)

[<Fact>]
let ``Book nested type has Title property and GetSummary method``() =
    let library = loadTestAssembly ()
    let book = library.GetNestedType("Book", BindingFlags.Public)
    Assert.NotNull(book)

    let titleProp   = book.GetProperty("Title",      BindingFlags.Instance ||| BindingFlags.Public)
    let summaryMeth = book.GetMethod  ("GetSummary", BindingFlags.Instance ||| BindingFlags.Public)
    Assert.NotNull(titleProp)
    Assert.NotNull(summaryMeth)
    Assert.Equal(typeof<string>, titleProp.PropertyType)
    Assert.Equal(typeof<string>, summaryMeth.ReturnType)

[<Fact>]
let ``Book nested type constructor sets fields; GetSummary returns expected string``() =
    let library = loadTestAssembly ()
    let book = library.GetNestedType("Book", BindingFlags.Public)
    Assert.NotNull(book)

    let ctor = book.GetConstructor([| typeof<string>; typeof<string> |])
    Assert.NotNull(ctor)
    let instance = ctor.Invoke([| box "FSharp for Fun"; box "Don Syme" |])
    Assert.NotNull(instance)

    let summaryMeth = book.GetMethod("GetSummary", BindingFlags.Instance ||| BindingFlags.Public)
    let result = summaryMeth.Invoke(instance, [||]) :?> string
    Assert.Equal("FSharp for Fun by Don Syme", result)

[<Fact>]
let ``Shelf nested type Label property reflects constructor argument``() =
    let library = loadTestAssembly ()
    let shelf = library.GetNestedType("Shelf", BindingFlags.Public)
    Assert.NotNull(shelf)

    let ctor = shelf.GetConstructor([| typeof<string> |])
    Assert.NotNull(ctor)
    let instance = ctor.Invoke([| box "Science Fiction" |])

    let labelProp = shelf.GetProperty("Label", BindingFlags.Instance ||| BindingFlags.Public)
    Assert.NotNull(labelProp)
    let value = labelProp.GetValue(instance) :?> string
    Assert.Equal("Science Fiction", value)

[<Fact>]
let ``Shelf nested type static property MaxItems returns 100``() =
    let library = loadTestAssembly ()
    let shelf = library.GetNestedType("Shelf", BindingFlags.Public)
    Assert.NotNull(shelf)

    let maxItemsProp = shelf.GetProperty("MaxItems", BindingFlags.Static ||| BindingFlags.Public)
    Assert.NotNull(maxItemsProp)
    let value = maxItemsProp.GetValue(null) :?> int
    Assert.Equal(100, value)
