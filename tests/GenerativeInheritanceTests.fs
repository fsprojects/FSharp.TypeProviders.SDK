module TPSDK.GenerativeInheritanceTests

// Tests for generative type inheritance: one ProvidedTypeDefinition deriving from
// another, with virtual/abstract method dispatch verified at runtime.
//
// Scenario:
//   abstract class Animal  { abstract Speak() : string }
//   class Dog(_name:string) extends Animal { override Speak() = "Woof! I am " + _name }
//   class Cat(_name:string) extends Animal { override Speak() = "Meow! I am " + _name }

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
type GenerativeInheritanceProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "Inheritance.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Animals", Some typeof<obj>, isErased = false)

    do
        // Abstract base class: Animal
        let animalType =
            ProvidedTypeDefinition("Animal", Some typeof<obj>,
                isErased = false, isAbstract = true, isSealed = false)

        // Abstract method - no invokeCode; the TPSDK auto-applies Abstract|Virtual when class is abstract
        let speakMethod = ProvidedMethod("Speak", [], typeof<string>)
        animalType.AddMember speakMethod
        // Default constructor so derived classes can chain to it
        animalType.AddMember (ProvidedConstructor([], invokeCode = fun _ -> <@@ () @@>))

        // Concrete class: Dog
        let dogType =
            ProvidedTypeDefinition("Dog", Some (animalType :> Type),
                isErased = false, isSealed = false)

        let dogNameField = ProvidedField("_name", typeof<string>)
        dogType.AddMember dogNameField

        dogType.AddMember
            (ProvidedConstructor(
                [ProvidedParameter("name", typeof<string>)],
                invokeCode = fun args ->
                    Expr.FieldSetUnchecked(args.[0], dogNameField, args.[1])))

        let dogSpeak =
            ProvidedMethod("Speak", [], typeof<string>, isStatic = false,
                invokeCode = fun args ->
                    let nameExpr = Expr.FieldGetUnchecked(args.[0], dogNameField)
                    <@@ "Woof! I am " + (%%nameExpr : string) @@>)
        dogSpeak.AddMethodAttrs(MethodAttributes.Virtual ||| MethodAttributes.HideBySig)
        dogType.AddMember dogSpeak
        dogType.DefineMethodOverride(dogSpeak, speakMethod)

        // Concrete class: Cat
        let catType =
            ProvidedTypeDefinition("Cat", Some (animalType :> Type),
                isErased = false, isSealed = false)

        let catNameField = ProvidedField("_name", typeof<string>)
        catType.AddMember catNameField

        catType.AddMember
            (ProvidedConstructor(
                [ProvidedParameter("name", typeof<string>)],
                invokeCode = fun args ->
                    Expr.FieldSetUnchecked(args.[0], catNameField, args.[1])))

        let catSpeak =
            ProvidedMethod("Speak", [], typeof<string>, isStatic = false,
                invokeCode = fun args ->
                    let nameExpr = Expr.FieldGetUnchecked(args.[0], catNameField)
                    <@@ "Meow! I am " + (%%nameExpr : string) @@>)
        catSpeak.AddMethodAttrs(MethodAttributes.Virtual ||| MethodAttributes.HideBySig)
        catType.AddMember catSpeak
        catType.DefineMethodOverride(catSpeak, speakMethod)

        container.AddMembers [animalType; dogType; catType]
        tempAssembly.AddTypes [container]
        this.AddNamespace(ns, [container])

let loadTestAssembly () =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig(__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs)
    let tp = GenerativeInheritanceProvider(cfg) :> TypeProviderForNamespaces
    let providedType = tp.Namespaces.[0].GetTypes().[0]
    let bytes = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedType.Assembly)
    let assem = Assembly.Load bytes
    assem.GetType("Inheritance.Provided.Animals")

[<Fact>]
let ``Generative Dog type is a subclass of generative Animal type``() =
    let animals = loadTestAssembly()
    let animalType = animals.GetNestedType("Animal")
    let dogType    = animals.GetNestedType("Dog")
    Assert.NotNull(animalType)
    Assert.NotNull(dogType)
    Assert.True(dogType.IsSubclassOf(animalType), "Dog should be a subclass of Animal")

[<Fact>]
let ``Generative Animal Speak method is abstract``() =
    let animals = loadTestAssembly()
    let animalType = animals.GetNestedType("Animal")
    let speakMethod = animalType.GetMethod("Speak", BindingFlags.Instance ||| BindingFlags.Public)
    Assert.NotNull(speakMethod)
    Assert.True(speakMethod.IsAbstract, "Animal.Speak should be abstract")
    Assert.True(speakMethod.IsVirtual,  "Animal.Speak should be virtual")

[<Fact>]
let ``Generative Dog Speak method returns expected string``() =
    let animals = loadTestAssembly()
    let dogType = animals.GetNestedType("Dog")
    Assert.NotNull(dogType)
    let ctor = dogType.GetConstructor([| typeof<string> |])
    Assert.NotNull(ctor)
    let dog = ctor.Invoke([| box "Buddy" |])
    let speakMethod = dogType.GetMethod("Speak", BindingFlags.Instance ||| BindingFlags.Public)
    Assert.NotNull(speakMethod)
    let result = speakMethod.Invoke(dog, [||]) :?> string
    Assert.Equal("Woof! I am Buddy", result)

[<Fact>]
let ``Generative Cat Speak method returns expected string``() =
    let animals = loadTestAssembly()
    let catType = animals.GetNestedType("Cat")
    Assert.NotNull(catType)
    let ctor = catType.GetConstructor([| typeof<string> |])
    Assert.NotNull(ctor)
    let cat = ctor.Invoke([| box "Whiskers" |])
    let speakMethod = catType.GetMethod("Speak", BindingFlags.Instance ||| BindingFlags.Public)
    let result = speakMethod.Invoke(cat, [||]) :?> string
    Assert.Equal("Meow! I am Whiskers", result)

[<Fact>]
let ``Generative Speak override is dispatched polymorphically via Animal base reference``() =
    let animals = loadTestAssembly()
    let animalType = animals.GetNestedType("Animal")
    let dogType    = animals.GetNestedType("Dog")
    let catType    = animals.GetNestedType("Cat")
    let speakViaBase = animalType.GetMethod("Speak", BindingFlags.Instance ||| BindingFlags.Public)
    Assert.NotNull(speakViaBase)

    let dog = dogType.GetConstructor([| typeof<string> |]).Invoke([| box "Rex" |])
    let cat = catType.GetConstructor([| typeof<string> |]).Invoke([| box "Luna" |])

    // Invoke via the base-class MethodInfo — exercises virtual dispatch
    let dogResult = speakViaBase.Invoke(dog, [||]) :?> string
    let catResult = speakViaBase.Invoke(cat, [||]) :?> string
    Assert.Equal("Woof! I am Rex",   dogResult)
    Assert.Equal("Meow! I am Luna",  catResult)
