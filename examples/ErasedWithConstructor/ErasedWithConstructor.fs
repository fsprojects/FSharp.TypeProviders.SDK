#if INTERACTIVE
#load "../../src/ProvidedTypes.fsi" "../../src/ProvidedTypes.fs"
#endif

namespace ProviderImplementation

open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection

[<TypeProvider>]
type BasicErasingProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "ErasedWithConstructor.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)

        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        myType.AddMember(ctor)

        let ctor2 = ProvidedConstructor([ProvidedParameter("InnerState", typeof<string>)], invokeCode = fun args -> <@@ (%%(args.[0]):string) :> obj @@>)
        myType.AddMember(ctor2)

        let innerState = ProvidedProperty("InnerState", typeof<string>, getterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
        myType.AddMember(innerState)

        [myType]

    do
        this.AddNamespace(ns, createTypes())

[<assembly:TypeProviderAssembly>]
do ()

