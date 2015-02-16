#if INTERACTIVE
#load "../src/ProvidedTypes.fsi"
#load "../src/ProvidedTypes.fs"
#endif

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection

[<TypeProvider>]
type BasicProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "ErasedWithConstructor.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)

        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        myType.AddMember(ctor)

        let ctor2 = ProvidedConstructor(
                        [ProvidedParameter("InnerState", typeof<string>)],
                        InvokeCode = fun args -> <@@ (%%(args.[0]):string) :> obj @@>)
        myType.AddMember(ctor2)

        let innerState = ProvidedProperty("InnerState", typeof<string>,
                            GetterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
        myType.AddMember(innerState)

        [myType]

    do
        this.AddNamespace(ns, createTypes())

[<assembly:TypeProviderAssembly>]
do ()