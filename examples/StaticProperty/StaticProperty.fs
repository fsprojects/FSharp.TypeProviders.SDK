#if INTERACTIVE
#load "../../src/ProvidedTypes.fsi" "../../src/ProvidedTypes.fs" 
#endif

namespace ProviderImplementation

open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection

[<TypeProvider>]
type BasicProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "StaticProperty.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        let myProp = ProvidedProperty("MyProperty", typeof<string>, isStatic = true, getterCode = (fun args -> <@@ "Hello world" @@>))
        myType.AddMember(myProp)
        [myType]

    do
        this.AddNamespace(ns, createTypes())

[<assembly:TypeProviderAssembly>]
do ()