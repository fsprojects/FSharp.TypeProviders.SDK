
namespace ProviderImplementation

open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open FSharp.Core.CompilerServices
open System
open System.Reflection


type SomeRuntimeHelper() = 
    static member Help() = "help"

[<AllowNullLiteral>]
type SomeRuntimeHelper2() = 
    static member Help() = "help"

type Server (name : string) =
    member x.Name with get() : string = name

[<TypeProvider>]
type StressErasingProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "StressProvider"
    let asm = Assembly.GetExecutingAssembly()

    let newProperty t name getter isStatic = ProvidedProperty(name, t, getter, isStatic = isStatic)
    let newStaticProperty t name getter = newProperty t name (fun _ -> getter) true
    let newInstanceProperty t name getter = newProperty t name (fun _ -> getter) false
    let addStaticProperty t name getter (typ:ProvidedTypeDefinition) = typ.AddMember (newStaticProperty t name getter); typ
    let addInstanceProperty t name getter (typ:ProvidedTypeDefinition) = typ.AddMember (newInstanceProperty t name getter); typ

    let provider = ProvidedTypeDefinition(asm, ns, "Provider", Some typeof<obj>, hideObjectMethods = true)
    let tags = ProvidedTypeDefinition(asm, ns, "Tags", Some typeof<obj>, hideObjectMethods = true)           
    do [1..2000] |> Seq.iter (fun i -> addInstanceProperty typeof<int> (sprintf "Tag%d" i) <@@ i @@> tags |> ignore)

    do provider.DefineStaticParameters([ProvidedStaticParameter("Host", typeof<string>)], fun name args ->
        let provided = ProvidedTypeDefinition(asm, ns, name, Some typeof<obj>, hideObjectMethods = true)
        addStaticProperty tags "Tags" <@@ obj() @@> provided |> ignore
        provided
    )

    // An example provider with one _optional_ static parameter
    let provider2 = ProvidedTypeDefinition(asm, ns, "Provider2", Some typeof<obj>, hideObjectMethods = true)
    do provider2.DefineStaticParameters([ProvidedStaticParameter("Host", typeof<string>, "default")], fun name args ->
        let provided = 
            let srv = args.[0] :?> string
            let prop = ProvidedProperty("Server", typeof<Server>, (fun _ -> <@@ Server(srv) @@>), isStatic = true)
            let provided = ProvidedTypeDefinition(asm, ns, name, Some typeof<obj>, hideObjectMethods = true)
            provided.AddMember prop
            addStaticProperty tags "Tags" <@@ obj() @@> provided |> ignore
            provided

        provided
    )

    let provider3 = ProvidedTypeDefinition(asm, ns, "Provider3", Some typeof<obj>, hideObjectMethods = true)

    do provider3.DefineStaticParameters([ProvidedStaticParameter("Host", typeof<string>)], fun name _ ->
        let provided = ProvidedTypeDefinition(asm, ns, name, Some typeof<obj>, hideObjectMethods = true)

        let fn = ProvidedMethod("Test", [ ProvidedParameter("disp", typeof<IDisposable>) ], typeof<string>, fun [ arg ] ->
            <@@
                use __ = (%%arg : IDisposable)
                let mutable res = ""

                try
                    failwith "This will throw anyway, don't mind it."

                    res <- "[-] Should not get here."
                finally
                    res <- "[+] Caught try-finally, nice."

                    try
                        failwith "It failed again."

                        res <- "[-] Should not get here."
                    with
                    | _ ->
                        res <- "[+] Caught try-with, nice."

                    try
                        res <- "[?] Gonna go to finally without throwing..."
                    finally
                        res <- "[+] Yup, it worked totally."
                res
            @@>
        , isStatic = true)

        provided.AddMember fn
        provided
    )

    do this.AddNamespace(ns, [provider; provider2; provider3; tags])

[<TypeProvider>]
type StressGenerativeProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "StressProvider"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<SomeRuntimeHelper>.Assembly.GetName().Name = asm.GetName().Name)  

    let createType typeName (count:int) =
        let asm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased=false)

        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        myType.AddMember(ctor)

        let ctor2 = ProvidedConstructor([ProvidedParameter("InnerState", typeof<string>)], invokeCode = fun args -> <@@ (%%(args.[1]):string) :> obj @@>)
        myType.AddMember(ctor2)

        for i in 1 .. count do 
            let prop = ProvidedProperty("PropertyWithTryCatch" + string i, typeof<int>, getterCode = fun args -> <@@ try i with _ -> i+1 @@>)
            myType.AddMember(prop)

        for i in 1 .. count do 
            let prop = ProvidedProperty("PropertyWithTryFinally" + string i, typeof<int>, getterCode = fun args -> <@@ try i finally ignore i @@>)
            myType.AddMember(prop)

        let meth = ProvidedMethod("StaticMethod", [], typeof<SomeRuntimeHelper>, isStatic=true, invokeCode = (fun args -> Expr.Value(null, typeof<SomeRuntimeHelper>)))
        myType.AddMember(meth)
        asm.AddTypes [ myType ]

        myType

    let provider = 
        let t = ProvidedTypeDefinition(asm, ns, "GenerativeProvider", Some typeof<obj>, isErased=false)
        t.DefineStaticParameters( [ProvidedStaticParameter("Count", typeof<int>)], fun typeName args -> createType typeName (unbox<int> args.[0]))
        t

    let provider3 = ProvidedTypeDefinition(asm, ns, "GenerativeProvider3", Some typeof<obj>, hideObjectMethods = true)

    do provider3.DefineStaticParameters([ProvidedStaticParameter("Host", typeof<string>)], fun name _ ->
        let provided = ProvidedTypeDefinition(asm, ns, name, Some typeof<obj>, hideObjectMethods = true)

        let fn = ProvidedMethod("Test", [ ProvidedParameter("disp", typeof<IDisposable>) ], typeof<string>, fun [ arg ] ->
            <@@
                use __ = (%%arg : IDisposable)
                let mutable res = ""

                try
                    failwith "This will throw anyway, don't mind it."

                    res <- "[-] Should not get here."
                finally
                    res <- "[+] Caught try-finally, nice."

                    try
                        failwith "It failed again."

                        res <- "[-] Should not get here."
                    with
                    | _ ->
                        res <- "[+] Caught try-with, nice."

                    try
                        res <- "[?] Gonna go to finally without throwing..."
                    finally
                        res <- "[+] Yup, it worked totally."
                res
            @@>
        , isStatic = true)

        provided.AddMember fn
        provided
    )

    do
        this.AddNamespace(ns, [provider; provider3])


[<assembly:CompilerServices.TypeProviderAssembly()>]
do ()
