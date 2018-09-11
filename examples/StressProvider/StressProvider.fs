
namespace ProviderImplementation

open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open FSharp.Core.CompilerServices
open System.Reflection


type SomeRuntimeHelper() = 
    static member Help() = "help"

[<AllowNullLiteral>]
type SomeRuntimeHelper2() = 
    static member Help() = "help"

type Server (name : string) =
    member x.Name with get() : string = name

[<TypeProvider>]
type ComboErasingProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "MyNamespace"
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

    do this.AddNamespace(ns, [provider; provider2; tags])


[<assembly:CompilerServices.TypeProviderAssembly()>]
do ()
