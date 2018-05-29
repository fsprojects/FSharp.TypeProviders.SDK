
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

    do this.AddNamespace(ns, [provider; tags])

[<assembly:CompilerServices.TypeProviderAssembly()>]
do ()
