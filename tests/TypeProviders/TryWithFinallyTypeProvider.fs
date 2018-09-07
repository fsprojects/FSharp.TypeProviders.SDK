namespace FSharp.TypeProviders.SDK.Tests.TypeProviders

open System

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

#nowarn "0025"

[<TypeProvider>]
type TypeProviderWithTryWithFinally(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config)

    let ns = "Tests"
    let asm = System.Reflection.Assembly.GetExecutingAssembly()

    let provider = ProvidedTypeDefinition(asm, ns, "TryWithFinallyProvider", Some typeof<obj>)

    do provider.DefineStaticParameters([ProvidedStaticParameter("Host", typeof<string>)], fun name _ ->
        let provided = ProvidedTypeDefinition(asm, ns, name, Some typeof<obj>)

        let providedParameters = [
            ProvidedParameter("disp", typeof<IDisposable>)
        ]

        ProvidedMethod("TestThrowing", providedParameters, typeof<int>, fun [ arg ] ->
            <@@
                use __ = (%%arg : IDisposable)

                failwith "This will throw and dispose the given argument."
                0
            @@>
        , isStatic = true) |> provided.AddMember
        
        ProvidedMethod("TestNonThrowing", [], typeof<int>, fun [] ->
            <@@
                try
                    raise <| ArgumentException("Throwing exception...")
                with
                | :? ArgumentException as exn when exn.Message = "Throwing an exception..." ->
                    let mutable status = 3

                    try
                        try
                            failwith "Failing"
                        finally
                            status <- 2
                    with _ ->
                        assert(status = 2)

                        status <- 1
                    
                    assert(status = 1)

                    try
                        try
                            failwith "This will not get caught."
                        with
                        | :? ArgumentException -> status <- 5
                        | exn when exn.Message = "This will get caught." -> status <- 6
                    with
                    | _ -> assert(status = 1) // Status shouldn't have changed.

                    assert(status = 1)

                    status <-
                        try
                            try
                                failwith "Nope."
                            with
                            | _ -> 0
                        with
                        | _ -> -1
                    
                    status
            @@>
        , isStatic = true) |> provided.AddMember

        provided
    )

    do this.AddNamespace(ns, [provider])
