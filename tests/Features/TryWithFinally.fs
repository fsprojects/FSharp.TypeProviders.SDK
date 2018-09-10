module TryWithFinally

open System
open Tests
open Xunit

type TestType = TryWithFinallyProvider<"unused">

[<Fact>]
let ``TryWith and TryFinally expressions are supported``() =
    // Now for the actual tests...
    let mutable wasDisposed = false

    let notifyOnDeath = { new IDisposable with
        member __.Dispose() = wasDisposed <- true
    }

    let status = TestType.TestNonThrowing()

    Assert.Equal(status, 0)

    let exn = Assert.Throws<Exception>(Action(fun () ->
        TestType.TestThrowing(notifyOnDeath) |> ignore
    ))

    Assert.Equal(exn.Message, "This will throw and dispose the given argument.")
    Assert.Equal(wasDisposed, true)
