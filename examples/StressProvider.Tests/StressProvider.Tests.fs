#if INTERACTIVE
#r @"../test/StressProvider.dll"
#endif

module StressProvider.Tests

open Xunit


[<Fact>]
let ``StressProvider basic tests 1``() =

    let x = File1.providedTags.Tag1
    let x2a = File1.providedTags2a.Tag1
    let x2empty = File1.providedTags2empty.Tag1

    let v1 = File1.Generated1("innerstate").PropertyWithTryCatch3
    let v2 = File1.Generated1("innerstate").PropertyWithTryFinally1
    ()

[<Fact>]
let ``StressProvider erased try/finally``() =

    let mutable disposed = false
    let disp = { new System.IDisposable with member __.Dispose() = disposed <- true } 
    let v1 = File1.Provided3.Test(disp)
    Assert.Equal("[+] Yup, it worked totally.", v1)
    Assert.Equal(disposed, true)

[<Fact>]
let ``StressProvider generative try/finally``() =

    let mutable disposed = false
    let disp = { new System.IDisposable with member __.Dispose() = disposed <- true } 
    let v1 = File1.Generated3.Test(disp)
    Assert.Equal("[+] Yup, it worked totally.", v1)
    Assert.Equal(disposed, true)

