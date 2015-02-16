#if INTERACTIVE
#r @"../packages/Nunit.Runners/tools/nunit.framework.dll"
#r @"../test/ErasedWithConstructor.dll"
#endif

open NUnit.Framework
open ErasedWithConstructor.Provided

[<Test>]
let ``Default constructor should create instance`` () =
    Assert.AreEqual("My internal state", MyType().InnerState)

[<Test>]
let ``Constructor with parameter should create instance`` () =
    Assert.AreEqual("override", MyType("override").InnerState)
    