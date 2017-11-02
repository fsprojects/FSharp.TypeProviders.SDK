#if INTERACTIVE
#r @"../test/ComboProvider.dll"
#endif

module ComboProvider.Tests

open ComboProvider.Provided
open Xunit

[<Fact>]
let ``Default constructor should create instance`` () =
    Assert.Equal("My internal state", MyType().InnerState)

[<Fact>]
let ``Constructor with parameter should create instance`` () =
    Assert.Equal("override", MyType("override").InnerState)
    