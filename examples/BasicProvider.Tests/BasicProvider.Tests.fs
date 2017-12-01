#if INTERACTIVE
#r @"../test/BasicProvider.dll"
#endif

module BasicProvider.Tests

open BasicProvider.Provided
open Xunit

[<Fact>]
let ``Default constructor should create instance`` () =
    Assert.Equal("My internal state", MyType().InnerState)

[<Fact>]
let ``Constructor with parameter should create instance`` () =
    Assert.Equal("override", MyType("override").InnerState)

[<Fact>]
let ``Method with ReflectedDefinition parameter should get its name`` () =
    let myValue = 2
    Assert.Equal("myValue", MyType.NameOf(myValue))
