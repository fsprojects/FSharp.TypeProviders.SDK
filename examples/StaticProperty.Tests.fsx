#if INTERACTIVE
#r @"../test/StaticProperty.dll"
#endif

open StaticProperty.Provided

[<Fact>]
let ``Static property should have been created`` () =
    Assert.Equal("Hello world", MyType.MyProperty)
    