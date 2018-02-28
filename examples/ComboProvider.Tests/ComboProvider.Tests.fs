#if INTERACTIVE
#r @"../test/ComboProvider.dll"
#endif

module ComboProvider.Tests

open ComboProvider
open Xunit

[<Fact>]
let ``Default constructor should create instance`` () =
    Assert.Equal("My internal state", MyType().InnerState)

[<Fact>]
let ``Constructor with parameter should create instance`` () =
    Assert.Equal("override", MyType("override").InnerState)

[<Fact>]
let ``Static method returns an object of right type`` () =
    Assert.Equal("SomeRuntimeHelper", MyType.StaticMethod().GetType().Name)

[<Fact>]
let ``StaticMethod2 returns a null`` () =
    Assert.True(MyType.StaticMethod2() = null)

let ``MyType supports null`` () =
    Assert.True(MyType("test it") <> null)
                

type Generative2 = ComboProvider.GenerativeProvider<2>
type Generative4 = ComboProvider.GenerativeProvider<4>

[<Fact>]
let ``Can access properties of generative provider 2`` () =
    let obj = Generative2()
    Assert.Equal(obj.Property1, 1)
    Assert.Equal(obj.Property2, 2)

[<Fact>]
let ``Can access properties of generative provider 4`` () =
    let obj = Generative4()
    Assert.Equal(obj.Property1, 1)
    Assert.Equal(obj.Property2, 2)
    Assert.Equal(obj.Property3, 3)
    Assert.Equal(obj.Property4, 4)

                