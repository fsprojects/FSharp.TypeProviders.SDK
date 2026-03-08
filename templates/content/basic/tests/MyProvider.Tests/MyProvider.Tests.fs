module MyProviderTests

open MyNamespace
open NUnit.Framework

[<Test>]
let ``Default constructor should create instance`` () =
    Assert.That(MyType().InnerState, Is.EqualTo("My internal state"))

[<Test>]
let ``Constructor with parameter should create instance`` () =
    Assert.That(MyType("override").InnerState, Is.EqualTo("override"))

[<Test>]
let ``Method with ReflectedDefinition parameter should get its name`` () =
    let myValue = 2
    Assert.That(MyType.NameOf(myValue), Is.EqualTo("myValue"))

type Generative2 = MyProvider.GenerativeProvider<2>
type Generative4 = MyProvider.GenerativeProvider<4>

[<Test>]
let ``Can access properties of generative provider 2`` () =
    let obj = Generative2()
    Assert.That(obj.Property1, Is.EqualTo(1))
    Assert.That(obj.Property2, Is.EqualTo(2))

[<Test>]
let ``Can access properties of generative provider 4`` () =
    let obj = Generative4()
    Assert.That(obj.Property1, Is.EqualTo(1))
    Assert.That(obj.Property2, Is.EqualTo(2))
    Assert.That(obj.Property3, Is.EqualTo(3))
    Assert.That(obj.Property4, Is.EqualTo(4))

