#if INTERACTIVE
#r @"../packages/NUnit/lib/net45/nunit.framework.dll"
#r @"../test/StaticProperty.dll"
#endif

open NUnit.Framework
open StaticProperty.Provided

[<Test>]
let ``Static property should have been created`` () =
    Assert.AreEqual("Hello world", MyType.MyProperty)
    