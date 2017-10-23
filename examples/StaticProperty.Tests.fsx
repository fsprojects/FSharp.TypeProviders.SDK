#if INTERACTIVE
#r @"StaticProperty\bin\Release\net461\StaticProperty.dll"
#endif

open StaticProperty.Provided

let t1 = MyType.MyProperty
let t2 = typeof<MyType>
    