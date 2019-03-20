#r "bin/Release/net461/FSharp.TypeProviders.SDK.Tests.dll"

open System
open FSharp.TypeProviders.SDK.Tests

typeof<IComparable>.GetMethod("CompareTo")
typeof<IO.Stream>.GetMethod("Read")
typeof<IO.Stream>.GetMethod("BeginRead")
let t = typeof<FSharp.TypeProviders.SDK.Tests.StaticProperty.SampleTypeProvider>
let x = t.Assembly.GetTypes()
x
|> Array.map (fun t -> t.Name)
|> Array.sort
|> Array.filter (fun t -> t.StartsWith("IContract"))
