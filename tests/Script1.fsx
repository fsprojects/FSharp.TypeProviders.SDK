#r "bin/Release/net461/FSharp.TypeProviders.SDK.Tests.dll"

open System
open FSharp.TypeProviders.SDK.Tests

let comparable = typeof<IComparable>
comparable.Attributes
comparable.GetMethod("CompareTo")
typeof<Collections.IEnumerable>.Attributes
let stream = typeof<IO.Stream>
stream.Attributes
stream.GetMethod("Read")
stream.GetMethod("BeginRead")
typeof<IO.TextReader>.Attributes
let t = typeof<FSharp.TypeProviders.SDK.Tests.StaticProperty.SampleTypeProvider>
let x = t.Assembly.GetTypes()
x
|> Array.map (fun t -> t.Name)
|> Array.sort
|> Array.filter (fun t -> t.StartsWith("IContract"))
