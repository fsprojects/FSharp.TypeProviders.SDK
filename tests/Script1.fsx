#r "bin/Release/net461/FSharp.TypeProviders.SDK.Tests.dll"

open System

let comparable = typeof<IComparable>
comparable.BaseType
comparable.Attributes
comparable.GetMethod("CompareTo")

typeof<Collections.IEnumerable>.Attributes

let stream = typeof<IO.Stream>
stream.BaseType
stream.Attributes
stream.GetMethod("Read")
stream.GetMethod("BeginRead")

let textReader = typeof<IO.TextReader>
textReader.BaseType
textReader.Attributes

let streamReader = typeof<IO.StreamReader>
streamReader.BaseType
streamReader.Attributes

typeof<obj>.Attributes


open FSharp.TypeProviders.SDK.Tests

let t = typeof<FSharp.TypeProviders.SDK.Tests.StaticProperty.SampleTypeProvider>
let x = t.Assembly.GetTypes()
x
|> Array.map (fun t -> t.Name)
|> Array.sort
|> Array.filter (fun t -> t.StartsWith("IContract"))
