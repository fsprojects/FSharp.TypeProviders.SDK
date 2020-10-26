#if INTERACTIVE
#load "../src/ProvidedTypes.fsi" "../src/ProvidedTypes.fs" 
#load "../src/ProvidedTypesTesting.fs"

#else

module TPSDK.GeneratedOpTests
#endif

#if !NO_GENERATIVE

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting
open Microsoft.FSharp.Quotations

#nowarn "760" // IDisposable needs new

let testCases() = 
    let fsCoreVersion = typeof<list<int>>.Assembly.GetName().Version.ToString()
    [ (sprintf "FSharp.Core %s .NET Standard 2.0" fsCoreVersion, fsCoreVersion, (fun _ ->  true), Targets.DotNetStandard20FSharpRefs) ]

let testProvidedAssembly exprs =
    let runtimeAssemblyRefs = Targets.DotNetStandard20FSharpRefs()
    let runtimeAssembly = runtimeAssemblyRefs.[0]
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
    let tp = TypeProviderForNamespaces(cfg) //:> TypeProviderForNamespaces
    let ns = "Tests"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)
    let mutable counter = 0
        
    let create (expr : Expr) =  
        counter <- counter + 1
        let name = sprintf "F%d" counter
        ProvidedMethod(name,[],expr.Type,invokeCode = (fun _args -> expr), isStatic = true)
        |> container.AddMember
        name
    let names = exprs |> List.map (fst >> create)
    do tempAssembly.AddTypes [container]
    do tp.AddNamespace(container.Namespace, [container])
    let providedNamespace = tp.Namespaces.[0] 
    let providedTypes  = providedNamespace.GetTypes()
    let providedType = providedTypes.[0] 
    let providedTypeDefinition = providedType :?> ProvidedTypeDefinition
    Assert.Equal("Container", providedTypeDefinition.Name)
    let test (container : Type) = 
        let call name = container.GetMethod(name).Invoke(null,[||])
        (names, exprs)
        ||> List.iter2 (fun name (_,f) -> f(call name))
    let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedTypeDefinition.Assembly)
    let assembly = Assembly.Load assemContents
    assembly.ExportedTypes |> Seq.find (fun ty -> ty.Name = "Container") |> test

let runningOnMono = try Type.GetType("Mono.Runtime") <> null with _ -> false 

let check (e : Expr<'a>) expected = 
    e.Raw, fun o -> 
        let actual = Assert.IsType<'a>(o)
        let tolerate = 
            match typeof<'a> with 
            | ty when ty.FullName = "System.Single" -> abs ((box expected :?> single) - (box actual :?> single)) < 0.00001f
            | ty when ty.FullName = "System.Double" -> abs ((box expected :?> double) - (box actual :?> double)) < 0.00001
            | _ -> (expected = actual)
        Assert.True(tolerate, sprintf "%A Expected %A got %A. (%A)" (expected.GetType(), actual.GetType(), expected = actual) expected actual e)

let checkExpr (e : Expr<'a>) = 
    let expected = FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation(e) :?> 'a
    check e expected


[<Fact>]
let ``sub execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ 50 - 30 @>
            checkExpr <@ 50.0 - 30.0 @>
            checkExpr <@ 50.f - 30.f @>
            checkExpr <@ 50L - 30L @>
            checkExpr <@ 50UL - 30UL @>
            checkExpr <@ 50l - 30l @>
            checkExpr <@ 50s - 30s @>
            checkExpr <@ 50us - 30us @>
            check <@ 50y - 30y @> 20y
            check <@ 50uy - 30uy @> 20uy
            checkExpr <@ 50m - 30m @>
            checkExpr <@ TimeSpan.FromMinutes 50.0 - TimeSpan.FromMinutes 30.0 @>
        ]

[<Fact>]
let ``add execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ 50 + 30 @>
            checkExpr <@ 50.0 + 30.0 @>
            checkExpr <@ 50.f + 30.f @>
            checkExpr <@ 50L + 30L @>
            checkExpr <@ 50UL + 30UL @>
            checkExpr <@ 50l + 30l @>
            checkExpr <@ 50s + 30s @>
            checkExpr <@ 50us + 30us @>
            check <@ 50y + 30y @> 80y
            check <@ 50uy + 30uy @> 80uy
            checkExpr <@ 50m + 30m @>
            checkExpr <@ "50m" + "30m" @>
            checkExpr <@ TimeSpan.FromMinutes 50.0 + TimeSpan.FromMinutes 30.0 @>
        ]        
        
[<Fact>]
let ``mul execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ 50 * 30 @>
            checkExpr <@ 50.0 * 30.0 @>
            checkExpr <@ 50.f * 30.f @>
            checkExpr <@ 50L * 30L @>
            checkExpr <@ 50UL * 30UL @>
            checkExpr <@ 50l * 30l @>
            checkExpr <@ 50s * 30s @>
            checkExpr <@ 50us * 30us @>
            check <@ 5y * 3y @> 15y
            check <@ 5uy * 3uy @> 15uy
            checkExpr <@ 50m * 30m @>
            //TODO: mul method test
        ]         
        
[<Fact>]
let ``div execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ 50 / 30 @>
            checkExpr <@ 50.0 / 30.0 @>
            checkExpr <@ 50.f / 30.f @>
            checkExpr <@ 50L / 30L @>
            checkExpr <@ 50UL / 30UL @>
            checkExpr <@ 50l / 30l @>
            checkExpr <@ 50s / 30s @>
            checkExpr <@ 50us / 30us @>
            checkExpr <@ 50 / 10 @>
            checkExpr <@ 50.0 / 10.0 @>
            checkExpr <@ 50.f / 10.f @>
            checkExpr <@ 50L / 30L @>
            checkExpr <@ 50UL / 10UL @>
            checkExpr <@ 50l / 10l @>
            checkExpr <@ 50s / 10s @>
            checkExpr <@ 50us / 10us @>
            check <@ 50y / 10y @> 5y
            check <@ 50uy / 10uy @> 5uy
            checkExpr <@ 50m / 30m @>
            //TODO: mul method test
        ]        

[<Fact>]
let ``neg execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ (~-) 50 @>
            checkExpr <@ (~-) 30.0 @>
            checkExpr <@ (~-) 30.f @>
            checkExpr <@ (~-) 30L @>
            checkExpr <@ (~-) 30l @>
            checkExpr <@ (~-) 30s @>
            check <@ (~-) 30y @> -30y
            checkExpr <@ (~-) 30m @>
            checkExpr <@ (~-) (TimeSpan.FromMinutes 50.0) @>
        ]     

[<Fact>]
let ``pos execute correctly``() =
    testProvidedAssembly 
        [
            check <@ (~+) 50 @> 50
            check <@ (~+) 30.0 @> 30.0
            check <@ (~+) 30.f @> 30.f
            check <@ (~+) 30L @> 30L
            check <@ (~+) 30l @> 30l
            check <@ (~+) 30s @> 30s
            check <@ (~+) 30y @> 30y
            check <@ (~+) 30m @> 30m
            check <@ (~+) (TimeSpan.FromMinutes 50.0) @> (TimeSpan.FromMinutes 50.0)
        ]     


[<Fact>]
let ``rem execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ 50 % 30 @>
            checkExpr <@ 50.0 % 30.0 @>
            checkExpr <@ 50.f % 30.f @>
            checkExpr <@ 50L % 30L @>
            checkExpr <@ 50UL % 30UL @>
            checkExpr <@ 50l % 30l @>
            checkExpr <@ 50s % 30s @>
            checkExpr <@ 50us % 30us @>
            check <@ 50y % 30y @> 20y
            check <@ 50uy % 30uy @> 20uy
            checkExpr <@ 50m % 30m @>
            //TODO: rem method test
        ]


[<Fact>]
let ``shl execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ 2 <<< 3 @>
            checkExpr <@ 2L <<< 3 @>
            checkExpr <@ 5UL <<< 3 @>
            checkExpr <@ 5l <<< 3 @>
            checkExpr <@ 5s <<< 3 @>
            checkExpr <@ 5us <<< 3 @>
            checkExpr <@ 5y <<< 3 @>
            checkExpr <@ 5uy <<< 3 @>
            checkExpr <@ 2 <<< 1 @>
            checkExpr <@ 2L <<< 1 @>
            checkExpr <@ 5UL <<< 1 @>
            checkExpr <@ 5l <<< 1 @>
            checkExpr <@ 5s <<< 1 @>
            checkExpr <@ 5us <<< 1 @>
            checkExpr <@ 5y <<< 1 @>
            checkExpr <@ 5uy <<< 1 @>
        ]

[<Fact>]
let ``shr execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ 2 >>> 3 @>
            checkExpr <@ 2L >>> 3 @>
            checkExpr <@ 5UL >>> 3 @>
            checkExpr <@ 5l >>> 3 @>
            checkExpr <@ 5s >>> 3 @>
            checkExpr <@ 5us >>> 3 @>
            checkExpr <@ 5y >>> 3 @>
            checkExpr <@ 5uy >>> 3 @>
            checkExpr <@ 2 >>> 1 @>
            checkExpr <@ 2L >>> 1 @>
            checkExpr <@ 5UL >>> 1 @>
            checkExpr <@ 5l >>> 1 @>
            checkExpr <@ 5s >>> 1 @>
            checkExpr <@ 5us >>> 1 @>
            checkExpr <@ 5y >>> 1 @>
            checkExpr <@ 5uy >>> 1 @>
        ]


[<Fact>]
let ``int execute correctly``() =
    testProvidedAssembly 
        [
            check <@ int "50" @> 50
            checkExpr <@ int 50 @>
            checkExpr <@ int 50.0 @>
            checkExpr <@ int 50.f @>
            checkExpr <@ int 50UL @>
            checkExpr <@ int 50l @>
            checkExpr <@ int 50s @>
            checkExpr <@ int 50us @>
            checkExpr <@ int 50y @>
            checkExpr <@ int 50uy @> 
            checkExpr <@ int 50m @>
            checkExpr <@ int 50I @>
        ]
    
[<Fact>]
let ``int32 execute correctly``() =
    testProvidedAssembly 
        [
            check <@ int32 "50" @> 50
            checkExpr <@ int32 50 @>
            checkExpr <@ int32 50.0 @>
            checkExpr <@ int32 50.f @>
            checkExpr <@ int32 50UL @>
            checkExpr <@ int32 50l @>
            checkExpr <@ int32 50s @>
            checkExpr <@ int32 50us @>
            checkExpr <@ int32 50y @>
            checkExpr <@ int32 50uy @> 
            checkExpr <@ int32 50m @>
            checkExpr <@ int32 50I @>
        ]


[<Fact>]
let ``int64 execute correctly``() =
    testProvidedAssembly 
        [
            check <@ int64 "50" @> 50L
            checkExpr <@ int64 50 @>
            checkExpr <@ int64 50.0 @>
            checkExpr <@ int64 50.f @>
            checkExpr <@ int64 50UL @>
            checkExpr <@ int64 50l @>
            checkExpr <@ int64 50s @>
            checkExpr <@ int64 50us @>
            checkExpr <@ int64 50y @>
            checkExpr <@ int64 50uy @> 
            checkExpr <@ int64 50m @>
            checkExpr <@ int64 50I @>
        ]

[<Fact>]
let ``int16 execute correctly``() =
    testProvidedAssembly 
        [
            check <@ int16 "50" @> 50s
            checkExpr <@ int16 50 @>
            checkExpr <@ int16 50.0 @>
            checkExpr <@ int16 50.f @>
            checkExpr <@ int16 50UL @>
            checkExpr <@ int16 50l @>
            checkExpr <@ int16 50s @>
            checkExpr <@ int16 50us @>
            checkExpr <@ int16 50y @>
            checkExpr <@ int16 50uy @> 
            checkExpr <@ int16 50m @>
            checkExpr <@ int16 50I @>
        ]


[<Fact>]
let ``uint32 execute correctly``() =
    testProvidedAssembly 
        [
            check <@ uint32 "50" @> 50u
            checkExpr <@ uint32 50 @>
            checkExpr <@ uint32 50.0 @>
            checkExpr <@ uint32 50.f @>
            checkExpr <@ uint32 50UL @>
            checkExpr <@ uint32 50l @>
            checkExpr <@ uint32 50s @>
            checkExpr <@ uint32 50us @>
            checkExpr <@ uint32 50y @>
            checkExpr <@ uint32 50uy @> 
            checkExpr <@ uint32 50m @>
            checkExpr <@ uint32 50I @>
        ]


[<Fact>]
let ``uint64 execute correctly``() =
    testProvidedAssembly 
        [
            check <@ uint64 "50" @> 50UL
            checkExpr <@ uint64 50 @>
            checkExpr <@ uint64 50.0 @>
            checkExpr <@ uint64 50.f @>
            checkExpr <@ uint64 50UL @>
            checkExpr <@ uint64 50l @>
            checkExpr <@ uint64 50s @>
            checkExpr <@ uint64 50us @>
            checkExpr <@ uint64 50y @>
            checkExpr <@ uint64 50uy @> 
            checkExpr <@ uint64 50m @>
            checkExpr <@ uint64 50I @>
        ]

[<Fact>]
let ``uint16 execute correctly``() =
    testProvidedAssembly 
        [
            check <@ uint16 "50" @> 50us
            checkExpr <@ uint16 50 @>
            checkExpr <@ uint16 50.0 @>
            checkExpr <@ uint16 50.f @>
            checkExpr <@ uint16 50UL @>
            checkExpr <@ uint16 50l @>
            checkExpr <@ uint16 50s @>
            checkExpr <@ uint16 50us @>
            checkExpr <@ uint16 50y @>
            checkExpr <@ uint16 50uy @> 
            checkExpr <@ uint16 50m @>
            checkExpr <@ uint16 50I @>
        ]

[<Fact>]
let ``byte execute correctly``() =
    testProvidedAssembly 
        [
            check <@ byte "50" @> 50uy
            checkExpr <@ byte 50 @>
            checkExpr <@ byte 50.0 @>
            checkExpr <@ byte 50.f @>
            checkExpr <@ byte 50UL @>
            checkExpr <@ byte 50l @>
            checkExpr <@ byte 50s @>
            checkExpr <@ byte 50us @>
            checkExpr <@ byte 50y @>
            checkExpr <@ byte 50uy @> 
            checkExpr <@ byte 50m @>
            checkExpr <@ byte 50I @>
        ]


[<Fact>]
let ``sbyte execute correctly``() =
    testProvidedAssembly 
        [
            check <@ sbyte "50" @> 50y
            checkExpr <@ sbyte 50 @>
            checkExpr <@ sbyte 50.0 @>
            checkExpr <@ sbyte 50.f @>
            checkExpr <@ sbyte 50UL @>
            checkExpr <@ sbyte 50l @>
            checkExpr <@ sbyte 50s @>
            checkExpr <@ sbyte 50us @>
            checkExpr <@ sbyte 50y @>
            checkExpr <@ sbyte 50uy @> 
            checkExpr <@ sbyte 50m @>
            checkExpr <@ sbyte 50I @>
        ]

        

[<Fact>]
let ``float32 execute correctly``() =
    testProvidedAssembly 
        [
            check <@ float32 "50" @> 50.f
            checkExpr <@ float32 50 @>
            checkExpr <@ float32 50.0 @>
            checkExpr <@ float32 50.f @>
            checkExpr <@ float32 50UL @>
            checkExpr <@ float32 50l @>
            checkExpr <@ float32 50s @>
            checkExpr <@ float32 50us @>
            checkExpr <@ float32 50y @>
            checkExpr <@ float32 50uy @> 
            checkExpr <@ float32 50m @>
            checkExpr <@ float32 50I @>
        ]


[<Fact>]
let ``float execute correctly``() =
    testProvidedAssembly 
        [
            check <@ float "50" @> 50.0
            checkExpr <@ float 50 @>
            checkExpr <@ float 50.0 @>
            checkExpr <@ float 50.f @>
            checkExpr <@ float 50UL @>
            checkExpr <@ float 50l @>
            checkExpr <@ float 50s @>
            checkExpr <@ float 50us @>
            checkExpr <@ float 50y @>
            checkExpr <@ float 50uy @> 
            checkExpr <@ float 50m @>
            checkExpr <@ float 50I @>
        ]


[<Fact>]
let ``double execute correctly``() =
    testProvidedAssembly 
        [
            check <@ double "50" @> 50.0
            checkExpr <@ double 50 @>
            checkExpr <@ double 50.0 @>
            checkExpr <@ double 50.f @>
            checkExpr <@ double 50UL @>
            checkExpr <@ double 50l @>
            checkExpr <@ double 50s @>
            checkExpr <@ double 50us @>
            checkExpr <@ double 50y @>
            checkExpr <@ double 50uy @> 
            checkExpr <@ double 50m @>
            checkExpr <@ double 50I @>
        ]


[<Fact>]
let ``decimal execute correctly``() =
    testProvidedAssembly 
        [
            check <@ decimal "50" @> 50m
            checkExpr <@ decimal 50 @>
            checkExpr <@ decimal 50.0 @>
            checkExpr <@ decimal 50.f @>
            checkExpr <@ decimal 50UL @>
            checkExpr <@ decimal 50l @>
            checkExpr <@ decimal 50s @>
            checkExpr <@ decimal 50us @>
            checkExpr <@ decimal 50y @>
            checkExpr <@ decimal 50uy @> 
            checkExpr <@ decimal 50m @>
            checkExpr <@ decimal 50I @>
        ]


[<Fact>]
let ``char execute correctly``() =
    testProvidedAssembly 
        [
            check <@ char "5" @> '5'
            checkExpr <@ char 50 @>
            checkExpr <@ char 50.0 @>
            checkExpr <@ char 50.f @>
            checkExpr <@ char 50UL @>
            checkExpr <@ char 50l @>
            checkExpr <@ char 50s @>
            checkExpr <@ char 50us @>
            checkExpr <@ char 50y @>
            checkExpr <@ char 50uy @> 
            checkExpr <@ char 50m @>
        ]



[<Fact>]
let ``eq execute correctly``() =
    testProvidedAssembly 
        [
           checkExpr <@ true = true @>
           checkExpr <@ true = false @>
           checkExpr <@ false = false @>
           checkExpr <@ false = true @>
           checkExpr <@ 1y = 1y @>
           checkExpr <@ 2y = 1y @>
           checkExpr <@ 1y = 2y @>
           checkExpr <@ 1s = 1s @>
           checkExpr <@ 2s = 1s @>
           checkExpr <@ 1s = 2s @>
           checkExpr <@ 1 = 1 @>
           checkExpr <@ 2 = 1 @>
           checkExpr <@ 1 = 2 @>
           checkExpr <@ 1L = 1L @>
           checkExpr <@ 2L = 1L @>
           checkExpr <@ 1L = 2L @>
           checkExpr <@ 1uy = 1uy @>
           checkExpr <@ 2uy = 1uy @>
           checkExpr <@ 1uy = 2uy @>
           checkExpr <@ 1us = 1us @>
           checkExpr <@ 2us = 1us @>
           checkExpr <@ 1us = 2us @>
           checkExpr <@ 1u = 1u @>
           checkExpr <@ 2u = 1u @>
           checkExpr <@ 1u = 2u @>
           checkExpr <@ 1UL = 1UL @>
           checkExpr <@ 2UL = 1UL @>
           checkExpr <@ 1UL = 2UL @>
           checkExpr <@ 1.0 = 1.0 @>
           checkExpr <@ 2.0 = 1.0 @>
           checkExpr <@ 1.0 = 2.0 @>
           checkExpr <@ 1.0 = nan @>
           checkExpr <@ nan = nan @>
           checkExpr <@ nan = 1.0 @>
           checkExpr <@ 1.0f = nanf @>
           checkExpr <@ nanf = nanf @>
           checkExpr <@ nanf = 1.0f @>
           checkExpr <@ 1.f = 1.f @>
           checkExpr <@ 2.f = 1.f @>
           checkExpr <@ 1.f = 2.f @>
           checkExpr <@ '1' = '1' @>
           checkExpr <@ '2' = '1' @>
           checkExpr <@ '1' = '2' @>
           checkExpr <@ 1m = 1m @>
           checkExpr <@ 2m = 1m @>
           checkExpr <@ 1m = 2m @>
           checkExpr <@ "1" = "1" @>
           checkExpr <@ "2" = "1" @>
           checkExpr <@ "1" = "2" @>
           checkExpr <@ DateTime(2000,1,1) = DateTime(2000,1,1) @>
           checkExpr <@ DateTime(2000,1,1) = DateTime(2000,1,2) @>
        ]



[<Fact>]
let ``lt execute correctly``() =
    testProvidedAssembly 
        [
           check <@ true < true @> (true < true)
           check <@ true < false @> (true < false)
           check <@ false < false @> (false < false)
           check <@ false < true @> (false < true)
           checkExpr <@ 1y < 1y @>
           checkExpr <@ 2y < 1y @>
           checkExpr <@ 1y < 2y @>
           checkExpr <@ 1s < 1s @>
           checkExpr <@ 2s < 1s @>
           checkExpr <@ 1s < 2s @>
           checkExpr <@ 1 < 1 @>
           checkExpr <@ 2 < 1 @>
           checkExpr <@ 1 < 2 @>
           checkExpr <@ 1L < 1L @>
           checkExpr <@ 2L < 1L @>
           checkExpr <@ 1L < 2L @>
           checkExpr <@ 1uy < 1uy @>
           checkExpr <@ 2uy < 1uy @>
           checkExpr <@ 1uy < 2uy @>
           checkExpr <@ 1us < 1us @>
           checkExpr <@ 2us < 1us @>
           checkExpr <@ 1us < 2us @>
           checkExpr <@ 1u < 1u @>
           checkExpr <@ 2u < 1u @>
           checkExpr <@ 1u < 2u @>
           checkExpr <@ 1UL < 1UL @>
           checkExpr <@ 2UL < 1UL @>
           checkExpr <@ 1UL < 2UL @>
           checkExpr <@ 1.0 < 1.0 @>
           checkExpr <@ 2.0 < 1.0 @>
           checkExpr <@ 1.0 < 2.0 @>
           checkExpr <@ 1.0 < nan @>
           checkExpr <@ nan < nan @>
           checkExpr <@ nan < 1.0 @>
           checkExpr <@ 1.0f < nanf @>
           checkExpr <@ nanf < nanf @>
           checkExpr <@ nanf < 1.0f @>
           checkExpr <@ 1.f < 1.f @>
           checkExpr <@ 2.f < 1.f @>
           checkExpr <@ 1.f < 2.f @>
           checkExpr <@ '1' < '1' @>
           checkExpr <@ '2' < '1' @>
           checkExpr <@ '1' < '2' @>
           checkExpr <@ 1m < 1m @>
           checkExpr <@ 2m < 1m @>
           checkExpr <@ 1m < 2m @>
           check <@ "1" < "1" @> ("1" < "1")
           check <@ "2" < "1" @> ("2" < "1")
           check <@ "1" < "2" @> ("1" < "2")
           checkExpr <@ DateTime(2000,1,1) < DateTime(2000,1,1) @>
           checkExpr <@ DateTime(2000,1,1) < DateTime(2000,1,2) @>
        ]



[<Fact>]
let ``lte execute correctly``() =
    testProvidedAssembly 
        [
           check <@ true <= true @> (true <= true)
           check <@ true <= false @> (true <= false)
           check <@ false <= false @> (false <= false)
           check <@ false <= true @> (false <= true)
           checkExpr <@ 1y <= 1y @>
           checkExpr <@ 2y <= 1y @>
           checkExpr <@ 1y <= 2y @>
           checkExpr <@ 1s <= 1s @>
           checkExpr <@ 2s <= 1s @>
           checkExpr <@ 1s <= 2s @>
           checkExpr <@ 1 <= 1 @>
           checkExpr <@ 2 <= 1 @>
           checkExpr <@ 1 <= 2 @>
           checkExpr <@ 1L <= 1L @>
           checkExpr <@ 2L <= 1L @>
           checkExpr <@ 1L <= 2L @>
           checkExpr <@ 1uy <= 1uy @>
           checkExpr <@ 2uy <= 1uy @>
           checkExpr <@ 1uy <= 2uy @>
           checkExpr <@ 1us <= 1us @>
           checkExpr <@ 2us <= 1us @>
           checkExpr <@ 1us <= 2us @>
           checkExpr <@ 1u <= 1u @>
           checkExpr <@ 2u <= 1u @>
           checkExpr <@ 1u <= 2u @>
           checkExpr <@ 1UL <= 1UL @>
           checkExpr <@ 2UL <= 1UL @>
           checkExpr <@ 1UL <= 2UL @>
           checkExpr <@ 1.0 <= 1.0 @>
           checkExpr <@ 2.0 <= 1.0 @>
           checkExpr <@ 1.0 <= 2.0 @>
           checkExpr <@ 1.0 <= nan @>
           checkExpr <@ nan <= nan @>
           checkExpr <@ nan <= 1.0 @>
           checkExpr <@ 1.0f <= nanf @>
           checkExpr <@ nanf <= nanf @>
           checkExpr <@ nanf <= 1.0f @>
           checkExpr <@ 1.f <= 1.f @>
           checkExpr <@ 2.f <= 1.f @>
           checkExpr <@ 1.f <= 2.f @>
           checkExpr <@ '1' <= '1' @>
           checkExpr <@ '2' <= '1' @>
           checkExpr <@ '1' <= '2' @>
           checkExpr <@ 1m <= 1m @>
           checkExpr <@ 2m <= 1m @>
           checkExpr <@ 1m <= 2m @>
           check <@ "1" <= "1" @> ("1" <= "1")
           check <@ "2" <= "1" @> ("2" <= "1")
           check <@ "1" <= "2" @> ("1" <= "2")
           checkExpr <@ DateTime(2000,1,1) <= DateTime(2000,1,1) @>
           checkExpr <@ DateTime(2000,1,1) <= DateTime(2000,1,2) @>
        ]


[<Fact>]
let ``gt execute correctly``() =
    testProvidedAssembly 
        [
           check <@ true > true @> (true > true)
           check <@ true > false @> (true > false)
           check <@ false > false @> (false > false)
           check <@ false > true @> (false > true)
           checkExpr <@ 1y > 1y @>
           checkExpr <@ 2y > 1y @>
           checkExpr <@ 1y > 2y @>
           checkExpr <@ 1s > 1s @>
           checkExpr <@ 2s > 1s @>
           checkExpr <@ 1s > 2s @>
           checkExpr <@ 1 > 1 @>
           checkExpr <@ 2 > 1 @>
           checkExpr <@ 1 > 2 @>
           checkExpr <@ 1L > 1L @>
           checkExpr <@ 2L > 1L @>
           checkExpr <@ 1L > 2L @>
           checkExpr <@ 1uy > 1uy @>
           checkExpr <@ 2uy > 1uy @>
           checkExpr <@ 1uy > 2uy @>
           checkExpr <@ 1us > 1us @>
           checkExpr <@ 2us > 1us @>
           checkExpr <@ 1us > 2us @>
           checkExpr <@ 1u > 1u @>
           checkExpr <@ 2u > 1u @>
           checkExpr <@ 1u > 2u @>
           checkExpr <@ 1UL > 1UL @>
           checkExpr <@ 2UL > 1UL @>
           checkExpr <@ 1UL > 2UL @>
           checkExpr <@ 1.0 > 1.0 @>
           checkExpr <@ 2.0 > 1.0 @>
           checkExpr <@ 1.0 > 2.0 @>
           checkExpr <@ 1.0 > nan @>
           checkExpr <@ nan > nan @>
           checkExpr <@ nan > 1.0 @>
           checkExpr <@ 1.0f > nanf @>
           checkExpr <@ nanf > nanf @>
           checkExpr <@ nanf > 1.0f @>
           checkExpr <@ 1.f > 1.f @>
           checkExpr <@ 2.f > 1.f @>
           checkExpr <@ 1.f > 2.f @>
           checkExpr <@ '1' > '1' @>
           checkExpr <@ '2' > '1' @>
           checkExpr <@ '1' > '2' @>
           checkExpr <@ 1m > 1m @>
           checkExpr <@ 2m > 1m @>
           checkExpr <@ 1m > 2m @>
           check <@ "1" > "1" @> ("1" > "1")
           check <@ "2" > "1" @> ("2" > "1")
           check <@ "1" > "2" @> ("1" > "2")
           checkExpr <@ DateTime(2000,1,1) > DateTime(2000,1,1) @>
           checkExpr <@ DateTime(2000,1,1) > DateTime(2000,1,2) @>
        ]


[<Fact>]
let ``gte execute correctly``() =
    testProvidedAssembly 
        [
           check <@ true >= true @> (true >= true)
           check <@ true >= false @> (true >= false)
           check <@ false >= false @> (false >= false)
           check <@ false >= true @> (false >= true)
           checkExpr <@ 1y >= 1y @>
           checkExpr <@ 2y >= 1y @>
           checkExpr <@ 1y >= 2y @>
           checkExpr <@ 1s >= 1s @>
           checkExpr <@ 2s >= 1s @>
           checkExpr <@ 1s >= 2s @>
           checkExpr <@ 1 >= 1 @>
           checkExpr <@ 2 >= 1 @>
           checkExpr <@ 1 >= 2 @>
           checkExpr <@ 1L >= 1L @>
           checkExpr <@ 2L >= 1L @>
           checkExpr <@ 1L >= 2L @>
           checkExpr <@ 1uy >= 1uy @>
           checkExpr <@ 2uy >= 1uy @>
           checkExpr <@ 1uy >= 2uy @>
           checkExpr <@ 1us >= 1us @>
           checkExpr <@ 2us >= 1us @>
           checkExpr <@ 1us >= 2us @>
           checkExpr <@ 1u >= 1u @>
           checkExpr <@ 2u >= 1u @>
           checkExpr <@ 1u >= 2u @>
           checkExpr <@ 1UL >= 1UL @>
           checkExpr <@ 2UL >= 1UL @>
           checkExpr <@ 1UL >= 2UL @>
           checkExpr <@ 1.0 >= 1.0 @>
           checkExpr <@ 2.0 >= 1.0 @>
           checkExpr <@ 1.0 >= 2.0 @>
           checkExpr <@ 1.0 >= nan @>
           checkExpr <@ nan >= nan @>
           checkExpr <@ nan >= 1.0 @>
           checkExpr <@ 1.0f >= nanf @>
           checkExpr <@ nanf >= nanf @>
           checkExpr <@ nanf >= 1.0f @>
           checkExpr <@ 1.f >= 1.f @>
           checkExpr <@ 2.f >= 1.f @>
           checkExpr <@ 1.f >= 2.f @>
           checkExpr <@ '1' >= '1' @>
           checkExpr <@ '2' >= '1' @>
           checkExpr <@ '1' >= '2' @>
           checkExpr <@ 1m >= 1m @>
           checkExpr <@ 2m >= 1m @>
           checkExpr <@ 1m >= 2m @>
           check <@ "1" >= "1" @> ("1" >= "1")
           check <@ "2" >= "1" @> ("2" >= "1")
           check <@ "1" >= "2" @> ("1" >= "2")
           checkExpr <@ DateTime(2000,1,1) >= DateTime(2000,1,1) @>
           checkExpr <@ DateTime(2000,1,1) >= DateTime(2000,1,2) @>
        ]


[<Fact>]
let ``abs execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ abs 50 @>
            checkExpr <@ abs 50.0 @>
            checkExpr <@ abs 50.f @>
            checkExpr <@ abs 50l @>
            checkExpr <@ abs 50s @>
            checkExpr <@ abs 50y @>
            checkExpr <@ abs 50m @>
            checkExpr <@ abs 50I @>
            checkExpr <@ abs -50 @>
            checkExpr <@ abs -50.0 @>
            checkExpr <@ abs -50.f @>
            checkExpr <@ abs -50l @>
            checkExpr <@ abs -50s @>
            checkExpr <@ abs -50y @>
            checkExpr <@ abs -50m @>
            checkExpr <@ abs -50I @>
        ]

[<Fact>]
let ``acos execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ acos 0.1 @>
            checkExpr <@ acos 0.1f @>
            checkExpr <@ acos -0.1 @>
            checkExpr <@ acos -0.1f @>
        ]

[<Fact>]
let ``asin execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ asin 0.1 @>
            checkExpr <@ asin 0.1f @>
            checkExpr <@ asin -0.1 @>
            checkExpr <@ asin -0.1f @>
        ]

[<Fact>]
let ``atan execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ atan 0.1 @>
            checkExpr <@ atan 0.1f @>
            checkExpr <@ atan -0.1 @>
            checkExpr <@ atan -0.1f @>
        ]

[<Fact>]
let ``atan2 execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ atan2 0.1 0.3 @>
            checkExpr <@ atan2 0.1f 0.3f @>
            checkExpr <@ atan2 -0.1 0.3 @>
            checkExpr <@ atan2 -0.1f 0.3f @>
        ]

[<Fact>]
let ``ceil execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ ceil 0.1 @>
            checkExpr <@ ceil 0.1f @>
            checkExpr <@ ceil -0.1 @>
            checkExpr <@ ceil -0.1f @>
        ]

[<Fact>]
let ``exp execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ exp 0.1 @>
            checkExpr <@ exp 0.1f @>
            checkExpr <@ exp -0.1 @>
            checkExpr <@ exp -0.1f @>
        ]

[<Fact>]
let ``floor execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ floor 0.1 @>
            checkExpr <@ floor 0.1f @>
            checkExpr <@ floor -0.1 @>
            checkExpr <@ floor -0.1f @>
        ]

[<Fact>]
let ``truncate execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ truncate 0.1 @>
            checkExpr <@ truncate 0.1f @>
            checkExpr <@ truncate -0.1 @>
            checkExpr <@ truncate -0.1f @>
        ]

[<Fact>]
let ``round execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ round 0.1 @>
            checkExpr <@ round 0.1f @>
            checkExpr <@ round -0.1 @>
            checkExpr <@ round -0.1f @>
        ]

[<Fact>]
let ``sign execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ sign 0.1 @>
            checkExpr <@ sign 0.1f @>
            checkExpr <@ sign -0.1 @>
            checkExpr <@ sign -0.1f @>
        ]

[<Fact>]
let ``log execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ log 0.1 @>
            checkExpr <@ log 0.1f @>
        ]

[<Fact>]
let ``log10 execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ log10 0.1 @>
            checkExpr <@ log10 0.1f @>
        ]

[<Fact>]
let ``sqrt execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ sqrt 0.1 @>
            checkExpr <@ sqrt 0.1f @>
        ]

[<Fact>]
let ``cos execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ cos 0.1 @>
            checkExpr <@ cos 0.1f @>
            checkExpr <@ cos -0.1 @>
            checkExpr <@ cos -0.1f @>
        ]

[<Fact>]
let ``cosh execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ cosh 0.1 @>
            checkExpr <@ cosh 0.1f @>
            checkExpr <@ cosh -0.1 @>
            checkExpr <@ cosh -0.1f @>
        ]


[<Fact>]
let ``sin execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ sin 0.1 @>
            checkExpr <@ sin 0.1f @>
            checkExpr <@ sin -0.1 @>
            checkExpr <@ sin -0.1f @>
        ]


[<Fact>]
let ``sinh execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ sinh 0.1 @>
            checkExpr <@ sinh 0.1f @>
            checkExpr <@ sinh -0.1 @>
            checkExpr <@ sinh -0.1f @>
        ]


[<Fact>]
let ``tan execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ tan 0.1 @>
            checkExpr <@ tan 0.1f @>
            checkExpr <@ tan -0.1 @>
            checkExpr <@ tan -0.1f @>
        ]


[<Fact>]
let ``tanh execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ tanh 0.1 @>
            checkExpr <@ tanh 0.1f @>
            checkExpr <@ tanh -0.1 @>
            checkExpr <@ tanh -0.1f @>
        ]


[<Fact>]
let ``pow execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ 0.1**2.5 @>
            checkExpr <@ 0.1f**2.5f @>
        ]


[<Fact>]
let ``max execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ max 0.1 2.5 @>
            checkExpr <@ max 2.5 0.1 @>
            checkExpr <@ max 1 2 @>
            checkExpr <@ max 2 1 @>
            checkExpr <@ max 0.1f 2.5f @>
            checkExpr <@ max 2.5f 0.1f @>
            checkExpr <@ max 10I 20I @>
            check
                <@ 
                    let mutable a = 2
                    let mx = max (a <- a + 1; 2) (a <- a*7; 5) 
                    (mx,a)
                @>
                (
                    let mutable a = 2
                    let mx = max (a <- a + 1; 2) (a <- a*7; 5) 
                    (mx,a)
                )
        ]

[<Fact>]
let ``min execute correctly``() =
    testProvidedAssembly 
        [
            checkExpr <@ min 0.1 2.5 @>
            checkExpr <@ min 2.5 0.1 @>
            checkExpr <@ min 1 2 @>
            checkExpr <@ min 2 1 @>
            checkExpr <@ min 0.1f 2.5f @>
            checkExpr <@ min 2.5f 0.1f @>
            checkExpr <@ min 10I 20I @>
            check
                <@ 
                    let mutable a = 2
                    let mn = min (a <- a + 1; 2) (a <- a*7; 5) 
                    (mn,a)
                @>
                (
                    let mutable a = 2
                    let mn = min (a <- a + 1; 2) (a <- a*7; 5) 
                    (mn,a)
                )
        ]


[<Fact>]
let ``bitwise and execute correctly``() =
    testProvidedAssembly 
        [
        
            checkExpr <@ 123441 &&& 4125211 @>
            checkExpr <@ 123441L &&& 4125211L @>
            checkExpr <@ 1234s &&& 4125s @>
            checkExpr <@ 12y &&& 41y @>
            checkExpr <@ 123441u &&& 4125211u @>
            checkExpr <@ 123441UL &&& 4125211UL @>
            checkExpr <@ 1234us &&& 4125us @>
            checkExpr <@ 12uy &&& 41uy @>
        ]
        
[<Fact>]
let ``bitwise or execute correctly``() =
    testProvidedAssembly 
        [
        
            checkExpr <@ 123441 ||| 4125211 @>
            checkExpr <@ 123441L ||| 4125211L @>
            checkExpr <@ 1234s ||| 4125s @>
            checkExpr <@ 12y ||| 41y @>
            checkExpr <@ 123441u ||| 4125211u @>
            checkExpr <@ 123441UL ||| 4125211UL @>
            checkExpr <@ 1234us ||| 4125us @>
            checkExpr <@ 12uy ||| 41uy @>
        ]

[<Fact>]
let ``bitwise xor execute correctly``() =
   testProvidedAssembly 
       [
       
           checkExpr <@ 123441 ^^^ 4125211 @>
           checkExpr <@ 123441L ^^^ 4125211L @>
           checkExpr <@ 1234s ^^^ 4125s @>
           checkExpr <@ 12y ^^^ 41y @>
           checkExpr <@ 123441u ^^^ 4125211u @>
           checkExpr <@ 123441UL ^^^ 4125211UL @>
           checkExpr <@ 1234us ^^^ 4125us @>
           checkExpr <@ 12uy ^^^ 41uy @>
       ]

[<Fact>]
let ``bitwise not execute correctly``() =
   testProvidedAssembly 
       [
       
           checkExpr <@ ~~~123441 @>
           checkExpr <@ ~~~123441L @>
           checkExpr <@ ~~~1234s @>
           checkExpr <@ ~~~12y @>
           checkExpr <@ ~~~123441u @>
           checkExpr <@ ~~~123441UL @>
           checkExpr <@ ~~~1234us @>
           checkExpr <@ ~~~12uy @>
       ]
#endif