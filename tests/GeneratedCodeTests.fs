#if INTERACTIVE
#load "../src/ProvidedTypes.fsi" "../src/ProvidedTypes.fs" 
#load "../src/ProvidedTypesTesting.fs"

#else

module TPSDK.GeneratedCodeTests
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

let testProvidedAssembly exprs = 
    let asms = AppDomain.CurrentDomain.GetAssemblies() |> Array.filter (fun x -> not x.IsDynamic) |> Array.map (fun x -> x.Location) 
    let runtimeAssemblyRefs = Array.toList asms
    let runtimeAssembly = asms |> Array.find (fun x -> match IO.Path.GetFileNameWithoutExtension(x).ToLower() with "mscorlib" | "system.runtime" -> true | _ -> false)
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

let check (e : Expr<'a>) expected = 
    e.Raw, fun o -> 
        let actual = Assert.IsType<'a>(o)
        Assert.True((expected = actual), sprintf "%A Expected %A got %A. (%A)" (expected.GetType(), actual.GetType(), expected = actual) expected actual e)

let checkWith (comp : _ -> _ -> bool)(e : Expr<'a>) expected = 
    e.Raw, fun o -> 
        let actual = Assert.IsType<'a>(o)
        Assert.True((comp expected actual), sprintf "%A Expected %A got %A. (%A)" (expected.GetType(), actual.GetType(), (comp expected actual)) expected actual e)
let checkExpr (e : Expr<'a>) = 
    let expected = FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation(e) :?> 'a
    check e expected

[<Fact>]
let ``lambdas``() =
    testProvidedAssembly 
        [
            check 
                <@ 
                    let a = [|0 .. 10|]
                    a |> Array.iteri (fun i x -> a.[i] <- x + 1)
                    a
                @> [|1 .. 11|]
            check 
                <@ 
                    let a = [|0 .. 10|]
                    a |> Array.map (fun x -> x + 1)
                @> [|1 .. 11|]
            check 
                <@
                    let x = ref 0
                    let f a = 
                        x := x.Value + a
                        fun b -> 
                            x := x.Value + b
                            fun c -> 
                                x := x.Value + c
                    let g = f 1
                    let x0 = x.Value
                    let h = g 2
                    let x1 = x.Value
                    h 3
                    x0, x1, x.Value
                @> (1,3,6)
            check 
                <@
                    let f a b = double a + b
                    let g() = 1M
                    f (g()) 123.0
                @> 124.0
            check 
                <@
                    let rec f x = 
                        x + "." |> g
                    and g x =
                        if x.Length = 5 then 
                            x
                        else    
                            x + "|" |> f
                    f ""
                @> ".|.|."
        ]
    
[<Fact(Skip = "Need to replace captured mutables with refs")>]
let ``lambdas - failing``() =
    testProvidedAssembly 
        [
            check 
                <@
                    let mutable x = 0
                    let f a = 
                        x <- x + a
                        fun b -> 
                            x <- x + b
                            fun c -> 
                                x <- x + c
                    let g = f 1
                    let x0 = x
                    let h = g 2
                    let x1 = x
                    h 3
                    x0, x1, x
                @> (1,3,6)
        ]

[<Fact>]
let ``value tuple``() =
    testProvidedAssembly 
        [
            check 
                <@ 
                    let a = struct(2,3)
                    let struct(b,c) = a
                    b + c
                @> 5
            check 
                <@ 
                    struct(1.0,2,3L)
                @> struct(1.0,2,3L)
        ]

[<Fact>]
let ``struct``() =
    testProvidedAssembly 
        [
            check <@ %%(Expr.DefaultValue(typeof<DateTime>)) @> DateTime.MinValue
            checkWith (fun (a : System.Security.Cryptography.DSAParameters) (b : System.Security.Cryptography.DSAParameters) -> a.J = b.J)
                <@
                    let mutable a = System.Security.Cryptography.DSAParameters()
                    a.J <- [|23uy; 60uy|]
                    a
                @> 
                (
                    let mutable a = System.Security.Cryptography.DSAParameters()
                    a.J <- [|23uy; 60uy|]
                    a
                )
            check 
                <@
                    let mutable a = System.Security.Cryptography.DSAParameters()
                    a.J <- [|23uy; 60uy|]
                    a.J
                @> [|23uy; 60uy|]
            
        ]
    
    
#endif