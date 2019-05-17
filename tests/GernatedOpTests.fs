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

[<TypeProvider>]
type GenerativeOpsProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "OpTests"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Container", Some typeof<obj>, isErased = false)

    let create name (expr : Expr<'a>) =  
        ProvidedMethod(name,[],typeof<'a>,invokeCode = (fun _args -> expr.Raw), isStatic = true)
        |> container.AddMember
    do
        create "Sub20 int" <@ 50 - 30 @>
        create "Sub20 decimal" <@ 50m - 30m @>
        create "Sub20 double" <@ 50.0 - 30.0 @>
        create "Sub20 single" <@ 50.f - 30.f @>
        create "Sub1 DateTime" <@ DateTime.Today - DateTime.Today.AddDays(-1.0) @>
        create "Add80 int" <@ 50 + 30 @>
        create "Add80 decimal" <@ 50m + 30m @>
        create "Add80 double" <@ 50.0 + 30.0 @>
        create "Add80 single" <@ 50.f + 30.f @>
        create "Add20000102 DateTime" <@ DateTime(2000,1,1) + TimeSpan.FromDays(1.0) @>
        create "Neg80 int" <@ -80 @>
        create "Neg80 decimal" <@ -80m @>
        create "Neg80 double" <@ -80.0 @>
        create "Neg80 single" <@ -80.f @>
        create "Neg1 TimeSpan" <@ -TimeSpan.FromDays(1.0) @>
        create "Mul10 int" <@ 5*2 @>
        create "Mul10 decimal" <@ 5m * 2m @>
        create "Mul10 double" <@ 5.0 * 2.0 @>
        create "Mul10 single" <@ 5.f * 2.f @>
        create "Div10 int" <@ 20 / 2 @>
        create "Div10 decimal" <@ 20m / 2m @>
        create "Div10 double" <@ 20.0 / 2.0 @>
        create "Div10 single" <@ 20.f / 2.f @>
        create "Div10 int16" <@ 20s / 2s @>
        create "Pos10 int" <@ (~+) 10 @>
        create "Pos10 decimal" <@ (~+) 10m @>
        create "Pos10 double" <@ (~+) 10.0 @>
        create "Pos10 single" <@ (~+) 10.f @>
        create "Rem3 int" <@ 15 % 4 @>
        create "Rem3 decimal" <@ 15m % 4m @>
        create "Rem3 double" <@ 15.0 % 4.0 @>
        create "Rem3 single" <@ 15.f % 4.f@>
        create "Shl2 int" <@ 1 <<< 1 @>
        create "Shl4 uint" <@ 1u <<< 2 @>
        create "int1 int" <@ int 1 @>
        create "int1 double" <@ int 1.0 @>
        create "int1 string" <@ int "1" @>
        //create "Mul10 TimeSpan" <@ TimeSpan.FromDays(5.0) * 2.0 @>
        //create "Mul20 TimeSpan" <@ 2.0 * TimeSpan.FromDays(10.0) @>
        tempAssembly.AddTypes [container]
        this.AddNamespace(container.Namespace, [container])
 
let testCases() = 
    [("F# 3.1 Portable 259", "3.259.3.1", (fun _ ->  Targets.hasPortable259Assemblies()), Targets.Portable259FSharp31Refs)
     ("F# 4.0 Portable 259", "3.259.4.0", (fun _ ->  Targets.hasPortable259Assemblies() && Targets.supportsFSharp40()), Targets.Portable259FSharp40Refs)
     ("F# 3.1 .NET 4.5", "4.3.1.0", (fun _ ->  Targets.supportsFSharp31()), Targets.DotNet45FSharp31Refs)
     ("F# 4.0 .NET 4.5", "4.4.0.0", (fun _ ->  Targets.supportsFSharp40()), Targets.DotNet45FSharp40Refs)
     ("F# 4.1 .NET 4.5", "4.4.1.0", (fun _ ->  true), Targets.DotNet45FSharp41Refs)
     ("F# 4.1 .NET Standard 2.0", "4.4.1.0", (fun _ ->  true), Targets.DotNetStandard20FSharp41Refs)
     ("F# 4.1 .NET CoreApp 2.0", "4.4.1.0", (fun _ ->  true), Targets.DotNetCoreApp20FSharp41Refs) ]

let possibleVersions = 
    [ "3.259.3.1"
      "3.259.4.0"
      "4.3.1.0"
      "4.4.0.0"
      "4.4.1.0"
      "4.4.3.0"
      (typeof<list<int>>.Assembly.GetName().Version.ToString()) ]

let hostedTestCases() = 
    [("4.4.0.0", (fun _ ->  Targets.supportsFSharp40()), Targets.DotNet45FSharp40Refs) ]


let testProvidedAssembly test = 
    if Targets.supportsFSharp40() then
        let runtimeAssemblyRefs = Targets.DotNet45FSharp40Refs()
        let runtimeAssembly = runtimeAssemblyRefs.[0]
        let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
        let tp = GenerativeOpsProvider(cfg) :> TypeProviderForNamespaces
        let providedNamespace = tp.Namespaces.[0] 
        let providedTypes  = providedNamespace.GetTypes()
        let providedType = providedTypes.[0] 
        let providedTypeDefinition = providedType :?> ProvidedTypeDefinition
        Assert.Equal("Container", providedTypeDefinition.Name)

        let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedTypeDefinition.Assembly)
        let assembly = Assembly.Load assemContents
        assembly.ExportedTypes |> Seq.find (fun ty -> ty.Name = "Container") |> test

let runningOnMono = try Type.GetType("Mono.Runtime") <> null with _ -> false 

[<Fact>]
let ``GenerativeOpsProvider sub execute correctly``() =
    testProvidedAssembly <| fun container -> 
        let call name = 
            Assert.IsType<'a>(container.GetMethod(name).Invoke(null,[||]))
        Assert.Equal(20, call "Sub20 int")
        Assert.Equal(20.0, call "Sub20 double")
        Assert.Equal(20.f, call "Sub20 single")
        Assert.Equal(20m, call "Sub20 decimal")
        Assert.Equal(TimeSpan.FromDays 1.0, call "Sub1 DateTime")
        
[<Fact>]
let ``GenerativeOpsProvider add execute correctly``() =
    testProvidedAssembly <| fun container -> 
        let call name = 
            Assert.IsType<'a>(container.GetMethod(name).Invoke(null,[||]))
        Assert.Equal(80, call "Add80 int")
        Assert.Equal(80.0, call "Add80 double")
        Assert.Equal(80.f, call "Add80 single")
        Assert.Equal(80m, call "Add80 decimal")
        Assert.Equal(DateTime(2000,01,02), call "Add20000102 DateTime")
        
[<Fact>]
let ``GenerativeOpsProvider neg execute correctly``() =
    testProvidedAssembly <| fun container -> 
           let call name = 
               Assert.IsType<'a>(container.GetMethod(name).Invoke(null,[||]))
           Assert.Equal(-80, call "Neg80 int")
           Assert.Equal(-80.0, call "Neg80 double")
           Assert.Equal(-80.f, call "Neg80 single")
           Assert.Equal(-80m, call "Neg80 decimal")
           Assert.Equal(-(TimeSpan.FromDays 1.0), call "Neg1 TimeSpan")

[<Fact>]
let ``GenerativeOpsProvider mul execute correctly``() =
    testProvidedAssembly <| fun container -> 
           let call name = 
               Assert.IsType<'a>(container.GetMethod(name).Invoke(null,[||]))
           Assert.Equal(10, call "Mul10 int")
           Assert.Equal(10.0, call "Mul10 double")
           Assert.Equal(10.f, call "Mul10 single")
           Assert.Equal(10m, call "Mul10 decimal")
           
[<Fact>]
let ``GenerativeOpsProvider div execute correctly``() =
    testProvidedAssembly <| fun container -> 
           let call name = 
               Assert.IsType<'a>(container.GetMethod(name).Invoke(null,[||]))
           Assert.Equal(10, call "Div10 int")
           Assert.Equal(10.0, call "Div10 double")
           Assert.Equal(10.f, call "Div10 single")
           Assert.Equal(10m, call "Div10 decimal")
           Assert.Equal(10s, call "Div10 int16")
           
[<Fact>]
let ``GenerativeOpsProvider pos execute correctly``() =
         testProvidedAssembly <| fun container -> 
                let call name = 
                    Assert.IsType<'a>(container.GetMethod(name).Invoke(null,[||]))
                Assert.Equal(10, call "Pos10 int")
                Assert.Equal(10.0, call "Pos10 double")
                Assert.Equal(10.f, call "Pos10 single")
                Assert.Equal(10m, call "Pos10 decimal")

[<Fact>]
let ``GenerativeOpsProvider rem execute correctly``() =
     testProvidedAssembly <| fun container -> 
            let call name = 
                Assert.IsType<'a>(container.GetMethod(name).Invoke(null,[||]))
            Assert.Equal(3, call "Rem3 int")
            Assert.Equal(3.0, call "Rem3 double")
            Assert.Equal(3.f, call "Rem3 single")
            Assert.Equal(3m, call "Rem3 decimal")

[<Fact>]
let ``GenerativeOpsProvider shl execute correctly``() =
     testProvidedAssembly <| fun container -> 
            let call name = 
                Assert.IsType<'a>(container.GetMethod(name).Invoke(null,[||]))
            Assert.Equal(2, call "Shl2 int")
            Assert.Equal(4u, call "Shl4 uint")

[<Fact>]
let ``GenerativeOpsProvider int execute correctly``() =
     testProvidedAssembly <| fun container -> 
            let call name = 
                Assert.IsType<'a>(container.GetMethod(name).Invoke(null,[||]))
            Assert.Equal(1, call "int1 int")
            Assert.Equal(1, call "int1 double")
            Assert.Equal(1, call "int1 string")

#endif