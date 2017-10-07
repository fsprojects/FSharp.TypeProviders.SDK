#if INTERACTIVE
#load "../src/ProvidedTypes.fsi" "../src/ProvidedTypes.fs" "../src/AssemblyReader.fs" "../src/AssemblyReaderReflection.fs" "../src/ProvidedTypesContext.fs" 
#load "../src/ProvidedTypesTesting.fs"

#else
module FSharp.TypeProviders.SDK.Tests.BasicGenerativeTests
#endif

open System.Reflection
open System.IO
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting
open Microsoft.FSharp.Core.CompilerServices
open Xunit

#nowarn "760" // IDisposable needs new

#if !NO_GENERATIVE

[<TypeProvider>]
type GenerativePropertyProviderWithStaticParams (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "StaticProperty.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createType (typeName, n:int) =
        let tmp = Path.ChangeExtension(Path.GetTempFileName(), "dll")
        let myAssem = ProvidedAssembly(tmp)
        let myType = ProvidedTypes.ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, IsErased=false)
        let myProp = ProvidedTypes.ProvidedProperty("MyProperty", typeof<string list>, IsStatic = true, GetterCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>))
        myType.AddMember(myProp)
        myAssem.AddTypes [myType]
        myType

    do
        let myType = ProvidedTypes.ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        let parameters = [ ProvidedTypes.ProvidedStaticParameter("Count", typeof<int>) 
                           ProvidedTypes.ProvidedStaticParameter("Count2", typeof<int>, 3) ]
        myType.DefineStaticParameters(parameters, (fun typeName args -> createType(typeName, (args.[0] :?> int) + (args.[1] :?> int))))

        this.AddNamespace(ns, [myType])



[<Fact>]
let ``GenerativePropertyProviderWithStaticParams generates for .NET 4.5 F# 4.0 correctly``() : unit  = 
  if Targets.supportsFSharp40 then 
    let args = [|  box 3; box 4  |] 
    let runtimeAssembly = Targets.DotNet45FSharp40Refs.[0]
    let runtimeAssemblyRefs = Targets.DotNet45FSharp40Refs
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
    let typeProviderForNamespaces = GenerativePropertyProviderWithStaticParams cfg :> TypeProviderForNamespaces
    let providedTypeDefinition = typeProviderForNamespaces.Namespaces |> Seq.last |> snd |> Seq.last
    let typeName = providedTypeDefinition.Name + (args |> Seq.map (fun s -> ",\"" + (if isNull s then "" else s.ToString()) + "\"") |> Seq.reduce (+))

    let t = providedTypeDefinition.MakeParametricType(typeName, args)
    Assert.True(t.Assembly.FullName.Contains("tmp"))
    let assemContents = (typeProviderForNamespaces :> ITypeProvider).GetGeneratedAssemblyContents(t.Assembly)
    Assert.NotEqual(assemContents.Length, 0)



[<TypeProvider>]
type GenerativePropertyProviderWithStaticParamsUsingProvidedTypesContext (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "StaticProperty.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let ctxt = ProvidedTypesContext.Create(config) 

    let createType (typeName, n:int) =
        let tmp = Path.ChangeExtension(Path.GetTempFileName(), "dll")
        let myGeneratedAssem = ProvidedAssembly(tmp)
        let myGeneratedType = ctxt.ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, IsErased=false)
        let myGeneratedProp = ctxt.ProvidedProperty("MyProperty", typeof<string list>, IsStatic = true, getterCode = (fun args -> <@@ Set.ofList [ "Hello world" ] @@>))
        myGeneratedType.AddMember(myGeneratedProp)
        myGeneratedAssem.AddTypes [myGeneratedType]
        myGeneratedType

    do
        let myType = ctxt.ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        let parameters = [ ctxt.ProvidedStaticParameter("Count", typeof<int>) 
                           ctxt.ProvidedStaticParameter("Count2", typeof<int>, 3) ]
        myType.DefineStaticParameters(parameters, (fun typeName args -> createType(typeName, (args.[0] :?> int) + (args.[1] :?> int))))

        this.AddNamespace(ns, [myType])



(*
 // Generative providers don't yet work with ProvidedTypesContext.
 // The problem is that the ProvidedTypesContext creates artificial types for things like mscorlib Object
 // System.Refelction.Emit can't cope with the existence of such types -  System.Refelction.Emit can only
 // write tpes that refer to the runtime loaded mscorlib.
 //
 // An alternative may be to use the Abstract IL writer and code generator.  Or Mono Cecil.

[<Fact>]
let ``GenerativePropertyProviderWithStaticParams using ProvidedTypesContext generates for .NET 4.5 F# 4.0 correctly``() : unit  = 
  if Targets.supportsFSharp40 then 
    let args = [|  box 3; box 4  |] 
    let runtimeAssembly = Targets.DotNet45FSharp40Refs.[0]
    let runtimeAssemblyRefs = Targets.DotNet45FSharp40Refs
    let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
    let typeProviderForNamespaces = GenerativePropertyProviderWithStaticParamsUsingProvidedTypesContext cfg :> TypeProviderForNamespaces
    let providedTypeDefinition = typeProviderForNamespaces.Namespaces |> Seq.last |> snd |> Seq.last
    let typeName = providedTypeDefinition.Name + (args |> Seq.map (fun s -> ",\"" + (if isNull s then "" else s.ToString()) + "\"") |> Seq.reduce (+))

    let t = providedTypeDefinition.MakeParametricType(typeName, args)
    Assert.True(t.Assembly.FullName.Contains("tmp"))
    let assemContents = (typeProviderForNamespaces :> ITypeProvider).GetGeneratedAssemblyContents(t.Assembly)
    Assert.NotEqual(assemContents.Length, 0)
*)

#endif
