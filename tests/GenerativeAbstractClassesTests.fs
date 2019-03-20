#if INTERACTIVE
#load "../src/ProvidedTypes.fsi" "../src/ProvidedTypes.fs" 
#load "../src/ProvidedTypesTesting.fs"

#else

module FSharp.TypeProviders.SDK.Tests.GenerativeAbstractClassesTests
#endif

#nowarn "760" // IDisposable needs new

#if !NO_GENERATIVE

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting

[<TypeProvider>]
type GenerativeAbstractClassesProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "AbstractClasses.Provided"
    let tempAssembly = ProvidedAssembly()
    let container = ProvidedTypeDefinition(tempAssembly, ns, "Contracts", Some typeof<obj>, isErased = false)

    let createAbstractClass name (members: (string * (string * Type) list * Type * bool) list) =
        let t = ProvidedTypeDefinition(name, Some typeof<obj>, hideObjectMethods = true, isErased = false)
        t.SetAttributes(TypeAttributes.AutoLayout ||| TypeAttributes.AnsiClass ||| TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Serializable ||| TypeAttributes.BeforeFieldInit)
        
        members
        |> List.map (fun (name, parameters, retType, isVirtual) ->
            let ps =
                parameters
                |> List.map (fun (name, ty) ->
                    ProvidedParameter(name, ty))
            if isVirtual then
                let m = ProvidedMethod(name, ps, retType, invokeCode = fun args ->
                    <@ raise (NotImplementedException(name + " is not implemented")) @>.Raw
                    )
                m.SetMethodAttrs (MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig ||| MethodAttributes.VtableLayoutMask ||| MethodAttributes.HasSecurity)
                m
            else
                let m = ProvidedMethod(name, ps, retType)
                m.SetMethodAttrs (MethodAttributes.PrivateScope ||| MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig ||| MethodAttributes.VtableLayoutMask ||| MethodAttributes.Abstract)
                m
            )
        |> t.AddMembers
        
        t

    do
        let members = [ "GetString", [], typeof<string>, false
                        "Sum", [("x", typeof<int>); ("y", typeof<int>)], typeof<int>, false ]
        let contract = createAbstractClass "Contract" members
        container.AddMember contract

        let members = [ "GetString", [], typeof<string>, true
                        "Sum", [("x", typeof<int>); ("y", typeof<int>)], typeof<int>, true ]
        let virtualContract = createAbstractClass "VirtualContract" members
        container.AddMember virtualContract

        tempAssembly.AddTypes [container]
        this.AddNamespace(container.Namespace, [container])

let testProvidedAssembly test = 
    if Targets.supportsFSharp40() then
        let runtimeAssemblyRefs = Targets.DotNet45FSharp40Refs()
        let runtimeAssembly = runtimeAssemblyRefs.[0]
        let cfg = Testing.MakeSimulatedTypeProviderConfig (__SOURCE_DIRECTORY__, runtimeAssembly, runtimeAssemblyRefs) 
        let tp = GenerativeAbstractClassesProvider(cfg) :> TypeProviderForNamespaces
        let providedNamespace = tp.Namespaces.[0] 
        let providedTypes  = providedNamespace.GetTypes()
        let providedType = providedTypes.[0] 
        let providedTypeDefinition = providedType :?> ProvidedTypeDefinition
        Assert.Equal("Contracts", providedTypeDefinition.Name)

        let assemContents = (tp :> ITypeProvider).GetGeneratedAssemblyContents(providedTypeDefinition.Assembly)
        let assembly = Assembly.Load assemContents
        assembly.ExportedTypes |> Seq.find (fun ty -> ty.Name = "Contracts") |> test

let runningOnMono = try Type.GetType("Mono.Runtime") <> null with _ -> false 

[<Fact>]
let ``Abstract classes are generated correctly``() =
  // // See tracking bug https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/211 
  // if not runningOnMono then 
    testProvidedAssembly <| fun container -> 
        let contract = container.GetNestedType "Contract"
        Assert.NotNull contract

        let virtualContract = container.GetNestedType "VirtualContract"
        Assert.NotNull virtualContract
#endif