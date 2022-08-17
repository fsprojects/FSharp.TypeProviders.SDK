module TPSDK.ReferencedAssemblies

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Xunit
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting

[<Fact>]
// See https://github.com/dotnet/fsharp/issues/13710
let ``test incremental target assemblies are available via simulated tcImports additions``() =
    let refs = Targets.DotNetStandard20FSharpRefs()
    let config, tcImports = Testing.MakeSimulatedTypeProviderConfigIncremental (resolutionFolder=__SOURCE_DIRECTORY__, runtimeAssembly="whatever.dll", runtimeAssemblyRefs=refs)
    use tp1 = new TypeProviderForNamespaces(config)
    let ctxt1 = tp1.TargetContext
    let ns = "TestNamespace"
    let thisAssembly = Assembly.GetExecutingAssembly()
    let myType = ProvidedTypeDefinition(thisAssembly, ns, "TestType", Some typeof<obj>)
    tp1.AddNamespace(ns, [myType] )

    // The initial assembly references should include at least netstandard
    printfn "finding netstandard in targets..."
    Assert.True(ctxt1.GetTargetAssemblies() |> Array.exists (fun a -> a.GetName().Name = "netstandard"))

    // We add a dummy assembly as an additional reference (we use the current assembly for no particular reason)
    tcImports.AddReferencedDlls [ thisAssembly.Location ]

    // We now evaluate the static parameters, indicating the type provider actually starts to get used
    // This causes the type provider to do a one-off re-evaluation of the available referenced DLLs
    let ty = tp1.Namespaces.[0].GetTypes().[0]
    let tp2 = (tp1 :> ITypeProvider)
    let _dummy = tp2.GetStaticParameters(ty)

    // After this, the asembly is available in the target assemblies
    printfn "finding thisAssembly in targets..."
    Assert.True(ctxt1.GetTargetAssemblies() |> Array.exists (fun a -> a.GetName().Name = thisAssembly.GetName().Name))

