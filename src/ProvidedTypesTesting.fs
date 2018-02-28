// --------------------------------------------------------------------------------------
// Helpers for writing type providers
// ----------------------------------------------------------------------------------------------

namespace ProviderImplementation.ProvidedTypesTesting

open System
open System.Collections.Generic
open System.Reflection
open System.IO
open System.Text
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.Printf
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Reflection
open ProviderImplementation.ProvidedTypes

[<AutoOpen>]
module Utils = 
    let isNull x = match x with null -> true | _ -> false


/// Simulate a real host of TypeProviderConfig
type internal DllInfo(path: string) =
    member __.FileName = path

/// Simulate a real host of TypeProviderConfig
type internal TcImports(bas: TcImports option, dllInfos: DllInfo list) =
    member __.Base = bas
    member __.DllInfos = dllInfos


type internal Testing() =

    /// Simulates a real instance of TypeProviderConfig
    static member MakeSimulatedTypeProviderConfig (resolutionFolder: string, runtimeAssembly: string, runtimeAssemblyRefs: string list, ?isHostedExecution, ?isInvalidationSupported) =

        let cfg = TypeProviderConfig(fun _ -> false)
        cfg.IsHostedExecution <- defaultArg isHostedExecution false
        cfg.IsInvalidationSupported <- defaultArg isInvalidationSupported true
        let (?<-) cfg prop value =
            let ty = cfg.GetType()
            match ty.GetProperty(prop,BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic) with
            | null -> 
                let fld = ty.GetField(prop,BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
                if fld = null then failwith ("expected TypeProviderConfig to have a property or field "+prop)
                fld.SetValue(cfg, value)|> ignore
            | p -> 
                p.GetSetMethod(nonPublic = true).Invoke(cfg, [| box value |]) |> ignore
        cfg?ResolutionFolder <- resolutionFolder
        cfg?RuntimeAssembly <- runtimeAssembly
        cfg?ReferencedAssemblies <- Array.zeroCreate<string> 0

        // Fake an implementation of SystemRuntimeContainsType the shape expected by AssemblyResolver.fs.
        let dllInfos = [yield DllInfo(runtimeAssembly); for r in runtimeAssemblyRefs do yield DllInfo(r)]
        let tcImports = TcImports(Some(TcImports(None,[])),dllInfos)
        let systemRuntimeContainsType = (fun (_s:string) -> if tcImports.DllInfos.Length = 1 then true else true)
        cfg?systemRuntimeContainsType <- systemRuntimeContainsType

        //Diagnostics.Debugger.Launch() |> ignore
        Diagnostics.Debug.Assert(cfg.GetType().GetField("systemRuntimeContainsType",BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) |> isNull |> not)
        Diagnostics.Debug.Assert(systemRuntimeContainsType.GetType().GetField("tcImports",BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) |> isNull |> not)
        Diagnostics.Debug.Assert(typeof<TcImports>.GetField("dllInfos",BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) |> isNull |> not)
        Diagnostics.Debug.Assert(typeof<TcImports>.GetProperty("Base",BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) |> isNull |> not)
        Diagnostics.Debug.Assert(typeof<DllInfo>.GetProperty("FileName",BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) |> isNull |> not)

        cfg

    /// Simulates a real instance of TypeProviderConfig and then creates an instance of the last
    /// type provider added to a namespace by the type provider constructor
    static member GenerateProvidedTypeInstantiation (resolutionFolder: string, runtimeAssembly: string, runtimeAssemblyRefs: string list, typeProviderForNamespacesConstructor, args: obj[]) =
        let cfg = Testing.MakeSimulatedTypeProviderConfig (resolutionFolder, runtimeAssembly, runtimeAssemblyRefs)

        let tp = typeProviderForNamespacesConstructor cfg :> TypeProviderForNamespaces

        let providedNamespace = tp.Namespaces.[0] 
        let providedTypes  = providedNamespace.GetTypes()
        let providedType = 
            providedTypes 
            |> Array.tryFind (fun ty -> (tp :> ITypeProvider).GetStaticParameters(ty).Length = args.Length ) 
            |> function None -> failwithf "couldn't find a type in type provider with %d static args " args.Length | Some ty -> ty
        let providedTypeDefinition = providedType :?> ProvidedTypeDefinition

        match args with
        | [||] -> providedTypeDefinition
        | args ->
            let typeName =
                if providedTypeDefinition.IsErased then
                    providedTypeDefinition.Name + (args |> Seq.map (fun s -> ",\"" + (if s = null then "" else s.ToString()) + "\"") |> Seq.reduce (+))
                else
                    // The type name ends up quite mangled in the dll output if we combine the name using static parameters, so for generated types we don't do that
                    providedTypeDefinition.Name
            providedTypeDefinition.ApplyStaticArguments(typeName, args)

    /// Returns a string representation of the signature (and optionally also the body) of all the
    /// types generated by the type provider up to a certain depth and width
    /// If ignoreOutput is true, this will still visit the full graph, but it will output an empty string to be faster
    static member FormatProvidedType (t: ProvidedTypeDefinition, ?signatureOnly, ?ignoreOutput, ?maxDepth, ?maxWidth, ?useQualifiedNames) =

        let signatureOnly = defaultArg signatureOnly false
        let ignoreOutput = defaultArg ignoreOutput false
        let maxDepth = defaultArg maxDepth 10
        let maxWidth = defaultArg maxWidth 100
        let useQualifiedNames = defaultArg useQualifiedNames false

        let knownNamespaces =
            [ t.Namespace
              "Microsoft.FSharp.Core"
              "Microsoft.FSharp.Core.Operators"
              "Microsoft.FSharp.Collections"
              "Microsoft.FSharp.Control"
              "Microsoft.FSharp.Text" ]
            |> Set.ofSeq

        let pending = new Queue<_>()
        let visited = new HashSet<_>()

        let add t =
            if visited.Add t then
                pending.Enqueue t

        let fullName (t: Type) =
            let fullName =
                if useQualifiedNames && not (t :? ProvidedTypeDefinition) then
                    t.AssemblyQualifiedName
                else t.Namespace + "." + t.Name
            if fullName.StartsWith "FSI_" then
                fullName.Substring(fullName.IndexOf('.') + 1)
            else
                fullName

        let rec toString useFullName (t: Type) =

            let hasUnitOfMeasure = (match t with :? ProvidedTypeSymbol as p -> p.IsFSharpUnitAnnotated | _ -> false) 

            let innerToString (t: Type) =
                match t with
                | _ when t.Name = typeof<bool>.Name && not hasUnitOfMeasure -> "bool"
                | _ when t.Name = typeof<obj>.Name && not hasUnitOfMeasure  -> "obj"
                | _ when t.Name = typeof<int>.Name && not hasUnitOfMeasure  -> "int"
                | _ when t.Name = typeof<int64>.Name && not hasUnitOfMeasure  -> "int64"
                | _ when t.Name = typeof<float>.Name && not hasUnitOfMeasure  -> "float"
                | _ when t.Name = typeof<float32>.Name && not hasUnitOfMeasure  -> "float32"
                | _ when t.Name = typeof<decimal>.Name && not hasUnitOfMeasure  -> "decimal"
                | _ when t.Name = typeof<string>.Name && not hasUnitOfMeasure  -> "string"
                | _ when t.Name = typeof<Void>.Name -> "()"
                | _ when t.Name = typeof<unit>.Name -> "()"
                | t when t.IsArray -> (t.GetElementType() |> toString useFullName) + "[]"
                | :? ProvidedTypeDefinition as t ->
                    add t
                    t.Name.Split(',').[0]
                | t when t.IsGenericType ->
                    let args =
                        if useFullName then
                            t.GetGenericArguments()
                            |> Seq.map (if hasUnitOfMeasure then (fun t -> t.Name) else toString useFullName)
                        else
                            t.GetGenericArguments()
                            |> Seq.map (fun _ -> "_")
                    if t.FullName.StartsWith "System.Tuple`" then
                        String.concat " * " args
                    elif t.Name.StartsWith "FSharpFunc`" then
                        "(" + (String.concat " -> " args) + ")"
                    else
                        let args = String.concat "," args
                        let name, reverse =
                            match t with
                            | t when hasUnitOfMeasure -> toString useFullName t.UnderlyingSystemType, false
                            // Short names for some known generic types
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<int seq>.GetGenericTypeDefinition().Name -> "seq", true
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<int list>.GetGenericTypeDefinition().Name -> "list", true
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<int option>.GetGenericTypeDefinition().Name -> "option", true
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<int ref>.GetGenericTypeDefinition().Name -> "ref", true
                            | t when not useQualifiedNames && t.Name = "FSharpAsync`1" -> "async", true
                            // Short names for types in F# namespaces
                            | t when not useQualifiedNames && knownNamespaces.Contains t.Namespace -> t.Name, false
                            | t -> (if useFullName then fullName t else t.Name), false
                        let name = name.Split('`').[0]
                        if reverse then
                            args + " " + name
                        else
                            name + "<" + args + ">"
                // Short names for types in F# namespaces
                | t when not useQualifiedNames && knownNamespaces.Contains t.Namespace -> t.Name
                // Short names for generic parameters
                | t when t.IsGenericParameter -> t.Name
                | t -> if useFullName then fullName t else t.Name

            let rec warnIfWrongAssembly (t:Type) =
                match t with
                | :? ProvidedTypeDefinition -> ""
                | t when t.IsGenericType -> defaultArg (t.GetGenericArguments() |> Seq.map warnIfWrongAssembly |> Seq.tryFind (fun s -> s <> "")) ""
                | t when t.IsArray -> warnIfWrongAssembly <| t.GetElementType()
                | t -> if not t.IsGenericParameter && t.Assembly = Assembly.GetExecutingAssembly() then " [DESIGNTIME]" else ""

            if ignoreOutput then
                ""
            elif hasUnitOfMeasure || t.IsGenericParameter || t.DeclaringType = null then
                innerToString t + (warnIfWrongAssembly t)
            else
                (toString useFullName t.DeclaringType) + "+" + (innerToString t) + (warnIfWrongAssembly t)

        let toSignature (parameters: ParameterInfo[]) =
            if parameters.Length = 0 then
                "()"
            else
                parameters
                |> Seq.map (fun p -> p.Name + ":" + (toString true p.ParameterType))
                |> String.concat " -> "

        let printExpr expr =

            let sb = StringBuilder ()
            let print (str:string) = sb.Append(str) |> ignore

            let getCurrentIndent() =
                let lastEnterPos = sb.ToString().LastIndexOf('\n')
                if lastEnterPos = -1 then sb.Length + 4 else sb.Length - lastEnterPos - 1

            let breakLine indent =
                print "\n"
                print (String(' ', indent))

            let isBigExpression = function
            | Let _ | NewArray _ | NewTuple _ -> true
            | _ -> false

            let inline getAttrs attrName m =
                ( ^a : (member GetCustomAttributesData : unit -> IList<CustomAttributeData>) m)
                |> Seq.filter (fun attr -> attr.Constructor.DeclaringType.Name = attrName)

            let inline hasAttr attrName m =
                not (Seq.isEmpty (getAttrs attrName m))

            let rec printSeparatedByCommas exprs =
                match exprs with
                | [] -> ()
                | e::es ->
                    printExpr false true e
                    for e in es do
                        print ", "
                        printExpr false true e

            and printCall fromPipe printName (mi:MethodInfo) args =
                //eprintfn "printCall: %s" mi.Name
                if fromPipe && List.length args = 1 then
                    printName()
                elif not (hasAttr "CompilationArgumentCountsAttribute" mi) then
                    printName()
                    match args with
                    | [] -> print "()"
                    | arg::args ->
                        print "("
                        let indent = getCurrentIndent()
                        printExpr false true arg
                        for arg in args do
                            print ", "
                            if isBigExpression arg then
                                breakLine indent
                            printExpr false true arg
                        print ")"
                else
                    print "("
                    printName()
                    for arg in args do
                      print " "
                      printExpr false true arg
                    print ")"

            and printExpr fromPipe needsParens = function
                | Call (instance, mi, args) ->
                    if mi.Name = "GetArray" && mi.DeclaringType.FullName = "Microsoft.FSharp.Core.LanguagePrimitives+IntrinsicFunctions" then
                        printExpr false true args.Head
                        print ".["
                        printExpr false true args.Tail.Head
                        print "]"
                    elif mi.DeclaringType.IsGenericType && mi.DeclaringType.GetGenericTypeDefinition().Name = typeof<int option>.GetGenericTypeDefinition().Name then
                        if args.IsEmpty then
                            match instance with
                            | None -> print "None"
                            | Some instance ->
                                printExpr false true instance
                                print "."
                                print <| mi.Name.Substring("get_".Length)
                        else
                          print "Some "
                          printExpr false true args.Head
                    elif mi.Name.Contains "." && not args.IsEmpty then
                        // instance method in type extension
                        let printName() =
                            printExpr false true args.Head
                            print "."
                            print (mi.Name.Substring(mi.Name.IndexOf '.' + 1))
                        printCall fromPipe printName mi args.Tail
                    elif mi.Attributes &&& MethodAttributes.SpecialName = MethodAttributes.SpecialName && mi.Name.StartsWith "get_" && args.IsEmpty then
                        // property get
                        match instance with
                        | Some expr -> printExpr false true expr
                        | None -> print (toString false mi.DeclaringType)
                        print "."
                        print <| mi.Name.Substring("get_".Length)
                    elif mi.Name = "op_PipeRight" && args.Length = 2 then
                        printExpr false false args.Head
                        print " |> "
                        match args.Tail.Head with
                        | Lambda (_, (Call(_,_,_) as call)) -> printExpr true false call
                        | expr -> printExpr false false expr
                    else
                        let printName() =
                            match instance with
                            | Some expr -> printExpr false true expr
                            | None -> print (toString false mi.DeclaringType)
                            print "."
                            print mi.Name
                        let isOptional (arg:Expr, param:ParameterInfo) =
                            hasAttr "OptionalArgumentAttribute" param
                            && arg.ToString() = "Call (None, get_None, [])"
                        let args =
                            mi.GetParameters()
                            |> List.ofArray
                            |> List.zip args
                            |> List.filter (not << isOptional)
                            |> List.map fst
                        printCall fromPipe printName mi args
                | Let (var1, TupleGet (Var x, 1), Let (var2, TupleGet (Var y, 0), body)) when x = y ->
                    let indent = getCurrentIndent()
                    bprintf sb "let %s, %s = %s" var2.Name var1.Name x.Name
                    breakLine indent
                    printExpr false false body
                | Let (var, value, body) ->
                    let indent = getCurrentIndent()
                    let usePattern = sprintf "IfThenElse(TypeTest(IDisposable,Coerce(%s,Object)),Call(Some(Call(None,UnboxGeneric,[Coerce(%s,Object)])),Dispose,[]),Value(<null>))" var.Name var.Name
                    let body =
                        match body with
                        | TryFinally (tryExpr, finallyExpr) when finallyExpr.ToString().Replace("\n", null).Replace(" ", null) = usePattern ->
                            bprintf sb "use %s = " var.Name
                            tryExpr
                        | _ ->
                            if var.IsMutable then
                                bprintf sb "let mutable %s = " var.Name
                            else
                                bprintf sb "let %s = " var.Name
                            body
                    match value with
                    | Let _ ->
                        breakLine (indent + 4)
                        printExpr false false value
                    | _ -> printExpr false false value
                    breakLine indent
                    printExpr false false body
                | Value (null, _) ->
                    print "null"
                | Value (value, typ) when typ = typeof<string> && (value :?> string).Contains("\\") ->
                    bprintf sb "@%A" value
                | Value (value, _) ->
                    bprintf sb "%A" value
                | Var (var) ->
                    print var.Name
                | NewObject (ci, args) ->
                    let getSourceConstructFlags (attr:CustomAttributeData) =
                        let arg = attr.ConstructorArguments
                                  |> Seq.filter (fun arg -> arg.ArgumentType.Name = "SourceConstructFlags")
                                  |> Seq.head
                        arg.Value :?> int
                    let compilationMappings = getAttrs "CompilationMappingAttribute" ci.DeclaringType
                    if not (Seq.isEmpty compilationMappings) && (getSourceConstructFlags (Seq.head compilationMappings)) = int SourceConstructFlags.RecordType then
                        print "{ "
                        let indent = getCurrentIndent()
                        let recordFields = FSharpType.GetRecordFields(ci.DeclaringType)
                        args |> List.iteri (fun i arg ->
                            if i > 0 then
                                breakLine indent
                            print recordFields.[i].Name
                            print " = "
                            printExpr false false arg)
                        print " }"
                    else
                        print "(new "
                        print (toString false ci.DeclaringType)
                        print "("
                        printSeparatedByCommas args
                        print "))"
                | NewDelegate (typ, vars, expr) ->
                    print "new "
                    print (toString false typ)
                    match expr with
                    | Var v when not vars.IsEmpty && vars.Tail.IsEmpty && vars.Head = v -> print "(id)"
                    | _ ->
                        let indent = getCurrentIndent()
                        if vars.IsEmpty then
                            print "(fun () -> "
                        else
                            print "(fun"
                            for var in vars do
                                bprintf sb " (%s:%s)" var.Name (toString false var.Type)
                            print " -> "
                        if isBigExpression expr then
                            breakLine (indent + 4)
                            printExpr false false expr
                        else
                            printExpr false false expr
                    print ")"
                | NewTuple (exprs) ->
                    if needsParens then print "("
                    let indent = getCurrentIndent()
                    printExpr false true exprs.Head
                    for e in exprs.Tail do
                        print ","
                        breakLine indent
                        printExpr false true e
                    if needsParens then print ")"
                | NewArray (_, exprs) ->
                    if exprs.Length = 0 then print "[| |]"
                    else
                        print "[| "
                        let indent = getCurrentIndent()
                        printExpr false true exprs.Head
                        for e in exprs.Tail do
                            breakLine indent
                            printExpr false true e
                        print " |]"
                | Coerce (expr, typ) ->
                    print "("
                    printExpr false false expr
                    print " :> "
                    print (toString false typ)
                    print ")"
                | TupleGet (expr, index) ->
                    print "(let "
                    let rec getTupleLength (typ:Type) =
                        let length = typ.GetGenericArguments().Length
                        if length = 0 then // happens in the Apiary provider
                            let typeNameSuffix = typ.Name.Substring(typ.Name.IndexOf('`') + 1)
                            typeNameSuffix.Substring(0, typeNameSuffix.IndexOf('[')) |> Int32.Parse
                        else
                            let lastItem = typ.GetGenericArguments() |> Seq.last
                            if lastItem.Name.StartsWith "Tuple`"
                            then length + getTupleLength lastItem - 1
                            else length
                    let tupleLength = getTupleLength expr.Type
                    let varName = "t" + (string (index + 1))
                    for i in 0..tupleLength-1 do
                        if i = index then
                            print varName
                        else
                            print "_"
                        if i <> tupleLength-1 then
                            print ","
                    print " = "
                    printExpr false false expr
                    print (" in " + varName + ")")
                | expr -> print (expr.ToString())

            printExpr false false expr
            sb.ToString()

        let sb = StringBuilder ()

        let print (str: string) =
            if not ignoreOutput then
                sb.Append(str) |> ignore

        let println() =
            if not ignoreOutput then
                sb.AppendLine() |> ignore

        let printMember (memberInfo: MemberInfo) =

            let print str =
                print "    "
                print str
                println()

            let getMethodBody (m: ProvidedMethod) =
                let vs = 
                    [ if not m.IsStatic then yield ("this", ProvidedTypeDefinition.EraseType m.DeclaringType)
                      for p in m.GetParameters() do yield (p.Name, ProvidedTypeDefinition.EraseType p.ParameterType) ]
                    |> List.map (Var.Global >> Expr.Var)
                m.GetInvokeCode  vs

            let getConstructorBody (c: ProvidedConstructor) =
                let vs = 
                    [ for p in c.GetParameters() do yield (p.Name, ProvidedTypeDefinition.EraseType p.ParameterType) ]
                    |> List.map (Var.Global >> Expr.Var)
                c.GetInvokeCode vs

            let printExpr x =
                if not ignoreOutput then
                    let rec removeParams x =
                      match x with
                      | Let (_, Value(null, _), body) -> removeParams body
                      | _ -> x
                    let formattedExpr = printExpr (removeParams x)
                    print formattedExpr
                    println()

            let printObj x =
                if ignoreOutput then
                    ""
                else
                    sprintf "\n%O\n" x

            let getName (m:MemberInfo) =
                if memberInfo.Name.Contains(" ") then
                    "``" + m.Name + "``"
                else
                    m.Name

            match memberInfo with

            | :? ProvidedConstructor as cons ->
                if not ignoreOutput then
                    print <| "new : " +
                             (toSignature <| cons.GetParameters()) + " -> " +
                             (toString true memberInfo.DeclaringType)
                if not signatureOnly then
                    cons |> getConstructorBody |> printExpr

            | :? ProvidedField as field ->
                let value =
                    if signatureOnly then ""
                    else field.GetRawConstantValue() |> printObj
                if not ignoreOutput then
                    print <| "val " + (getName field) + ": " +
                             (toString true field.FieldType) +
                             value

            | :? ProvidedProperty as prop ->
                if not ignoreOutput then
                    print <| (if prop.IsStatic then "static " else "") + "member " +
                             (getName prop) + ": " + (toString true prop.PropertyType) +
                             " with " + (if prop.CanRead && prop.CanWrite then "get, set" else if prop.CanRead then "get" else "set")
                if not signatureOnly then
                    if prop.CanRead then
                        getMethodBody (prop.GetGetMethod() :?> ProvidedMethod) |> printExpr
                    if prop.CanWrite then
                        getMethodBody (prop.GetSetMethod() :?> ProvidedMethod) |> printExpr

            | :? ProvidedMethod as m ->
                if m.Attributes &&& MethodAttributes.SpecialName <> MethodAttributes.SpecialName then
                    if not ignoreOutput then
                        print <| (if m.IsStatic then "static " else "") + "member " +
                        (getName m) + ": " + (toSignature <| m.GetParameters()) +
                        " -> " + (toString true m.ReturnType)
                    if not signatureOnly then
                        m |> getMethodBody |> printExpr

            | _ -> ()

        add t

        let currentDepth = ref 0

        while pending.Count <> 0 && !currentDepth <= maxDepth do
            let pendingForThisDepth = new List<_>(pending)
            pending.Clear()
            let pendingForThisDepth =
                pendingForThisDepth
                |> Seq.sortBy (fun m -> m.Name)
                |> Seq.truncate maxWidth
            for t in pendingForThisDepth do
                //Disabled because not working on Mono
                //for attr in t.GetCustomAttributesData() do
                //     print <| (sprintf "[<%A>]" attr).Replace("Microsoft.FSharp.Core.", null).Replace("CompilerServices.", null).Replace("Attribute(", "(")
                //     println()
                match t with
                | t when FSharpType.IsRecord t-> "record "
                | t when FSharpType.IsModule t -> "module "
                | t when t.IsValueType -> "struct "
                | t when t.IsClass && t.IsSealed && t.IsAbstract -> "static class "
                | t when t.IsClass && t.IsAbstract -> "abstract class "
                | t when t.IsClass -> "class "
                | _ -> ""
                |> print
                print (toString true t)
                let bt = if isNull t.BaseType then typeof<obj> else t.BaseType
                print " : "
                print (toString true bt)
                println()
                t.GetMembers(BindingFlags.DeclaredOnly ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public)
                |> Seq.sortBy (fun m -> m.Name)
                |> Seq.iter printMember
                println()
            currentDepth := !currentDepth + 1

        sb.ToString()


module internal Targets =

    let private (++) a b = System.IO.Path.Combine(a,b)

    let runningOnMono = Type.GetType("Mono.Runtime") |> isNull |> not

    let runningOnMac =
        (Environment.OSVersion.Platform = PlatformID.MacOSX)
        || (Environment.OSVersion.Platform = PlatformID.Unix) && Directory.Exists("/Applications") && Directory.Exists("/System") && Directory.Exists("/Users") && Directory.Exists("/Volumes")

    let runningOnLinux =
        (Environment.OSVersion.Platform = PlatformID.Unix) && not runningOnMac

    let runningOnWindows = 
        match System.Environment.OSVersion.Platform with
        | System.PlatformID.Win32NT -> true
        | _ -> false

    /// When compiling .NET 4.5 code on Linux, use the Mono reference assemblies, even when running with the .NET Core toolchain
    let monoRoot() =
        let tryDir dir = if (try Directory.Exists dir with _ -> false) then Some dir else None
        match tryDir "/Library/Frameworks/Mono.framework/Versions/Current/lib/mono" with 
        | Some d -> d
        | None -> 
        match tryDir "/usr/lib/mono" with 
        | Some d -> d
        | None -> 
        match tryDir "/usr/local/lib/mono" with 
        | Some d -> d
        | None -> "/usr/lib/mono"

    let installedReferenceAssembliesRootOnWindows() = Environment.GetFolderPath Environment.SpecialFolder.ProgramFilesX86 ++ "Reference Assemblies" ++ "Microsoft"
    let fsharpInstalledAssembliesRoot() = if runningOnWindows then installedReferenceAssembliesRootOnWindows() ++ "FSharp" else monoRoot() ++ "fsharp" ++ "api" 
    let installedPortableAssembliesRoot() = (if runningOnWindows then installedReferenceAssembliesRootOnWindows() ++ "Framework" else monoRoot() ++ "xbuild-frameworks") ++ ".NETPortable"  
    let installedNet45AssembliesRoot() = if runningOnWindows then installedReferenceAssembliesRootOnWindows() ++ "Framework" ++ ".NETFramework" ++ "v4.5" else monoRoot() ++ "4.5-api"
    
    let packagesDirectory() = 
        // this takes into account both linux-on-windows (can't use __SOURCE_DIRECTORY__) and shadow copying (can't use .Location)
        let root = Path.GetDirectoryName(Uri(Assembly.GetExecutingAssembly().CodeBase).LocalPath)
        let rec loop dir = 
             //printfn "looking for references in %s" dir
             if Directory.Exists(dir ++ "packages" ) then 
                 dir ++ "packages" 
             else
                 let parent = Path.GetDirectoryName(dir)
                 match parent with
                 | null | "" -> failwith ("couldn't find packages directory anywhere above  " + root)
                 | _ ->  loop parent
                 
        loop  root

    let paketPackagesGroupDirectory paketGroup = 
        let pd = packagesDirectory()
        if Directory.Exists (pd ++ paketGroup) then 
            Some (pd ++ paketGroup)
        else 
            None


    let private fsharpCoreFromInstalledAssemblies fsharp profile =
         match fsharp, profile with
         | "3.1", "net45" -> fsharpInstalledAssembliesRoot() ++  ".NETFramework" ++ "v4.0" ++ "4.3.1.0" ++ "FSharp.Core.dll" |> Some
         | "4.0", "net45" -> fsharpInstalledAssembliesRoot() ++  ".NETFramework" ++ "v4.0" ++ "4.4.0.0" ++ "FSharp.Core.dll" |> Some
         | "4.1", "net45" -> fsharpInstalledAssembliesRoot() ++  ".NETFramework" ++ "v4.0" ++ "4.4.1.0" ++ "FSharp.Core.dll" |> Some
         | "3.1", "portable47" -> fsharpInstalledAssembliesRoot() ++ ".NETPortable" ++ "2.3.5.1" ++ "FSharp.Core.dll" |> Some
         | "3.1", "portable7" -> fsharpInstalledAssembliesRoot() ++ ".NETCore" ++ "3.3.1.0" ++ "FSharp.Core.dll" |> Some
         | "3.1", "portable78" -> fsharpInstalledAssembliesRoot() ++ ".NETCore" ++ "3.78.3.1" ++ "FSharp.Core.dll" |> Some
         | "3.1", "portable259" -> fsharpInstalledAssembliesRoot() ++ ".NETCore" ++ "3.259.3.1" ++ "FSharp.Core.dll" |> Some
         | "4.0", "portable47" -> fsharpInstalledAssembliesRoot() ++ ".NETPortable" ++ "3.47.4.0" ++ "FSharp.Core.dll" |> Some
         | "4.0", "portable7" -> fsharpInstalledAssembliesRoot() ++ ".NETCore" ++ "3.7.4.0" ++ "FSharp.Core.dll" |> Some
         | "4.0", "portable78" -> fsharpInstalledAssembliesRoot() ++ ".NETCore" ++ "3.78.4.0" ++ "FSharp.Core.dll" |> Some
         | "4.0", "portable259" -> fsharpInstalledAssembliesRoot() ++ ".NETCore" ++ "3.259.4.0" ++ "FSharp.Core.dll" |> Some
         | "4.1", "portable47" -> fsharpInstalledAssembliesRoot() ++ ".NETPortable" ++ "3.47.4.1" ++ "FSharp.Core.dll" |> Some
         | "4.1", "portable7" -> fsharpInstalledAssembliesRoot() ++ ".NETCore" ++ "3.7.4.1" ++ "FSharp.Core.dll" |> Some
         | "4.1", "portable78" -> fsharpInstalledAssembliesRoot() ++ ".NETCore" ++ "3.78.4.1" ++ "FSharp.Core.dll" |> Some
         | "4.1", "portable259" -> fsharpInstalledAssembliesRoot() ++ ".NETCore" ++ "3.259.4.1" ++ "FSharp.Core.dll" |> Some
         | "4.1", "netstandard1.6" -> None
         | "4.1", "netstandard2.0" -> None
         | "4.1", "netcoreapp2.0" -> None
         | _ -> failwith (sprintf "unimplemented  profile, fsharpVersion = %s, profile = %s" fsharp profile)


    let paketPackageFromMainPaketGroup packageName = 
        let pd = packagesDirectory()
        if Directory.Exists (pd ++ packageName) then 
            pd ++ packageName
        else 
            failwithf "couldn't find %s/NETStandard.Library, whcih is needed for testing .NET Standard 2.0 code generation of a type provider using these utilities" pd

    /// Compute a path to an FSharp.Core suitable for the target profile
    let private fsharpRestoredAssembliesPath fsharp profile =
        let paketGroup =
           match fsharp with
           | "3.1" -> "fs31"
           | "4.0" -> "fs40"
           | "4.1" -> "fs41"
           | _ -> failwith ("unimplemented F# version" + fsharp)
        let compatProfiles =
            match profile with
            | "net45"    -> ["net45";"net40" ]
            | "netstandard1.6"    -> [ "netstandard1.6" ]
            | "netstandard2.0"    -> [ "netstandard2.0"; "netstandard1.6" ]
            | "netcoreapp2.0"    -> [ "netcoreapp2.0"; "netstandard2.0"; "netstandard1.6" ]
            | "portable47"    -> ["portable-net45+sl5+netcore45"]
            | "portable7"     -> ["portable-net45+netcore45"]
            | "portable78"    -> ["portable-net45+netcore45+wp8"]
            | "portable259"   -> ["portable-net45+netcore45+wpa81+wp8"]
            | _ -> failwith "unimplemented portable profile"
        let groupDirectory = 
            match paketPackagesGroupDirectory paketGroup with 
            | None -> 
                 printfn "couldn't find paket packages group %s.  Assuming %s/FSharp.Core is for F# version %s" paketGroup (packagesDirectory()) fsharp
                 packagesDirectory()
            | Some dir -> dir
                
        compatProfiles |> List.tryPick (fun profileFolder -> 
            let file = groupDirectory ++ "FSharp.Core" ++ "lib" ++ profileFolder ++ "FSharp.Core.dll"
            if File.Exists file then Some file else None
        ) |> function 
             | None -> groupDirectory ++ "no.compat.FSharp.Core.dll.found.under.here"
             | Some res -> res
        

    let sysAssembliesPath profile =
        match profile with
        | "net45"-> installedNet45AssembliesRoot()
        | "netstandard2.0"->
            let packageDir = paketPackageFromMainPaketGroup "NETStandard.Library" 
            packageDir ++ "build" ++ "netstandard2.0" ++ "ref"
        | "netcoreapp2.0"->
            let packageDir = paketPackageFromMainPaketGroup "Microsoft.NETCore.App" 
            packageDir ++ "ref" ++ "netcoreapp2.0"
        | "portable47" -> installedPortableAssembliesRoot() ++ "v4.0" ++ "Profile" ++ "Profile47"
        | "portable7" -> installedPortableAssembliesRoot() ++ "v4.5" ++ "Profile" ++ "Profile7"
        | "portable78" -> installedPortableAssembliesRoot() ++ "v4.5" ++ "Profile" ++ "Profile78"
        | "portable259" -> installedPortableAssembliesRoot() ++ "v4.5" ++ "Profile" ++ "Profile259"
        | _ -> failwith (sprintf "unimplemented profile '%s'" profile)

    let FSharpCoreRef fsharp profile = 
        let installedFSharpCore = fsharpCoreFromInstalledAssemblies fsharp profile
        match installedFSharpCore with 
        | Some path when File.Exists path -> path
        | _ -> 
        let restoredFSharpCore  = fsharpRestoredAssembliesPath fsharp profile
        match restoredFSharpCore  with 
        | path when File.Exists(restoredFSharpCore) -> path
        | _ -> 
        match installedFSharpCore with 
        | Some path -> failwith ("couldn't find FSharp.Core.dll at either '" + path + "' or '" + restoredFSharpCore + "'")
        | None  -> failwith ("couldn't find FSharp.Core.dll at '" + restoredFSharpCore + "'")

    let FSharpRefs fsharp profile =
        [ match profile with
          | "portable7" | "portable78" | "portable259" ->
              let sysPath = sysAssembliesPath profile
              for asm in [ "System.Runtime"; "mscorlib"; "System.Collections"; "System.Core"; "System"; "System.Globalization"; "System.IO"; "System.Linq"; "System.Linq.Expressions";
                           "System.Linq.Queryable"; "System.Net"; "System.Net.NetworkInformation"; "System.Net.Primitives"; "System.Net.Requests"; "System.ObjectModel"; "System.Reflection";
                           "System.Reflection.Extensions"; "System.Reflection.Primitives"; "System.Resources.ResourceManager"; "System.Runtime.Extensions";
                           "System.Runtime.InteropServices.WindowsRuntime"; "System.Runtime.Serialization"; "System.Threading"; "System.Threading.Tasks"; "System.Xml"; "System.Xml.Linq"; "System.Xml.XDocument";
                           "System.Runtime.Serialization.Json"; "System.Runtime.Serialization.Primitives"; "System.Windows" ] do
                  yield sysPath ++ asm + ".dll"
          | "portable47" -> 
              let sysPath = sysAssembliesPath profile
              yield sysPath ++ "mscorlib.dll"
              yield sysPath ++ "System.Xml.Linq.dll"
          | "net45" ->
              // See typical command line in https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/190#issuecomment-356564344
              // This is just a subset
              let sysPath = sysAssembliesPath profile
              yield sysPath ++ "mscorlib.dll"
              yield sysPath ++ "System.Numerics.dll" 
              yield sysPath ++ "System.Xml.dll" 
              yield sysPath ++ "System.Core.dll"
              yield sysPath ++ "System.Xml.Linq.dll"
              yield sysPath ++ "System.dll" 
          | "netstandard2.0" ->
             // See typical command line in https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/190#issuecomment-356564344
             let sysPath = sysAssembliesPath profile
             yield sysPath ++ "Microsoft.Win32.Primitives.dll"
             yield sysPath ++ "mscorlib.dll"
             yield sysPath ++ "netstandard.dll"
             yield sysPath ++ "System.AppContext.dll"
             yield sysPath ++ "System.Collections.Concurrent.dll"
             yield sysPath ++ "System.Collections.dll"
             yield sysPath ++ "System.Collections.NonGeneric.dll"
             yield sysPath ++ "System.Collections.Specialized.dll"
             yield sysPath ++ "System.ComponentModel.Composition.dll"
             yield sysPath ++ "System.ComponentModel.dll"
             yield sysPath ++ "System.ComponentModel.EventBasedAsync.dll"
             yield sysPath ++ "System.ComponentModel.Primitives.dll"
             yield sysPath ++ "System.ComponentModel.TypeConverter.dll"
             yield sysPath ++ "System.Console.dll"
             yield sysPath ++ "System.Core.dll"
             yield sysPath ++ "System.Data.Common.dll"
             yield sysPath ++ "System.Data.dll"
             yield sysPath ++ "System.Diagnostics.Contracts.dll"
             yield sysPath ++ "System.Diagnostics.Debug.dll"
             yield sysPath ++ "System.Diagnostics.FileVersionInfo.dll"
             yield sysPath ++ "System.Diagnostics.Process.dll"
             yield sysPath ++ "System.Diagnostics.StackTrace.dll"
             yield sysPath ++ "System.Diagnostics.TextWriterTraceListener.dll"
             yield sysPath ++ "System.Diagnostics.Tools.dll"
             yield sysPath ++ "System.Diagnostics.TraceSource.dll"
             yield sysPath ++ "System.Diagnostics.Tracing.dll"
             yield sysPath ++ "System.dll"
             yield sysPath ++ "System.Drawing.dll"
             yield sysPath ++ "System.Drawing.Primitives.dll"
             yield sysPath ++ "System.Dynamic.Runtime.dll"
             yield sysPath ++ "System.Globalization.Calendars.dll"
             yield sysPath ++ "System.Globalization.dll"
             yield sysPath ++ "System.Globalization.Extensions.dll"
             yield sysPath ++ "System.IO.Compression.dll"
             yield sysPath ++ "System.IO.Compression.FileSystem.dll"
             yield sysPath ++ "System.IO.Compression.ZipFile.dll"
             yield sysPath ++ "System.IO.dll"
             yield sysPath ++ "System.IO.FileSystem.dll"
             yield sysPath ++ "System.IO.FileSystem.DriveInfo.dll"
             yield sysPath ++ "System.IO.FileSystem.Primitives.dll"
             yield sysPath ++ "System.IO.FileSystem.Watcher.dll"
             yield sysPath ++ "System.IO.IsolatedStorage.dll"
             yield sysPath ++ "System.IO.MemoryMappedFiles.dll"
             yield sysPath ++ "System.IO.Pipes.dll"
             yield sysPath ++ "System.IO.UnmanagedMemoryStream.dll"
             yield sysPath ++ "System.Linq.dll"
             yield sysPath ++ "System.Linq.Expressions.dll"
             yield sysPath ++ "System.Linq.Parallel.dll"
             yield sysPath ++ "System.Linq.Queryable.dll"
             yield sysPath ++ "System.Net.dll"
             yield sysPath ++ "System.Net.Http.dll"
             yield sysPath ++ "System.Net.NameResolution.dll"
             yield sysPath ++ "System.Net.NetworkInformation.dll"
             yield sysPath ++ "System.Net.Ping.dll"
             yield sysPath ++ "System.Net.Primitives.dll"
             yield sysPath ++ "System.Net.Requests.dll"
             yield sysPath ++ "System.Net.Security.dll"
             yield sysPath ++ "System.Net.Sockets.dll"
             yield sysPath ++ "System.Net.WebHeaderCollection.dll"
             yield sysPath ++ "System.Net.WebSockets.Client.dll"
             yield sysPath ++ "System.Net.WebSockets.dll"
             yield sysPath ++ "System.Numerics.dll"
             yield sysPath ++ "System.ObjectModel.dll"
             yield sysPath ++ "System.Reflection.dll"
             yield sysPath ++ "System.Reflection.Extensions.dll"
             yield sysPath ++ "System.Reflection.Primitives.dll"
             yield sysPath ++ "System.Resources.Reader.dll"
             yield sysPath ++ "System.Resources.ResourceManager.dll"
             yield sysPath ++ "System.Resources.Writer.dll"
             yield sysPath ++ "System.Runtime.CompilerServices.VisualC.dll"
             yield sysPath ++ "System.Runtime.dll"
             yield sysPath ++ "System.Runtime.Extensions.dll"
             yield sysPath ++ "System.Runtime.Handles.dll"
             yield sysPath ++ "System.Runtime.InteropServices.dll"
             yield sysPath ++ "System.Runtime.InteropServices.RuntimeInformation.dll"
             yield sysPath ++ "System.Runtime.Numerics.dll"
             yield sysPath ++ "System.Runtime.Serialization.dll"
             yield sysPath ++ "System.Runtime.Serialization.Formatters.dll"
             yield sysPath ++ "System.Runtime.Serialization.Json.dll"
             yield sysPath ++ "System.Runtime.Serialization.Primitives.dll"
             yield sysPath ++ "System.Runtime.Serialization.Xml.dll"
             yield sysPath ++ "System.Security.Claims.dll"
             yield sysPath ++ "System.Security.Cryptography.Algorithms.dll"
             yield sysPath ++ "System.Security.Cryptography.Csp.dll"
             yield sysPath ++ "System.Security.Cryptography.Encoding.dll"
             yield sysPath ++ "System.Security.Cryptography.Primitives.dll"
             yield sysPath ++ "System.Security.Cryptography.X509Certificates.dll"
             yield sysPath ++ "System.Security.Principal.dll"
             yield sysPath ++ "System.Security.SecureString.dll"
             yield sysPath ++ "System.ServiceModel.Web.dll"
             yield sysPath ++ "System.Text.Encoding.dll"
             yield sysPath ++ "System.Text.Encoding.Extensions.dll"
             yield sysPath ++ "System.Text.RegularExpressions.dll"
             yield sysPath ++ "System.Threading.dll"
             yield sysPath ++ "System.Threading.Overlapped.dll"
             yield sysPath ++ "System.Threading.Tasks.dll"
             yield sysPath ++ "System.Threading.Tasks.Parallel.dll"
             yield sysPath ++ "System.Threading.Thread.dll"
             yield sysPath ++ "System.Threading.ThreadPool.dll"
             yield sysPath ++ "System.Threading.Timer.dll"
             yield sysPath ++ "System.Transactions.dll"
             yield sysPath ++ "System.ValueTuple.dll"
             yield sysPath ++ "System.Web.dll"
             yield sysPath ++ "System.Windows.dll"
             yield sysPath ++ "System.Xml.dll"
             yield sysPath ++ "System.Xml.Linq.dll"
             yield sysPath ++ "System.Xml.ReaderWriter.dll"
             yield sysPath ++ "System.Xml.Serialization.dll"
             yield sysPath ++ "System.Xml.XDocument.dll"
             yield sysPath ++ "System.Xml.XmlDocument.dll"
             yield sysPath ++ "System.Xml.XmlSerializer.dll"
             yield sysPath ++ "System.Xml.XPath.dll"
             yield sysPath ++ "System.Xml.XPath.XDocument.dll"
          | "netcoreapp2.0" ->
             // See typical command line in https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/190#issuecomment-356564344
             let sysPath = sysAssembliesPath profile
             yield sysPath ++ "Microsoft.CSharp.dll"
             yield sysPath ++ "Microsoft.VisualBasic.dll"
             yield sysPath ++ "Microsoft.Win32.Primitives.dll"
             yield sysPath ++ "mscorlib.dll"
             yield sysPath ++ "netstandard.dll"
             yield sysPath ++ "System.AppContext.dll"
             yield sysPath ++ "System.Buffers.dll"
             yield sysPath ++ "System.Collections.Concurrent.dll"
             yield sysPath ++ "System.Collections.dll"
             yield sysPath ++ "System.Collections.Immutable.dll"
             yield sysPath ++ "System.Collections.NonGeneric.dll"
             yield sysPath ++ "System.Collections.Specialized.dll"
             yield sysPath ++ "System.ComponentModel.Annotations.dll"
             yield sysPath ++ "System.ComponentModel.Composition.dll"
             yield sysPath ++ "System.ComponentModel.DataAnnotations.dll"
             yield sysPath ++ "System.ComponentModel.dll"
             yield sysPath ++ "System.ComponentModel.EventBasedAsync.dll"
             yield sysPath ++ "System.ComponentModel.Primitives.dll"
             yield sysPath ++ "System.ComponentModel.TypeConverter.dll"
             yield sysPath ++ "System.Configuration.dll"
             yield sysPath ++ "System.Console.dll"
             yield sysPath ++ "System.Core.dll"
             yield sysPath ++ "System.Data.Common.dll"
             yield sysPath ++ "System.Data.dll"
             yield sysPath ++ "System.Diagnostics.Contracts.dll"
             yield sysPath ++ "System.Diagnostics.Debug.dll"
             yield sysPath ++ "System.Diagnostics.DiagnosticSource.dll"
             yield sysPath ++ "System.Diagnostics.FileVersionInfo.dll"
             yield sysPath ++ "System.Diagnostics.Process.dll"
             yield sysPath ++ "System.Diagnostics.StackTrace.dll"
             yield sysPath ++ "System.Diagnostics.TextWriterTraceListener.dll"
             yield sysPath ++ "System.Diagnostics.Tools.dll"
             yield sysPath ++ "System.Diagnostics.TraceSource.dll"
             yield sysPath ++ "System.Diagnostics.Tracing.dll"
             yield sysPath ++ "System.dll"
             yield sysPath ++ "System.Drawing.dll"
             yield sysPath ++ "System.Drawing.Primitives.dll"
             yield sysPath ++ "System.Dynamic.Runtime.dll"
             yield sysPath ++ "System.Globalization.Calendars.dll"
             yield sysPath ++ "System.Globalization.dll"
             yield sysPath ++ "System.Globalization.Extensions.dll"
             yield sysPath ++ "System.IO.Compression.dll"
             yield sysPath ++ "System.IO.Compression.FileSystem.dll"
             yield sysPath ++ "System.IO.Compression.ZipFile.dll"
             yield sysPath ++ "System.IO.dll"
             yield sysPath ++ "System.IO.FileSystem.dll"
             yield sysPath ++ "System.IO.FileSystem.DriveInfo.dll"
             yield sysPath ++ "System.IO.FileSystem.Primitives.dll"
             yield sysPath ++ "System.IO.FileSystem.Watcher.dll"
             yield sysPath ++ "System.IO.IsolatedStorage.dll"
             yield sysPath ++ "System.IO.MemoryMappedFiles.dll"
             yield sysPath ++ "System.IO.Pipes.dll"
             yield sysPath ++ "System.IO.UnmanagedMemoryStream.dll"
             yield sysPath ++ "System.Linq.dll"
             yield sysPath ++ "System.Linq.Expressions.dll"
             yield sysPath ++ "System.Linq.Parallel.dll"
             yield sysPath ++ "System.Linq.Queryable.dll"
             yield sysPath ++ "System.Net.dll"
             yield sysPath ++ "System.Net.Http.dll"
             yield sysPath ++ "System.Net.HttpListener.dll"
             yield sysPath ++ "System.Net.Mail.dll"
             yield sysPath ++ "System.Net.NameResolution.dll"
             yield sysPath ++ "System.Net.NetworkInformation.dll"
             yield sysPath ++ "System.Net.Ping.dll"
             yield sysPath ++ "System.Net.Primitives.dll"
             yield sysPath ++ "System.Net.Requests.dll"
             yield sysPath ++ "System.Net.Security.dll"
             yield sysPath ++ "System.Net.ServicePoint.dll"
             yield sysPath ++ "System.Net.Sockets.dll"
             yield sysPath ++ "System.Net.WebClient.dll"
             yield sysPath ++ "System.Net.WebHeaderCollection.dll"
             yield sysPath ++ "System.Net.WebProxy.dll"
             yield sysPath ++ "System.Net.WebSockets.Client.dll"
             yield sysPath ++ "System.Net.WebSockets.dll"
             yield sysPath ++ "System.Numerics.dll"
             yield sysPath ++ "System.Numerics.Vectors.dll"
             yield sysPath ++ "System.ObjectModel.dll"
             yield sysPath ++ "System.Reflection.DispatchProxy.dll"
             yield sysPath ++ "System.Reflection.dll"
             yield sysPath ++ "System.Reflection.Emit.dll"
             yield sysPath ++ "System.Reflection.Emit.ILGeneration.dll"
             yield sysPath ++ "System.Reflection.Emit.Lightweight.dll"
             yield sysPath ++ "System.Reflection.Extensions.dll"
             yield sysPath ++ "System.Reflection.Metadata.dll"
             yield sysPath ++ "System.Reflection.Primitives.dll"
             yield sysPath ++ "System.Reflection.TypeExtensions.dll"
             yield sysPath ++ "System.Resources.Reader.dll"
             yield sysPath ++ "System.Resources.ResourceManager.dll"
             yield sysPath ++ "System.Resources.Writer.dll"
             yield sysPath ++ "System.Runtime.CompilerServices.VisualC.dll"
             yield sysPath ++ "System.Runtime.dll"
             yield sysPath ++ "System.Runtime.Extensions.dll"
             yield sysPath ++ "System.Runtime.Handles.dll"
             yield sysPath ++ "System.Runtime.InteropServices.dll"
             yield sysPath ++ "System.Runtime.InteropServices.RuntimeInformation.dll"
             yield sysPath ++ "System.Runtime.InteropServices.WindowsRuntime.dll"
             yield sysPath ++ "System.Runtime.Loader.dll"
             yield sysPath ++ "System.Runtime.Numerics.dll"
             yield sysPath ++ "System.Runtime.Serialization.dll"
             yield sysPath ++ "System.Runtime.Serialization.Formatters.dll"
             yield sysPath ++ "System.Runtime.Serialization.Json.dll"
             yield sysPath ++ "System.Runtime.Serialization.Primitives.dll"
             yield sysPath ++ "System.Runtime.Serialization.Xml.dll"
             yield sysPath ++ "System.Security.Claims.dll"
             yield sysPath ++ "System.Security.Cryptography.Algorithms.dll"
             yield sysPath ++ "System.Security.Cryptography.Csp.dll"
             yield sysPath ++ "System.Security.Cryptography.Encoding.dll"
             yield sysPath ++ "System.Security.Cryptography.Primitives.dll"
             yield sysPath ++ "System.Security.Cryptography.X509Certificates.dll"
             yield sysPath ++ "System.Security.dll"
             yield sysPath ++ "System.Security.Principal.dll"
             yield sysPath ++ "System.Security.SecureString.dll"
             yield sysPath ++ "System.ServiceModel.Web.dll"
             yield sysPath ++ "System.ServiceProcess.dll"
             yield sysPath ++ "System.Text.Encoding.dll"
             yield sysPath ++ "System.Text.Encoding.Extensions.dll"
             yield sysPath ++ "System.Text.RegularExpressions.dll"
             yield sysPath ++ "System.Threading.dll"
             yield sysPath ++ "System.Threading.Overlapped.dll"
             yield sysPath ++ "System.Threading.Tasks.Dataflow.dll"
             yield sysPath ++ "System.Threading.Tasks.dll"
             yield sysPath ++ "System.Threading.Tasks.Extensions.dll"
             yield sysPath ++ "System.Threading.Tasks.Parallel.dll"
             yield sysPath ++ "System.Threading.Thread.dll"
             yield sysPath ++ "System.Threading.ThreadPool.dll"
             yield sysPath ++ "System.Threading.Timer.dll"
             yield sysPath ++ "System.Transactions.dll"
             yield sysPath ++ "System.Transactions.Local.dll"
             yield sysPath ++ "System.ValueTuple.dll"
             yield sysPath ++ "System.Web.dll"
             yield sysPath ++ "System.Web.HttpUtility.dll"
             yield sysPath ++ "System.Windows.dll"
             yield sysPath ++ "System.Xml.dll"
             yield sysPath ++ "System.Xml.Linq.dll"
             yield sysPath ++ "System.Xml.ReaderWriter.dll"
             yield sysPath ++ "System.Xml.Serialization.dll"
             yield sysPath ++ "System.Xml.XDocument.dll"
             yield sysPath ++ "System.Xml.XmlDocument.dll"
             yield sysPath ++ "System.Xml.XmlSerializer.dll"
             yield sysPath ++ "System.Xml.XPath.dll"
             yield sysPath ++ "System.Xml.XPath.XDocument.dll"
             yield sysPath ++ "WindowsBase.dll"
             yield sysPath ++ "Microsoft.Win32.Primitives.dll"
          | _ -> 
             failwith (sprintf "unimplemented profile, fsharpVersion = %s, profile = %s" fsharp profile)

          yield FSharpCoreRef fsharp profile
        ]
    let FSharpCore31Ref() = FSharpCoreRef "3.1" "net45"
    let DotNet45FSharp31Refs() = FSharpRefs "3.1" "net45"
    let Portable47FSharp31Refs() = FSharpRefs "3.1" "portable47"
    let Portable7FSharp31Refs() = FSharpRefs "3.1" "portable7"
    let Portable78FSharp31Refs() = FSharpRefs "3.1" "portable78"
    let Portable259FSharp31Refs() = FSharpRefs "3.1" "portable259"

    let FSharpCore40Ref() = FSharpCoreRef "4.0" "net45"
    let DotNet45FSharp40Refs() = FSharpRefs "4.0" "net45"
    let Portable7FSharp40Refs() = FSharpRefs "4.0" "portable7"
    let Portable78FSharp40Refs() = FSharpRefs "4.0" "portable78"
    let Portable259FSharp40Refs() = FSharpRefs "4.0" "portable259"
    let DotNet45Ref r = sysAssembliesPath "net45" ++ r

    let FSharpCore41Ref() = FSharpCoreRef "4.1" "net45"
    let DotNet45FSharp41Refs() = FSharpRefs "4.1" "net45"
    let Portable7FSharp41Refs() = FSharpRefs "4.1" "portable7"
    let Portable78FSharp41Refs() = FSharpRefs "4.1" "portable78"
    let Portable259FSharp41Refs() = FSharpRefs "4.1" "portable259"

    let DotNetStandard20FSharp41Refs() = FSharpRefs "4.1" "netstandard2.0"
    let DotNetCoreApp20FSharp41Refs() = FSharpRefs "4.1" "netcoreapp2.0"
    
    let supportsFSharp31() = (try File.Exists (FSharpCore31Ref()) with _ -> false)
    let supportsFSharp40() = (try File.Exists (FSharpCore40Ref()) with _ -> false)
    let supportsFSharp41() = (try File.Exists (FSharpCore41Ref()) with _ -> false)
    let hasPortable47Assemblies() = Directory.Exists (sysAssembliesPath "portable47")
    let hasPortable7Assemblies() = Directory.Exists (sysAssembliesPath "portable7")
    let hasPortable78Assemblies() = Directory.Exists (sysAssembliesPath "portable78")
    let hasPortable259Assemblies() = Directory.Exists (sysAssembliesPath "portable259")
