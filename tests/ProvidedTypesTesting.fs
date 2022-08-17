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

module Utils = 
    let isNull x = match x with null -> true | _ -> false


/// Simulate a real host of TypeProviderConfig
type internal DllInfo(path: string) =
   // Must have a property called FileName
    member _.FileName = path

/// Simulate a real host of TypeProviderConfig
// Must be a type called TcImports
type internal TcImports(bas: TcImports option, dllInfosInitial: DllInfo list) =
    // Must have a field called "dllInfos"
    let mutable dllInfos = dllInfosInitial
    member _.Base = bas
    member _.DllInfos = dllInfos
    member _.AddReferencedDlls paths = dllInfos <- dllInfos @ [ for p in paths -> DllInfo(p) ]

type internal Testing() =

    /// Simulates a real instance of TypeProviderConfig
    static member MakeSimulatedTypeProviderConfigIncremental (resolutionFolder: string, runtimeAssembly: string, runtimeAssemblyRefs: string list, ?isHostedExecution, ?isInvalidationSupported) =
        let runtimeAssemblyRefs = (runtimeAssembly :: runtimeAssemblyRefs) |> List.distinct
        let cfg = TypeProviderConfig(fun _ -> false)
        cfg.IsHostedExecution <- defaultArg isHostedExecution false
        cfg.IsInvalidationSupported <- defaultArg isInvalidationSupported true
        cfg.ResolutionFolder <- resolutionFolder
        cfg.RuntimeAssembly <- runtimeAssembly
        cfg.ReferencedAssemblies <- Array.ofList runtimeAssemblyRefs
        let (?<-) cfg prop value =
            let ty = cfg.GetType()
            match ty.GetProperty(prop,BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic) with
            | null -> 
                let fld = ty.GetField(prop,BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
                if fld = null then failwith ("expected TypeProviderConfig to have a property or field "+prop)
                fld.SetValue(cfg, value)|> ignore
            | p -> 
                p.GetSetMethod(nonPublic = true).Invoke(cfg, [| box value |]) |> ignore

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
        cfg, tcImports

    static member MakeSimulatedTypeProviderConfig (resolutionFolder: string, runtimeAssembly: string, runtimeAssemblyRefs: string list, ?isHostedExecution, ?isInvalidationSupported) =
        let cfg, _ = Testing.MakeSimulatedTypeProviderConfigIncremental (resolutionFolder, runtimeAssembly, runtimeAssemblyRefs, ?isHostedExecution=isHostedExecution, ?isInvalidationSupported=isInvalidationSupported)
        cfg

    /// Simulates a real instance of TypeProviderConfig and then creates an instance of the last
    /// type provider added to a namespace by the type provider constructor
    static member GenerateProvidedTypeInstantiation (resolutionFolder: string, runtimeAssembly: string, runtimeAssemblyRefs: string list, typeProviderForNamespacesConstructor, args: obj[]) =
        let cfg = Testing.MakeSimulatedTypeProviderConfig (resolutionFolder, runtimeAssembly, runtimeAssemblyRefs)

        let tp = typeProviderForNamespacesConstructor cfg :> ITypeProvider

        let providedNamespace = tp.GetNamespaces().[0] 
        let providedTypes  = providedNamespace.GetTypes()
        let providedType = 
            providedTypes 
            |> Array.tryFind (fun ty -> tp.GetStaticParameters(ty).Length = args.Length ) 
            |> function None -> failwithf "couldn't find a type in type provider with %d static args " args.Length | Some ty -> ty

        let providedType = 
            match args with
            | [||] -> providedType
            | args ->
                let typeName =
                    if (providedType.Attributes &&& enum (int32 TypeProviderTypeAttributes.IsErased)) <> enum 0 then
                        providedType.Name + (args |> Seq.map (fun s -> ",\"" + (if s = null then "" else s.ToString()) + "\"") |> Seq.reduce (+))
                    else
                        // The type name ends up quite mangled in the dll output if we combine the name using static parameters, so for generated types we don't do that
                        providedType.Name
                tp.ApplyStaticArguments(providedType, [| typeName |], args)
        tp, providedType

    /// Returns a string representation of the signature (and optionally also the body) of all the
    /// types generated by the type provider up to a certain depth and width
    /// If ignoreOutput is true, this will still visit the full graph, but it will output an empty string to be faster
    static member FormatProvidedType (tp: ITypeProvider, t: Type, ?signatureOnly, ?ignoreOutput, ?maxDepth, ?maxWidth, ?useQualifiedNames) =

        let signatureOnly = defaultArg signatureOnly false
        let ignoreOutput = defaultArg ignoreOutput false
        let maxDepth = defaultArg maxDepth 10
        let maxWidth = defaultArg maxWidth 100
        let useQualifiedNames = defaultArg useQualifiedNames false

        let hasUnitOfMeasure (t: System.Type) = 
            t.IsGenericType && 
            (t.Namespace = "System") &&
            (t.Name = typeof<bool>.Name ||
             t.Name = typeof<obj>.Name ||
             t.Name = typeof<int>.Name ||
             t.Name = typeof<int64>.Name ||
             t.Name = typeof<float>.Name ||
             t.Name = typeof<float32>.Name ||
             t.Name = typeof<decimal>.Name)

        let isFSharpUnitAnnotated (t:System.Type) = t.IsGenericType && not (t.GetGenericTypeDefinition().IsGenericTypeDefinition)

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
                if useQualifiedNames && not (t.GetType().Name = "ProvidedTypeDefinition") then
                    t.AssemblyQualifiedName
                else t.Namespace + "." + t.Name
            if fullName.StartsWith "FSI_" then
                fullName.Substring(fullName.IndexOf('.') + 1)
            else
                fullName

        let rec toString useFullName (t: Type) =

            let hasUnit =  hasUnitOfMeasure t

            let innerToString (t: Type) =
                match t with
                | _ when t.Name = typeof<bool>.Name && not hasUnit -> "bool"
                | _ when t.Name = typeof<obj>.Name && not hasUnit  -> "obj"
                | _ when t.Name = typeof<int>.Name && not hasUnit  -> "int"
                | _ when t.Name = typeof<int64>.Name && not hasUnit  -> "int64"
                | _ when t.Name = typeof<float>.Name && not hasUnit  -> "float"
                | _ when t.Name = typeof<float32>.Name && not hasUnit  -> "float32"
                | _ when t.Name = typeof<decimal>.Name && not hasUnit  -> "decimal"
                | _ when t.Name = typeof<string>.Name && not hasUnit  -> "string"
                | _ when t.Name = typeof<Void>.Name -> "()"
                | _ when t.Name = typeof<unit>.Name -> "()"
                | t when t.IsArray -> (t.GetElementType() |> toString useFullName) + "[]"
                | _ when t.GetType().Name = "ProvidedTypeDefinition" ->
                    add t
                    t.Name.Split(',').[0]
                | t when t.IsGenericType ->
                    let args =
                        if useFullName then
                            t.GetGenericArguments()
                            |> Seq.map (if hasUnit then (fun t -> t.Name) else toString useFullName)
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
                            | t when hasUnit -> toString useFullName t.UnderlyingSystemType, false
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
                | t when t.GetType().Name = "ProvidedTypeDefinition" -> ""
                | t when t.IsGenericType -> defaultArg (t.GetGenericArguments() |> Seq.map warnIfWrongAssembly |> Seq.tryFind (fun s -> s <> "")) ""
                | t when t.IsArray -> warnIfWrongAssembly <| t.GetElementType()
                | t -> if not t.IsGenericParameter && t.Assembly = Assembly.GetExecutingAssembly() then " [DESIGNTIME]" else ""

            if ignoreOutput then
                ""
            elif hasUnit || t.IsGenericParameter || t.DeclaringType = null then
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

            let rec eraseType(typ:Type): Type =
                match typ with
                | t when t.Attributes &&& enum (int32 TypeProviderTypeAttributes.IsErased) <> enum 0 -> eraseType typ.BaseType
                | t when isFSharpUnitAnnotated t -> eraseType typ.UnderlyingSystemType
                | t when t.IsArray ->
                    let rank = t.GetArrayRank()
                    let et = eraseType (t.GetElementType())
                    if rank = 0 then et.MakeArrayType() else et.MakeArrayType(rank)
                | t when t.IsGenericType && not t.IsGenericTypeDefinition ->
                    let genericTypeDefinition = t.GetGenericTypeDefinition()
                    let genericArguments = t.GetGenericArguments() |> Array.map eraseType
                    genericTypeDefinition.MakeGenericType(genericArguments)
                | t -> t

            let getMethodBody (m: MethodInfo) =
                let vs = 
                    [ if not m.IsStatic then yield ("this", eraseType m.DeclaringType)
                      for p in m.GetParameters() do yield (p.Name, eraseType p.ParameterType) ]
                    |> List.map (Var.Global >> Expr.Var)
                tp.GetInvokerExpression(m, List.toArray vs)

            let getConstructorBody (c: ConstructorInfo) =
                let vs = 
                    [ for p in c.GetParameters() do yield (p.Name, eraseType p.ParameterType) ]
                    |> List.map (Var.Global >> Expr.Var)
                tp.GetInvokerExpression(c, List.toArray vs)

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

            | (:? ConstructorInfo as cons) when cons.GetType().Name = "ProvidedConstructor" ->
                if not ignoreOutput then
                    print <| "new : " +
                             (toSignature <| cons.GetParameters()) + " -> " +
                             (toString true memberInfo.DeclaringType)
                if not signatureOnly then
                    cons |> getConstructorBody |> printExpr

            | (:? FieldInfo as field) when field.GetType().Name = "ProvidedField" ->
                let value =
                    if signatureOnly then ""
                    else field.GetRawConstantValue() |> printObj
                if not ignoreOutput then
                    print <| "val " + (getName field) + ": " +
                             (toString true field.FieldType) +
                             value

            | (:? PropertyInfo as prop) when prop.GetType().Name = "ProvidedProperty" ->
                if not ignoreOutput then
                    print <| (if (prop.CanRead && prop.GetGetMethod().IsStatic || prop.CanWrite && prop.GetSetMethod().IsStatic) then "static " else "") + "member " +
                             (getName prop) + ": " + (toString true prop.PropertyType) +
                             " with " + (if prop.CanRead && prop.CanWrite then "get, set" else if prop.CanRead then "get" else "set")
                if not signatureOnly then
                    if prop.CanRead then
                        getMethodBody (prop.GetGetMethod()) |> printExpr
                    if prop.CanWrite then
                        getMethodBody (prop.GetSetMethod()) |> printExpr

            | (:? MethodInfo as m) when m.GetType().Name = "ProvidedMethod" ->
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

    let runningOnMac =
        (Environment.OSVersion.Platform = PlatformID.MacOSX)
        || (Environment.OSVersion.Platform = PlatformID.Unix) && Directory.Exists("/Applications") && Directory.Exists("/System") && Directory.Exists("/Users") && Directory.Exists("/Volumes")

    let runningOnLinux =
        (Environment.OSVersion.Platform = PlatformID.Unix) && not runningOnMac

    let runningOnWindows = 
        match System.Environment.OSVersion.Platform with
        | System.PlatformID.Win32NT -> true
        | _ -> false
    
    let packagesDirectory isTest = 
        let root = Path.GetDirectoryName(Uri(Assembly.GetExecutingAssembly().Location).LocalPath)
        let rec loop dir =
             if Directory.Exists(dir ++ "packages" ++ (if isTest then "test" else "FSharp.Core")) then 
                 dir ++ "packages" ++ (if isTest then "test" else "FSharp.Core")
             else
                 let parent = Path.GetDirectoryName(dir)
                 match parent with
                 | null | "" -> failwith ("couldn't find packages directory anywhere above  " + root)
                 | _ ->  loop parent
                 
        loop  root

    let paketPackageDirectoryForFSharpCore() =
        let pd = packagesDirectory false
        if Directory.Exists(pd) then 
            Some pd
        else 
            None

    let paketPackageFromMainPaketGroup packageName = 
        let pd = packagesDirectory true
        if Directory.Exists (pd ++ packageName) then 
            pd ++ packageName
        else 
            failwithf "couldn't find %s/NETStandard.Library, which is needed for testing .NET Standard 2.0 code generation of a type provider using these utilities" pd

    /// Compute a path to an FSharp.Core suitable for the target profile
    let private fsharpRestoredAssembliesPath profile =
        let compatProfile = "netstandard2.0"
        let groupDirectory =
            match paketPackageDirectoryForFSharpCore() with 
            | None -> 
                 printfn "couldn't find paket package for FSHarp.Core.  Assuming %s/FSharp.Core is suitable" (packagesDirectory false)
                 packagesDirectory false
            | Some dir -> dir

        let file = groupDirectory ++ "lib" ++ compatProfile ++ "FSharp.Core.dll"
        if File.Exists file then
            Some file
        else
            None
        
    let sysAssembliesPath profile =
        match profile with
        | "netstandard2.0"->
            let packageDir = paketPackageFromMainPaketGroup "NETStandard.Library" 
            packageDir ++ "build" ++ "netstandard2.0" ++ "ref"
        | "netcoreapp3.1"
        | "net5.0"->
            let packageDir = paketPackageFromMainPaketGroup "Microsoft.NETCore.App" 
            packageDir ++ "ref" ++ "netcoreapp2.2"
        | _ -> failwith (sprintf "unimplemented profile '%s'" profile)

    let FSharpCoreRef profile = 
        let restoredFSharpCore  = fsharpRestoredAssembliesPath  profile
        match restoredFSharpCore  with 
        | Some path when File.Exists path -> path
        | _ ->
            failwithf "Couldn't find FSharp.Core at %s" (packagesDirectory false)

    let FSharpRefs profile =
        [ match profile with
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
          | "netcoreapp3.1"
          | "net5.0" ->
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
             failwith (sprintf "unimplemented profile = %s" profile)

          yield FSharpCoreRef  profile
        ]

    let DotNetStandard20FSharpRefs() = FSharpRefs "netstandard2.0"
