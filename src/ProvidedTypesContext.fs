// Copyright 2011-2015, Tomas Petricek (http://tomasp.net), Gustavo Guerra (http://functionalflow.co.uk), and other contributors
// Licensed under the Apache License, Version 2.0, see LICENSE.md in this project
//
// A binding context for cross-targeting type providers

namespace ProviderImplementation

#nowarn "8796"
open System
open System.Diagnostics
open System.IO
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Reflection
open ProviderImplementation.AssemblyReader
open ProviderImplementation.AssemblyReaderReflection
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations

[<AutoOpen>]
module private ImplementationUtils =
    type System.Object with
       member x.GetProperty(nm) =
           let ty = x.GetType()
           let prop = ty.GetProperty(nm, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
           let v = prop.GetValue(x,null)
           v
       member x.GetField(nm) =
           let ty = x.GetType()
           let fld = ty.GetField(nm, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
           let v = fld.GetValue(x)
           v
       member x.HasProperty(nm) =
           let ty = x.GetType()
           let p = ty.GetProperty(nm, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
           p <> null
       member x.HasField(nm) =
           let ty = x.GetType()
           let fld = ty.GetField(nm, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
           fld <> null
       member x.GetElements() = [ for v in (x :?> System.Collections.IEnumerable) do yield v ]

/// A cross-targeting type provider must ultimately provide quotations and reflection objects w.r.t.
/// the type binding context for the target assembly reference set.
///
/// To make building a cross-targeting type provider palatable, the type provider is written w.r.t. to
/// homogeneous quotations and reflection objects referring to a copy of the target runtime constructs held
/// in the design-time assembly itself.   These are then systematically remapped (replaced/translated) to the
/// corresponding reflection objects in the target assembly reference set.
///
/// The AssemblyReplacer acts as a way of creating provided objects where the replacement is automatically and
/// systematically applied.
///
/// When making a cross-targeting type provider, calls to
///    ProvidedTypeDefinition
///    ProvidedParameter,
///    ProvidedProperty
///    ProvidedConstructor,
///    ProvidedMethod
/// should be replaced by
///    bindingContext.ProvidedTypeDefinition
///    bindingContext.ProvidedParameter,
///    bindingContext.ProvidedProperty
///    bindingContext.ProvidedConstructor,
///    bindingContext.ProvidedMethod
///
type internal AssemblyReplacer(designTimeAssemblies: Lazy<Assembly[]>, referencedAssemblies: Lazy<Assembly[]>, ?assemblyReplacementMap) =

  let assemblyReplacementMap = defaultArg assemblyReplacementMap Seq.empty

  /// When translating quotations, Expr.Var's are translated to new variable respecting reference equality.
  let varTable = Dictionary<Var, Var>()
  let typeCacheFwd = Dictionary<Type, Type>()
  let typeCacheBwd = Dictionary<Type, Type>()

  let fixName (fullName:string) =
      if fullName.StartsWith("FSI_") then
          // when F# Interactive is the host of the design time assembly,
          // all namespaces are prefixed with FSI_, in the runtime assembly
          // the name won't have that prefix
          fullName.Substring(fullName.IndexOf('.') + 1)
      else
          fullName

  let tryGetTypeFromAssembly (originalAssemblyName:string) fullName (asm:Assembly) =
        // if the original assembly of the type being replaced is in `assemblyReplacementMap`,
        // then we only map it to assemblies with a name specified in `assemblyReplacementMap`
        let notRestrictedOrMatching =
            assemblyReplacementMap
            |> Seq.forall (fun (originalName, newName) ->
                if originalAssemblyName.StartsWith originalName then asm.FullName.StartsWith(newName)
                else true)

        if not notRestrictedOrMatching then None
        elif asm.FullName.StartsWith "FSI-ASSEMBLY" then
            // when F# Interactive is the host of the design time assembly,
            // for each type in the runtime assembly there might be multiple
            // versions (FSI_0001.FullTypeName, FSI_0002.FullTypeName, etc).
            // Get the last one.
            asm.GetTypes()
            |> Seq.filter (fun t -> fixName t.FullName = fullName)
            |> Seq.sortBy (fun t -> t.FullName)
            |> Seq.toList
            |> function [] -> None | xs -> Some (Seq.last xs, false)
        else
            asm.GetType fullName |> function null -> None | x -> Some (x, true)


  let replaceTypeDefinition fwd (t:Type) =
      let cache = (if fwd then typeCacheFwd else typeCacheBwd)
      match cache.TryGetValue(t) with
      | true, newT -> newT
      | false, _ ->
            let asms = (if fwd then referencedAssemblies.Force() else designTimeAssemblies.Force())
            let fullName = fixName t.FullName

            // For normal type provider hosts like fsc.exe System.Void is a special case and these
            // hosts expect it to always be provided as [FSharp.Core 4.4.0.0]typeof<System.Void>.
            // This is really a mistake in ExtensionTyping.fs in the F# compiler which calls
            // typeof<System.Void>.Equals(ty).
            if fullName = "System.Void" then typeof<System.Void> else

            match Array.choose (tryGetTypeFromAssembly t.Assembly.FullName fullName) asms |> Seq.distinct |> Seq.toArray with
            //| [| (newT, canSave) |] ->
            //     if canSave then cache.[t] <- newT
            //     newT
            //| r when r.Length > 1 ->
            //    let msg =
            //        if fwd then sprintf "The type '%O' utilized by a type provider was found in multiple assemblies in the reference assembly set '%A'. You may need to adjust your assembly references to avoid ambiguities." t referencedAssemblies
            //        else sprintf "The type '%O' utilized by a type provider was not found in the assembly set '%A' used by the type provider itself. Please report this problem to the project site for the type provider." t designTimeAssemblies
            //    failwith msg
            | r when r.Length > 0 ->
                 let (newT, canSave) = r.[0]
                 if canSave then cache.[t] <- newT
                 newT
            | _ ->
                let msg =
                    if fwd then sprintf "The type '%O' utilized by a type provider was not found in reference assembly set '%A'. You may be referencing a portable profile which contains fewer types than those needed by the type provider you are using." t (referencedAssemblies.Force())
                    elif designTimeAssemblies.Force().Length = 0 then
                        sprintf "A failure occured while determining compilation references"
                    else sprintf "The runtime-time type '%O' utilized by a type provider was not found in the compilation-time assembly set '%A'. You may be referencing a portable profile which contains fewer types than those needed by the type provider you are using. Please report this problem to the project site for the type provider." t (designTimeAssemblies.Force())
                failwith msg


  let rec replaceType fwd (t:Type) =
      if t :? ProvidedTypeDefinition then t
      // Don't try to translate F# abbreviations
      elif t :? ProvidedSymbolType && (t :?> ProvidedSymbolType).IsFSharpTypeAbbreviation then t
      // Types annotated with units-of-measure
      elif t :? ProvidedSymbolType && (t :?> ProvidedSymbolType).IsFSharpUnitAnnotated then
          let genericType = t.GetGenericTypeDefinition()
          let newT = replaceTypeDefinition fwd genericType
          let typeArguments = t.GetGenericArguments() |> Array.map (replaceType fwd) |> Array.toList
          ProvidedMeasureBuilder.Default.AnnotateType(newT, typeArguments)
      elif t.IsGenericType && not t.IsGenericTypeDefinition then
          let genericType = t.GetGenericTypeDefinition()
          let newT = replaceTypeDefinition fwd genericType
          let typeArguments = t.GetGenericArguments() |> Array.map (replaceType fwd)
          newT.MakeGenericType(typeArguments)
      elif t.IsGenericParameter then t
      elif t.IsArray || t.IsByRef || t.IsPointer then
          let elemType = t.GetElementType()
          let elemTypeT = replaceType fwd elemType
          if t.IsArray then
              let rank = t.GetArrayRank()
              if rank = 1 then elemTypeT.MakeArrayType() else elemTypeT.MakeArrayType(t.GetArrayRank())
          elif t.IsByRef then elemTypeT.MakeByRefType()
          else elemTypeT.MakePointerType()

      else
          replaceTypeDefinition fwd t

  let replaceProperty fwd (p : PropertyInfo) =
    if p :? ProvidedProperty then p
    else
      let t = replaceType fwd p.DeclaringType
      let isStatic =
        p.CanRead && p.GetGetMethod().IsStatic ||
        p.CanWrite && p.GetSetMethod().IsStatic
      let bindingFlags =
        BindingFlags.Public ||| BindingFlags.NonPublic |||
          (if isStatic then BindingFlags.Static else BindingFlags.Instance)
      let newP = t.GetProperty(p.Name, bindingFlags)
      if newP = null then
        failwithf "Property '%O' of type '%O' not found" p t
      newP

  let replaceField fwd (f : FieldInfo) =
    if f :? ProvidedField then f
    else
      let t = replaceType fwd f.DeclaringType
      let bindingFlags =
        (if f.IsPublic then BindingFlags.Public else BindingFlags.NonPublic) |||
        (if f.IsStatic then BindingFlags.Static else BindingFlags.Instance)
      let newF = t.GetField(f.Name, bindingFlags)
      if newF = null then failwithf "Field '%O' of type '%O' not found" f t
      newF

  let replaceMethod fwd (m : MethodInfo) =
    if m :? ProvidedMethod then m
    else
      let declTyT = replaceType fwd m.DeclaringType
      let mT =
          if m.IsGenericMethod then
            let genericMethod = m.GetGenericMethodDefinition()
            let parameterTypesT = genericMethod.GetParameters() |> Array.map (fun p -> replaceType fwd p.ParameterType)
            let genericMethodT = declTyT.GetMethod(genericMethod.Name,parameterTypesT)
            if genericMethodT = null then null else
            let typeArgumentsT =  m.GetGenericArguments() |> Array.map (replaceType fwd)
            genericMethodT.MakeGenericMethod(typeArgumentsT)
          else
            let parameterTypesT = m.GetParameters() |> Array.map (fun p -> replaceType fwd p.ParameterType)
            declTyT.GetMethod(m.Name, parameterTypesT)
      match mT with
      | null -> failwithf "Method '%O' not found in type '%O'" m mT
      | _ -> mT

  let replaceConstructor fwd (cons : ConstructorInfo) =
    if cons :? ProvidedConstructor then cons
    else
        let declTyT = replaceType fwd cons.DeclaringType
        let parameterTypesT = cons.GetParameters() |> Array.map (fun p -> replaceType fwd p.ParameterType)
        let consT = declTyT.GetConstructor(parameterTypesT)
        match consT with
        | null -> failwithf "Constructor '%O' not found in type '%O'" cons declTyT
        | _ -> consT

  let replaceVar fwd (v: Var) =
    if v.Type :? ProvidedTypeDefinition then v
    else
      let createNewVar() =
        Var (v.Name, replaceType fwd v.Type, v.IsMutable)
      if fwd then
        match varTable.TryGetValue v with
        | true, v -> v
        | false, _ ->
            // It's a variable local to the quotation
            let newVar = createNewVar()
            // store it so we reuse it from now on
            varTable.Add(v, newVar)
            newVar
      else
        let newVar = createNewVar()
        // store the original var as we'll have to revert to it later
        varTable.Add(newVar, v)
        newVar

  let rec replaceExpr fwd quotation =

    match quotation with
    | Call (obj, m, args) ->
        let mR = replaceMethod fwd m
        let argsR = List.map (replaceExpr fwd) args
        match obj with
        | Some obj -> Expr.CallUnchecked (replaceExpr fwd obj, mR, argsR)
        | None -> Expr.CallUnchecked (mR, argsR)
    | PropertyGet (obj, p, indexArgs) ->
        let pR = replaceProperty fwd p
        let indexArgsR = List.map (replaceExpr fwd) indexArgs
        match obj with
        | Some obj -> Expr.PropertyGetUnchecked (replaceExpr fwd obj, pR, indexArgsR)
        | None -> Expr.PropertyGetUnchecked (pR, indexArgsR)
    | PropertySet (obj, p, indexArgs, value) ->
        let pR = replaceProperty fwd p
        let indexArgsR = List.map (replaceExpr fwd) indexArgs
        match obj with
        | Some obj -> Expr.PropertySetUnchecked (replaceExpr fwd obj, pR, replaceExpr fwd value, indexArgsR)
        | None -> Expr.PropertySetUnchecked (pR, replaceExpr fwd value, indexArgsR)
    | NewObject (c, exprs) ->
        let exprsR = List.map (replaceExpr fwd) exprs
        Expr.NewObjectUnchecked (replaceConstructor fwd c, exprsR)
    | Coerce (expr, t) ->
        Expr.Coerce (replaceExpr fwd expr, replaceType fwd t)
    | NewArray (t, exprs) ->
        Expr.NewArrayUnchecked (replaceType fwd t, List.map (replaceExpr fwd) exprs)
    | NewTuple (exprs) ->
        Expr.NewTuple (List.map (replaceExpr fwd) exprs)
    | TupleGet (expr, i) ->
        Expr.TupleGetUnchecked (replaceExpr fwd expr, i)
    | NewDelegate (t, vars, expr) ->
        Expr.NewDelegateUnchecked (replaceType fwd t, List.map (replaceVar fwd) vars, replaceExpr fwd expr)
    | FieldGet (obj, f) ->
        match obj with
        | Some obj -> Expr.FieldGetUnchecked (replaceExpr fwd obj, replaceField fwd f)
        | None -> Expr.FieldGetUnchecked (replaceField fwd f)
    | FieldSet (obj, f, value) ->
        match obj with
        | Some obj -> Expr.FieldSetUnchecked (replaceExpr fwd obj, replaceField fwd f, replaceExpr fwd value)
        | None -> Expr.FieldSetUnchecked (replaceField fwd f, replaceExpr fwd value)
    | Let (var, value, body) ->
        Expr.LetUnchecked(replaceVar fwd var, replaceExpr fwd value, replaceExpr fwd body)

    // Eliminate some F# constructs which do not cross-target well
    | Application(f,e) ->
        replaceExpr fwd (Expr.CallUnchecked(f, f.Type.GetMethod "Invoke", [ e ]) )
    | NewUnionCase(ci, es) ->
        replaceExpr fwd (Expr.CallUnchecked(Reflection.FSharpValue.PreComputeUnionConstructorInfo ci, es) )
    | NewRecord(ci, es) ->
        replaceExpr fwd (Expr.NewObjectUnchecked(FSharpValue.PreComputeRecordConstructorInfo ci, es) )
    | UnionCaseTest(e,uc) ->
        let tagInfo = FSharpValue.PreComputeUnionTagMemberInfo uc.DeclaringType
        let tagExpr =
            match tagInfo with
            | :? PropertyInfo as tagProp -> Expr.PropertyGetUnchecked(e,tagProp)
            | :? MethodInfo as tagMeth ->
                    if tagMeth.IsStatic then Expr.CallUnchecked(tagMeth, [e])
                    else Expr.CallUnchecked(e,tagMeth,[])
            | _ -> failwith "unreachable: unexpected result from PreComputeUnionTagMemberInfo"
        let tagNumber = uc.Tag
        replaceExpr fwd <@@ (%%(tagExpr) : int) = tagNumber @@>

    // Traverse remaining constructs
    | ShapeVar v ->
        Expr.Var (replaceVar fwd v)
    | ShapeLambda _ ->
        failwith ("It's not possible to create a Lambda when cross targetting to a different FSharp.Core.\n" +
                  "Make sure you're not calling a function with signature A->(B->C) instead of A->B->C (using |> causes this).")
    | ShapeCombination (o, exprs) ->
        RebuildShapeCombination (o, List.map (replaceExpr fwd) exprs)

  // Gets the equivalent runtime type
  member __.ConvertDesignTimeTypeToTargetType t = t |> replaceType true
  // Gets an equivalent expression with all the types replaced with runtime equivalents
  member __.ConvertDesignTimeExprToTargetExpr e = e |> replaceExpr true
  // Gets an equivalent expression with all the types replaced with designTime equivalents
  member __.ConvertTargetExprToDesignTimeExpr e = e |> replaceExpr false

  // For the Provided* type InvokeCode and GetterCode, we to first transform the argument expressions
  // to the design time types, so we can splice it in the quotation, and then after that we have to convert
  // it back to the runtime type.

/// Represents the type binding context for the type provider based on the set of assemblies
/// referenced by the compilation.
type internal ProvidedTypesContext(referencedAssemblyPaths : string list, ?assemblyReplacementMap : seq<string*string>) as this =

    let assemblyReplacementMap = defaultArg assemblyReplacementMap Seq.empty

    /// Find which assembly defines System.Object etc.
    let systemRuntimeScopeRef =
      lazy
        referencedAssemblyPaths |> List.tryPick (fun path ->
          try
            let simpleName = Path.GetFileNameWithoutExtension path
            if simpleName = "mscorlib" || simpleName = "System.Runtime" then
                let reader = ILModuleReaderAfterReadingAllBytes (path, mkILGlobals EcmaMscorlibScopeRef)
                let mdef = reader.ILModuleDef
                match mdef.TypeDefs.TryFindByName(USome "System", "Object") with
                | None -> None
                | Some _ ->
                    let m = mdef.ManifestOfAssembly
                    let assRef = ILAssemblyRef(m.Name, None, (match m.PublicKey with Some k -> Some (PublicKey.KeyAsToken(k)) | None -> None), m.Retargetable, m.Version, m.Locale)
                    Some (ILScopeRef.Assembly assRef)
            else
                None
          with _ -> None )
        |> function
           | None -> EcmaMscorlibScopeRef // failwith "no reference to mscorlib.dll or System.Runtime.dll found"
           | Some r -> r

    let fsharpCoreRefVersion =
      lazy
        referencedAssemblyPaths |> List.tryPick (fun path ->
          try
            let simpleName = Path.GetFileNameWithoutExtension path
            if simpleName = "FSharp.Core" then
                let reader = ILModuleReaderAfterReadingAllBytes (path, mkILGlobals EcmaMscorlibScopeRef)
                match reader.ILModuleDef.Manifest with
                | Some m -> m.Version
                | None -> None
            else
                None
          with _ -> None )
        |> function
           | None -> typeof<int list>.Assembly.GetName().Version // failwith "no reference to FSharp.Core found"
           | Some r -> r

    let ilGlobals = lazy mkILGlobals (systemRuntimeScopeRef.Force())
    let readers =
        lazy ([| for ref in referencedAssemblyPaths ->
                  ref,lazy (try let reader = ILModuleReaderAfterReadingAllBytes(ref, ilGlobals.Force())
                                Choice1Of2(ContextAssembly(ilGlobals.Force(), this.TryBindAssembly, reader, ref))
                            with err -> Choice2Of2 err) |])
    let readersTable =  lazy ([| for (ref, asm) in readers.Force() do let simpleName = Path.GetFileNameWithoutExtension ref in yield simpleName, asm |] |> Map.ofArray)
    let referencedAssemblies = lazy ([| for (_,asm) in readers.Force() do match asm.Force() with Choice2Of2 _ -> () | Choice1Of2 asm -> yield asm :> Assembly |])

    let TryBindAssemblySimple(simpleName:string) : Choice<ContextAssembly, exn> =
        if readersTable.Force().ContainsKey(simpleName) then readersTable.Force().[simpleName].Force()
        else Choice2Of2 (Exception(sprintf "assembly %s not found" simpleName))

    let designTimeAssemblies =
        lazy
          [| yield Assembly.GetExecutingAssembly()
             for asm in Assembly.GetExecutingAssembly().GetReferencedAssemblies() do
                let asm = try Assembly.Load(asm) with _ -> null
                if asm <> null then
                    yield asm |]

    let replacer = AssemblyReplacer (designTimeAssemblies, referencedAssemblies, assemblyReplacementMap)

    // convToTgt is used to patch up some types like typeof<System.Array> reported by ProvidedTypes
    let convToTgt = replacer.ConvertDesignTimeTypeToTargetType
    let ptb = ZProvidedTypeBuilder(convToTgt)

    member __.TryBindAssembly(aref: ILAssemblyRef) : Choice<ContextAssembly, exn> = TryBindAssemblySimple(aref.Name)
    member __.TryBindAssembly(aref: AssemblyName) : Choice<ContextAssembly, exn> = TryBindAssemblySimple(aref.Name)
    member __.ReferencedAssemblyPaths = referencedAssemblyPaths
    member __.ReferencedAssemblies =  referencedAssemblies
    member x.TryGetFSharpCoreAssemblyVersion() = fsharpCoreRefVersion.Force()

    /// Create a new provided static parameter, for use with DefineStaticParamaeters on a provided type definition.
    ///
    /// When making a cross-targeting type provider, use this method instead of the ProvidedParameter constructor from ProvidedTypes
    member __.ProvidedStaticParameter(parameterName, parameterType, ?parameterDefaultValue) =
      new ProvidedStaticParameter(parameterName, parameterType, ?parameterDefaultValue=parameterDefaultValue)

    /// Create a new provided field. It is not initially associated with any specific provided type definition.
    ///
    /// When making a cross-targeting type provider, use this method instead of the ProvidedProperty constructor from ProvidedTypes
    member __.ProvidedField(fieldName, fieldType) =
      new ProvidedField(fieldName, fieldType  |> replacer.ConvertDesignTimeTypeToTargetType)

    /// Create a new provided literal field. It is not initially associated with any specific provided type definition.
    ///
    /// When making a cross-targeting type provider, use this method instead of the ProvidedProperty constructor from ProvidedTypes
    member __.ProvidedLiteralField(fieldName, fieldType, literalValue:obj) =
      new ProvidedLiteralField(fieldName, fieldType  |> replacer.ConvertDesignTimeTypeToTargetType, literalValue)

    /// Create a new provided parameter.
    ///
    /// When making a cross-targeting type provider, use this method instead of the ProvidedProperty constructor from ProvidedTypes
    member __.ProvidedParameter(parameterName, parameterType, ?isOut, ?optionalValue) =
      new ProvidedParameter(parameterName, parameterType |> replacer.ConvertDesignTimeTypeToTargetType, ?isOut = isOut, ?optionalValue = optionalValue)

    /// Create a new provided getter property. It is not initially associated with any specific provided type definition.
    ///
    /// When making a cross-targeting type provider, use this method instead of the ProvidedProperty constructor from ProvidedTypes
    member __.ProvidedProperty(propertyName, propertyType, getterCode, ?parameters) =
      new ProvidedProperty(propertyName, propertyType |> replacer.ConvertDesignTimeTypeToTargetType, GetterCode = (fun args -> args |> List.map replacer.ConvertTargetExprToDesignTimeExpr |> getterCode |> replacer.ConvertDesignTimeExprToTargetExpr), ?parameters=parameters)

    /// Create a new provided getter/setter property. It is not initially associated with any specific provided type definition.
    ///
    /// When making a cross-targeting type provider, use this method instead of the ProvidedProperty constructor from ProvidedTypes
    member __.ProvidedProperty(propertyName, propertyType, getterCode, setterCode, ?parameters) =
      new ProvidedProperty(propertyName, propertyType |> replacer.ConvertDesignTimeTypeToTargetType,
                           GetterCode = (fun args -> args |> List.map replacer.ConvertTargetExprToDesignTimeExpr |> getterCode |> replacer.ConvertDesignTimeExprToTargetExpr),
                           SetterCode = (fun args -> args |> List.map replacer.ConvertTargetExprToDesignTimeExpr |> setterCode |> replacer.ConvertDesignTimeExprToTargetExpr), ?parameters=parameters)

    /// Create a new provided event. It is not initially associated with any specific provided type definition.
    ///
    /// When making a cross-targeting type provider, use this method instead of the ProvidedProperty constructor from ProvidedTypes
    member __.ProvidedEvent(propertyName, eventHandlerType, adderCode, removerCode) =
      new ProvidedEvent(propertyName, eventHandlerType |> replacer.ConvertDesignTimeTypeToTargetType,
                           AdderCode = (fun args -> args |> List.map replacer.ConvertTargetExprToDesignTimeExpr |> adderCode |> replacer.ConvertDesignTimeExprToTargetExpr),
                           RemoverCode = (fun args -> args |> List.map replacer.ConvertTargetExprToDesignTimeExpr |> removerCode |> replacer.ConvertDesignTimeExprToTargetExpr))

    /// When making a cross-targeting type provider, use this method instead of the ProvidedConstructor constructor from ProvidedTypes
    member __.ProvidedConstructor(parameters) = 
      new ProvidedConstructor(parameters)

    /// When making a cross-targeting type provider, use this method instead of the ProvidedConstructor constructor from ProvidedTypes
    member __.ProvidedConstructor(parameters, invokeCode: Expr list -> Expr) =
      new ProvidedConstructor(parameters, InvokeCode = (fun args -> args |> List.map replacer.ConvertTargetExprToDesignTimeExpr |> invokeCode |> replacer.ConvertDesignTimeExprToTargetExpr))

    /// When making a cross-targeting type provider, use this method instead of the ProvidedMethod constructor from ProvidedTypes
    member __.ProvidedMethod(methodName, parameters, returnType: Type, invokeCode: Expr list -> Expr, ?isStatic: bool) =
      new ProvidedMethod(methodName, parameters, returnType |> replacer.ConvertDesignTimeTypeToTargetType, InvokeCode = (fun args -> args |> List.map replacer.ConvertTargetExprToDesignTimeExpr |> invokeCode |> replacer.ConvertDesignTimeExprToTargetExpr), IsStaticMethod = defaultArg isStatic false)

    /// When making a cross-targeting type provider, use this method instead of the corresponding ProvidedTypeDefinition constructor from ProvidedTypes
    member __.ProvidedTypeDefinition(className, baseType: Type option, ?hideObjectMethods: bool, ?nonNullable: bool) =
      new ProvidedTypeDefinition(className, baseType |> Option.map replacer.ConvertDesignTimeTypeToTargetType, convToTgt, HideObjectMethods = (defaultArg hideObjectMethods false), NonNullable = (defaultArg nonNullable false))

    /// When making a cross-targeting type provider, use this method instead of the corresponding ProvidedTypeDefinition constructor from ProvidedTypes
    member __.ProvidedTypeDefinition(assembly, namespaceName, className, baseType: Type option, ?hideObjectMethods: bool, ?nonNullable: bool) =
      new ProvidedTypeDefinition(assembly, namespaceName, className, baseType |> Option.map replacer.ConvertDesignTimeTypeToTargetType, convToTgt, HideObjectMethods = (defaultArg hideObjectMethods false), NonNullable = (defaultArg nonNullable false))

    /// When making a cross-targeting type provider, use this method instead of ProvidedTypeBuilder.MakeGenericType
    member __.MakeGenericType(genericTypeDefinition, genericArguments) = ptb.MakeGenericType(genericTypeDefinition, genericArguments)

    /// When making a cross-targeting type provider, use this method instead of ProvidedTypeBuilder.MakeGenericMethod
    member __.MakeGenericMethod(genericMethodDefinition, genericArguments) = ptb.MakeGenericMethod(genericMethodDefinition, genericArguments)

    static member Create (cfg : TypeProviderConfig, ?assemblyReplacementMap) =

        // Use the reflection hack to determine the set of referenced assemblies by reflecting over the SystemRuntimeContainsType
        // closure in the TypeProviderConfig object.
        let referencedAssemblyPaths =
          try
            if not (cfg.GetType().GetField("systemRuntimeContainsType",BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) <> null) then
                failwith "Invalid host of cross-targeting type provider: a field called systemRuntimeContainsType must exist in the TypeProviderConfiguration object. Please check that the type provider being hosted by the F# compiler tools or a simulation of them."

            let systemRuntimeContainsTypeObj = cfg.GetField("systemRuntimeContainsType")
            // Account for https://github.com/Microsoft/visualfsharp/pull/591
            let systemRuntimeContainsTypeObj2 =
                if systemRuntimeContainsTypeObj.HasField("systemRuntimeContainsTypeRef") then
                    systemRuntimeContainsTypeObj.GetField("systemRuntimeContainsTypeRef").GetProperty("Value")
                else
                    systemRuntimeContainsTypeObj
            if not (systemRuntimeContainsTypeObj2.HasField("tcImports")) then
                failwith "Invalid host of cross-targeting type provider: a field called tcImports must exist in the systemRuntimeContainsType closure. Please check that the type provider being hosted by the F# compiler tools or a simulation of them."
            let tcImports = systemRuntimeContainsTypeObj2.GetField("tcImports")
            if not (tcImports.HasField("dllInfos")) then
                failwith "Invalid host of cross-targeting type provider: a field called dllInfos must exist in the tcImports object. Please check that the type provider being hosted by the F# compiler tools or a simulation of them."
            if not (tcImports.HasProperty("Base")) then
                failwith "Invalid host of cross-targeting type provider: a field called Base must exist in the tcImports object. Please check that the type provider being hosted by the F# compiler tools or a simulation of them."
            let dllInfos = tcImports.GetField("dllInfos")
            let baseObj = tcImports.GetProperty("Base")

            [ for dllInfo in dllInfos.GetElements() -> (dllInfo.GetProperty("FileName") :?> string)
              for dllInfo in baseObj.GetProperty("Value").GetField("dllInfos").GetElements() -> (dllInfo.GetProperty("FileName") :?> string) ]
          with e ->
            failwith (sprintf "Invalid host of cross-targeting type provider. Exception: %A" e)


        ProvidedTypesContext(referencedAssemblyPaths, defaultArg assemblyReplacementMap Seq.empty)

