// Copyright (c) Microsoft Corporation, Tomas Petricek (http://tomasp.net), Gustavo Guerra (http://functionalflow.co.uk), and other contributors
// 
// Licensed under the Apache License, Version 2.0, see LICENSE.md in this project
// This sample code is provided "as is" without warranty of any kind.
// We disclaim all warranties, either express or implied, including the
// warranties of merchantability and fitness for a particular purpose.

#nowarn "49" // upercase parameter names

namespace ProviderImplementation.ProvidedTypes

    // This file contains a set of helper types and methods for providing types in an implementation
    // of ITypeProvider.
    //
    // This code has been modified and is appropriate for use in conjunction with the F# 4.x releases

    open System
    open System.Text
    open System.IO
    open System.Reflection
    open System.Linq.Expressions
    open System.Collections
    open System.Collections.Generic
    open System.Diagnostics

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Core.CompilerServices

    //--------------------------------------------------------------------------------
    // UncheckedQuotations

    // The FSharp.Core 2.0 - 4.0 (4.0.0.0 - 4.4.0.0) quotations implementation is overly strict in that it doesn't allow
    // generation of quotations for cross-targeted FSharp.Core.  Below we define a series of Unchecked methods
    // implemented via reflection hacks to allow creation of various nodes when using a cross-targets FSharp.Core and
    // mscorlib.dll.
    //
    //   - Most importantly, these cross-targeted quotations can be provided to the F# compiler by a type provider.
    //     They are generally produced via the AssemblyReplacer.fs component through a process of rewriting design-time quotations that
    //     are not cross-targeted.
    //
    //   - However, these quotation values are a bit fragile. Using existing FSharp.Core.Quotations.Patterns
    //     active patterns on these quotation nodes will generally work correctly. But using ExprShape.RebuildShapeCombination
    //     on these new nodes will not succed, nor will operations that build new quotations such as Expr.Call.
    //     Instead, use the replacement provided in this module.
    //
    //   - Likewise, some operations in these quotation values like "expr.Type" may be a bit fragile, possibly returning non cross-targeted types in
    //     the result. However those operations are not used by the F# compiler.
    [<AutoOpen>]
    module internal UncheckedQuotations =

        let qTy = typeof<Microsoft.FSharp.Quotations.Var>.Assembly.GetType("Microsoft.FSharp.Quotations.ExprConstInfo")
        assert (not (isNull qTy))
        let pTy = typeof<Microsoft.FSharp.Quotations.Var>.Assembly.GetType("Microsoft.FSharp.Quotations.PatternsModule")
        assert (not (isNull pTy))

        // These are handles to the internal functions that create quotation nodes of different sizes. Although internal,
        // these function names have been stable since F# 2.0.
        let mkFE0 = pTy.GetMethod("mkFE0", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (not (isNull mkFE0))
        let mkFE1 = pTy.GetMethod("mkFE1", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (not (isNull mkFE1))
        let mkFE2 = pTy.GetMethod("mkFE2", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (mkFE2 |> isNull |> not)
        let mkFEN = pTy.GetMethod("mkFEN", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (mkFEN |> isNull |> not)

        // These are handles to the internal tags attached to quotation nodes of different sizes. Although internal,
        // these function names have been stable since F# 2.0.
        let newDelegateOp = qTy.GetMethod("NewNewDelegateOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (newDelegateOp |> isNull |> not)
        let instanceCallOp = qTy.GetMethod("NewInstanceMethodCallOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (instanceCallOp |> isNull |> not)
        let staticCallOp = qTy.GetMethod("NewStaticMethodCallOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (staticCallOp |> isNull |> not)
        let newObjectOp = qTy.GetMethod("NewNewObjectOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (newObjectOp |> isNull |> not)
        let newArrayOp = qTy.GetMethod("NewNewArrayOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (newArrayOp |> isNull |> not)
        let appOp = qTy.GetMethod("get_AppOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (appOp |> isNull |> not)
        let instancePropGetOp = qTy.GetMethod("NewInstancePropGetOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (instancePropGetOp |> isNull |> not)
        let staticPropGetOp = qTy.GetMethod("NewStaticPropGetOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (staticPropGetOp |> isNull |> not)
        let instancePropSetOp = qTy.GetMethod("NewInstancePropSetOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (instancePropSetOp |> isNull |> not)
        let staticPropSetOp = qTy.GetMethod("NewStaticPropSetOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (staticPropSetOp |> isNull |> not)
        let instanceFieldGetOp = qTy.GetMethod("NewInstanceFieldGetOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (instanceFieldGetOp |> isNull |> not)
        let staticFieldGetOp = qTy.GetMethod("NewStaticFieldGetOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (staticFieldGetOp |> isNull |> not)
        let instanceFieldSetOp = qTy.GetMethod("NewInstanceFieldSetOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (instanceFieldSetOp |> isNull |> not)
        let staticFieldSetOp = qTy.GetMethod("NewStaticFieldSetOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (staticFieldSetOp |> isNull |> not)
        let tupleGetOp = qTy.GetMethod("NewTupleGetOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (tupleGetOp |> isNull |> not)
        let letOp = qTy.GetMethod("get_LetOp", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        assert (letOp |> isNull |> not)

        type Microsoft.FSharp.Quotations.Expr with

            static member NewDelegateUnchecked (ty: Type, vs: Var list, body: Expr) =
                let e =  List.foldBack (fun v acc -> Expr.Lambda(v,acc)) vs body
                let op = newDelegateOp.Invoke(null, [| box ty |])
                mkFE1.Invoke(null, [| box op; box e |]) :?> Expr

            static member NewObjectUnchecked (cinfo: ConstructorInfo, args: Expr list) =
                let op = newObjectOp.Invoke(null, [| box cinfo |])
                mkFEN.Invoke(null, [| box op; box args |]) :?> Expr

            static member NewArrayUnchecked (elementType: Type, elements: Expr list) =
                let op = newArrayOp.Invoke(null, [| box elementType |])
                mkFEN.Invoke(null, [| box op; box elements |]) :?> Expr

            static member CallUnchecked (minfo: MethodInfo, args: Expr list) =
                let op = staticCallOp.Invoke(null, [| box minfo |])
                mkFEN.Invoke(null, [| box op; box args |]) :?> Expr

            static member CallUnchecked (obj: Expr, minfo: MethodInfo, args: Expr list) =
                let op = instanceCallOp.Invoke(null, [| box minfo |])
                mkFEN.Invoke(null, [| box op; box (obj::args) |]) :?> Expr

            static member ApplicationUnchecked (f: Expr, x: Expr) =
                let op = appOp.Invoke(null, [| |])
                mkFE2.Invoke(null, [| box op; box f; box x |]) :?> Expr

            static member PropertyGetUnchecked (pinfo: PropertyInfo, args: Expr list) =
                let op = staticPropGetOp.Invoke(null, [| box pinfo |])
                mkFEN.Invoke(null, [| box op; box args |]) :?> Expr

            static member PropertyGetUnchecked (obj: Expr, pinfo: PropertyInfo, ?args: Expr list) =
                let args = defaultArg args []
                let op = instancePropGetOp.Invoke(null, [| box pinfo |])
                mkFEN.Invoke(null, [| box op; box (obj::args) |]) :?> Expr

            static member PropertySetUnchecked (pinfo: PropertyInfo, value: Expr, ?args: Expr list) =
                let args = defaultArg args []
                let op = staticPropSetOp.Invoke(null, [| box pinfo |])
                mkFEN.Invoke(null, [| box op; box (args@[value]) |]) :?> Expr

            static member PropertySetUnchecked (obj: Expr, pinfo: PropertyInfo, value: Expr, args: Expr list) =
                let op = instancePropSetOp.Invoke(null, [| box pinfo |])
                mkFEN.Invoke(null, [| box op; box (obj::(args@[value])) |]) :?> Expr

            static member FieldGetUnchecked (pinfo: FieldInfo) =
                let op = staticFieldGetOp.Invoke(null, [| box pinfo |])
                mkFE0.Invoke(null, [| box op; |]) :?> Expr

            static member FieldGetUnchecked (obj: Expr, pinfo: FieldInfo) =
                let op = instanceFieldGetOp.Invoke(null, [| box pinfo |])
                mkFE1.Invoke(null, [| box op; box obj |]) :?> Expr

            static member FieldSetUnchecked (pinfo: FieldInfo, value: Expr) =
                let op = staticFieldSetOp.Invoke(null, [| box pinfo |])
                mkFE1.Invoke(null, [| box op; box value |]) :?> Expr

            static member FieldSetUnchecked (obj: Expr, pinfo: FieldInfo, value: Expr) =
                let op = instanceFieldSetOp.Invoke(null, [| box pinfo |])
                mkFE2.Invoke(null, [| box op; box obj; box value |]) :?> Expr

            static member TupleGetUnchecked (e: Expr, n:int) =
                let op = tupleGetOp.Invoke(null, [| box e.Type; box n |])
                mkFE1.Invoke(null, [| box op; box e |]) :?> Expr

            static member LetUnchecked (v:Var, e: Expr, body:Expr) =
                let lam = Expr.Lambda(v,body)
                let op = letOp.Invoke(null, [| |])
                mkFE2.Invoke(null, [| box op; box e; box lam |]) :?> Expr

        type Shape = Shape of (Expr list -> Expr)

        let (|ShapeCombinationUnchecked|ShapeVarUnchecked|ShapeLambdaUnchecked|) e =
            match e with
            | NewObject (cinfo, args) ->
                ShapeCombinationUnchecked (Shape (function args -> Expr.NewObjectUnchecked (cinfo, args)), args)
            | NewArray (ty, args) ->
                ShapeCombinationUnchecked (Shape (function args -> Expr.NewArrayUnchecked (ty, args)), args)
            | NewDelegate (t, vars, expr) ->
                ShapeCombinationUnchecked (Shape (function [expr] -> Expr.NewDelegateUnchecked (t, vars, expr) | _ -> invalidArg "expr" "invalid shape"), [expr])
            | TupleGet (expr, n) ->
                ShapeCombinationUnchecked (Shape (function [expr] -> Expr.TupleGetUnchecked (expr, n) | _ -> invalidArg "expr" "invalid shape"), [expr])
            | Application (f, x) ->
                ShapeCombinationUnchecked (Shape (function [f; x] -> Expr.ApplicationUnchecked (f, x) | _ -> invalidArg "expr" "invalid shape"), [f; x])
            | Call (objOpt, minfo, args) ->
                match objOpt with
                | None -> ShapeCombinationUnchecked (Shape (function args -> Expr.CallUnchecked (minfo, args)), args)
                | Some obj -> ShapeCombinationUnchecked (Shape (function (obj::args) -> Expr.CallUnchecked (obj, minfo, args) | _ -> invalidArg "expr" "invalid shape"), obj::args)
            | PropertyGet (objOpt, pinfo, args) ->
                match objOpt with
                | None -> ShapeCombinationUnchecked (Shape (function args -> Expr.PropertyGetUnchecked (pinfo, args)), args)
                | Some obj -> ShapeCombinationUnchecked (Shape (function (obj::args) -> Expr.PropertyGetUnchecked (obj, pinfo, args) | _ -> invalidArg "expr" "invalid shape"), obj::args)
            | PropertySet (objOpt, pinfo, args, value) ->
                match objOpt with
                | None -> ShapeCombinationUnchecked (Shape (function (value::args) -> Expr.PropertySetUnchecked (pinfo, value, args) | _ -> invalidArg "expr" "invalid shape"), value::args)
                | Some obj -> ShapeCombinationUnchecked (Shape (function (obj::value::args) -> Expr.PropertySetUnchecked (obj, pinfo, value, args) | _ -> invalidArg "expr" "invalid shape"), obj::value::args)
            | FieldGet (objOpt, pinfo) ->
                match objOpt with
                | None -> ShapeCombinationUnchecked (Shape (function _ -> Expr.FieldGetUnchecked (pinfo)), [])
                | Some obj -> ShapeCombinationUnchecked (Shape (function [obj] -> Expr.FieldGetUnchecked (obj, pinfo) | _ -> invalidArg "expr" "invalid shape"), [obj])
            | FieldSet (objOpt, pinfo, value) ->
                match objOpt with
                | None -> ShapeCombinationUnchecked (Shape (function [value] -> Expr.FieldSetUnchecked (pinfo, value) | _ -> invalidArg "expr" "invalid shape"), [value])
                | Some obj -> ShapeCombinationUnchecked (Shape (function [obj;value] -> Expr.FieldSetUnchecked (obj, pinfo, value) | _ -> invalidArg "expr" "invalid shape"), [obj; value])
            | Let (var, value, body) ->
                ShapeCombinationUnchecked (Shape (function [value;Lambda(var, body)] -> Expr.LetUnchecked(var, value, body) | _ -> invalidArg "expr" "invalid shape"), [value; Expr.Lambda(var, body)])
            | TupleGet (expr, i) ->
                ShapeCombinationUnchecked (Shape (function [expr] -> Expr.TupleGetUnchecked (expr, i) | _ -> invalidArg "expr" "invalid shape"), [expr])
            | ExprShape.ShapeCombination (comb,args) ->
                ShapeCombinationUnchecked (Shape (fun args -> ExprShape.RebuildShapeCombination(comb, args)), args)
            | ExprShape.ShapeVar v -> ShapeVarUnchecked v
            | ExprShape.ShapeLambda (v, e) -> ShapeLambdaUnchecked (v,e)

        let RebuildShapeCombinationUnchecked (Shape comb,args) = comb args

    //--------------------------------------------------------------------------------
    // The quotation simplifier
    //
    // This is invoked for each quotation specified by the type provider, just before it is
    // handed to the F# compiler, allowing a broader range of
    // quotations to be accepted. Specifically accept:
    //
    //     - NewTuple nodes (for generative type providers)
    //     - TupleGet nodes (for generative type providers)
    //     - array and list values as constants
    //     - PropertyGet and PropertySet nodes
    //     - Application, NewUnionCase, NewRecord, UnionCaseTest nodes
    //     - Let nodes (defining "byref" values)
    //     - LetRecursive nodes
    //
    // Additionally, a set of code optimizations are applied to generated code:
    //    - inlineRightPipe
    //    - optimizeCurriedApplications
    //    - inlineValueBindings

    type QuotationSimplifier(isGenerated: bool, convToTgt: Type -> Type) =

        let rec transExpr q =
            match q with
            // convert NewTuple to the call to the constructor of the Tuple type (only for generated types)
            | NewTuple(items) when isGenerated ->
                let rec mkCtor args ty =
                    let ctor, restTyOpt = Reflection.FSharpValue.PreComputeTupleConstructorInfo ty
                    match restTyOpt with
                    | None -> Expr.NewObject(ctor, List.map transExpr args)
                    | Some restTy ->
                        let curr = [for a in Seq.take 7 args -> transExpr a]
                        let rest = List.ofSeq (Seq.skip 7 args)
                        Expr.NewObject(ctor, curr @ [mkCtor rest restTy])
                let tys = [| for e in items -> e.Type |]
                let tupleTy = Reflection.FSharpType.MakeTupleType tys
                transExpr (mkCtor items tupleTy)
            // convert TupleGet to the chain of PropertyGet calls (only for generated types)
            | TupleGet(e, i) when isGenerated ->
                let rec mkGet ty i (e: Expr)  =
                    let pi, restOpt = Reflection.FSharpValue.PreComputeTuplePropertyInfo(ty, i)
                    let propGet = Expr.PropertyGet(e, pi)
                    match restOpt with
                    | None -> propGet
                    | Some (restTy, restI) -> mkGet restTy restI propGet
                transExpr (mkGet e.Type i (transExpr e))
            | Value(value, ty) ->
                if value |> isNull |> not then
                    let tyOfValue = value.GetType()
                    transValue(value, tyOfValue, ty)
                else q
            // Eliminate F# property gets to method calls
            | PropertyGet(obj,propInfo,args) ->
                match obj with
                | None -> transExpr (Expr.CallUnchecked(propInfo.GetGetMethod(),args))
                | Some o -> transExpr (Expr.CallUnchecked(transExpr o,propInfo.GetGetMethod(),args))
            // Eliminate F# property sets to method calls
            | PropertySet(obj,propInfo,args,v) ->
                    match obj with
                    | None -> transExpr (Expr.CallUnchecked(propInfo.GetSetMethod(),args@[v]))
                    | Some o -> transExpr (Expr.CallUnchecked(transExpr o,propInfo.GetSetMethod(),args@[v]))
            // Eliminate F# function applications to FSharpFunc<_,_>.Invoke calls
            | Application(f,e) ->
                transExpr (Expr.CallUnchecked(transExpr f, f.Type.GetMethod "Invoke", [ e ]) )
            | NewUnionCase(ci, es) ->
                transExpr (Expr.CallUnchecked(Reflection.FSharpValue.PreComputeUnionConstructorInfo ci, es) )
            | NewRecord(ci, es) ->
                transExpr (Expr.NewObjectUnchecked(Reflection.FSharpValue.PreComputeRecordConstructorInfo ci, es) )
            | UnionCaseTest(e,uc) ->
                let tagInfo = Reflection.FSharpValue.PreComputeUnionTagMemberInfo uc.DeclaringType
                let tagExpr =
                    match tagInfo with
                    | :? PropertyInfo as tagProp ->
                            transExpr (Expr.PropertyGet(e,tagProp) )
                    | :? MethodInfo as tagMeth ->
                            if tagMeth.IsStatic then transExpr (Expr.Call(tagMeth, [e]))
                            else transExpr (Expr.Call(e,tagMeth,[]))
                    | _ -> failwith "unreachable: unexpected result from PreComputeUnionTagMemberInfo"
                let tagNumber = uc.Tag
                transExpr <@@ (%%(tagExpr): int) = tagNumber @@>

            // Explicitly handle weird byref variables in lets (used to populate out parameters), since the generic handlers can't deal with byrefs.
            //
            // The binding must have leaves that are themselves variables (due to the limited support for byrefs in expressions)
            // therefore, we can perform inlining to translate this to a form that can be compiled
            | Let(v,vexpr,bexpr) when v.Type.IsByRef -> transLetOfByref v vexpr bexpr

            // Eliminate recursive let bindings (which are unsupported by the type provider API) to regular let bindings
            | LetRecursive(bindings, expr) -> transLetRec bindings expr

            // Handle the generic cases
            | ShapeLambdaUnchecked(v,body) -> Expr.Lambda(v, transExpr body)
            | ShapeCombinationUnchecked(comb,args) -> RebuildShapeCombinationUnchecked(comb,List.map transExpr args)
            | ShapeVarUnchecked _ -> q

        and transLetRec bindings expr =
                // This uses a "lets and sets" approach, converting something like
                //    let rec even = function
                //    | 0 -> true
                //    | n -> odd (n-1)
                //    and odd = function
                //    | 0 -> false
                //    | n -> even (n-1)
                //    X
                // to something like
                //    let even = ref Unchecked.defaultof<_>
                //    let odd = ref Unchecked.defaultof<_>
                //    even := function
                //            | 0 -> true
                //            | n -> !odd (n-1)
                //    odd  := function
                //            | 0 -> false
                //            | n -> !even (n-1)
                //    X'
                // where X' is X but with occurrences of even/odd substituted by !even and !odd (since now even and odd are references)
                // Translation relies on typedefof<_ ref> - does this affect ability to target different runtime and design time environments?
                let vars = List.map fst bindings
                let vars' = vars |> List.map (fun v -> Quotations.Var(v.Name, typedefof<_ ref>.MakeGenericType(v.Type)))

                // "init t" generates the equivalent of <@ ref Unchecked.defaultof<t> @>
                let init (t:Type) =
                    let r = match <@ ref 1 @> with Call(None, r, [_]) -> r | _ -> failwith "Extracting MethodInfo from <@ 1 @> failed"
                    let d = match <@ Unchecked.defaultof<_> @> with Call(None, d, []) -> d | _ -> failwith "Extracting MethodInfo from <@ Unchecked.defaultof<_> @> failed"
                    Expr.Call(r.GetGenericMethodDefinition().MakeGenericMethod(t), [Expr.Call(d.GetGenericMethodDefinition().MakeGenericMethod(t),[])])

                // deref v generates the equivalent of <@ !v @>
                // (so v's type must be ref<something>)
                let deref (v:Quotations.Var) =
                    let m = match <@ !(ref 1) @> with Call(None, m, [_]) -> m | _ -> failwith "Extracting MethodInfo from <@ !(ref 1) @> failed"
                    let tyArgs = v.Type.GetGenericArguments()
                    Expr.Call(m.GetGenericMethodDefinition().MakeGenericMethod(tyArgs), [Expr.Var v])

                // substitution mapping a variable v to the expression <@ !v' @> using the corresponding new variable v' of ref type
                let subst =
                    let map =
                        vars'
                        |> List.map deref
                        |> List.zip vars
                        |> Map.ofList
                    fun v -> Map.tryFind v map

                let expr' = expr.Substitute(subst)

                // maps variables to new variables
                let varDict = List.zip vars vars' |> dict

                // given an old variable v and an expression e, returns a quotation like <@ v' := e @> using the corresponding new variable v' of ref type
                let setRef (v:Quotations.Var) e =
                    let m = match <@ (ref 1) := 2 @> with Call(None, m, [_;_]) -> m | _ -> failwith "Extracting MethodInfo from <@ (ref 1) := 2 @> failed"
                    Expr.Call(m.GetGenericMethodDefinition().MakeGenericMethod(v.Type), [Expr.Var varDict.[v]; e])

                // Something like
                //  <@
                //      v1 := e1'
                //      v2 := e2'
                //      ...
                //      expr'
                //  @>
                // Note that we must substitute our new variable dereferences into the bound expressions
                let body =
                    bindings
                    |> List.fold (fun b (v,e) -> Expr.Sequential(setRef v (e.Substitute subst), b)) expr'

                // Something like
                //   let v1 = ref Unchecked.defaultof<t1>
                //   let v2 = ref Unchecked.defaultof<t2>
                //   ...
                //   body
                vars
                |> List.fold (fun b v -> Expr.LetUnchecked(varDict.[v], init v.Type, b)) body
                |> transExpr


        and transLetOfByref v vexpr bexpr =
            match vexpr with
            | Sequential(e',vexpr') ->
                (* let v = (e'; vexpr') in bexpr => e'; let v = vexpr' in bexpr *)
                Expr.Sequential(e', transLetOfByref v vexpr' bexpr)
                |> transExpr
            | IfThenElse(c,b1,b2) ->
                (* let v = if c then b1 else b2 in bexpr => if c then let v = b1 in bexpr else let v = b2 in bexpr *)
                //
                // Note, this duplicates "bexpr"
                Expr.IfThenElse(c, transLetOfByref v b1 bexpr, transLetOfByref v b2 bexpr)
                |> transExpr
            | Var _ ->
                (* let v = v1 in bexpr => bexpr[v/v1] *)
                bexpr.Substitute(fun v' -> if v = v' then Some vexpr else None)
                |> transExpr
            | _ ->
                failwith (sprintf "Unexpected byref binding: %A = %A" v vexpr)

        and transValueArray (o: Array, ty: Type) =
            let elemTy = ty.GetElementType()
            let converter = getValueConverterForType elemTy
            let elements = [ for el in o -> converter el ]
            Expr.NewArrayUnchecked(elemTy, elements)

        and transValueList(o, ty: Type, nil, cons) =
            let converter = getValueConverterForType (ty.GetGenericArguments().[0])
            o
            |> Seq.cast
            |> List.ofSeq
            |> fun l -> List.foldBack(fun o s -> Expr.NewUnionCase(cons, [ converter(o); s ])) l (Expr.NewUnionCase(nil, []))
            |> transExpr

        and getValueConverterForType (ty: Type) =
            if ty.IsArray then
                fun (v: obj) -> transValueArray(v :?> Array, ty)
            elif ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<_ list> then
                let nil, cons =
                    let cases = Reflection.FSharpType.GetUnionCases(ty)
                    let a = cases.[0]
                    let b = cases.[1]
                    if a.Name = "Empty" then a,b
                    else b,a

                fun v -> transValueList (v :?> IEnumerable, ty, nil, cons)
            else
                fun v -> Expr.Value(v, ty)

        and transValue (v: obj, tyOfValue: Type, expectedTy: Type) =
            let converter = getValueConverterForType tyOfValue
            let r = converter v
            if tyOfValue <> expectedTy then Expr.Coerce(r, expectedTy)
            else r

    #if !NO_GENERATIVE
        let getFastFuncType (args: list<Expr>) resultType =
            let types =
                [|  for arg in args -> arg.Type
                    yield resultType |]
            let fastFuncTy =
                match List.length args with
                | 2 -> typedefof<OptimizedClosures.FSharpFunc<_, _, _>>.MakeGenericType(types) |> convToTgt
                | 3 -> typedefof<OptimizedClosures.FSharpFunc<_, _, _, _>>.MakeGenericType(types) |> convToTgt
                | 4 -> typedefof<OptimizedClosures.FSharpFunc<_, _, _, _, _>>.MakeGenericType(types) |> convToTgt
                | 5 -> typedefof<OptimizedClosures.FSharpFunc<_, _, _, _, _, _>>.MakeGenericType(types) |> convToTgt
                | _ -> invalidArg "args" "incorrect number of arguments"
            fastFuncTy.GetMethod("Adapt")

        let (===) a b = LanguagePrimitives.PhysicalEquality a b

        let traverse f =
            let rec fallback e =
                match e with
                | Let(v, value, body) ->
                    let fixedValue = f fallback value
                    let fixedBody = f fallback body
                    if fixedValue === value && fixedBody === body then
                        e
                    else
                        Expr.Let(v, fixedValue, fixedBody)
                | ShapeVarUnchecked _ -> e
                | ShapeLambdaUnchecked(v, body) ->
                    let fixedBody = f fallback body
                    if fixedBody === body then
                        e
                    else
                        Expr.Lambda(v, fixedBody)
                | ShapeCombinationUnchecked(shape, exprs) ->
                    let exprs1 = List.map (f fallback) exprs
                    if List.forall2 (===) exprs exprs1 then
                        e
                    else
                        RebuildShapeCombinationUnchecked(shape, exprs1)
            fun e -> f fallback e

        // TODO: this works over design-time quotations. Thes quotation need to be retargeted to the target runtime.
        let rightPipe = <@@ (|>) @@>
        let inlineRightPipe expr =
            let rec loop expr = traverse loopCore expr
            and loopCore fallback orig =
                match orig with
                | SpecificCall rightPipe (None, _, [operand; applicable]) ->
                    let fixedOperand = loop operand
                    match loop applicable with
                    | Lambda(arg, body) ->
                        let v = Quotations.Var("__temp", operand.Type)
                        let ev = Expr.Var v

                        let fixedBody = loop body
                        Expr.Let(v, fixedOperand, fixedBody.Substitute(fun v1 -> if v1 = arg then Some ev else None))
                    | fixedApplicable -> Expr.Application(fixedApplicable, fixedOperand)
                | x -> fallback x
            loop expr

        let inlineValueBindings e =
            let map = Dictionary(HashIdentity.Reference)
            let rec loop expr = traverse loopCore expr
            and loopCore fallback orig =
                match orig with
                | Let(id, (Value(_) as v), body) when not id.IsMutable ->
                    map.[id] <- v
                    let fixedBody = loop body
                    map.Remove(id) |> ignore
                    fixedBody
                | ShapeVarUnchecked v ->
                    match map.TryGetValue v with
                    | true, e -> e
                    | _ -> orig
                | x -> fallback x
            loop e


        let optimizeCurriedApplications expr =
            let rec loop expr = traverse loopCore expr
            and loopCore fallback orig =
                match orig with
                | Application(e, arg) ->
                    let e1 = tryPeelApplications e [loop arg]
                    if e1 === e then
                        orig
                    else
                        e1
                | x -> fallback x
            and tryPeelApplications orig args =
                let n = List.length args
                match orig with
                | Application(e, arg) ->
                    let e1 = tryPeelApplications e ((loop arg)::args)
                    if e1 === e then
                        orig
                    else
                        e1
                | Let(id, applicable, (Lambda(_) as body)) when n > 0 ->
                    let numberOfApplication = countPeelableApplications body id 0
                    if numberOfApplication = 0 then orig
                    elif n = 1 then Expr.Application(applicable, List.head args)
                    elif n <= 5 then
                        let resultType =
                            applicable.Type
                            |> Seq.unfold (fun t ->
                                if not t.IsGenericType then None else
                                let args = t.GetGenericArguments()
                                if args.Length <> 2 then None else
                                Some (args.[1], args.[1])
                            )
                            |> Seq.toArray
                            |> (fun arr -> arr.[n - 1])

                        let adaptMethod = getFastFuncType args resultType
                        let adapted = Expr.Call(adaptMethod, [loop applicable])
                        let invoke = adapted.Type.GetMethod("Invoke", [| for arg in args -> arg.Type |])
                        Expr.Call(adapted, invoke, args)
                    else
                        (applicable, args) ||> List.fold (fun e a -> Expr.Application(e, a))
                | _ ->
                    orig
            and countPeelableApplications expr v n =
                match expr with
                // v - applicable entity obtained on the prev step
                // \arg -> let v1 = (f arg) in rest ==> f
                | Lambda(arg, Let(v1, Application(Var f, Var arg1), rest)) when v = f && arg = arg1 -> countPeelableApplications rest v1 (n + 1)
                // \arg -> (f arg) ==> f
                | Lambda(arg, Application(Var f, Var arg1)) when v = f && arg = arg1 -> n
                | _ -> n
            loop expr
    #endif

        member __.TranslateExpression q = transExpr q

        member __.TranslateQuotationToCode qexprf (paramNames: string[]) (argExprs: Expr[]) =
            // Use the real variable names instead of indices, to improve output of Debug.fs
            // Add let bindings for arguments to ensure that arguments will be evaluated
            let vars = argExprs |> Array.mapi (fun i e -> Quotations.Var(paramNames.[i], e.Type))
            let expr = qexprf ([for v in vars -> Expr.Var v])

            let pairs = Array.zip argExprs vars
            let expr = Array.foldBack (fun (arg, var) e -> Expr.LetUnchecked(var, arg, e)) pairs expr
    #if !NO_GENERATIVE
            let expr =
                if isGenerated then
                    let e1 = inlineRightPipe expr
                    let e2 = optimizeCurriedApplications e1
                    let e3 = inlineValueBindings e2
                    e3
                else
                    expr
    #endif

            transExpr expr

    //--------------------------------------------------------------------------------
    // ProvidedMethod, ProvidedConstructor, ProvidedTypeDefinition and other provided objects


    [<AutoOpen>]
    module internal Misc =

        /// Internal code of .NET expects the obj[] returned by GetCustomAttributes to be an Attribute[] even in the case of empty arrays
        let emptyAttributes = (([| |] : Attribute[]) |> box |> unbox<obj[]>)

        let nonNull str x = if isNull x then failwith ("Null in " + str) else x

        let notRequired opname item =
            let msg = sprintf "The operation '%s' on item '%s' should not be called on provided type, member or parameter" opname item
            Debug.Assert (false, msg)
            raise (NotSupportedException msg)

        let mkParamArrayCustomAttributeData() =
            { new CustomAttributeData() with
                member __.Constructor =  typeof<ParamArrayAttribute>.GetConstructors().[0]
                member __.ConstructorArguments = upcast [| |]
                member __.NamedArguments = upcast [| |] }

        let mkEditorHideMethodsCustomAttributeData() =
            { new CustomAttributeData() with
                member __.Constructor =  typeof<TypeProviderEditorHideMethodsAttribute>.GetConstructors().[0]
                member __.ConstructorArguments = upcast [| |]
                member __.NamedArguments = upcast [| |] }

        let mkAllowNullLiteralCustomAttributeData value =
            { new CustomAttributeData() with
                member __.Constructor = typeof<AllowNullLiteralAttribute>.GetConstructors().[0]
                member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<bool>, value) |]
                member __.NamedArguments = upcast [| |] }

        /// This makes an xml doc attribute w.r.t. an amortized computation of an xml doc string.
        /// It is important that the text of the xml doc only get forced when poking on the ConstructorArguments
        /// for the CustomAttributeData object.
        let mkXmlDocCustomAttributeDataLazy(lazyText: Lazy<string>) =
            { new CustomAttributeData() with
                member __.Constructor =  typeof<TypeProviderXmlDocAttribute>.GetConstructors().[0]
                member __.ConstructorArguments = upcast [| CustomAttributeTypedArgument(typeof<string>, lazyText.Force())  |]
                member __.NamedArguments = upcast [| |] }

        let mkXmlDocCustomAttributeData(s:string) =  mkXmlDocCustomAttributeDataLazy (lazy s)

        let mkDefinitionLocationAttributeCustomAttributeData(line:int,column:int,filePath:string) =
            { new CustomAttributeData() with
                member __.Constructor =  typeof<TypeProviderDefinitionLocationAttribute>.GetConstructors().[0]
                member __.ConstructorArguments = upcast [| |]
                member __.NamedArguments =
                    upcast [| CustomAttributeNamedArgument(typeof<TypeProviderDefinitionLocationAttribute>.GetProperty("FilePath"), CustomAttributeTypedArgument(typeof<string>, filePath));
                                CustomAttributeNamedArgument(typeof<TypeProviderDefinitionLocationAttribute>.GetProperty("Line"), CustomAttributeTypedArgument(typeof<int>, line)) ;
                                CustomAttributeNamedArgument(typeof<TypeProviderDefinitionLocationAttribute>.GetProperty("Column"), CustomAttributeTypedArgument(typeof<int>, column))
                            |] }
        let mkObsoleteAttributeCustomAttributeData(message:string, isError: bool) =
            { new CustomAttributeData() with
                    member __.Constructor =  typeof<ObsoleteAttribute>.GetConstructors() |> Array.find (fun x -> x.GetParameters().Length = 2)
                    member __.ConstructorArguments = upcast [|CustomAttributeTypedArgument(typeof<string>, message) ; CustomAttributeTypedArgument(typeof<bool>, isError)  |]
                    member __.NamedArguments = upcast [| |] }

        let mkReflectedDefinitionCustomAttributeData() =
            { new CustomAttributeData() with
                    member __.Constructor =  typeof<ReflectedDefinitionAttribute>.GetConstructors().[0]
                    member __.ConstructorArguments = upcast [| |]
                    member __.NamedArguments = upcast [| |] }

        type CustomAttributesImpl() =
            let customAttributes = ResizeArray<CustomAttributeData>()
            let mutable hideObjectMethods = false
            let mutable nonNullable = false
            let mutable obsoleteMessage = None
            let mutable xmlDocDelayed = None
            let mutable xmlDocAlwaysRecomputed = None
            let mutable hasParamArray = false
            let mutable hasReflectedDefinition = false

            // XML doc text that we only compute once, if any. This must _not_ be forced until the ConstructorArguments
            // property of the custom attribute is foced.
            let xmlDocDelayedText =
                lazy
                    (match xmlDocDelayed with None -> assert false; "" | Some f -> f())

            // Custom atttributes that we only compute once
            let customAttributesOnce =
                lazy
                   [| if hideObjectMethods then yield mkEditorHideMethodsCustomAttributeData()
                      if nonNullable then yield mkAllowNullLiteralCustomAttributeData false
                      match xmlDocDelayed with None -> () | Some _ -> customAttributes.Add(mkXmlDocCustomAttributeDataLazy xmlDocDelayedText)
                      match obsoleteMessage with None -> () | Some s -> customAttributes.Add(mkObsoleteAttributeCustomAttributeData s)
                      if hasParamArray then yield mkParamArrayCustomAttributeData()
                      if hasReflectedDefinition then yield mkReflectedDefinitionCustomAttributeData()
                      yield! customAttributes |]

            member __.AddDefinitionLocation(line:int,column:int,filePath:string) = customAttributes.Add(mkDefinitionLocationAttributeCustomAttributeData(line, column, filePath))
            member __.AddObsolete(message: string, isError) = obsoleteMessage <- Some (message,isError)
            member __.HasParamArray with get() = hasParamArray and set(v) = hasParamArray <- v
            member __.HasReflectedDefinition with get() = hasReflectedDefinition and set(v) = hasReflectedDefinition <- v
            member __.AddXmlDocComputed xmlDocFunction = xmlDocAlwaysRecomputed <- Some xmlDocFunction
            member __.AddXmlDocDelayed xmlDocFunction = xmlDocDelayed <- Some xmlDocFunction
            member __.AddXmlDoc xmlDoc =  xmlDocDelayed <- Some (fun () -> xmlDoc)
            member __.HideObjectMethods with get() = hideObjectMethods and set v = hideObjectMethods <- v
            member __.NonNullable with get () = nonNullable and set v = nonNullable <- v
            member __.AddCustomAttribute(attribute) = customAttributes.Add(attribute)
            member __.GetCustomAttributesData() =
                [| yield! customAttributesOnce.Force()
                   // Recomputed XML doc is evaluated on every call to GetCustomAttributesData()
                   match xmlDocAlwaysRecomputed with None -> () | Some f -> yield mkXmlDocCustomAttributeData (f())  |]
                :> IList<_>


        let adjustTypeAttributes attributes isNested =
            let visibilityAttributes =
                match attributes &&& TypeAttributes.VisibilityMask with
                | TypeAttributes.Public when isNested -> TypeAttributes.NestedPublic
                | TypeAttributes.NotPublic when isNested -> TypeAttributes.NestedAssembly
                | TypeAttributes.NestedPublic when not isNested -> TypeAttributes.Public
                | TypeAttributes.NestedAssembly
                | TypeAttributes.NestedPrivate
                | TypeAttributes.NestedFamORAssem
                | TypeAttributes.NestedFamily
                | TypeAttributes.NestedFamANDAssem when not isNested -> TypeAttributes.NotPublic
                | a -> a
            (attributes &&& ~~~TypeAttributes.VisibilityMask) ||| visibilityAttributes



    type ProvidedStaticParameter(parameterName:string,parameterType:Type,?parameterDefaultValue:obj) =
        inherit ParameterInfo()

        let customAttributesImpl = CustomAttributesImpl()

        member __.AddXmlDocDelayed xmlDocFunction = customAttributesImpl.AddXmlDocDelayed xmlDocFunction
        member __.AddXmlDocComputed xmlDocFunction = customAttributesImpl.AddXmlDocComputed xmlDocFunction
        member __.AddXmlDoc xmlDoc = customAttributesImpl.AddXmlDoc xmlDoc

        override __.RawDefaultValue = defaultArg parameterDefaultValue null
        override __.Attributes = if parameterDefaultValue.IsNone then enum 0 else ParameterAttributes.Optional
        override __.Position = 0
        override __.ParameterType = parameterType
        override __.Name = parameterName

        override __.GetCustomAttributes(_inherit) = emptyAttributes
        override __.GetCustomAttributes(_attributeType, _inherit) = emptyAttributes

    type ProvidedParameter(parameterName:string, parameterType:Type, isOut:bool, optionalValue:obj option) =
        inherit ParameterInfo()
        let customAttributesImpl = CustomAttributesImpl()
        member __.IsParamArray with get() = customAttributesImpl.HasParamArray and set(v) = customAttributesImpl.HasParamArray <- v
        member __.IsReflectedDefinition with get() = customAttributesImpl.HasReflectedDefinition and set(v) = customAttributesImpl.HasReflectedDefinition <- v
        override __.Name = parameterName
        override __.ParameterType = parameterType
        override __.Attributes = (base.Attributes ||| (if isOut then ParameterAttributes.Out else enum 0)
                                                  ||| (match optionalValue with None -> enum 0 | Some _ -> ParameterAttributes.Optional ||| ParameterAttributes.HasDefault))
        override __.RawDefaultValue = defaultArg optionalValue null
        member __.HasDefaultParameterValue = Option.isSome optionalValue
        override __.GetCustomAttributesData() = customAttributesImpl.GetCustomAttributesData()

    type ProvidedConstructor(parameters: ProvidedParameter list, invokeCode : option<Expr list -> Expr>) =
        inherit ConstructorInfo()
        let parameters = parameters |> List.map (fun p -> p :> ParameterInfo)
        let mutable baseCall = None

        let mutable declaringType = null: Type
        let mutable isImplicitCtor = false
        let mutable ctorAttributes = MethodAttributes.Public ||| MethodAttributes.RTSpecialName
        let nameText () = sprintf "constructor for %s" (if isNull declaringType then "<not yet known type>" else declaringType.FullName)
        let isStatic() = ctorAttributes.HasFlag(MethodAttributes.Static)

        let customAttributesImpl = CustomAttributesImpl()
        member __.IsTypeInitializer
            with get() = isStatic() && ctorAttributes.HasFlag(MethodAttributes.Private)
            and set(v) =
                let typeInitializerAttributes = MethodAttributes.Static ||| MethodAttributes.Private
                ctorAttributes <- if v then ctorAttributes ||| typeInitializerAttributes else ctorAttributes &&& ~~~typeInitializerAttributes

        member __.AddXmlDocComputed xmlDocFunction = customAttributesImpl.AddXmlDocComputed xmlDocFunction
        member __.AddXmlDocDelayed xmlDocFunction = customAttributesImpl.AddXmlDocDelayed xmlDocFunction
        member __.AddXmlDoc xmlDoc = customAttributesImpl.AddXmlDoc xmlDoc
        member __.AddObsoleteAttribute (message,?isError) = customAttributesImpl.AddObsolete (message,defaultArg isError false)
        member __.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
        override __.GetCustomAttributesData() = customAttributesImpl.GetCustomAttributesData()

        member __.DeclaringTypeImpl
            with set x =
                if not (isNull declaringType) then failwith (sprintf "ProvidedConstructor: declaringType already set on '%s'" (nameText()));
                declaringType <- x

        member __.BaseConstructorCall
            with set (d:Expr list -> (ConstructorInfo * Expr list)) =
                match baseCall with
                | None -> baseCall <- Some d
                | Some _ -> failwith (sprintf "ProvidedConstructor: base call already given for '%s'" (nameText()))

        member __.GetInvokeCodeInternal(isGenerated, convToTgt) =
            match invokeCode with
            | Some f ->
                // FSharp.Data change: use the real variable names instead of indices, to improve output of Debug.fs
                let paramNames =
                    parameters
                    |> List.map (fun p -> p.Name)
                    |> List.append (if not isGenerated || isStatic() then [] else ["this"])
                    |> Array.ofList
                QuotationSimplifier(isGenerated, convToTgt).TranslateQuotationToCode f paramNames
            | None -> failwith (sprintf "ProvidedConstructor: no invoker for '%s'" (nameText()))

        member __.GetBaseConstructorCallInternal(isGenerated, convToTgt) =
            match baseCall with
            | Some f -> 
                Some(fun ctorArgs -> 
                    let c,baseCtorArgExprs = f ctorArgs 
                    c, List.map (QuotationSimplifier(isGenerated, convToTgt).TranslateExpression) baseCtorArgExprs)
            | None -> None

        member __.IsImplicitCtor with get() = isImplicitCtor and set v = isImplicitCtor <- v

        // Implement overloads
        override __.GetParameters() = parameters |> List.toArray
        override __.Attributes = ctorAttributes
        override __.Name = if isStatic() then ".cctor" else ".ctor"
        override __.DeclaringType = declaringType |> nonNull "ProvidedConstructor.DeclaringType"
        override __.IsDefined(_attributeType, _inherit) = true

        override __.Invoke(_invokeAttr, _binder, _parameters, _culture) = notRequired "Invoke" (nameText())
        override __.Invoke(_obj, _invokeAttr, _binder, _parameters, _culture) = notRequired "Invoke" (nameText())
        override __.ReflectedType = notRequired "ReflectedType" (nameText())
        override __.GetMethodImplementationFlags() = notRequired "GetMethodImplementationFlags" (nameText())
        override __.MethodHandle = notRequired "MethodHandle" (nameText())
        override __.GetCustomAttributes(_inherit) = emptyAttributes
        override __.GetCustomAttributes(_attributeType, _inherit) = emptyAttributes

    type ProvidedMethod(methodName: string, parameters: ProvidedParameter list, returnType: Type, isStatic: bool, invokeCode : option<Expr list -> Expr>) =
        inherit MethodInfo()
        let argParams = parameters |> List.map (fun p -> p :> ParameterInfo)

        // State
        let mutable declaringType: Type = null
        let mutable methodAttrs = if isStatic then MethodAttributes.Public ||| MethodAttributes.Static else MethodAttributes.Public
        let mutable staticParams = [ ]
        let mutable staticParamsApply = None
        let customAttributesImpl = CustomAttributesImpl()

        member __.AddXmlDocComputed xmlDocFunction = customAttributesImpl.AddXmlDocComputed xmlDocFunction
        member __.AddXmlDocDelayed xmlDocFunction = customAttributesImpl.AddXmlDocDelayed xmlDocFunction
        member __.AddXmlDoc xmlDoc = customAttributesImpl.AddXmlDoc xmlDoc
        member __.AddObsoleteAttribute (message,?isError) = customAttributesImpl.AddObsolete (message,defaultArg isError false)
        member __.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
        member __.AddCustomAttribute(attribute) = customAttributesImpl.AddCustomAttribute(attribute)
        override __.GetCustomAttributesData() = customAttributesImpl.GetCustomAttributesData()

        member __.SetMethodAttrs attributes = methodAttrs <- attributes
        member __.AddMethodAttrs attributes = methodAttrs <- methodAttrs ||| attributes
        member __.DeclaringTypeImpl with set x = declaringType <- x // check: not set twice


        /// Abstract a type to a parametric-type. Requires "formal parameters" and "instantiation function".
        member __.DefineStaticParameters(parameters: list<ProvidedStaticParameter>, instantiationFunction: (string -> obj[] -> ProvidedMethod)) =
            staticParams      <- parameters
            staticParamsApply <- Some instantiationFunction

        /// Get ParameterInfo[] for the parametric type parameters
        member __.GetStaticParametersInternal() = [| for p in staticParams -> p :> ParameterInfo |]

        /// Instantiate parametrics type
        member __.ApplyStaticArgumentsInternal(mangledName:string, args:obj[]) =
            if staticParams.Length>0 then
                if staticParams.Length <> args.Length then
                    failwith (sprintf "ProvidedTypeDefinition: expecting %d static parameters but given %d for method %s" staticParams.Length args.Length methodName)
                match staticParamsApply with
                | None -> failwith "ProvidedTypeDefinition: DefineStaticParameters was not called"
                | Some f -> f mangledName args
            else
                failwith (sprintf "ProvidedTypeDefinition: static parameters supplied but not expected for method %s" methodName)

        member __.GetInvokeCodeInternal(isGenerated, convToTgt) =
            match invokeCode with
            | Some f ->
                // FSharp.Data change: use the real variable names instead of indices, to improve output of Debug.fs
                let paramNames =
                    parameters
                    |> List.map (fun p -> p.Name)
                    |> List.append (if isStatic then [] else ["this"])
                    |> Array.ofList
                QuotationSimplifier(isGenerated, convToTgt).TranslateQuotationToCode f paramNames
            | None -> failwith (sprintf "ProvidedMethod: no invoker for %s on type %s" methodName (if declaringType=null then "<not yet known type>" else declaringType.FullName))

       // Implement overloads
        override __.GetParameters() = argParams |> Array.ofList
        override __.Attributes = methodAttrs
        override __.Name = methodName
        override __.DeclaringType = declaringType |> nonNull "ProvidedMethod.DeclaringType"
        override __.IsDefined(_attributeType, _inherit): bool = true
        override __.MemberType = MemberTypes.Method
        override __.CallingConvention =
            let cc = CallingConventions.Standard
            let cc = if not isStatic then cc ||| CallingConventions.HasThis else cc
            cc
        override __.ReturnType = returnType
        override __.ReturnParameter = null // REVIEW: Give it a name and type?
        override __.ToString() = "Method " + methodName

        // These don't have to return fully accurate results - they are used
        // by the F# Quotations library function SpecificCall as a pre-optimization
        // when comparing methods
        override __.MetadataToken = hash declaringType + hash methodName
        override __.MethodHandle = RuntimeMethodHandle()

        override __.ReturnTypeCustomAttributes = notRequired "ReturnTypeCustomAttributes" methodName
        override __.GetBaseDefinition() = notRequired "GetBaseDefinition" methodName
        override __.GetMethodImplementationFlags() = notRequired "GetMethodImplementationFlags" methodName
        override __.Invoke(_obj, _invokeAttr, _binder, _parameters, _culture) = notRequired "Invoke" methodName
        override __.ReflectedType = notRequired "ReflectedType" methodName
        override __.GetCustomAttributes(_inherit) = emptyAttributes
        override __.GetCustomAttributes(_attributeType, _inherit) =  emptyAttributes


    type ProvidedProperty(propertyName: string, propertyType: Type, parameters: ProvidedParameter list, isStatic: bool, getterCode: option<Expr list -> Expr>, setterCode: option<Expr list -> Expr>) =
        inherit PropertyInfo()

        let mutable declaringType = null

        let hasGetter() = getterCode.IsSome
        let hasSetter() = setterCode.IsSome

        // Delay construction - to pick up the latest isStatic
        let markSpecialName (m:ProvidedMethod) = m.AddMethodAttrs(MethodAttributes.SpecialName); m
        let getter = lazy (ProvidedMethod("get_" + propertyName,parameters,propertyType,isStatic=isStatic,DeclaringTypeImpl=declaringType,invokeCode=getterCode) |> markSpecialName)
        let setter = lazy (ProvidedMethod("set_" + propertyName,parameters @ [ProvidedParameter("value",propertyType,isOut=false,optionalValue=None)],typeof<Void>,isStatic=isStatic,DeclaringTypeImpl=declaringType,invokeCode=setterCode) |> markSpecialName)

        let customAttributesImpl = CustomAttributesImpl()
        member __.AddXmlDocComputed xmlDocFunction = customAttributesImpl.AddXmlDocComputed xmlDocFunction
        member __.AddXmlDocDelayed xmlDocFunction = customAttributesImpl.AddXmlDocDelayed xmlDocFunction
        member __.AddXmlDoc xmlDoc = customAttributesImpl.AddXmlDoc xmlDoc
        member __.AddObsoleteAttribute (message,?isError) = customAttributesImpl.AddObsolete (message,defaultArg isError false)
        member __.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
        member __.AddCustomAttribute attribute = customAttributesImpl.AddCustomAttribute attribute
        override __.GetCustomAttributesData() = customAttributesImpl.GetCustomAttributesData()

        member __.DeclaringTypeImpl with set x = declaringType <- x // check: not set twice

        member __.IsStatic = isStatic

        // Implement overloads
        override __.PropertyType = propertyType
        override __.SetValue(_obj, _value, _invokeAttr, _binder, _index, _culture) = notRequired "SetValue" propertyName
        override __.GetAccessors _nonPublic = notRequired "nonPublic" propertyName
        override __.GetGetMethod _nonPublic = if hasGetter() then getter.Force() :> MethodInfo else null
        override __.GetSetMethod _nonPublic = if hasSetter() then setter.Force() :> MethodInfo else null
        override __.GetIndexParameters() = [| for p in parameters -> upcast p |]
        override __.Attributes = PropertyAttributes.None
        override __.CanRead = hasGetter()
        override __.CanWrite = hasSetter()
        override __.GetValue(_obj, _invokeAttr, _binder, _index, _culture): obj = notRequired "GetValue" propertyName
        override __.Name = propertyName
        override __.DeclaringType = declaringType |> nonNull "ProvidedProperty.DeclaringType"
        override __.MemberType: MemberTypes = MemberTypes.Property

        override __.ReflectedType = notRequired "ReflectedType" propertyName
        override __.GetCustomAttributes(_inherit) = emptyAttributes
        override __.GetCustomAttributes(_attributeType, _inherit) = emptyAttributes
        override __.IsDefined(_attributeType, _inherit) = notRequired "IsDefined" propertyName

    type ProvidedEvent(propertyName:string,eventHandlerType:Type, isStatic: bool, adderCode: option<Expr list -> Expr>, removerCode: option<Expr list -> Expr>) =
        inherit EventInfo()

        let mutable declaringType = null

        // Delay construction - to pick up the latest isStatic
        let markSpecialName (m:ProvidedMethod) = m.AddMethodAttrs(MethodAttributes.SpecialName); m
        let adder = lazy (ProvidedMethod("add_" + propertyName, [ProvidedParameter("handler", eventHandlerType,isOut=false,optionalValue=None)],typeof<Void>,isStatic=isStatic,DeclaringTypeImpl=declaringType,invokeCode=adderCode) |> markSpecialName)
        let remover = lazy (ProvidedMethod("remove_" + propertyName, [ProvidedParameter("handler", eventHandlerType,isOut=false,optionalValue=None)],typeof<Void>,isStatic=isStatic,DeclaringTypeImpl=declaringType,invokeCode=removerCode) |> markSpecialName)

        let customAttributesImpl = CustomAttributesImpl()
        member __.AddXmlDocComputed xmlDocFunction = customAttributesImpl.AddXmlDocComputed xmlDocFunction
        member __.AddXmlDocDelayed xmlDocFunction = customAttributesImpl.AddXmlDocDelayed xmlDocFunction
        member __.AddXmlDoc xmlDoc = customAttributesImpl.AddXmlDoc xmlDoc
        member __.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
        override __.GetCustomAttributesData() = customAttributesImpl.GetCustomAttributesData()

        member __.DeclaringTypeImpl with set x = declaringType <- x // check: not set twice
        member __.IsStatic = isStatic

        member __.AdderCode = adderCode.Value

        member __.RemoverCode = removerCode.Value

        // Implement overloads
        override __.EventHandlerType = eventHandlerType
        override __.GetAddMethod _nonPublic = adder.Force() :> MethodInfo
        override __.GetRemoveMethod _nonPublic = remover.Force() :> MethodInfo
        override __.Attributes = EventAttributes.None
        override __.Name = propertyName
        override __.DeclaringType = declaringType |> nonNull "ProvidedEvent.DeclaringType"
        override __.MemberType: MemberTypes = MemberTypes.Event

        override __.GetRaiseMethod _nonPublic = notRequired "GetRaiseMethod" propertyName
        override __.ReflectedType = notRequired "ReflectedType" propertyName
        override __.GetCustomAttributes(_inherit) = emptyAttributes
        override __.GetCustomAttributes(_attributeType, _inherit) = emptyAttributes
        override __.IsDefined(_attributeType, _inherit) = notRequired "IsDefined" propertyName

    type ProvidedLiteralField(fieldName:string,fieldType:Type,literalValue:obj) =
        inherit FieldInfo()

        let mutable declaringType = null

        let customAttributesImpl = CustomAttributesImpl()
        member __.AddXmlDocComputed xmlDocFunction = customAttributesImpl.AddXmlDocComputed xmlDocFunction
        member __.AddXmlDocDelayed xmlDocFunction = customAttributesImpl.AddXmlDocDelayed xmlDocFunction
        member __.AddXmlDoc xmlDoc = customAttributesImpl.AddXmlDoc xmlDoc
        member __.AddObsoleteAttribute (message,?isError) = customAttributesImpl.AddObsolete (message,defaultArg isError false)
        member __.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
        override __.GetCustomAttributesData() = customAttributesImpl.GetCustomAttributesData()

        member __.DeclaringTypeImpl with set x = declaringType <- x // check: not set twice


        // Implement overloads
        override __.FieldType = fieldType
        override __.GetRawConstantValue() = literalValue
        override __.Attributes = FieldAttributes.Static ||| FieldAttributes.Literal ||| FieldAttributes.Public
        override __.Name = fieldName
        override __.DeclaringType = declaringType |> nonNull "ProvidedLiteralField.DeclaringType"
        override __.MemberType: MemberTypes = MemberTypes.Field

        override __.ReflectedType = notRequired "ReflectedType" fieldName
        override __.GetCustomAttributes(_inherit) = emptyAttributes
        override __.GetCustomAttributes(_attributeType, _inherit) = emptyAttributes
        override __.IsDefined(_attributeType, _inherit) = notRequired "IsDefined" fieldName

        override __.SetValue(_obj, _value, _invokeAttr, _binder, _culture) = notRequired "SetValue" fieldName
        override __.GetValue(_obj): obj = notRequired "GetValue" fieldName
        override __.FieldHandle = notRequired "FieldHandle" fieldName

    type ProvidedField(fieldName:string,fieldType:Type) =
        inherit FieldInfo()

        let mutable declaringType = null

        let customAttributesImpl = CustomAttributesImpl()
        let mutable fieldAttrs = FieldAttributes.Private
        member __.AddXmlDocComputed xmlDocFunction = customAttributesImpl.AddXmlDocComputed xmlDocFunction
        member __.AddXmlDocDelayed xmlDocFunction = customAttributesImpl.AddXmlDocDelayed xmlDocFunction
        member __.AddXmlDoc xmlDoc = customAttributesImpl.AddXmlDoc xmlDoc
        member __.AddObsoleteAttribute (message,?isError) = customAttributesImpl.AddObsolete (message,defaultArg isError false)
        member __.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
        override __.GetCustomAttributesData() = customAttributesImpl.GetCustomAttributesData()

        member __.DeclaringTypeImpl with set x = declaringType <- x // check: not set twice

        member __.SetFieldAttributes attributes = fieldAttrs <- attributes
        // Implement overloads
        override __.FieldType = fieldType
        override __.GetRawConstantValue() = null
        override __.Attributes = fieldAttrs
        override __.Name = fieldName
        override __.DeclaringType = declaringType |> nonNull "ProvidedField.DeclaringType"
        override __.MemberType: MemberTypes = MemberTypes.Field

        override __.ReflectedType = notRequired "ReflectedType" fieldName
        override __.GetCustomAttributes(_inherit) = emptyAttributes
        override __.GetCustomAttributes(_attributeType, _inherit) = emptyAttributes
        override __.IsDefined(_attributeType, _inherit) = notRequired "IsDefined" fieldName

        override __.SetValue(_obj, _value, _invokeAttr, _binder, _culture) = notRequired "SetValue" fieldName
        override __.GetValue(_obj): obj = notRequired "GetValue" fieldName
        override __.FieldHandle = notRequired "FieldHandle" fieldName

    /// Represents the type constructor in a provided symbol type.
    [<NoComparison>]
    type ProvidedSymbolKind =
        | SDArray
        | Array of int
        | Pointer
        | ByRef
        | Generic of Type
        | FSharpTypeAbbreviation of (Assembly * string * string[])


    /// Represents an array or other symbolic type involving a provided type as the argument.
    /// See the type provider spec for the methods that must be implemented.
    /// Note that the type provider specification does not require us to implement pointer-equality for provided types.
    type ProvidedSymbolType(kind: ProvidedSymbolKind, args: Type list, convToTgt: Type -> Type) as this =
        inherit TypeDelegator()

        let rec isEquivalentTo (thisTy: Type) (otherTy: Type) =
            match thisTy, otherTy with
            | (:? ProvidedSymbolType as thisTy), (:? ProvidedSymbolType as thatTy) -> (thisTy.Kind,thisTy.Args) = (thatTy.Kind, thatTy.Args)
            | (:? ProvidedSymbolType as thisTy), otherTy | otherTy, (:? ProvidedSymbolType as thisTy) ->
                match thisTy.Kind, thisTy.Args with
                | ProvidedSymbolKind.SDArray, [ty] | ProvidedSymbolKind.Array _, [ty] when otherTy.IsArray-> ty.Equals(otherTy.GetElementType())
                | ProvidedSymbolKind.ByRef, [ty] when otherTy.IsByRef -> ty.Equals(otherTy.GetElementType())
                | ProvidedSymbolKind.Pointer, [ty] when otherTy.IsPointer -> ty.Equals(otherTy.GetElementType())
                | ProvidedSymbolKind.Generic baseTy, args -> otherTy.IsGenericType && isEquivalentTo baseTy (otherTy.GetGenericTypeDefinition()) && Seq.forall2 isEquivalentTo args (otherTy.GetGenericArguments())
                | _ -> false
            | a, b -> a.Equals b

        let nameText() =
            match kind,args with
            | ProvidedSymbolKind.SDArray,[arg] -> arg.Name + "[]"
            | ProvidedSymbolKind.Array _,[arg] -> arg.Name + "[*]"
            | ProvidedSymbolKind.Pointer,[arg] -> arg.Name + "*"
            | ProvidedSymbolKind.ByRef,[arg] -> arg.Name + "&"
            | ProvidedSymbolKind.Generic gty, args -> gty.Name + (sprintf "%A" args)
            | ProvidedSymbolKind.FSharpTypeAbbreviation (_,_,path),_ -> path.[path.Length-1]
            | _ -> failwith "unreachable"

        do this.typeImpl <- this

        /// Substitute types for type variables.
        static member ConvType (parameters: Type list) (ty:Type) =
            if isNull ty then null
            elif ty.IsGenericType then
                let args = Array.map (ProvidedSymbolType.ConvType parameters) (ty.GetGenericArguments())
                ty.GetGenericTypeDefinition().MakeGenericType(args)
            elif ty.HasElementType then
                let ety = ProvidedSymbolType.ConvType parameters (ty.GetElementType())
                if ty.IsArray then
                    let rank = ty.GetArrayRank()
                    if rank = 1 then ety.MakeArrayType()
                    else ety.MakeArrayType(rank)
                elif ty.IsPointer then ety.MakePointerType()
                elif ty.IsByRef then ety.MakeByRefType()
                else ty
            elif ty.IsGenericParameter then
                if ty.GenericParameterPosition <= parameters.Length - 1 then
                    parameters.[ty.GenericParameterPosition]
                else
                    ty
            else ty

        override __.FullName =
            match kind,args with
            | ProvidedSymbolKind.SDArray,[arg] -> arg.FullName + "[]"
            | ProvidedSymbolKind.Array _,[arg] -> arg.FullName + "[*]"
            | ProvidedSymbolKind.Pointer,[arg] -> arg.FullName + "*"
            | ProvidedSymbolKind.ByRef,[arg] -> arg.FullName + "&"
            | ProvidedSymbolKind.Generic gty, args -> gty.FullName + "[" + (args |> List.map (fun arg -> arg.ToString()) |> String.concat ",") + "]"
            | ProvidedSymbolKind.FSharpTypeAbbreviation (_,nsp,path),args -> String.concat "." (Array.append [| nsp |] path) + (match args with [] -> "" | _ -> args.ToString())
            | _ -> failwith "unreachable"

        /// Although not strictly required by the type provider specification, this is required when doing basic operations like FullName on
        /// .NET symbolic types made from this type, e.g. when building Nullable<SomeProvidedType[]>.FullName
        override __.DeclaringType =
            match kind,args with
            | ProvidedSymbolKind.SDArray,[arg] -> arg
            | ProvidedSymbolKind.Array _,[arg] -> arg
            | ProvidedSymbolKind.Pointer,[arg] -> arg
            | ProvidedSymbolKind.ByRef,[arg] -> arg
            | ProvidedSymbolKind.Generic gty,_ -> gty
            | ProvidedSymbolKind.FSharpTypeAbbreviation _,_ -> null
            | _ -> failwith "unreachable"

        override __.IsAssignableFrom(otherTy: Type) =
            match kind with
            | Generic gtd ->
                if otherTy.IsGenericType then
                    let otherGtd = otherTy.GetGenericTypeDefinition()
                    let otherArgs = otherTy.GetGenericArguments()
                    let yes = gtd.Equals(otherGtd) && Seq.forall2 isEquivalentTo args otherArgs
                    yes
                    else
                        base.IsAssignableFrom(otherTy)
            | _ -> base.IsAssignableFrom(otherTy)

        override __.Name = nameText()

        override __.BaseType =
            match kind with
            | ProvidedSymbolKind.SDArray -> convToTgt typeof<Array>
            | ProvidedSymbolKind.Array _ -> convToTgt typeof<Array>
            | ProvidedSymbolKind.Pointer -> convToTgt typeof<ValueType>
            | ProvidedSymbolKind.ByRef -> convToTgt typeof<ValueType>
            | ProvidedSymbolKind.Generic gty  ->
                if isNull gty.BaseType then null else
                ProvidedSymbolType.ConvType args gty.BaseType
            | ProvidedSymbolKind.FSharpTypeAbbreviation _ -> convToTgt typeof<obj>

        override __.GetArrayRank() = (match kind with ProvidedSymbolKind.Array n -> n | ProvidedSymbolKind.SDArray -> 1 | _ -> invalidOp "non-array type")
        override __.IsValueTypeImpl() = (match kind with ProvidedSymbolKind.Generic gtd -> gtd.IsValueType | _ -> false)
        override __.IsArrayImpl() = (match kind with ProvidedSymbolKind.Array _ | ProvidedSymbolKind.SDArray -> true | _ -> false)
        override __.IsByRefImpl() = (match kind with ProvidedSymbolKind.ByRef _ -> true | _ -> false)
        override __.IsPointerImpl() = (match kind with ProvidedSymbolKind.Pointer _ -> true | _ -> false)
        override __.IsPrimitiveImpl() = false
        override __.IsGenericType = (match kind with ProvidedSymbolKind.Generic _ -> true | _ -> false)
        override __.GetGenericArguments() = (match kind with ProvidedSymbolKind.Generic _ -> args |> List.toArray | _ -> invalidOp "non-generic type")
        override __.GetGenericTypeDefinition() = (match kind with ProvidedSymbolKind.Generic e -> e | _ -> invalidOp "non-generic type")
        override __.IsCOMObjectImpl() = false
        override __.HasElementTypeImpl() = (match kind with ProvidedSymbolKind.Generic _ -> false | _ -> true)
        override __.GetElementType() = (match kind,args with (ProvidedSymbolKind.Array _  | ProvidedSymbolKind.SDArray | ProvidedSymbolKind.ByRef | ProvidedSymbolKind.Pointer),[e] -> e | _ -> invalidOp "not an array, pointer or byref type")
        override this.ToString() = this.FullName

        override __.Assembly =
            match kind with
            | ProvidedSymbolKind.FSharpTypeAbbreviation (assembly,_nsp,_path) -> assembly
            | ProvidedSymbolKind.Generic gty -> gty.Assembly
            | _ -> notRequired "Assembly" (nameText())

        override __.Namespace =
            match kind with
            | ProvidedSymbolKind.FSharpTypeAbbreviation (_assembly,nsp,_path) -> nsp
            | _ -> notRequired "Namespace" (nameText())

        override __.GetHashCode()                                                                    =
            match kind,args with
            | ProvidedSymbolKind.SDArray,[arg] -> 10 + hash arg
            | ProvidedSymbolKind.Array _,[arg] -> 163 + hash arg
            | ProvidedSymbolKind.Pointer,[arg] -> 283 + hash arg
            | ProvidedSymbolKind.ByRef,[arg] -> 43904 + hash arg
            | ProvidedSymbolKind.Generic gty,_ -> 9797 + hash gty + List.sumBy hash args
            | ProvidedSymbolKind.FSharpTypeAbbreviation _,_ -> 3092
            | _ -> failwith "unreachable"

        override __.Equals(other: obj) =
            match other with
            | :? ProvidedSymbolType as otherTy -> (kind, args) = (otherTy.Kind, otherTy.Args)
            | _ -> false

        member __.Kind = kind
        member __.Args = args

        member __.IsFSharpTypeAbbreviation = match kind with FSharpTypeAbbreviation _ -> true | _ -> false
        // For example, int<kg>
        member __.IsFSharpUnitAnnotated = match kind with ProvidedSymbolKind.Generic gtd -> not gtd.IsGenericTypeDefinition | _ -> false

        override __.Module: Module = notRequired "Module" (nameText())
        override __.GetConstructors _bindingAttr = notRequired "GetConstructors" (nameText())
        override __.GetMethodImpl(_name, _bindingAttr, _binderBinder, _callConvention, _types, _modifiers) =
            match kind with
            | Generic gtd ->
                let ty = gtd.GetGenericTypeDefinition().MakeGenericType(Array.ofList args)
                ty.GetMethod(_name, _bindingAttr)
            | _ -> notRequired "GetMethodImpl" (nameText())
        override __.GetMembers _bindingAttr = notRequired "GetMembers" (nameText())
        override __.GetMethods _bindingAttr = notRequired "GetMethods" (nameText())
        override __.GetField(_name, _bindingAttr) = notRequired "GetField" (nameText())
        override __.GetFields _bindingAttr = notRequired "GetFields" (nameText())
        override __.GetInterface(_name, _ignoreCase) = notRequired "GetInterface" (nameText())
        override __.GetInterfaces() = notRequired "GetInterfaces" (nameText())
        override __.GetEvent(_name, _bindingAttr) = notRequired "GetEvent" (nameText())
        override __.GetEvents _bindingAttr = notRequired "GetEvents" (nameText())
        override __.GetProperties _bindingAttr = notRequired "GetProperties" (nameText())
        override __.GetPropertyImpl(_name, _bindingAttr, _binder, _returnType, _types, _modifiers) = notRequired "GetPropertyImpl" (nameText())
        override __.GetNestedTypes _bindingAttr = notRequired "GetNestedTypes" (nameText())
        override __.GetNestedType(_name, _bindingAttr) = notRequired "GetNestedType" (nameText())
        override __.GetAttributeFlagsImpl() = notRequired "GetAttributeFlagsImpl" (nameText())
        override this.UnderlyingSystemType =
            match kind with
            | ProvidedSymbolKind.SDArray
            | ProvidedSymbolKind.Array _
            | ProvidedSymbolKind.Pointer
            | ProvidedSymbolKind.FSharpTypeAbbreviation _
            | ProvidedSymbolKind.ByRef -> upcast this
            | ProvidedSymbolKind.Generic gty -> gty.UnderlyingSystemType
        override __.GetCustomAttributesData() =  ([| |] :> IList<_>)
        override __.MemberType = notRequired "MemberType" (nameText())
        override __.GetMember(_name,_mt,_bindingAttr) = notRequired "GetMember" (nameText())
        override __.GUID = notRequired "GUID" (nameText())
        override __.InvokeMember(_name, _invokeAttr, _binder, _target, _args, _modifiers, _culture, _namedParameters) = notRequired "InvokeMember" (nameText())
        override __.AssemblyQualifiedName = notRequired "AssemblyQualifiedName" (nameText())
        override __.GetConstructorImpl(_bindingAttr, _binder, _callConvention, _types, _modifiers) = notRequired "GetConstructorImpl" (nameText())
        override __.GetCustomAttributes(_inherit) = emptyAttributes
        override __.GetCustomAttributes(_attributeType, _inherit) = emptyAttributes
        override __.IsDefined(_attributeType, _inherit) = false
        // FSharp.Data addition: this was added to support arrays of arrays
        override this.MakeArrayType() = ProvidedSymbolType(ProvidedSymbolKind.SDArray, [this], convToTgt) :> Type
        override this.MakeArrayType arg = ProvidedSymbolType(ProvidedSymbolKind.Array arg, [this], convToTgt) :> Type

        //interface IReflectableType with 
        //    member  this.GetTypeInfo() = (new TypeDelegator(this) :> TypeInfo)

    type ProvidedSymbolMethod(genericMethodDefinition: MethodInfo, parameters: Type list) =
        inherit MethodInfo()

        let convParam (p:ParameterInfo) =
            { new ParameterInfo() with
                  override __.Name = p.Name
                  override __.ParameterType = ProvidedSymbolType.ConvType parameters p.ParameterType
                  override __.Attributes = p.Attributes
                  override __.RawDefaultValue = p.RawDefaultValue
                  override __.GetCustomAttributesData() = p.GetCustomAttributesData()
            }

        override this.IsGenericMethod =
            (if this.DeclaringType.IsGenericType then this.DeclaringType.GetGenericArguments().Length else 0) < parameters.Length

        override this.GetGenericArguments() =
            Seq.skip (if this.DeclaringType.IsGenericType then this.DeclaringType.GetGenericArguments().Length else 0) parameters |> Seq.toArray

        override __.GetGenericMethodDefinition() = genericMethodDefinition

        override __.DeclaringType = ProvidedSymbolType.ConvType parameters genericMethodDefinition.DeclaringType
        override __.ToString() = "Method " + genericMethodDefinition.Name
        override __.Name = genericMethodDefinition.Name
        override __.MetadataToken = genericMethodDefinition.MetadataToken
        override __.Attributes = genericMethodDefinition.Attributes
        override __.CallingConvention = genericMethodDefinition.CallingConvention
        override __.MemberType = genericMethodDefinition.MemberType

        override __.IsDefined(_attributeType, _inherit): bool = notRequired "IsDefined" genericMethodDefinition.Name
        override __.ReturnType = ProvidedSymbolType.ConvType parameters genericMethodDefinition.ReturnType
        override __.GetParameters() = genericMethodDefinition.GetParameters() |> Array.map convParam
        override __.ReturnParameter = genericMethodDefinition.ReturnParameter |> convParam
        override __.ReturnTypeCustomAttributes = notRequired "ReturnTypeCustomAttributes" genericMethodDefinition.Name
        override __.GetBaseDefinition() = notRequired "GetBaseDefinition" genericMethodDefinition.Name
        override __.GetMethodImplementationFlags() = notRequired "GetMethodImplementationFlags" genericMethodDefinition.Name
        override __.MethodHandle = notRequired "MethodHandle" genericMethodDefinition.Name
        override __.Invoke(_obj, _invokeAttr, _binder, _parameters, _culture) = notRequired "Invoke" genericMethodDefinition.Name
        override __.ReflectedType = notRequired "ReflectedType" genericMethodDefinition.Name
        override __.GetCustomAttributes(_inherit) = emptyAttributes
        override __.GetCustomAttributes(_attributeType, _inherit) =  emptyAttributes



    type ProvidedTypeBuilder() =
        static member MakeGenericType(genericTypeDefinition, genericArguments) = ProvidedSymbolType(Generic genericTypeDefinition, genericArguments, id) :> Type
        static member MakeGenericMethod(genericMethodDefinition, genericArguments) = ProvidedSymbolMethod(genericMethodDefinition, genericArguments) :> MethodInfo

    type ZProvidedTypeBuilder(convToTgt: Type -> Type) =
        member __.MakeGenericType(genericTypeDefinition, genericArguments) = ProvidedSymbolType(Generic genericTypeDefinition, genericArguments, convToTgt) :> Type
        member __.MakeGenericMethod(genericMethodDefinition, genericArguments) = ProvidedSymbolMethod(genericMethodDefinition, genericArguments) :> MethodInfo

    [<Class>]
    type ProvidedMeasureBuilder() =

        // TODO: this shouldn't be hardcoded, but without creating a dependency on FSharp.Compiler.Service
        // there seems to be no way to check if a type abbreviation exists
        let unitNamesTypeAbbreviations =
            [ "meter"; "hertz"; "newton"; "pascal"; "joule"; "watt"; "coulomb";
              "volt"; "farad"; "ohm"; "siemens"; "weber"; "tesla"; "henry"
              "lumen"; "lux"; "becquerel"; "gray"; "sievert"; "katal" ]
            |> Set.ofList

        let unitSymbolsTypeAbbreviations =
            [ "m"; "kg"; "s"; "A"; "K"; "mol"; "cd"; "Hz"; "N"; "Pa"; "J"; "W"; "C"
              "V"; "F"; "S"; "Wb"; "T"; "lm"; "lx"; "Bq"; "Gy"; "Sv"; "kat"; "H" ]
            |> Set.ofList

        static let theBuilder = ProvidedMeasureBuilder()
        static member Default = theBuilder
        member __.One = typeof<CompilerServices.MeasureOne>
        member __.Product (measure1, measure2) = typedefof<CompilerServices.MeasureProduct<_,_>>.MakeGenericType [| measure1;measure2 |]
        member __.Inverse denominator = typedefof<CompilerServices.MeasureInverse<_>>.MakeGenericType [| denominator |]
        member b.Ratio (numerator, denominator) = b.Product(numerator, b.Inverse denominator)
        member b.Square ``measure`` = b.Product(``measure``, ``measure``)

        // FSharp.Data change: if the unit is not a valid type, instead
        // of assuming it's a type abbreviation, which may not be the case and cause a
        // problem later on, check the list of valid abbreviations
        member __.SI (unitName:string) =
            let mLowerCase = unitName.ToLowerInvariant()
            let abbreviation =
                if unitNamesTypeAbbreviations.Contains mLowerCase then
                    Some ("Microsoft.FSharp.Data.UnitSystems.SI.UnitNames", mLowerCase)
                elif unitSymbolsTypeAbbreviations.Contains unitName then
                    Some ("Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols", unitName)
                else
                    None
            match abbreviation with
            | Some (ns, unitName) ->
                ProvidedSymbolType(ProvidedSymbolKind.FSharpTypeAbbreviation(typeof<Core.CompilerServices.MeasureOne>.Assembly,ns,[| unitName |]), [], id) :> Type
            | None ->
                typedefof<list<int>>.Assembly.GetType("Microsoft.FSharp.Data.UnitSystems.SI.UnitNames." + mLowerCase)

        member __.AnnotateType (basic, argument) = ProvidedSymbolType(Generic basic, argument, id) :> Type



    [<RequireQualifiedAccess; NoComparison>]
    type TypeContainer =
      | Namespace of Assembly * string // namespace
      | Type of Type
      | TypeToBeDecided

    type ProvidedTypeDefinition(container:TypeContainer, className: string, baseType: Type option, convToTgt) as this =
        inherit TypeDelegator()

        do match container, !ProvidedTypeDefinition.Logger with
           | TypeContainer.Namespace _, Some logger -> logger (sprintf "Creating ProvidedTypeDefinition %s [%d]" className (System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode this))
           | _ -> ()

        // state
        let mutable attributes   =
            TypeAttributes.Public |||
            TypeAttributes.Class |||
            TypeAttributes.Sealed |||
            enum (int32 TypeProviderTypeAttributes.IsErased)


        let mutable enumUnderlyingType = None
        let mutable baseType =  lazy baseType
        let mutable membersKnown = ResizeArray<MemberInfo>()
        let mutable membersQueue = ResizeArray<(unit -> list<MemberInfo>)>()
        let mutable staticParams = [ ]
        let mutable staticParamsApply = None
        let mutable container = container
        let mutable interfaceImpls = ResizeArray<Type>()
        let mutable interfaceImplsDelayed = ResizeArray<unit -> list<Type>>()
        let mutable methodOverrides = ResizeArray<ProvidedMethod * MethodInfo>()

        // members API
        let getMembers() =
            if membersQueue.Count > 0 then
                let elems = membersQueue |> Seq.toArray // take a copy in case more elements get added
                membersQueue.Clear()
                for  f in elems do
                    for i in f() do
                        membersKnown.Add i
                        match i with
                        | :? ProvidedProperty    as p ->
                            if p.CanRead then membersKnown.Add (p.GetGetMethod true)
                            if p.CanWrite then membersKnown.Add (p.GetSetMethod true)
                        | :? ProvidedEvent       as e ->
                            membersKnown.Add (e.GetAddMethod true)
                            membersKnown.Add (e.GetRemoveMethod true)
                        | _ -> ()

            membersKnown.ToArray()

                // members API
        let getInterfaceImpls() =
            if interfaceImplsDelayed.Count > 0 then
                let elems = interfaceImplsDelayed |> Seq.toArray // take a copy in case more elements get added
                interfaceImplsDelayed.Clear()
                for  f in elems do
                    for i in f() do
                        interfaceImpls.Add i

            interfaceImpls.ToArray()

        let mutable theAssembly =
          lazy
            match container with
            | TypeContainer.Namespace (theAssembly, rootNamespace) ->
                if isNull theAssembly then failwith "Null assemblies not allowed"
                if not (isNull rootNamespace)  && rootNamespace.Length=0 then failwith "Use 'null' for global namespace"
                theAssembly
            | TypeContainer.Type superTy -> superTy.Assembly
            | TypeContainer.TypeToBeDecided -> failwith (sprintf "type '%s' was not added as a member to a declaring type" className)

        let rootNamespace =
          lazy
            match container with
            | TypeContainer.Namespace (_,rootNamespace) -> rootNamespace
            | TypeContainer.Type enclosingTyp           -> enclosingTyp.Namespace
            | TypeContainer.TypeToBeDecided -> failwith (sprintf "type '%s' was not added as a member to a declaring type" className)

        let declaringType =
          lazy
            match container with
            | TypeContainer.Namespace _ -> null
            | TypeContainer.Type enclosingTyp           -> enclosingTyp
            | TypeContainer.TypeToBeDecided -> failwith (sprintf "type '%s' was not added as a member to a declaring type" className)

        let fullName =
          lazy
            match container with
            | TypeContainer.Type declaringType -> declaringType.FullName + "+" + className
            | TypeContainer.Namespace (_,namespaceName) ->
                if namespaceName="" then failwith "use null for global namespace"
                match namespaceName with
                | null -> className
                | _    -> namespaceName + "." + className
            | TypeContainer.TypeToBeDecided -> failwith (sprintf "type '%s' was not added as a member to a declaring type" className)

        let patchUpAddedMemberInfo (this:Type) (m:MemberInfo) =
            match m with
            | :? ProvidedConstructor as c -> c.DeclaringTypeImpl <- this // patch up "declaring type" on provided MethodInfo
            | :? ProvidedMethod      as m -> m.DeclaringTypeImpl <- this // patch up "declaring type" on provided MethodInfo
            | :? ProvidedProperty    as p -> p.DeclaringTypeImpl <- this // patch up "declaring type" on provided MethodInfo
            | :? ProvidedEvent       as e -> e.DeclaringTypeImpl <- this // patch up "declaring type" on provided MethodInfo
            | :? ProvidedTypeDefinition  as t -> t.DeclaringTypeImpl <- this
            | :? ProvidedLiteralField as l -> l.DeclaringTypeImpl <- this
            | :? ProvidedField as l -> l.DeclaringTypeImpl <- this
            | _ -> ()

        let customAttributesImpl = CustomAttributesImpl()

        do this.typeImpl <- this

        //interface IReflectableType with 
        //    member  this.GetTypeInfo() = (new TypeDelegator(this) :> TypeInfo)

        member __.AddXmlDocComputed xmlDocFunction = customAttributesImpl.AddXmlDocComputed xmlDocFunction
        member __.AddXmlDocDelayed xmlDocFunction = customAttributesImpl.AddXmlDocDelayed xmlDocFunction
        member __.AddXmlDoc xmlDoc = customAttributesImpl.AddXmlDoc xmlDoc
        member __.AddObsoleteAttribute (message,?isError) = customAttributesImpl.AddObsolete (message,defaultArg isError false)
        member __.AddDefinitionLocation(line,column,filePath) = customAttributesImpl.AddDefinitionLocation(line, column, filePath)
        member __.HideObjectMethods with get() = customAttributesImpl.HideObjectMethods and set v = customAttributesImpl.HideObjectMethods <- v
        member __.NonNullable with get() = customAttributesImpl.NonNullable and set v = customAttributesImpl.NonNullable <- v
        member __.AddCustomAttribute attribute = customAttributesImpl.AddCustomAttribute attribute
        override __.GetCustomAttributesData() = customAttributesImpl.GetCustomAttributesData()

        member __.ResetEnclosingType (enclosingType) =
            container <- TypeContainer.Type enclosingType
        new (assembly:Assembly,namespaceName,className,baseType) = ProvidedTypeDefinition(TypeContainer.Namespace (assembly,namespaceName), className, baseType, id)
        new (className:string,baseType) = ProvidedTypeDefinition(TypeContainer.TypeToBeDecided, className, baseType, id)

        new (assembly:Assembly,namespaceName,className,baseType,convToTgt) = ProvidedTypeDefinition(TypeContainer.Namespace (assembly,namespaceName), className, baseType, convToTgt)
        new (className,baseType, convToTgt) = ProvidedTypeDefinition(TypeContainer.TypeToBeDecided, className, baseType, convToTgt)
        // state ops

        override __.UnderlyingSystemType = typeof<Type>

        member __.SetEnumUnderlyingType(ty) = enumUnderlyingType <- Some ty

        override __.GetEnumUnderlyingType() =
            if this.IsEnum then
                match enumUnderlyingType with
                | None -> convToTgt typeof<int>
                | Some ty -> ty
            else invalidOp "not enum type"

        member __.SetBaseType t = baseType <- lazy Some t

        member __.SetBaseTypeDelayed baseTypeFunction = baseType <- lazy (Some (baseTypeFunction()))

        member __.SetAttributes x = attributes <- x

        // Add MemberInfos
        member __.AddMembersDelayed(membersFunction: unit -> list<#MemberInfo>) =
            membersQueue.Add (fun () -> membersFunction() |> List.map (fun x -> patchUpAddedMemberInfo this x; x :> MemberInfo ))

        member __.AddMembers(memberInfos:list<#MemberInfo>) = (* strict *)
            memberInfos |> List.iter (patchUpAddedMemberInfo this) // strict: patch up now
            membersQueue.Add (fun () -> memberInfos |> List.map (fun x -> x :> MemberInfo))

        member __.AddMember(memberInfo:MemberInfo) =
            this.AddMembers [memberInfo]

        member __.AddMemberDelayed(memberFunction: unit -> #MemberInfo) =
            this.AddMembersDelayed(fun () -> [memberFunction()])

        member __.AddAssemblyTypesAsNestedTypesDelayed (assemblyFunction: unit -> Assembly)  =
            let bucketByPath nodef tipf (items: (string list * 'Value) list) =
                // Find all the items with an empty key list and call 'tipf'
                let tips =
                    [ for (keylist,v) in items do
                            match keylist with
                            | [] -> yield tipf v
                            | _ -> () ]

                // Find all the items with a non-empty key list. Bucket them together by
                // the first key. For each bucket, call 'nodef' on that head key and the bucket.
                let nodes =
                    let buckets = new Dictionary<_,_>(10)
                    for (keylist,v) in items do
                        match keylist with
                        | [] -> ()
                        | key::rest ->
                            buckets.[key] <- (rest,v) :: (if buckets.ContainsKey key then buckets.[key] else []);

                    [ for (KeyValue(key,items)) in buckets -> nodef key items ]

                tips @ nodes
            this.AddMembersDelayed (fun _ ->
                let topTypes = [ for ty in assemblyFunction().GetTypes() do
                                        if not ty.IsNested then
                                                let namespaceParts = match ty.Namespace with null -> [] | s -> s.Split '.' |> Array.toList
                                                yield namespaceParts,  ty ]
                let rec loop types =
                    types
                    |> bucketByPath
                        (fun namespaceComponent typesUnderNamespaceComponent ->
                            let t = ProvidedTypeDefinition(namespaceComponent, baseType = Some typeof<obj>)
                            t.AddMembers (loop typesUnderNamespaceComponent)
                            (t :> Type))
                        id
                loop topTypes)

        /// Abstract a type to a parametric-type. Requires "formal parameters" and "instantiation function".
        member __.DefineStaticParameters(parameters: list<ProvidedStaticParameter>, instantiationFunction: (string -> obj[] -> ProvidedTypeDefinition)) =
            staticParams      <- parameters
            staticParamsApply <- Some instantiationFunction

        /// Get ParameterInfo[] for the parametric type parameters 
        member __.GetStaticParametersInternal() = [| for p in staticParams -> p :> ParameterInfo |]

        /// Instantiate parametrics type
        member __.MakeParametricType(name:string,args:obj[]) =
            if staticParams.Length>0 then
                if staticParams.Length <> args.Length then
                    failwith (sprintf "ProvidedTypeDefinition: expecting %d static parameters but given %d for type %s" staticParams.Length args.Length (fullName.Force()))
                match staticParamsApply with
                | None -> failwith "ProvidedTypeDefinition: DefineStaticParameters was not called"
                | Some f -> f name args

            else
                failwith (sprintf "ProvidedTypeDefinition: static parameters supplied but not expected for %s" (fullName.Force()))

        member __.DeclaringTypeImpl
            with set x =
                match container with TypeContainer.TypeToBeDecided -> () | _ -> failwith (sprintf "container type for '%s' was already set to '%s'" this.FullName x.FullName);
                container <- TypeContainer.Type  x

        // Implement overloads
        override __.Assembly = theAssembly.Force()

        member __.SetAssemblyInternal assembly = theAssembly <- lazy assembly

        member __.SetAssemblyLazyInternal assembly = theAssembly <- assembly

        override __.FullName = fullName.Force()

        override __.Namespace = rootNamespace.Force()

        override __.BaseType = match baseType.Value with Some ty -> ty | None -> null

        // Constructors
        override __.GetConstructors bindingAttr =
            [| for m in this.GetMembers bindingAttr do
                    if m.MemberType = MemberTypes.Constructor then
                        yield (m :?> ConstructorInfo) |]
        // Methods
        override __.GetMethodImpl(name, bindingAttr, _binderBinder, _callConvention, _types, _modifiers): MethodInfo =
            let membersWithName =
                [ for m in this.GetMembers(bindingAttr) do
                    if m.MemberType.HasFlag(MemberTypes.Method) && m.Name = name then
                        yield  m ]
            match membersWithName with
            | []        -> null
            | [meth]    -> meth :?> MethodInfo
            | _several   -> failwith "GetMethodImpl. not support overloads"

        override __.GetMethods bindingAttr =
            this.GetMembers bindingAttr
            |> Array.filter (fun m -> m.MemberType.HasFlag(MemberTypes.Method))
            |> Array.map (fun m -> m :?> MethodInfo)

        // Fields
        override __.GetField(name, bindingAttr) =
            let fields = [| for m in this.GetMembers bindingAttr do
                                if m.MemberType.HasFlag(MemberTypes.Field) && (name = null || m.Name = name) then // REVIEW: name = null. Is that a valid query?!
                                    yield m |]
            if fields.Length > 0 then fields.[0] :?> FieldInfo else null

        override __.GetFields bindingAttr =
            [| for m in this.GetMembers bindingAttr do if m.MemberType.HasFlag(MemberTypes.Field) then yield m :?> FieldInfo |]

        override __.GetInterface(_name, _ignoreCase) = notRequired "GetInterface" this.Name

        override __.GetInterfaces() =
            [| yield! getInterfaceImpls()  |]

        member __.GetInterfaceImplementationsInternal() =
            [| yield! getInterfaceImpls() |]

        member __.AddInterfaceImplementation interfaceType = interfaceImpls.Add interfaceType

        member __.AddInterfaceImplementationsDelayed interfacesFunction = interfaceImplsDelayed.Add interfacesFunction

        member __.GetMethodOverridesInternal() =
            [| yield! methodOverrides |]

        member __.DefineMethodOverride (methodInfoBody,methodInfoDeclaration) = methodOverrides.Add (methodInfoBody, methodInfoDeclaration)

        // Events
        override __.GetEvent(name, bindingAttr) =
            let events = this.GetMembers bindingAttr
                         |> Array.filter(fun m -> m.MemberType.HasFlag(MemberTypes.Event) && (name = null || m.Name = name))
            if events.Length > 0 then events.[0] :?> EventInfo else null

        override __.GetEvents bindingAttr =
            [| for m in this.GetMembers bindingAttr do if m.MemberType.HasFlag(MemberTypes.Event) then yield downcast m |]

        // Properties
        override __.GetProperties bindingAttr =
            [| for m in this.GetMembers bindingAttr do if m.MemberType.HasFlag(MemberTypes.Property) then yield downcast m |]

        override __.GetPropertyImpl(name, bindingAttr, binder, returnType, types, modifiers) =
            if returnType |> isNull |> not then failwith "Need to handle specified return type in GetPropertyImpl"
            if types      |> isNull |> not then failwith "Need to handle specified parameter types in GetPropertyImpl"
            if modifiers  |> isNull |> not then failwith "Need to handle specified modifiers in GetPropertyImpl"
            if binder  |> isNull |> not then failwith "Need to handle binder in GetPropertyImpl"
            let props = this.GetMembers bindingAttr |> Array.filter(fun m -> m.MemberType.HasFlag(MemberTypes.Property) && (name = null || m.Name = name))  // Review: nam = null, valid query!?
            if props.Length > 0 then
                props.[0] :?> PropertyInfo
            else
                null
        // Nested Types
        override __.MakeArrayType() = ProvidedSymbolType(ProvidedSymbolKind.SDArray, [this], convToTgt) :> Type
        override __.MakeArrayType arg = ProvidedSymbolType(ProvidedSymbolKind.Array arg, [this], convToTgt) :> Type
        override __.MakePointerType() = ProvidedSymbolType(ProvidedSymbolKind.Pointer, [this], convToTgt) :> Type
        override __.MakeByRefType() = ProvidedSymbolType(ProvidedSymbolKind.ByRef, [this], convToTgt) :> Type

        // FSharp.Data addition: this method is used by Debug.fs and QuotationBuilder.fs
        // Emulate the F# type provider type erasure mechanism to get the
        // actual (erased) type. We erase ProvidedTypes to their base type
        // and we erase array of provided type to array of base type. In the
        // case of generics all the generic type arguments are also recursively
        // replaced with the erased-to types
        static member EraseType(typ:Type): Type =
            match typ with
            | :? ProvidedTypeDefinition as ptd when ptd.IsErased -> ProvidedTypeDefinition.EraseType typ.BaseType
            | t when t.IsArray ->
                let rank = t.GetArrayRank()
                let et = ProvidedTypeDefinition.EraseType (t.GetElementType())
                if rank = 0 then et.MakeArrayType() else et.MakeArrayType(rank)
            | :? ProvidedSymbolType as sym when sym.IsFSharpUnitAnnotated ->
                typ.UnderlyingSystemType
            | t when t.IsGenericType && not t.IsGenericTypeDefinition ->
                let genericTypeDefinition = t.GetGenericTypeDefinition()
                let genericArguments = t.GetGenericArguments() |> Array.map ProvidedTypeDefinition.EraseType
                genericTypeDefinition.MakeGenericType(genericArguments)
            | t -> t

        static member Logger: (string -> unit) option ref = ref None

        // The binding attributes are always set to DeclaredOnly ||| Static ||| Instance ||| Public when GetMembers is called directly by the F# compiler
        // However, it's possible for the framework to generate other sets of flags in some corner cases (e.g. via use of `enum` with a provided type as the target)
        override __.GetMembers bindingAttr =
            let mems =
                getMembers()
                |> Array.filter (fun mem ->
                                    let isStatic, isPublic =
                                        match mem with
                                        | :? FieldInfo as f -> f.IsStatic, f.IsPublic
                                        | :? MethodInfo as m -> m.IsStatic, m.IsPublic
                                        | :? ConstructorInfo as c -> c.IsStatic, c.IsPublic
                                        | :? PropertyInfo as p ->
                                            let m = if p.CanRead then p.GetGetMethod() else p.GetSetMethod()
                                            m.IsStatic, m.IsPublic
                                        | :? EventInfo as e ->
                                            let m = e.GetAddMethod()
                                            m.IsStatic, m.IsPublic
                                        | :? Type as ty ->
                                            true, ty.IsNestedPublic
                                        | _ -> failwith (sprintf "Member %O is of unexpected type" mem)
                                    bindingAttr.HasFlag(if isStatic then BindingFlags.Static else BindingFlags.Instance) &&
                                    (
                                        (bindingAttr.HasFlag(BindingFlags.Public) && isPublic) || (bindingAttr.HasFlag(BindingFlags.NonPublic) && not isPublic)
                                    ))

            if bindingAttr.HasFlag(BindingFlags.DeclaredOnly) || this.BaseType = null then mems
            else
                // FSharp.Data change: just using this.BaseType is not enough in the case of CsvProvider,
                // because the base type is CsvRow<RowType>, so we have to erase recursively to CsvRow<TupleType>
                let baseMems = (ProvidedTypeDefinition.EraseType this.BaseType).GetMembers bindingAttr
                Array.append mems baseMems

        override __.GetNestedTypes bindingAttr =
            this.GetMembers bindingAttr
            |> Array.filter(fun m ->
                m.MemberType.HasFlag(MemberTypes.NestedType) ||
                // Allow 'fake' nested types that are actually real .NET types
                m.MemberType.HasFlag(MemberTypes.TypeInfo)) |> Array.map(fun m -> m :?> Type)

        override __.GetMember(name,mt,_bindingAttr) =
            let mt =
                if mt &&& MemberTypes.NestedType = MemberTypes.NestedType then
                    mt ||| MemberTypes.TypeInfo
                else
                    mt
            getMembers() |> Array.filter(fun m->0<>(int(m.MemberType &&& mt)) && m.Name = name)

        override __.GetNestedType(name, bindingAttr) =
            let nt = this.GetMember(name, MemberTypes.NestedType ||| MemberTypes.TypeInfo, bindingAttr)
            match nt.Length with
            | 0 -> null
            | 1 -> downcast nt.[0]
            | _ -> failwith (sprintf "There is more than one nested type called '%s' in type '%s'" name this.FullName)

        // Attributes, etc..
        override __.GetAttributeFlagsImpl() = adjustTypeAttributes attributes this.IsNested
        override this.IsValueTypeImpl() = if isNull this.BaseType then false else this.BaseType = typeof<Enum> || this.BaseType.IsValueType 
        override __.IsArrayImpl() = false
        override __.IsByRefImpl() = false
        override __.IsPointerImpl() = false
        override __.IsPrimitiveImpl() = false
        override __.IsCOMObjectImpl() = false
        override __.HasElementTypeImpl() = false
        override __.Name = className
        override __.DeclaringType = declaringType.Force()
        override __.MemberType = if this.IsNested then MemberTypes.NestedType else MemberTypes.TypeInfo
        override __.GetHashCode() = rootNamespace.GetHashCode() ^^^ className.GetHashCode()
        override __.Equals(that:obj) =
            match that with
            | null              -> false
            | :? ProvidedTypeDefinition as ti -> Object.ReferenceEquals(this,ti)
            | _                 -> false

        override __.GetGenericArguments() = [||]
        override __.ToString() = this.Name


        override __.Module: Module = notRequired "Module" this.Name
        override __.GUID = Guid.Empty
        override __.GetConstructorImpl(_bindingAttr, _binder, _callConvention, _types, _modifiers) = null
        override __.GetCustomAttributes(_inherit) = emptyAttributes
        override __.GetCustomAttributes(_attributeType, _inherit) = emptyAttributes
        override __.IsDefined(_attributeType: Type, _inherit) = false

        override __.GetElementType() = notRequired "Module" this.Name
        override __.InvokeMember(_name, _invokeAttr, _binder, _target, _args, _modifiers, _culture, _namedParameters) = notRequired "Module" this.Name
        override __.AssemblyQualifiedName = notRequired "Module" this.Name
        member __.IsErased
            with get() = (attributes &&& enum (int32 TypeProviderTypeAttributes.IsErased)) <> enum 0
            and set v =
               if v then attributes <- attributes ||| enum (int32 TypeProviderTypeAttributes.IsErased)
               else attributes <- attributes &&& ~~~(enum (int32 TypeProviderTypeAttributes.IsErased))

        member __.SuppressRelocation
            with get() = (attributes &&& enum (int32 TypeProviderTypeAttributes.SuppressRelocate)) <> enum 0
            and set v =
               if v then attributes <- attributes ||| enum (int32 TypeProviderTypeAttributes.SuppressRelocate)
               else attributes <- attributes &&& ~~~(enum (int32 TypeProviderTypeAttributes.SuppressRelocate))



//====================================================================================================
// AssemblyReader for ProvidedContext
//
// A lightweight .NET assembly reader that fits in a single F# file.  Based on the well-tested Abstract IL
// binary reader code.  Used by the type provider to read referenced asssemblies.

namespace ProviderImplementation.ProvidedTypes.AssemblyReader

    open System
    open System.IO
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Reflection

    [<AutoOpen>]
    module Utils =
        [<Struct>]
        type StructOption<'T> (hasValue: bool, value: 'T) =
            member x.HasValue = hasValue
            member x.Value = value

        let UNone<'T> = StructOption<'T>(false, Unchecked.defaultof<'T>)
        let USome v = StructOption<'T>(true, v)
        let (|UNone|USome|) (x:StructOption<'T>) = if x.HasValue then USome x.Value else UNone


        let tryFindMulti k map = match Map.tryFind k map with Some res -> res | None -> [| |]

        let splitNameAt (nm:string) idx =
            if idx < 0 then failwith "splitNameAt: idx < 0";
            let last = nm.Length - 1
            if idx > last then failwith "splitNameAt: idx > last";
            (nm.Substring(0,idx)),
            (if idx < last then nm.Substring (idx+1,last - idx) else "")

        let splitILTypeName (nm:string) =
            match nm.LastIndexOf '.' with
            | -1 -> UNone, nm
            | idx -> let a,b = splitNameAt nm idx in USome a, b

        let joinILTypeName (nspace: string StructOption) (nm:string) =
            match nspace with
            | UNone -> nm
            | USome ns -> ns + "." + nm


        let singleOfBits (x:int32) = System.BitConverter.ToSingle(System.BitConverter.GetBytes(x),0)
        let doubleOfBits (x:int64) = System.BitConverter.Int64BitsToDouble(x)

        //---------------------------------------------------------------------
        // SHA1 hash-signing algorithm.  Used to get the public key token from
        // the public key.
        //---------------------------------------------------------------------

        let b0 n =  (n &&& 0xFF)
        let b1 n =  ((n >>> 8) &&& 0xFF)
        let b2 n =  ((n >>> 16) &&& 0xFF)
        let b3 n =  ((n >>> 24) &&& 0xFF)

        module SHA1 =
            let inline (>>>&)  (x:int) (y:int) = int32 (uint32 x >>> y)
            let f(t,b,c,d) =
                if t < 20 then (b &&& c) ||| ((~~~b) &&& d)
                elif t < 40 then b ^^^ c ^^^ d
                elif t < 60 then (b &&& c) ||| (b &&& d) ||| (c &&& d)
                else b ^^^ c ^^^ d

            let [<Literal>] k0to19 = 0x5A827999
            let [<Literal>] k20to39 = 0x6ED9EBA1
            let [<Literal>] k40to59 = 0x8F1BBCDC
            let [<Literal>] k60to79 = 0xCA62C1D6

            let k t =
                if t < 20 then k0to19
                elif t < 40 then k20to39
                elif t < 60 then k40to59
                else k60to79

            type SHAStream =
                { stream: byte[];
                  mutable pos: int;
                  mutable eof:  bool; }

            let rotLeft32 x n =  (x <<< n) ||| (x >>>& (32-n))

            // padding and length (in bits!) recorded at end
            let shaAfterEof sha  =
                let n = sha.pos
                let len = sha.stream.Length
                if n = len then 0x80
                else
                  let paddedLen = (((len + 9 + 63) / 64) * 64) - 8
                  if n < paddedLen - 8  then 0x0
                  elif (n &&& 63) = 56 then int32 ((int64 len * int64 8) >>> 56) &&& 0xff
                  elif (n &&& 63) = 57 then int32 ((int64 len * int64 8) >>> 48) &&& 0xff
                  elif (n &&& 63) = 58 then int32 ((int64 len * int64 8) >>> 40) &&& 0xff
                  elif (n &&& 63) = 59 then int32 ((int64 len * int64 8) >>> 32) &&& 0xff
                  elif (n &&& 63) = 60 then int32 ((int64 len * int64 8) >>> 24) &&& 0xff
                  elif (n &&& 63) = 61 then int32 ((int64 len * int64 8) >>> 16) &&& 0xff
                  elif (n &&& 63) = 62 then int32 ((int64 len * int64 8) >>> 8) &&& 0xff
                  elif (n &&& 63) = 63 then (sha.eof <- true; int32 (int64 len * int64 8) &&& 0xff)
                  else 0x0

            let shaRead8 sha =
                let s = sha.stream
                let b = if sha.pos >= s.Length then shaAfterEof sha else int32 s.[sha.pos]
                sha.pos <- sha.pos + 1
                b

            let shaRead32 sha  =
                let b0 = shaRead8 sha
                let b1 = shaRead8 sha
                let b2 = shaRead8 sha
                let b3 = shaRead8 sha
                let res = (b0 <<< 24) ||| (b1 <<< 16) ||| (b2 <<< 8) ||| b3
                res

            let sha1Hash sha =
                let mutable h0 = 0x67452301
                let mutable h1 = 0xEFCDAB89
                let mutable h2 = 0x98BADCFE
                let mutable h3 = 0x10325476
                let mutable h4 = 0xC3D2E1F0
                let mutable a = 0
                let mutable b = 0
                let mutable c = 0
                let mutable d = 0
                let mutable e = 0
                let w = Array.create 80 0x00
                while (not sha.eof) do
                    for i = 0 to 15 do
                        w.[i] <- shaRead32 sha
                    for t = 16 to 79 do
                        w.[t] <- rotLeft32 (w.[t-3] ^^^ w.[t-8] ^^^ w.[t-14] ^^^ w.[t-16]) 1
                    a <- h0
                    b <- h1
                    c <- h2
                    d <- h3
                    e <- h4
                    for t = 0 to 79 do
                        let temp = (rotLeft32 a 5) + f(t,b,c,d) + e + w.[t] + k(t)
                        e <- d
                        d <- c
                        c <- rotLeft32 b 30
                        b <- a
                        a <- temp
                    h0 <- h0 + a
                    h1 <- h1 + b
                    h2 <- h2 + c
                    h3 <- h3 + d
                    h4 <- h4 + e
                h0,h1,h2,h3,h4

            let sha1HashBytes s =
                let (_h0,_h1,_h2,h3,h4) = sha1Hash { stream = s; pos = 0; eof = false }   // the result of the SHA algorithm is stored in registers 3 and 4
                Array.map byte [|  b0 h4; b1 h4; b2 h4; b3 h4; b0 h3; b1 h3; b2 h3; b3 h3; |]


        let sha1HashBytes s = SHA1.sha1HashBytes s


    [<StructuralEquality; StructuralComparison>]
    type PublicKey =
        | PublicKey of byte[]
        | PublicKeyToken of byte[]
        member x.IsKey=match x with PublicKey _ -> true | _ -> false
        member x.IsKeyToken=match x with PublicKeyToken _ -> true | _ -> false
        member x.Key=match x with PublicKey b -> b | _ -> invalidOp "not a key"
        member x.KeyToken=match x with PublicKeyToken b -> b | _ -> invalidOp"not a key token"

        member x.ToToken() =
            match x with
            | PublicKey bytes -> SHA1.sha1HashBytes bytes
            | PublicKeyToken token -> token
        static member KeyAsToken(k) = PublicKeyToken(PublicKey(k).ToToken())

    [<Sealed>]
    type ILAssemblyRef(name: string, hash: byte[] option, publicKey: PublicKey option, retargetable: bool, version: Version option, locale: string StructOption)  =
        member x.Name=name
        member x.Hash=hash
        member x.PublicKey=publicKey
        member x.Retargetable=retargetable
        member x.Version=version
        member x.Locale=locale
        static member FromAssemblyName (aname:System.Reflection.AssemblyName) =
            let locale = UNone
            let publicKey =
               match aname.GetPublicKey()  with
               | null | [| |] ->
                   match aname.GetPublicKeyToken()  with
                   | null | [| |] -> None
                   | bytes -> Some (PublicKeyToken bytes)
               | bytes ->
                   Some (PublicKey bytes)

            let version =
               match aname.Version with
               | null -> None
               | v -> Some (Version(v.Major,v.Minor,v.Build,v.Revision))

            let retargetable = aname.Flags = System.Reflection.AssemblyNameFlags.Retargetable

            ILAssemblyRef(aname.Name,None,publicKey,retargetable,version,locale)

        member aref.QualifiedName =
            let b = new System.Text.StringBuilder(100)
            let add (s:string) = (b.Append(s) |> ignore)
            let addC (s:char) = (b.Append(s) |> ignore)
            add(aref.Name);
            match aref.Version with
            | None -> ()
            | Some v ->
                add ", Version=";
                add (string v.Major)
                add ".";
                add (string v.Minor)
                add ".";
                add (string v.Build)
                add ".";
                add (string v.Revision)
                add ", Culture="
                match aref.Locale with
                | UNone -> add "neutral"
                | USome b -> add b
                add ", PublicKeyToken="
                match aref.PublicKey with
                | None -> add "null"
                | Some pki ->
                      let pkt = pki.ToToken()
                      let convDigit(digit) =
                          let digitc =
                              if digit < 10
                              then  System.Convert.ToInt32 '0' + digit
                              else System.Convert.ToInt32 'a' + (digit - 10)
                          System.Convert.ToChar(digitc)
                      for i = 0 to pkt.Length-1 do
                          let v = pkt.[i]
                          addC (convDigit(System.Convert.ToInt32(v)/16))
                          addC (convDigit(System.Convert.ToInt32(v)%16))
                // retargetable can be true only for system assemblies that definitely have Version
                if aref.Retargetable then
                    add ", Retargetable=Yes"
            b.ToString()
        override x.ToString() = x.QualifiedName


    type ILModuleRef(name:string, hasMetadata: bool, hash: byte[] option) =
        member x.Name=name
        member x.HasMetadata=hasMetadata
        member x.Hash=hash
        override x.ToString() = "module " + name


    [<RequireQualifiedAccess>]
    type ILScopeRef =
        | Local
        | Module of ILModuleRef
        | Assembly of ILAssemblyRef
        member x.IsLocalRef = match x with ILScopeRef.Local      -> true | _ -> false
        member x.IsModuleRef = match x with ILScopeRef.Module _   -> true | _ -> false
        member x.IsAssemblyRef= match x with ILScopeRef.Assembly _ -> true | _ -> false
        member x.ModuleRef = match x with ILScopeRef.Module x   -> x | _ -> failwith "not a module reference"
        member x.AssemblyRef = match x with ILScopeRef.Assembly x -> x | _ -> failwith "not an assembly reference"

        member x.QualifiedName =
            match x with
            | ILScopeRef.Local -> ""
            | ILScopeRef.Module mref -> "module "+mref.Name
            | ILScopeRef.Assembly aref -> aref.QualifiedName

        override x.ToString() = x.QualifiedName

    type ILArrayBound = int32 option
    type ILArrayBounds = ILArrayBound * ILArrayBound

    [<StructuralEquality; StructuralComparison>]
    type ILArrayShape =
        | ILArrayShape of ILArrayBounds[] (* lobound/size pairs *)
        member x.Rank = (let (ILArrayShape l) = x in l.Length)
        static member SingleDimensional = ILArrayShapeStatics.SingleDimensional
        static member FromRank n = if n = 1 then ILArrayShape.SingleDimensional else ILArrayShape(List.replicate n (Some 0,None) |> List.toArray)


    and ILArrayShapeStatics() =
        static let singleDimensional = ILArrayShape [| (Some 0, None) |]
        static member SingleDimensional = singleDimensional

    /// Calling conventions.  These are used in method pointer types.
    [<StructuralEquality; StructuralComparison; RequireQualifiedAccess>]
    type ILArgConvention =
        | Default
        | CDecl
        | StdCall
        | ThisCall
        | FastCall
        | VarArg

    [<StructuralEquality; StructuralComparison; RequireQualifiedAccess>]
    type ILThisConvention =
        | Instance
        | InstanceExplicit
        | Static

    [<StructuralEquality; StructuralComparison>]
    type ILCallingConv =
        | Callconv of ILThisConvention * ILArgConvention
        member x.ThisConv = let (Callconv(a,_b)) = x in a
        member x.BasicConv = let (Callconv(_a,b)) = x in b
        member x.IsInstance = match x.ThisConv with ILThisConvention.Instance -> true | _ -> false
        member x.IsInstanceExplicit = match x.ThisConv with ILThisConvention.InstanceExplicit -> true | _ -> false
        member x.IsStatic = match x.ThisConv with ILThisConvention.Static -> true | _ -> false

        static member Instance = ILCallingConvStatics.Instance
        static member Static = ILCallingConvStatics.Static

    /// Static storage to amortize the allocation of ILCallingConv.Instance and ILCallingConv.Static
    and ILCallingConvStatics() =
        static let instanceCallConv = Callconv(ILThisConvention.Instance,ILArgConvention.Default)
        static let staticCallConv =  Callconv(ILThisConvention.Static,ILArgConvention.Default)
        static member Instance = instanceCallConv
        static member Static = staticCallConv

    type ILBoxity =
        | AsObject
        | AsValue

    [<RequireQualifiedAccess>]
    type ILTypeRefScope =
        | Top of ILScopeRef
        | Nested of ILTypeRef
        member x.AddQualifiedNameExtension(basic) =
            match x with
            | Top scoref ->
                let sco = scoref.QualifiedName
                if sco = "" then basic else String.concat ", " [basic;sco]
            | Nested tref ->
                tref.AddQualifiedNameExtension(basic)


    // IL type references have a pre-computed hash code to enable quick lookup tables during binary generation.
    and ILTypeRef(enc: ILTypeRefScope, nsp: string StructOption, name: string) =

        member x.Scope = enc
        member x.Name = name
        member x.Namespace = nsp

        member tref.FullName =
            match enc with
            | ILTypeRefScope.Top _ -> joinILTypeName tref.Namespace tref.Name
            | ILTypeRefScope.Nested enc -> enc.FullName + "." + tref.Name

        member tref.BasicQualifiedName =
            match enc with
            | ILTypeRefScope.Top _ -> tref.Name
            | ILTypeRefScope.Nested enc -> enc.BasicQualifiedName + "+" + tref.Name

        member tref.AddQualifiedNameExtension(basic) = enc.AddQualifiedNameExtension(basic)

        member tref.QualifiedName = enc.AddQualifiedNameExtension(tref.BasicQualifiedName)

        override x.ToString() = x.FullName


    and ILTypeSpec(typeRef: ILTypeRef, inst: ILGenericArgs) =
        member x.TypeRef = typeRef
        member x.Scope = x.TypeRef.Scope
        member x.Name = x.TypeRef.Name
        member x.Namespace = x.TypeRef.Namespace
        member x.GenericArgs = inst
        member x.BasicQualifiedName =
            let tc = x.TypeRef.BasicQualifiedName
            if x.GenericArgs.Length = 0 then
                tc
            else
                tc + "[" + String.concat "," (x.GenericArgs |> Array.map (fun arg -> "[" + arg.QualifiedName + "]")) + "]"

        member x.AddQualifiedNameExtension(basic) =
            x.TypeRef.AddQualifiedNameExtension(basic)

        member x.FullName = x.TypeRef.FullName

        override x.ToString() = x.TypeRef.ToString() + (if x.GenericArgs.Length = 0 then "" else "<...>")

    and [<RequireQualifiedAccess>]
        ILType =
        | Void
        | Array    of ILArrayShape * ILType
        | Value    of ILTypeSpec
        | Boxed    of ILTypeSpec
        | Ptr      of ILType
        | Byref    of ILType
        | FunctionPointer     of ILCallingSignature
        | Var    of int
        | Modified of bool * ILTypeRef * ILType

        member x.BasicQualifiedName =
            match x with
            | ILType.Var n -> "!" + string n
            | ILType.Modified(_,_ty1,ty2) -> ty2.BasicQualifiedName
            | ILType.Array (ILArrayShape(s),ty) -> ty.BasicQualifiedName + "[" + System.String(',',s.Length-1) + "]"
            | ILType.Value tr | ILType.Boxed tr -> tr.BasicQualifiedName
            | ILType.Void -> "void"
            | ILType.Ptr _ty -> failwith "unexpected pointer type"
            | ILType.Byref _ty -> failwith "unexpected byref type"
            | ILType.FunctionPointer _mref -> failwith "unexpected function pointer type"

        member x.AddQualifiedNameExtension(basic) =
            match x with
            | ILType.Var _n -> basic
            | ILType.Modified(_,_ty1,ty2) -> ty2.AddQualifiedNameExtension(basic)
            | ILType.Array (ILArrayShape(_s),ty) -> ty.AddQualifiedNameExtension(basic)
            | ILType.Value tr | ILType.Boxed tr -> tr.AddQualifiedNameExtension(basic)
            | ILType.Void -> failwith "void"
            | ILType.Ptr _ty -> failwith "unexpected pointer type"
            | ILType.Byref _ty -> failwith "unexpected byref type"
            | ILType.FunctionPointer _mref -> failwith "unexpected function pointer type"

        member x.QualifiedName =
            x.AddQualifiedNameExtension(x.BasicQualifiedName)

        member x.TypeSpec =
          match x with
          | ILType.Boxed tr | ILType.Value tr -> tr
          | _ -> invalidOp "not a nominal type"

        member x.Boxity =
          match x with
          | ILType.Boxed _ -> AsObject
          | ILType.Value _ -> AsValue
          | _ -> invalidOp "not a nominal type"

        member x.TypeRef =
          match x with
          | ILType.Boxed tspec | ILType.Value tspec -> tspec.TypeRef
          | _ -> invalidOp "not a nominal type"

        member x.IsNominal =
          match x with
          | ILType.Boxed _ | ILType.Value _ -> true
          | _ -> false

        member x.GenericArgs =
          match x with
          | ILType.Boxed tspec | ILType.Value tspec -> tspec.GenericArgs
          | _ -> [| |]

        member x.IsTyvar =
          match x with
          | ILType.Var _ -> true | _ -> false

        override x.ToString() = x.QualifiedName

    and ILCallingSignature(callingConv: ILCallingConv, argTypes: ILTypes, returnType: ILType) =
        member __.CallingConv = callingConv
        member __.ArgTypes = argTypes
        member __.ReturnType = returnType

    and ILGenericArgs = ILType[]
    and ILTypes = ILType[]


    type ILMethodRef(parent: ILTypeRef, callconv: ILCallingConv, genericArity: int, name: string, args: ILTypes, ret: ILType) =
        member x.EnclosingTypeRef = parent
        member x.CallingConv = callconv
        member x.Name = name
        member x.GenericArity = genericArity
        member x.ArgCount = args.Length
        member x.ArgTypes = args
        member x.ReturnType = ret

        member x.CallingSignature = ILCallingSignature (x.CallingConv,x.ArgTypes,x.ReturnType)
        override x.ToString() = x.EnclosingTypeRef.ToString() + "::" + x.Name + "(...)"


    type ILFieldRef(enclosingTypeRef: ILTypeRef, name: string, typ: ILType) =
        member __.EnclosingTypeRef = enclosingTypeRef
        member __.Name = name
        member __.Type = typ
        override x.ToString() = x.EnclosingTypeRef.ToString() + "::" + x.Name

    type ILMethodSpec(methodRef: ILMethodRef, enclosingType: ILType, methodInst: ILGenericArgs) =
        member x.MethodRef = methodRef
        member x.EnclosingType=enclosingType
        member x.GenericArgs=methodInst
        member x.Name=x.MethodRef.Name
        member x.CallingConv=x.MethodRef.CallingConv
        member x.GenericArity = x.MethodRef.GenericArity
        member x.FormalArgTypes = x.MethodRef.ArgTypes
        member x.FormalReturnType = x.MethodRef.ReturnType
        override x.ToString() = x.MethodRef.ToString() + "(...)"

    type ILFieldSpec(fieldRef: ILFieldRef, enclosingType: ILType) =
        member x.FieldRef = fieldRef
        member x.EnclosingType = enclosingType
        member x.FormalType = fieldRef.Type
        member x.Name = fieldRef.Name
        member x.EnclosingTypeRef = fieldRef.EnclosingTypeRef
        override x.ToString() = x.FieldRef.ToString()

    type ILPlatform =
        | X86
        | AMD64
        | IA64

    type ILCustomAttrArg =  (ILType * obj)
    type ILCustomAttrNamedArg =  (string * ILType * bool * obj)
    type ILCustomAttr =
        { Method: ILMethodSpec;
          Data: byte[] }

    type ILCustomAttrs =
       abstract Elements : ILCustomAttr[]

    type ILCustomAttrsStatics() =
       static let empty = { new ILCustomAttrs with member __.Elements = [| |] }
       static member Empty = empty

    [<RequireQualifiedAccess>]
    type ILMemberAccess =
        | Assembly
        | CompilerControlled
        | FamilyAndAssembly
        | FamilyOrAssembly
        | Family
        | Private
        | Public

    [<RequireQualifiedAccess>]
    type ILFieldInit =
        | String of string
        | Bool of bool
        | Char of uint16
        | Int8 of int8
        | Int16 of int16
        | Int32 of int32
        | Int64 of int64
        | UInt8 of uint8
        | UInt16 of uint16
        | UInt32 of uint32
        | UInt64 of uint64
        | Single of single
        | Double of double
        | Null

    type ILParameter =
        { Name: string StructOption
          ParameterType: ILType
          Default: ILFieldInit option
          //Marshal: ILNativeType option
          Attributes: ParameterAttributes
          CustomAttrs: ILCustomAttrs }
        member x.IsIn = ((x.Attributes &&& ParameterAttributes.In) <> enum 0)
        member x.IsOut = ((x.Attributes &&& ParameterAttributes.Out) <> enum 0)
        member x.IsOptional = ((x.Attributes &&& ParameterAttributes.Optional) <> enum 0)

    type ILParameters = ILParameter[]

    type ILReturn =
        { //Marshal: ILNativeType option;
          Type: ILType;
          CustomAttrs: ILCustomAttrs }

    type ILOverridesSpec =
        | OverridesSpec of ILMethodRef * ILType
        member x.MethodRef = let (OverridesSpec(mr,_ty)) = x in mr
        member x.EnclosingType = let (OverridesSpec(_mr,ty)) = x in ty

    type ILGenericParameterDef =
        { Name: string
          Constraints: ILTypes
          Attributes: GenericParameterAttributes
          CustomAttrs : ILCustomAttrs }

        member x.HasReferenceTypeConstraint= (x.Attributes &&& GenericParameterAttributes.ReferenceTypeConstraint) <> enum 0
        member x.HasNotNullableValueTypeConstraint= (x.Attributes &&& GenericParameterAttributes.NotNullableValueTypeConstraint) <> enum 0
        member x.HasDefaultConstructorConstraint= (x.Attributes &&& GenericParameterAttributes.DefaultConstructorConstraint) <> enum 0
        member x.IsCovariant = (x.Attributes &&& GenericParameterAttributes.Covariant) <> enum 0
        member x.IsContravariant = (x.Attributes &&& GenericParameterAttributes.Contravariant) <> enum 0
        override x.ToString() = x.Name

    type ILGenericParameterDefs = ILGenericParameterDef[]

    [<NoComparison; NoEquality>]
    type ILMethodDef =
        { MetadataToken: int32
          Name: string
          CallingConv: ILCallingConv
          Parameters: ILParameters
          Return: ILReturn
          Access: ILMemberAccess
          //mdBody: ILMethodBody
          ImplementationFlags : MethodImplAttributes
          //IsInternalCall: bool
          //IsManaged: bool
          //IsForwardRef: bool
          //SecurityDecls: ILPermissions
          //HasSecurity: bool
          //IsEntryPoint:bool
          //IsSynchronized: bool
          //IsPreserveSig: bool
          //IsMustRun: bool
          //IsNoInline: bool
          Attributes : MethodAttributes
          GenericParams: ILGenericParameterDefs
          CustomAttrs: ILCustomAttrs }
        member x.ParameterTypes = x.Parameters |> Array.map (fun p -> p.ParameterType)
        member x.IsStatic = x.Attributes &&& MethodAttributes.Static <> enum 0
        member x.IsAbstract = x.Attributes &&& MethodAttributes.Abstract <> enum 0
        member x.IsVirtual = x.Attributes &&& MethodAttributes.Virtual <> enum 0
        member x.IsCheckAccessOnOverride = x.Attributes &&& MethodAttributes.CheckAccessOnOverride <> enum 0
        member x.IsNewSlot = x.Attributes &&& MethodAttributes.NewSlot <> enum 0
        member x.IsFinal = x.Attributes &&& MethodAttributes.Final <> enum 0
        member x.IsSpecialName = x.Attributes &&& MethodAttributes.SpecialName <> enum 0
        member x.IsRTSpecialName = x.Attributes &&& MethodAttributes.RTSpecialName <> enum 0
        member x.IsHideBySig = x.Attributes &&& MethodAttributes.HideBySig <> enum 0
        member x.IsClassInitializer = x.Name = ".cctor"
        member x.IsConstructor = x.Name = ".ctor"
        member md.CallingSignature =  ILCallingSignature (md.CallingConv,md.ParameterTypes,md.Return.Type)
        override x.ToString() = "method " + x.Name

    type ILMethodDefs(larr: Lazy<ILMethodDef[]>) =

        let mutable lmap = null
        let getmap() =
            if lmap = null then
                lmap <- Dictionary()
                for y in larr.Force() do
                    let key = y.Name
                    if lmap.ContainsKey key then
                        lmap.[key] <- Array.append [| y |] lmap.[key]
                    else
                        lmap.[key] <- [| y |]
            lmap

        member x.Elements = larr.Force()
        member x.FindByName nm =  getmap().[nm]
        member x.FindByNameAndArity (nm,arity) =  x.FindByName nm |> Array.filter (fun x -> x.Parameters.Length = arity)


    [<NoComparison; NoEquality>]
    type ILEventDef =
        { //EventHandlerType: ILType option
          Name: string
          IsRTSpecialName: bool
          IsSpecialName: bool
          Attributes : System.Reflection.EventAttributes
          AddMethod: ILMethodRef
          RemoveMethod: ILMethodRef
          //FireMethod: ILMethodRef option
          //OtherMethods: ILMethodRef[]
          CustomAttrs: ILCustomAttrs }
        member x.EventHandlerType = x.AddMethod.ArgTypes.[0]
        member x.IsStatic = x.AddMethod.CallingConv.IsStatic
        override x.ToString() = "event " + x.Name

    type ILEventDefs =
        abstract Elements : ILEventDef[]

    [<NoComparison; NoEquality>]
    type ILPropertyDef =
        { Name: string
          Attributes : System.Reflection.PropertyAttributes
          SetMethod: ILMethodRef option
          GetMethod: ILMethodRef option
          CallingConv: ILThisConvention
          PropertyType: ILType
          Init: ILFieldInit option
          IndexParameterTypes: ILTypes
          CustomAttrs: ILCustomAttrs }
        member x.IsStatic = (match x.CallingConv with ILThisConvention.Static -> true | _ -> false)
        member x.IndexParameters = x.IndexParameterTypes |> Array.mapi (fun i ty ->
            {  Name = USome("arg"+string i)
               ParameterType = ty
               Default = None
               Attributes = ParameterAttributes.None
               CustomAttrs = ILCustomAttrsStatics.Empty })
        override x.ToString() = "property " + x.Name

    type ILPropertyDefs =
        abstract Elements : ILPropertyDef[]

    [<NoComparison; NoEquality>]
    type ILFieldDef =
        { Name: string
          FieldType: ILType
          IsStatic: bool
          Access: ILMemberAccess
          Attributes : System.Reflection.FieldAttributes
          //Data:  byte[] option
          LiteralValue:  ILFieldInit option
          //Offset:  int32 option
          IsSpecialName: bool
          //Marshal: ILNativeType option
          NotSerialized: bool
          IsLiteral: bool
          IsInitOnly: bool
          CustomAttrs: ILCustomAttrs }
        override x.ToString() = "field " + x.Name


    type ILFieldDefs =
        abstract Elements : ILFieldDef[]

    type ILMethodImplDef =
        { Overrides: ILOverridesSpec;
          OverrideBy: ILMethodSpec }

    // Index table by name and arity.
    type ILMethodImplDefs =
        abstract Elements : ILMethodImplDef[]

    and MethodImplsMap = Map<string * int, ILMethodImplDef array>

    [<RequireQualifiedAccess>]
    type ILTypeInit =
        | BeforeField
        | OnAny

    [<RequireQualifiedAccess>]
    type ILDefaultPInvokeEncoding =
        | Ansi
        | Auto
        | Unicode

    type ILTypeDefAccess =
        | Public
        | Private
        | Nested of ILMemberAccess

    [<RequireQualifiedAccess>]
    type ILTypeDefKind =
        | Class
        | ValueType
        | Interface
        | Enum
        | Delegate

    [<NoComparison; NoEquality>]
    type ILTypeDef =
        { Kind: ILTypeDefKind
          Namespace: string StructOption
          Name: string
          GenericParams: ILGenericParameterDefs
          Access: ILTypeDefAccess
          Attributes: TypeAttributes
          Encoding: ILDefaultPInvokeEncoding
          NestedTypes: ILTypeDefs
          Implements: ILTypes
          Extends: ILType option
          Methods: ILMethodDefs
          Fields: ILFieldDefs
          InitSemantics: ILTypeInit
          Events: ILEventDefs
          Properties: ILPropertyDefs
          CustomAttrs: ILCustomAttrs
          Token : int }
        member x.IsClass =     (match x.Kind with ILTypeDefKind.Class -> true | _ -> false)
        member x.IsInterface = (match x.Kind with ILTypeDefKind.Interface -> true | _ -> false)
        member x.IsEnum =      (match x.Kind with ILTypeDefKind.Enum -> true | _ -> false)
        member x.IsDelegate =  (match x.Kind with ILTypeDefKind.Delegate -> true | _ -> false)
        member x.IsAbstract= (x.Attributes &&& TypeAttributes.Abstract) <> enum 0
        member x.IsSealed= (x.Attributes &&& TypeAttributes.Sealed) <> enum 0
        member x.IsSerializable= (x.Attributes &&& TypeAttributes.Serializable) <> enum 0
        member x.IsComInterop= (x.Attributes &&& TypeAttributes.Import) <> enum 0
        member x.IsSpecialName= (x.Attributes &&& TypeAttributes.SpecialName) <> enum 0

        member tdef.IsStructOrEnum =
            match tdef.Kind with
            | ILTypeDefKind.ValueType | ILTypeDefKind.Enum -> true
            | _ -> false

        override x.ToString() = "type " + x.Name

    and ILTypeDefs(larr : Lazy<(string StructOption * string * Lazy<ILTypeDef>)[]>) =

        let mutable lmap = null
        let getmap() =
            if isNull lmap then
                lmap <- Dictionary()
                for (nsp, nm, ltd) in larr.Force() do
                    let key = nsp, nm
                    lmap.[key] <- ltd
            lmap

        member x.Elements =
            [| for (_,_,td) in larr.Force() -> td.Force() |]

        member x.TryFindByName (nsp,nm)  =
            let tdefs = getmap()
            let key = (nsp,nm)
            if tdefs.ContainsKey key then
                Some (tdefs.[key].Force())
            else
                None

    type ILNestedExportedType =
        { Name: string
          Access: ILMemberAccess
          Nested: ILNestedExportedTypesAndForwarders
          CustomAttrs: ILCustomAttrs }
        override x.ToString() = "nested fwd " + x.Name

    and ILNestedExportedTypesAndForwarders(larr:Lazy<ILNestedExportedType[]>) =
        let lmap = lazy ((Map.empty, larr.Force()) ||> Array.fold (fun m x -> m.Add(x.Name,x)))
        member x.Elements = larr.Force()
        member x.TryFindByName nm = lmap.Force().TryFind nm

    and [<NoComparison; NoEquality>]
        ILExportedTypeOrForwarder =
        { ScopeRef: ILScopeRef
          Namespace : string StructOption
          Name: string
          IsForwarder: bool }
        override x.ToString() = "fwd " + x.Name

    and ILExportedTypesAndForwarders(larr:Lazy<ILExportedTypeOrForwarder[]>) =
        let mutable lmap = null
        let getmap() =
            if lmap = null then
                lmap <- Dictionary()
                for ltd in larr.Force() do
                    let key = ltd.Namespace, ltd.Name
                    lmap.[key] <- ltd
            lmap
        member x.Elements = larr.Force()
        member x.TryFindByName (nsp,nm) = match getmap().TryGetValue ((nsp,nm)) with true,v -> Some v | false, _ -> None

    [<RequireQualifiedAccess>]
    type ILResourceAccess =
        | Public
        | Private

    [<RequireQualifiedAccess>]
    type ILResourceLocation =
        | Local of (unit -> byte[])
        | File of ILModuleRef * int32
        | Assembly of ILAssemblyRef

    type ILResource =
        { Name: string
          Location: ILResourceLocation
          Access: ILResourceAccess
          CustomAttrs: ILCustomAttrs }
        override x.ToString() = "resource " + x.Name

    type ILResources(larr: Lazy<ILResource[]>) =
        member x.Elements = larr.Force()

    type ILAssemblyManifest =
        { Name: string
          PublicKey: byte[] option
          Version: Version option
          Locale: string StructOption
          CustomAttrs: ILCustomAttrs
          Retargetable: bool
          ExportedTypes: ILExportedTypesAndForwarders
          EntrypointElsewhere: ILModuleRef option }
        member x.GetName() =
            let asmName = AssemblyName(Name=x.Name)
            x.PublicKey |> Option.iter (fun bytes -> asmName.SetPublicKey(bytes))
            x.Version |> Option.iter (fun v -> asmName.Version <- v)
    #if NETSTANDARD
            asmName.CultureName <- System.Globalization.CultureInfo.InvariantCulture.Name
    #else
            asmName.CultureInfo <- System.Globalization.CultureInfo.InvariantCulture
    #endif
            asmName
        override x.ToString() = "manifest " + x.Name

    type ILModuleDef =
        { Manifest: ILAssemblyManifest option
          CustomAttrs: ILCustomAttrs
          Name: string
          TypeDefs: ILTypeDefs
          Resources: ILResources  }

        member x.ManifestOfAssembly =
            match x.Manifest with
            | Some m -> m
            | None -> failwith "no manifest"

        member m.HasManifest = m.Manifest.IsSome

        override x.ToString() = "module " + x.Name


    [<NoEquality; NoComparison>]
    type ILGlobals =
        { typ_Object: ILType
          typ_String: ILType
          typ_Type: ILType
          typ_TypedReference: ILType option
          typ_SByte: ILType
          typ_Int16: ILType
          typ_Int32: ILType
          typ_Int64: ILType
          typ_Byte: ILType
          typ_UInt16: ILType
          typ_UInt32: ILType
          typ_UInt64: ILType
          typ_Single : ILType
          typ_Double: ILType
          typ_Boolean: ILType
          typ_Char: ILType
          typ_IntPtr: ILType
          typ_UIntPtr: ILType
          systemRuntimeScopeRef : ILScopeRef }
        override x.ToString() = "<ILGlobals>"

    [<Struct>]
    type ILTableName(idx: int) =
        member x.Index = idx
        static member FromIndex n = ILTableName n

    module private ILTableNames =
        let Module = ILTableName 0
        let TypeRef = ILTableName 1
        let TypeDef = ILTableName 2
        let FieldPtr = ILTableName 3
        let Field = ILTableName 4
        let MethodPtr = ILTableName 5
        let Method = ILTableName 6
        let ParamPtr = ILTableName 7
        let Param = ILTableName 8
        let InterfaceImpl = ILTableName 9
        let MemberRef = ILTableName 10
        let Constant = ILTableName 11
        let CustomAttribute = ILTableName 12
        let FieldMarshal = ILTableName 13
        let Permission = ILTableName 14
        let ClassLayout = ILTableName 15
        let FieldLayout = ILTableName 16
        let StandAloneSig = ILTableName 17
        let EventMap = ILTableName 18
        let EventPtr = ILTableName 19
        let Event = ILTableName 20
        let PropertyMap = ILTableName 21
        let PropertyPtr = ILTableName 22
        let Property = ILTableName 23
        let MethodSemantics = ILTableName 24
        let MethodImpl = ILTableName 25
        let ModuleRef = ILTableName 26
        let TypeSpec = ILTableName 27
        let ImplMap = ILTableName 28
        let FieldRVA = ILTableName 29
        let ENCLog = ILTableName 30
        let ENCMap = ILTableName 31
        let Assembly = ILTableName 32
        let AssemblyProcessor = ILTableName 33
        let AssemblyOS = ILTableName 34
        let AssemblyRef = ILTableName 35
        let AssemblyRefProcessor = ILTableName 36
        let AssemblyRefOS = ILTableName 37
        let File = ILTableName 38
        let ExportedType = ILTableName 39
        let ManifestResource = ILTableName 40
        let Nested = ILTableName 41
        let GenericParam = ILTableName 42
        let MethodSpec = ILTableName 43
        let GenericParamConstraint = ILTableName 44
        let UserStrings = ILTableName 0x70 (* Special encoding of embedded UserString tokens - See 1.9 Partition III *)

    [<Struct>]
    type TypeDefOrRefOrSpecTag(tag: int32) =
        member x.Tag = tag
        static member TypeDef = TypeDefOrRefOrSpecTag 0x00
        static member TypeRef = TypeDefOrRefOrSpecTag 0x01
        static member TypeSpec = TypeDefOrRefOrSpecTag 0x2

    [<Struct>]
    type HasConstantTag(tag: int32) =
        member x.Tag = tag
        static member FieldDef = HasConstantTag 0x0
        static member ParamDef = HasConstantTag 0x1
        static member Property = HasConstantTag 0x2

    [<Struct>]
    type HasCustomAttributeTag(tag: int32) =
        member x.Tag = tag
        static member MethodDef = HasCustomAttributeTag 0x0
        static member FieldDef = HasCustomAttributeTag 0x1
        static member TypeRef = HasCustomAttributeTag 0x2
        static member TypeDef = HasCustomAttributeTag 0x3
        static member ParamDef = HasCustomAttributeTag 0x4
        static member InterfaceImpl = HasCustomAttributeTag 0x5
        static member MemberRef = HasCustomAttributeTag 0x6
        static member Module = HasCustomAttributeTag 0x7
        static member Permission = HasCustomAttributeTag 0x8
        static member Property = HasCustomAttributeTag 0x9
        static member Event = HasCustomAttributeTag 0xa
        static member StandAloneSig = HasCustomAttributeTag 0xb
        static member ModuleRef = HasCustomAttributeTag 0xc
        static member TypeSpec = HasCustomAttributeTag 0xd
        static member Assembly = HasCustomAttributeTag 0xe
        static member AssemblyRef = HasCustomAttributeTag 0xf
        static member File = HasCustomAttributeTag 0x10
        static member ExportedType = HasCustomAttributeTag 0x11
        static member ManifestResource = HasCustomAttributeTag 0x12
        static member GenericParam = HasCustomAttributeTag 0x13
        static member GenericParamConstraint = HasCustomAttributeTag 0x14
        static member MethodSpec = HasCustomAttributeTag 0x15

    [<Struct>]
    type HasFieldMarshalTag(tag: int32) =
        member x.Tag = tag
        static member FieldDef =  HasFieldMarshalTag 0x00
        static member ParamDef =  HasFieldMarshalTag 0x01

    [<Struct>]
    type HasDeclSecurityTag(tag: int32) =
        member x.Tag = tag
        static member TypeDef =  HasDeclSecurityTag 0x00
        static member MethodDef =  HasDeclSecurityTag 0x01
        static member Assembly =  HasDeclSecurityTag 0x02

    [<Struct>]
    type MemberRefParentTag(tag: int32) =
        member x.Tag = tag
        static member TypeRef = MemberRefParentTag 0x01
        static member ModuleRef = MemberRefParentTag 0x02
        static member MethodDef = MemberRefParentTag 0x03
        static member TypeSpec = MemberRefParentTag 0x04

    [<Struct>]
    type HasSemanticsTag(tag: int32) =
        member x.Tag = tag
        static member Event =  HasSemanticsTag 0x00
        static member Property =  HasSemanticsTag 0x01

    [<Struct>]
    type MethodDefOrRefTag(tag: int32) =
        member x.Tag = tag
        static member MethodDef =  MethodDefOrRefTag 0x00
        static member MemberRef =  MethodDefOrRefTag 0x01
        static member MethodSpec =  MethodDefOrRefTag 0x02

    [<Struct>]
    type MemberForwardedTag(tag: int32) =
        member x.Tag = tag
        static member FieldDef =  MemberForwardedTag 0x00
        static member MethodDef =  MemberForwardedTag 0x01

    [<Struct>]
    type ImplementationTag(tag: int32) =
        member x.Tag = tag
        static member File =  ImplementationTag 0x00
        static member AssemblyRef =  ImplementationTag 0x01
        static member ExportedType =  ImplementationTag 0x02

    [<Struct>]
    type CustomAttributeTypeTag(tag: int32) =
        member x.Tag = tag
        static member MethodDef =  CustomAttributeTypeTag 0x02
        static member MemberRef =  CustomAttributeTypeTag 0x03

    [<Struct>]
    type ResolutionScopeTag(tag: int32) =
        member x.Tag = tag
        static member Module =  ResolutionScopeTag 0x00
        static member ModuleRef =  ResolutionScopeTag 0x01
        static member AssemblyRef =  ResolutionScopeTag 0x02
        static member TypeRef =  ResolutionScopeTag 0x03

    [<Struct>]
    type TypeOrMethodDefTag(tag: int32) =
        member x.Tag = tag
        static member TypeDef = TypeOrMethodDefTag 0x00
        static member MethodDef = TypeOrMethodDefTag 0x01

    [<Struct>]
    type TaggedIndex<'T> =
        val tag: 'T
        val index : int32
        new(tag,index) = { tag=tag; index=index }


    type ILImageChunk = { size: int32; addr: int32 }

    type ILRowElementKind =
        | UShort
        | ULong
        | Byte
        | Data
        | GGuid
        | Blob
        | SString
        | SimpleIndex of ILTableName
        | TypeDefOrRefOrSpec
        | TypeOrMethodDef
        | HasConstant
        | HasCustomAttribute
        | HasFieldMarshal
        | HasDeclSecurity
        | MemberRefParent
        | HasSemantics
        | MethodDefOrRef
        | MemberForwarded
        | Implementation
        | CustomAttributeType
        | ResolutionScope

    type ILRowKind = ILRowKind of ILRowElementKind list

    type TypeDefAsTypIdx = TypeDefAsTypIdx of ILBoxity * ILGenericArgs * int
    type TypeRefAsTypIdx = TypeRefAsTypIdx of ILBoxity * ILGenericArgs * int
    type BlobAsMethodSigIdx = BlobAsMethodSigIdx of int * int32
    type BlobAsFieldSigIdx = BlobAsFieldSigIdx of int * int32
    type BlobAsPropSigIdx = BlobAsPropSigIdx of int * int32
    type BlobAsLocalSigIdx = BlobAsLocalSigIdx of int * int32
    type MemberRefAsMspecIdx =  MemberRefAsMspecIdx of int * int
    type MethodSpecAsMspecIdx =  MethodSpecAsMspecIdx of int * int
    type MemberRefAsFspecIdx = MemberRefAsFspecIdx of int * int
    type CustomAttrIdx = CustomAttrIdx of CustomAttributeTypeTag * int * int32
    type SecurityDeclIdx = SecurityDeclIdx of uint16 * int32
    type GenericParamsIdx = GenericParamsIdx of int * TypeOrMethodDefTag * int

    type ILVarArgs = ILTypes option
    type MethodData = MethodData of ILType * ILCallingConv * string * ILTypes * ILType * ILTypes
    type VarArgMethodData = VarArgMethodData of ILType * ILCallingConv * string * ILTypes * ILVarArgs * ILType * ILTypes

    [<AutoOpen>]
    module Constants = 
        let et_END = 0x00uy
        let et_VOID = 0x01uy
        let et_BOOLEAN = 0x02uy
        let et_CHAR = 0x03uy
        let et_I1 = 0x04uy
        let et_U1 = 0x05uy
        let et_I2 = 0x06uy
        let et_U2 = 0x07uy
        let et_I4 = 0x08uy
        let et_U4 = 0x09uy
        let et_I8 = 0x0Auy
        let et_U8 = 0x0Buy
        let et_R4 = 0x0Cuy
        let et_R8 = 0x0Duy
        let et_STRING = 0x0Euy
        let et_PTR = 0x0Fuy
        let et_BYREF = 0x10uy
        let et_VALUETYPE = 0x11uy
        let et_CLASS = 0x12uy
        let et_VAR = 0x13uy
        let et_ARRAY = 0x14uy
        let et_WITH = 0x15uy
        let et_TYPEDBYREF = 0x16uy
        let et_I = 0x18uy
        let et_U = 0x19uy
        let et_FNPTR = 0x1Buy
        let et_OBJECT = 0x1Cuy
        let et_SZARRAY = 0x1Duy
        let et_MVAR = 0x1euy
        let et_CMOD_REQD = 0x1Fuy
        let et_CMOD_OPT = 0x20uy

        let et_SENTINEL = 0x41uy // sentinel for varargs
        let et_PINNED = 0x45uy

        let e_IMAGE_CEE_CS_CALLCONV_FASTCALL = 0x04uy
        let e_IMAGE_CEE_CS_CALLCONV_STDCALL = 0x02uy
        let e_IMAGE_CEE_CS_CALLCONV_THISCALL = 0x03uy
        let e_IMAGE_CEE_CS_CALLCONV_CDECL = 0x01uy
        let e_IMAGE_CEE_CS_CALLCONV_VARARG = 0x05uy
        let e_IMAGE_CEE_CS_CALLCONV_FIELD = 0x06uy
        let e_IMAGE_CEE_CS_CALLCONV_LOCAL_SIG = 0x07uy
        let e_IMAGE_CEE_CS_CALLCONV_PROPERTY = 0x08uy

        let e_IMAGE_CEE_CS_CALLCONV_GENERICINST = 0x0auy
        let e_IMAGE_CEE_CS_CALLCONV_GENERIC = 0x10uy
        let e_IMAGE_CEE_CS_CALLCONV_INSTANCE = 0x20uy
        let e_IMAGE_CEE_CS_CALLCONV_INSTANCE_EXPLICIT = 0x40uy


        // Logical shift right treating int32 as unsigned integer.
        // Code that uses this should probably be adjusted to use unsigned integer types.
        let (>>>&) (x:int32) (n:int32) = int32 (uint32 x >>> n)

        let align alignment n = ((n + alignment - 0x1) / alignment) * alignment

        let uncodedToken (tab:ILTableName) idx = ((tab.Index <<< 24) ||| idx)

        let i32ToUncodedToken tok  =
            let idx = tok &&& 0xffffff
            let tab = tok >>>& 24
            (ILTableName.FromIndex tab,  idx)


        let uncodedTokenToTypeDefOrRefOrSpec (tab,tok) =
            let tag =
                if tab = ILTableNames.TypeDef then TypeDefOrRefOrSpecTag.TypeDef
                elif tab = ILTableNames.TypeRef then TypeDefOrRefOrSpecTag.TypeRef
                elif tab = ILTableNames.TypeSpec then TypeDefOrRefOrSpecTag.TypeSpec
                else failwith "bad table in uncodedTokenToTypeDefOrRefOrSpec"
            TaggedIndex(tag,tok)

        let uncodedTokenToMethodDefOrRef (tab,tok) =
            let tag =
                if tab = ILTableNames.Method then MethodDefOrRefTag.MethodDef
                elif tab = ILTableNames.MemberRef then MethodDefOrRefTag.MemberRef
                else failwith "bad table in uncodedTokenToMethodDefOrRef"
            TaggedIndex(tag,tok)

        let (|TaggedIndex|) (x:TaggedIndex<'T>) = x.tag, x.index
        let tokToTaggedIdx f nbits tok =
            let tagmask =
                if nbits = 1 then 1
                elif nbits = 2 then 3
                elif nbits = 3 then 7
                elif nbits = 4 then 15
                   elif nbits = 5 then 31
                   else failwith "too many nbits"
            let tag = tok &&& tagmask
            let idx = tok >>>& nbits
            TaggedIndex(f tag, idx)

    type ByteFile(bytes:byte[]) =

        member x.Bytes = bytes
        member mc.ReadByte addr = bytes.[addr]
        member mc.ReadBytes addr len = Array.sub bytes addr len
        member m.CountUtf8String addr =
            let mutable p = addr
            while bytes.[p] <> 0uy do
                p <- p + 1
            p - addr

        member m.ReadUTF8String addr =
            let n = m.CountUtf8String addr
            System.Text.Encoding.UTF8.GetString (bytes, addr, n)

        member is.ReadInt32 addr =
            let b0 = is.ReadByte addr
            let b1 = is.ReadByte (addr+1)
            let b2 = is.ReadByte (addr+2)
            let b3 = is.ReadByte (addr+3)
            int b0 ||| (int b1 <<< 8) ||| (int b2 <<< 16) ||| (int b3 <<< 24)

        member is.ReadUInt16 addr =
            let b0 = is.ReadByte addr
            let b1 = is.ReadByte (addr+1)
            uint16 b0 ||| (uint16 b1 <<< 8)

    [<AutoOpen>]
    module Reader =
        let seekReadByte (is:ByteFile) addr = is.ReadByte addr
        let seekReadBytes (is:ByteFile) addr len = is.ReadBytes addr len
        let seekReadInt32 (is:ByteFile) addr = is.ReadInt32 addr
        let seekReadUInt16 (is:ByteFile) addr = is.ReadUInt16 addr

        let seekReadByteAsInt32 is addr = int32 (seekReadByte is addr)

        let seekReadInt64 is addr =
            let b0 = seekReadByte is addr
            let b1 = seekReadByte is (addr+1)
            let b2 = seekReadByte is (addr+2)
            let b3 = seekReadByte is (addr+3)
            let b4 = seekReadByte is (addr+4)
            let b5 = seekReadByte is (addr+5)
            let b6 = seekReadByte is (addr+6)
            let b7 = seekReadByte is (addr+7)
            int64 b0 ||| (int64 b1 <<< 8) ||| (int64 b2 <<< 16) ||| (int64 b3 <<< 24) |||
            (int64 b4 <<< 32) ||| (int64 b5 <<< 40) ||| (int64 b6 <<< 48) ||| (int64 b7 <<< 56)

        let seekReadUInt16AsInt32 is addr = int32 (seekReadUInt16 is addr)

        let seekReadCompressedUInt32 is addr =
            let b0 = seekReadByte is addr
            if b0 <= 0x7Fuy then int b0, addr+1
            elif b0 <= 0xBFuy then
                let b0 = b0 &&& 0x7Fuy
                let b1 = seekReadByteAsInt32 is (addr+1)
                (int b0 <<< 8) ||| int b1, addr+2
            else
                let b0 = b0 &&& 0x3Fuy
                let b1 = seekReadByteAsInt32 is (addr+1)
                let b2 = seekReadByteAsInt32 is (addr+2)
                let b3 = seekReadByteAsInt32 is (addr+3)
                (int b0 <<< 24) ||| (int b1 <<< 16) ||| (int b2 <<< 8) ||| int b3, addr+4

        let seekReadSByte         is addr = sbyte (seekReadByte is addr)

        let rec seekCountUtf8String is addr n =
            let c = seekReadByteAsInt32 is addr
            if c = 0 then n
            else seekCountUtf8String is (addr+1) (n+1)

        let seekReadUTF8String is addr =
            let n = seekCountUtf8String is addr 0
            let bytes = seekReadBytes is addr n
            System.Text.Encoding.UTF8.GetString (bytes, 0, bytes.Length)

        let seekReadBlob is addr =
            let len, addr = seekReadCompressedUInt32 is addr
            seekReadBytes is addr len

        let seekReadUserString is addr =
            let len, addr = seekReadCompressedUInt32 is addr
            let bytes = seekReadBytes is addr (len - 1)
            System.Text.Encoding.Unicode.GetString(bytes, 0, bytes.Length)

        let seekReadGuid is addr =  seekReadBytes is addr 0x10

        let seekReadUncodedToken is addr  =
            i32ToUncodedToken (seekReadInt32 is addr)

        let sigptrGetByte (bytes:byte[]) sigptr =
            bytes.[sigptr], sigptr + 1

        let sigptrGetBool bytes sigptr =
            let b0,sigptr = sigptrGetByte bytes sigptr
            (b0 = 0x01uy) ,sigptr

        let sigptrGetSByte bytes sigptr =
            let i,sigptr = sigptrGetByte bytes sigptr
            sbyte i,sigptr

        let sigptrGetUInt16 bytes sigptr =
            let b0,sigptr = sigptrGetByte bytes sigptr
            let b1,sigptr = sigptrGetByte bytes sigptr
            uint16 (int b0 ||| (int b1 <<< 8)),sigptr

        let sigptrGetInt16 bytes sigptr =
            let u,sigptr = sigptrGetUInt16 bytes sigptr
            int16 u,sigptr

        let sigptrGetInt32 (bytes: byte[]) sigptr =
            let b0 = bytes.[sigptr]
            let b1 = bytes.[sigptr+1]
            let b2 = bytes.[sigptr+2]
            let b3 = bytes.[sigptr+3]
            let res = int b0 ||| (int b1 <<< 8) ||| (int b2 <<< 16) ||| (int b3 <<< 24)
            res, sigptr + 4

        let sigptrGetUInt32 bytes sigptr =
            let u,sigptr = sigptrGetInt32 bytes sigptr
            uint32 u,sigptr

        let sigptrGetUInt64 bytes sigptr =
            let u0,sigptr = sigptrGetUInt32 bytes sigptr
            let u1,sigptr = sigptrGetUInt32 bytes sigptr
            (uint64 u0 ||| (uint64 u1 <<< 32)),sigptr

        let sigptrGetInt64 bytes sigptr =
            let u,sigptr = sigptrGetUInt64 bytes sigptr
            int64 u,sigptr

        let sigptrGetSingle bytes sigptr =
            let u,sigptr = sigptrGetInt32 bytes sigptr
            singleOfBits u,sigptr

        let sigptrGetDouble bytes sigptr =
            let u,sigptr = sigptrGetInt64 bytes sigptr
            doubleOfBits u,sigptr

        let sigptrGetZInt32 bytes sigptr =
            let b0,sigptr = sigptrGetByte bytes sigptr
            if b0 <= 0x7Fuy then int b0, sigptr
            elif b0 <= 0xBFuy then
                let b0 = b0 &&& 0x7Fuy
                let b1,sigptr = sigptrGetByte bytes sigptr
                (int b0 <<< 8) ||| int b1, sigptr
            else
                let b0 = b0 &&& 0x3Fuy
                let b1,sigptr = sigptrGetByte bytes sigptr
                let b2,sigptr = sigptrGetByte bytes sigptr
                let b3,sigptr = sigptrGetByte bytes sigptr
                (int b0 <<< 24) ||| (int  b1 <<< 16) ||| (int b2 <<< 8) ||| int b3, sigptr

        let rec sigptrFoldAcc f n (bytes:byte[]) (sigptr:int) i acc =
            if i < n then
                let x,sp = f bytes sigptr
                sigptrFoldAcc f n bytes sp (i+1) (x::acc)
            else
                Array.ofList (List.rev acc), sigptr

        let sigptrFold f n (bytes:byte[]) (sigptr:int) =
            sigptrFoldAcc f n bytes sigptr 0 []

        let sigptrGetBytes n (bytes:byte[]) sigptr =
                let res = Array.zeroCreate n
                for i = 0 to (n - 1) do
                    res.[i] <- bytes.[sigptr + i]
                res, sigptr + n

        let sigptrGetString n bytes sigptr =
            let bytearray,sigptr = sigptrGetBytes n bytes sigptr
            (System.Text.Encoding.UTF8.GetString(bytearray, 0, bytearray.Length)),sigptr

        let chunk sz next = ({addr=next; size=sz},next + sz)
        let nochunk next = ({addr= 0x0;size= 0x0; } ,next)


        let kindAssemblyRef = ILRowKind [ UShort; UShort; UShort; UShort; ULong; Blob; SString; SString; Blob; ]
        let kindModuleRef = ILRowKind [ SString ]
        let kindFileRef = ILRowKind [ ULong; SString; Blob ]
        let kindTypeRef = ILRowKind [ ResolutionScope; SString; SString ]
        let kindTypeSpec = ILRowKind [ Blob ]
        let kindTypeDef = ILRowKind [ ULong; SString; SString; TypeDefOrRefOrSpec; SimpleIndex ILTableNames.Field; SimpleIndex ILTableNames.Method ]
        let kindPropertyMap = ILRowKind [ SimpleIndex ILTableNames.TypeDef; SimpleIndex ILTableNames.Property ]
        let kindEventMap = ILRowKind [ SimpleIndex ILTableNames.TypeDef; SimpleIndex ILTableNames.Event ]
        let kindInterfaceImpl = ILRowKind [ SimpleIndex ILTableNames.TypeDef; TypeDefOrRefOrSpec ]
        let kindNested = ILRowKind [ SimpleIndex ILTableNames.TypeDef; SimpleIndex ILTableNames.TypeDef ]
        let kindCustomAttribute = ILRowKind [ HasCustomAttribute; CustomAttributeType; Blob ]
        let kindDeclSecurity = ILRowKind [ UShort; HasDeclSecurity; Blob ]
        let kindMemberRef = ILRowKind [ MemberRefParent; SString; Blob ]
        let kindStandAloneSig = ILRowKind [ Blob ]
        let kindFieldDef = ILRowKind [ UShort; SString; Blob ]
        let kindFieldRVA = ILRowKind [ Data; SimpleIndex ILTableNames.Field ]
        let kindFieldMarshal = ILRowKind [ HasFieldMarshal; Blob ]
        let kindConstant = ILRowKind [ UShort;HasConstant; Blob ]
        let kindFieldLayout = ILRowKind [ ULong; SimpleIndex ILTableNames.Field ]
        let kindParam = ILRowKind [ UShort; UShort; SString ]
        let kindMethodDef = ILRowKind [ ULong;  UShort; UShort; SString; Blob; SimpleIndex ILTableNames.Param ]
        let kindMethodImpl = ILRowKind [ SimpleIndex ILTableNames.TypeDef; MethodDefOrRef; MethodDefOrRef ]
        let kindImplMap = ILRowKind [ UShort; MemberForwarded; SString; SimpleIndex ILTableNames.ModuleRef ]
        let kindMethodSemantics = ILRowKind [ UShort; SimpleIndex ILTableNames.Method; HasSemantics ]
        let kindProperty = ILRowKind [ UShort; SString; Blob ]
        let kindEvent = ILRowKind [ UShort; SString; TypeDefOrRefOrSpec ]
        let kindManifestResource = ILRowKind [ ULong; ULong; SString; Implementation ]
        let kindClassLayout = ILRowKind [ UShort; ULong; SimpleIndex ILTableNames.TypeDef ]
        let kindExportedType = ILRowKind [ ULong; ULong; SString; SString; Implementation ]
        let kindAssembly = ILRowKind [ ULong; UShort; UShort; UShort; UShort; ULong; Blob; SString; SString ]
        let kindGenericParam_v1_1 = ILRowKind [ UShort; UShort; TypeOrMethodDef; SString; TypeDefOrRefOrSpec ]
        let kindGenericParam_v2_0 = ILRowKind [ UShort; UShort; TypeOrMethodDef; SString ]
        let kindMethodSpec = ILRowKind [ MethodDefOrRef; Blob ]
        let kindGenericParamConstraint = ILRowKind [ SimpleIndex ILTableNames.GenericParam; TypeDefOrRefOrSpec ]
        let kindModule = ILRowKind [ UShort; SString; GGuid; GGuid; GGuid ]
        let kindIllegal = ILRowKind [ ]

        let hcCompare (TaggedIndex((t1: HasConstantTag), (idx1:int))) (TaggedIndex((t2: HasConstantTag), idx2)) =
            if idx1 < idx2 then -1 elif idx1 > idx2 then 1 else compare t1.Tag t2.Tag

        let hsCompare (TaggedIndex((t1:HasSemanticsTag), (idx1:int))) (TaggedIndex((t2:HasSemanticsTag), idx2)) =
            if idx1 < idx2 then -1 elif idx1 > idx2 then 1 else compare t1.Tag t2.Tag

        let hcaCompare (TaggedIndex((t1:HasCustomAttributeTag), (idx1:int))) (TaggedIndex((t2:HasCustomAttributeTag), idx2)) =
            if idx1 < idx2 then -1 elif idx1 > idx2 then 1 else compare t1.Tag t2.Tag

        let mfCompare (TaggedIndex((t1:MemberForwardedTag), (idx1:int))) (TaggedIndex((t2:MemberForwardedTag), idx2)) =
            if idx1 < idx2 then -1 elif idx1 > idx2 then 1 else compare t1.Tag t2.Tag

        let hdsCompare (TaggedIndex((t1:HasDeclSecurityTag), (idx1:int))) (TaggedIndex((t2:HasDeclSecurityTag), idx2)) =
            if idx1 < idx2 then -1 elif idx1 > idx2 then 1 else compare t1.Tag t2.Tag

        let hfmCompare (TaggedIndex((t1:HasFieldMarshalTag), idx1)) (TaggedIndex((t2:HasFieldMarshalTag), idx2)) =
            if idx1 < idx2 then -1 elif idx1 > idx2 then 1 else compare t1.Tag t2.Tag

        let tomdCompare (TaggedIndex((t1:TypeOrMethodDefTag), idx1)) (TaggedIndex((t2:TypeOrMethodDefTag), idx2)) =
            if idx1 < idx2 then -1 elif idx1 > idx2 then 1 else compare t1.Tag t2.Tag

        let simpleIndexCompare (idx1:int) (idx2:int) =
            compare idx1 idx2

        let mkCacheInt32 lowMem _infile _nm _sz  =
            if lowMem then (fun f x -> f x) else
            let cache = ref null
            fun f (idx:int32) ->
                let cache =
                    match !cache with
                    | null -> cache :=  new Dictionary<int32,_>(11)
                    | _ -> ()
                    !cache
                let mutable res = Unchecked.defaultof<_>
                let ok = cache.TryGetValue(idx, &res)
                if ok then
                    res
                else
                    let res = f idx
                    cache.[idx] <- res;
                    res

        let mkCacheGeneric lowMem _inbase _nm _sz  =
            if lowMem then (fun f x -> f x) else
            let cache = ref null
            fun f (idx :'T) ->
                let cache =
                    match !cache with
                    | null -> cache := new Dictionary<_,_>(11 (* sz:int *) )
                    | _ -> ()
                    !cache
                if cache.ContainsKey idx then cache.[idx]
                else let res = f idx in cache.[idx] <- res; res

        let seekFindRow numRows rowChooser =
            let mutable i = 1
            while (i <= numRows &&  not (rowChooser i)) do
                i <- i + 1;
            i

        // search for rows satisfying predicate
        let seekReadIndexedRows (numRows, rowReader, keyFunc, keyComparer, binaryChop, rowConverter) =
            if binaryChop then
                let mutable low = 0
                let mutable high = numRows + 1
                begin
                  let mutable fin = false
                  while not fin do
                      if high - low <= 1  then
                          fin <- true
                      else
                          let mid = (low + high) / 2
                          let midrow = rowReader mid
                          let c = keyComparer (keyFunc midrow)
                          if c > 0 then
                              low <- mid
                          elif c < 0 then
                              high <- mid
                          else
                              fin <- true
                end;
                let mutable res = []
                if high - low > 1 then
                    // now read off rows, forward and backwards
                    let mid = (low + high) / 2
                    // read forward
                    begin
                        let mutable fin = false
                        let mutable curr = mid
                        while not fin do
                          if curr > numRows then
                              fin <- true;
                          else
                              let currrow = rowReader curr
                              if keyComparer (keyFunc currrow) = 0 then
                                  res <- rowConverter currrow :: res;
                              else
                                  fin <- true;
                              curr <- curr + 1;
                        done;
                    end;
                    res <- List.rev res;
                    // read backwards
                    begin
                        let mutable fin = false
                        let mutable curr = mid - 1
                        while not fin do
                          if curr = 0 then
                            fin <- true
                          else
                            let currrow = rowReader curr
                            if keyComparer (keyFunc currrow) = 0 then
                                res <- rowConverter currrow :: res;
                            else
                                fin <- true;
                            curr <- curr - 1;
                    end;
                res |> List.toArray
            else
                let res = ref []
                for i = 1 to numRows do
                    let rowinfo = rowReader i
                    if keyComparer (keyFunc rowinfo) = 0 then
                      res := rowConverter rowinfo :: !res;
                List.rev !res  |> List.toArray


        let seekReadOptionalIndexedRow (info) =
            match seekReadIndexedRows info with
            | [| |] -> None
            | xs -> Some xs.[0]

        let seekReadIndexedRow (info) =
            match seekReadOptionalIndexedRow info with
            | Some row -> row
            | None -> failwith ("no row found for key when indexing table")

        let getName (ltd: Lazy<ILTypeDef>) =
            let td = ltd.Force()
            (td.Name,ltd)

        let mkILTy boxed tspec =
            match boxed with
            | AsObject -> ILType.Boxed tspec
            | _ -> ILType.Value tspec

        let mkILArr1DTy ty = ILType.Array (ILArrayShape.SingleDimensional, ty)

        let typeNameForGlobalFunctions = "<Module>"

        let mkILNonGenericTySpec tref =  ILTypeSpec (tref,[| |])
        let mkILTypeForGlobalFunctions scoref = ILType.Boxed (mkILNonGenericTySpec (ILTypeRef(ILTypeRefScope.Top scoref, UNone, typeNameForGlobalFunctions)))

        let mkILMethSpecInTyRaw (typ:ILType, cc, nm, args, rty, minst:ILGenericArgs) =
            ILMethodSpec (ILMethodRef (typ.TypeRef,cc,minst.Length,nm,args,rty),typ,minst)

        let mkILFieldSpecInTy (typ:ILType,nm,fty) =
            ILFieldSpec (ILFieldRef (typ.TypeRef,nm,fty), typ)

        let mkILFormalGenericArgsRaw (gparams:ILGenericParameterDefs)  =
            gparams |> Array.mapi (fun n _gf -> ILType.Var n)

        let mkILGlobals systemRuntimeScopeRef =
              let mkILTyspec nsp nm =  mkILNonGenericTySpec(ILTypeRef(ILTypeRefScope.Top(systemRuntimeScopeRef),USome nsp,nm))
              { typ_Object = ILType.Boxed (mkILTyspec "System" "Object")
                typ_String = ILType.Boxed (mkILTyspec "System" "String")
                typ_Type = ILType.Boxed (mkILTyspec "System" "Type")
                typ_Int64 = ILType.Value (mkILTyspec "System" "Int64")
                typ_UInt64 = ILType.Value (mkILTyspec "System" "UInt64")
                typ_Int32 = ILType.Value (mkILTyspec "System" "Int32")
                typ_UInt32 = ILType.Value (mkILTyspec "System" "UInt32")
                typ_Int16 = ILType.Value (mkILTyspec "System" "Int16")
                typ_UInt16 = ILType.Value (mkILTyspec "System" "UInt16")
                typ_SByte = ILType.Value (mkILTyspec "System" "SByte")
                typ_Byte = ILType.Value (mkILTyspec "System" "Byte")
                typ_Single = ILType.Value (mkILTyspec "System" "Single")
                typ_Double = ILType.Value (mkILTyspec "System" "Double")
                typ_Boolean = ILType.Value (mkILTyspec "System" "Boolean")
                typ_Char = ILType.Value (mkILTyspec "System" "Char")
                typ_IntPtr = ILType.Value (mkILTyspec "System" "IntPtr")
                typ_TypedReference = Some (ILType.Value (mkILTyspec "System" "TypedReference"))
                typ_UIntPtr = ILType.Value (mkILTyspec "System" "UIntPtr")
                systemRuntimeScopeRef = systemRuntimeScopeRef }

        type ILModuleReader(infile: string, is: ByteFile, ilg: ILGlobals, lowMem: bool) =

            //-----------------------------------------------------------------------
            // Crack the binary headers, build a reader context and return the lazy
            // read of the AbsIL module.
            // ----------------------------------------------------------------------

            (* MSDOS HEADER *)
            let peSignaturePhysLoc = seekReadInt32 is 0x3c

            (* PE HEADER *)
            let peFileHeaderPhysLoc = peSignaturePhysLoc + 0x04
            let peOptionalHeaderPhysLoc = peFileHeaderPhysLoc + 0x14
            let peSignature = seekReadInt32 is (peSignaturePhysLoc + 0)
            do if peSignature <>  0x4550 then failwithf "not a PE file - bad magic PE number 0x%08x, is = %A" peSignature is;


            (* PE SIGNATURE *)
            //let machine = seekReadUInt16AsInt32 is (peFileHeaderPhysLoc + 0)
            let numSections = seekReadUInt16AsInt32 is (peFileHeaderPhysLoc + 2)
            let optHeaderSize = seekReadUInt16AsInt32 is (peFileHeaderPhysLoc + 16)
            do if optHeaderSize <>  0xe0 &&
                 optHeaderSize <> 0xf0 then failwith "not a PE file - bad optional header size";
            let x64adjust = optHeaderSize - 0xe0
            //let only64 = (optHeaderSize = 0xf0)    (* May want to read in the optional header Magic number and check that as well... *)
            //let platform = match machine with | 0x8664 -> Some(AMD64) | 0x200 -> Some(IA64) | _ -> Some(X86)
            let sectionHeadersStartPhysLoc = peOptionalHeaderPhysLoc + optHeaderSize

            //let flags = seekReadUInt16AsInt32 is (peFileHeaderPhysLoc + 18)
            //let isDll = (flags &&& 0x2000) <> 0x0

            (* OPTIONAL PE HEADER *)
            (* x86: 000000a0 *)
            (* x86: 000000b0 *)
            //let dataSegmentAddr = seekReadInt32 is (peOptionalHeaderPhysLoc + 24) (* e.g. 0x0000c000 *)
            //let imageBaseReal = if only64 then dataSegmentAddr else seekReadInt32 is (peOptionalHeaderPhysLoc + 28)  (* Image Base Always 0x400000 (see Section 23.1). - QUERY : no it's not always 0x400000, e.g. 0x034f0000 *)
            //let alignVirt = seekReadInt32 is (peOptionalHeaderPhysLoc + 32)   (*  Section Alignment Always 0x2000 (see Section 23.1). *)
            //let alignPhys = seekReadInt32 is (peOptionalHeaderPhysLoc + 36)  (* File Alignment Either 0x200 or 0x1000. *)
            (* x86: 000000c0 *)
            //let subsysMajor = seekReadUInt16AsInt32 is (peOptionalHeaderPhysLoc + 48)   (* SubSys Major Always 4 (see Section 23.1). *)
            //let subsysMinor = seekReadUInt16AsInt32 is (peOptionalHeaderPhysLoc + 50)   (* SubSys Minor Always 0 (see Section 23.1). *)
            (* x86: 000000d0 *)
            //let subsys = seekReadUInt16 is (peOptionalHeaderPhysLoc + 68)   (* SubSystem Subsystem required to run this image. Shall be either IMAGE_SUBSYSTEM_WINDOWS_CE_GUI (!0x3) or IMAGE_SUBSYSTEM_WINDOWS_GUI (!0x2). QUERY: Why is this 3 on the images ILASM produces??? *)
            //let useHighEntropyVA =
            //    let n = seekReadUInt16 is (peOptionalHeaderPhysLoc + 70)
            //    let highEnthropyVA = 0x20us
            //    (n &&& highEnthropyVA) = highEnthropyVA

             (* x86: 000000e0 *)
             (* x86: 000000f0, x64: 00000100 *)
             (* x86: 00000100 - these addresses are for x86 - for the x64 location, add x64adjust (0x10) *)
             (* x86: 00000110 *)
             (* x86: 00000120 *)
             (* x86: 00000130 *)
             (* x86: 00000140 *)
             (* x86: 00000150 *)
             (* x86: 00000160 *)
            let cliHeaderAddr = seekReadInt32 is (peOptionalHeaderPhysLoc + 208 + x64adjust)

            let anyV2P (n,v) =
              let rec look i pos =
                if i >= numSections then (failwith (infile + ": bad "+n+", rva "+string v); 0x0)
                else
                  let virtSize = seekReadInt32 is (pos + 8)
                  let virtAddr = seekReadInt32 is (pos + 12)
                  let physLoc = seekReadInt32 is (pos + 20)
                  if (v >= virtAddr && (v < virtAddr + virtSize)) then (v - virtAddr) + physLoc
                  else look (i+1) (pos + 0x28)
              look 0 sectionHeadersStartPhysLoc

            let cliHeaderPhysLoc = anyV2P ("cli header",cliHeaderAddr)

            let metadataAddr = seekReadInt32 is (cliHeaderPhysLoc + 8)
            //let cliFlags = seekReadInt32 is (cliHeaderPhysLoc + 16)
            //let ilOnly = (cliFlags &&& 0x01) <> 0x00
            //let only32 = (cliFlags &&& 0x02) <> 0x00
            //let is32bitpreferred = (cliFlags &&& 0x00020003) <> 0x00

            let entryPointToken = seekReadUncodedToken is (cliHeaderPhysLoc + 20)
            let resourcesAddr = seekReadInt32 is (cliHeaderPhysLoc + 24)

            let metadataPhysLoc = anyV2P ("metadata",metadataAddr)
            let magic = seekReadUInt16AsInt32 is metadataPhysLoc
            do if magic <> 0x5342 then failwith (infile + ": bad metadata magic number: " + string magic);
            let magic2 = seekReadUInt16AsInt32 is (metadataPhysLoc + 2)
            do if magic2 <> 0x424a then failwith "bad metadata magic number";

            let versionLength = seekReadInt32 is (metadataPhysLoc + 12)
            //let ilMetadataVersion = seekReadBytes is (metadataPhysLoc + 16) versionLength |> Array.filter (fun b -> b <> 0uy)
            let x = align 0x04 (16 + versionLength)
            let numStreams = seekReadUInt16AsInt32 is (metadataPhysLoc + x + 2)
            let streamHeadersStart = (metadataPhysLoc + x + 4)

            (* Crack stream headers *)

            let tryFindStream name =
              let rec look i pos =
                if i >= numStreams then None
                else
                  let offset = seekReadInt32 is (pos + 0)
                  let length = seekReadInt32 is (pos + 4)
                  let res = ref true
                  let fin = ref false
                  let n = ref 0
                  // read and compare the stream name byte by byte
                  while (not !fin) do
                      let c= seekReadByteAsInt32 is (pos + 8 + (!n))
                      if c = 0 then
                          fin := true
                      elif !n >= Array.length name || c <> name.[!n] then
                          res := false;
                      incr n
                  if !res then Some(offset + metadataPhysLoc,length)
                  else look (i+1) (align 0x04 (pos + 8 + (!n)))
              look 0 streamHeadersStart

            let findStream name =
                match tryFindStream name with
                | None -> (0x0, 0x0)
                | Some positions ->  positions

            let (tablesStreamPhysLoc, _tablesStreamSize) =
              match tryFindStream [| 0x23; 0x7e |] (* #~ *) with
              | Some res -> res
              | None ->
                match tryFindStream [| 0x23; 0x2d |] (* #-: at least one DLL I've seen uses this! *)   with
                | Some res -> res
                | None ->
                 let firstStreamOffset = seekReadInt32 is (streamHeadersStart + 0)
                 let firstStreamLength = seekReadInt32 is (streamHeadersStart + 4)
                 firstStreamOffset,firstStreamLength

            let (stringsStreamPhysicalLoc, stringsStreamSize) = findStream [| 0x23; 0x53; 0x74; 0x72; 0x69; 0x6e; 0x67; 0x73; |] (* #Strings *)
            let (blobsStreamPhysicalLoc, blobsStreamSize) = findStream [| 0x23; 0x42; 0x6c; 0x6f; 0x62; |] (* #Blob *)

            let tablesStreamMajorVersion = seekReadByteAsInt32 is (tablesStreamPhysLoc + 4)
            let tablesStreamMinorVersion = seekReadByteAsInt32 is (tablesStreamPhysLoc + 5)

            let usingWhidbeyBeta1TableSchemeForGenericParam = (tablesStreamMajorVersion = 1) && (tablesStreamMinorVersion = 1)

            let tableKinds =
                [|kindModule               (* Table 0  *);
                  kindTypeRef              (* Table 1  *);
                  kindTypeDef              (* Table 2  *);
                  kindIllegal (* kindFieldPtr *)             (* Table 3  *);
                  kindFieldDef                (* Table 4  *);
                  kindIllegal (* kindMethodPtr *)            (* Table 5  *);
                  kindMethodDef               (* Table 6  *);
                  kindIllegal (* kindParamPtr *)             (* Table 7  *);
                  kindParam                (* Table 8  *);
                  kindInterfaceImpl        (* Table 9  *);
                  kindMemberRef            (* Table 10 *);
                  kindConstant             (* Table 11 *);
                  kindCustomAttribute      (* Table 12 *);
                  kindFieldMarshal         (* Table 13 *);
                  kindDeclSecurity         (* Table 14 *);
                  kindClassLayout          (* Table 15 *);
                  kindFieldLayout          (* Table 16 *);
                  kindStandAloneSig        (* Table 17 *);
                  kindEventMap             (* Table 18 *);
                  kindIllegal (* kindEventPtr *)             (* Table 19 *);
                  kindEvent                (* Table 20 *);
                  kindPropertyMap          (* Table 21 *);
                  kindIllegal (* kindPropertyPtr *)          (* Table 22 *);
                  kindProperty             (* Table 23 *);
                  kindMethodSemantics      (* Table 24 *);
                  kindMethodImpl           (* Table 25 *);
                  kindModuleRef            (* Table 26 *);
                  kindTypeSpec             (* Table 27 *);
                  kindImplMap              (* Table 28 *);
                  kindFieldRVA             (* Table 29 *);
                  kindIllegal (* kindENCLog *)               (* Table 30 *);
                  kindIllegal (* kindENCMap *)               (* Table 31 *);
                  kindAssembly             (* Table 32 *);
                  kindIllegal (* kindAssemblyProcessor *)    (* Table 33 *);
                  kindIllegal (* kindAssemblyOS *)           (* Table 34 *);
                  kindAssemblyRef          (* Table 35 *);
                  kindIllegal (* kindAssemblyRefProcessor *) (* Table 36 *);
                  kindIllegal (* kindAssemblyRefOS *)        (* Table 37 *);
                  kindFileRef                 (* Table 38 *);
                  kindExportedType         (* Table 39 *);
                  kindManifestResource     (* Table 40 *);
                  kindNested               (* Table 41 *);
                 (if usingWhidbeyBeta1TableSchemeForGenericParam then kindGenericParam_v1_1 else  kindGenericParam_v2_0);        (* Table 42 *)
                  kindMethodSpec         (* Table 43 *);
                  kindGenericParamConstraint         (* Table 44 *);
                  kindIllegal         (* Table 45 *);
                  kindIllegal         (* Table 46 *);
                  kindIllegal         (* Table 47 *);
                  kindIllegal         (* Table 48 *);
                  kindIllegal         (* Table 49 *);
                  kindIllegal         (* Table 50 *);
                  kindIllegal         (* Table 51 *);
                  kindIllegal         (* Table 52 *);
                  kindIllegal         (* Table 53 *);
                  kindIllegal         (* Table 54 *);
                  kindIllegal         (* Table 55 *);
                  kindIllegal         (* Table 56 *);
                  kindIllegal         (* Table 57 *);
                  kindIllegal         (* Table 58 *);
                  kindIllegal         (* Table 59 *);
                  kindIllegal         (* Table 60 *);
                  kindIllegal         (* Table 61 *);
                  kindIllegal         (* Table 62 *);
                  kindIllegal         (* Table 63 *);
                |]

            let heapSizes = seekReadByteAsInt32 is (tablesStreamPhysLoc + 6)
            let valid = seekReadInt64 is (tablesStreamPhysLoc + 8)
            let sorted = seekReadInt64 is (tablesStreamPhysLoc + 16)
            let tableRowCount, startOfTables =
                let numRows = Array.create 64 0
                let prevNumRowIdx = ref (tablesStreamPhysLoc + 24)
                for i = 0 to 63 do
                    if (valid &&& (int64 1 <<< i)) <> int64  0 then
                        numRows.[i] <-  (seekReadInt32 is !prevNumRowIdx);
                        prevNumRowIdx := !prevNumRowIdx + 4
                numRows, !prevNumRowIdx

            let getNumRows (tab:ILTableName) = tableRowCount.[tab.Index]
            let stringsBigness = (heapSizes &&& 1) <> 0
            let guidsBigness = (heapSizes &&& 2) <> 0
            let blobsBigness = (heapSizes &&& 4) <> 0

            let tableBigness = Array.map (fun n -> n >= 0x10000) tableRowCount

            let codedBigness nbits tab =
              let rows = getNumRows tab
              rows >= (0x10000 >>>& nbits)

            let tdorBigness =
              codedBigness 2 ILTableNames.TypeDef ||
              codedBigness 2 ILTableNames.TypeRef ||
              codedBigness 2 ILTableNames.TypeSpec

            let tomdBigness =
              codedBigness 1 ILTableNames.TypeDef ||
              codedBigness 1 ILTableNames.Method

            let hcBigness =
              codedBigness 2 ILTableNames.Field ||
              codedBigness 2 ILTableNames.Param ||
              codedBigness 2 ILTableNames.Property

            let hcaBigness =
              codedBigness 5 ILTableNames.Method ||
              codedBigness 5 ILTableNames.Field ||
              codedBigness 5 ILTableNames.TypeRef  ||
              codedBigness 5 ILTableNames.TypeDef ||
              codedBigness 5 ILTableNames.Param ||
              codedBigness 5 ILTableNames.InterfaceImpl ||
              codedBigness 5 ILTableNames.MemberRef ||
              codedBigness 5 ILTableNames.Module ||
              codedBigness 5 ILTableNames.Permission ||
              codedBigness 5 ILTableNames.Property ||
              codedBigness 5 ILTableNames.Event ||
              codedBigness 5 ILTableNames.StandAloneSig ||
              codedBigness 5 ILTableNames.ModuleRef ||
              codedBigness 5 ILTableNames.TypeSpec ||
              codedBigness 5 ILTableNames.Assembly ||
              codedBigness 5 ILTableNames.AssemblyRef ||
              codedBigness 5 ILTableNames.File ||
              codedBigness 5 ILTableNames.ExportedType ||
              codedBigness 5 ILTableNames.ManifestResource ||
              codedBigness 5 ILTableNames.GenericParam ||
              codedBigness 5 ILTableNames.GenericParamConstraint ||
              codedBigness 5 ILTableNames.MethodSpec


            let hfmBigness =
              codedBigness 1 ILTableNames.Field ||
              codedBigness 1 ILTableNames.Param

            let hdsBigness =
              codedBigness 2 ILTableNames.TypeDef ||
              codedBigness 2 ILTableNames.Method ||
              codedBigness 2 ILTableNames.Assembly

            let mrpBigness =
              codedBigness 3 ILTableNames.TypeRef ||
              codedBigness 3 ILTableNames.ModuleRef ||
              codedBigness 3 ILTableNames.Method ||
              codedBigness 3 ILTableNames.TypeSpec

            let hsBigness =
              codedBigness 1 ILTableNames.Event ||
              codedBigness 1 ILTableNames.Property

            let mdorBigness =
              codedBigness 1 ILTableNames.Method ||
              codedBigness 1 ILTableNames.MemberRef

            let mfBigness =
              codedBigness 1 ILTableNames.Field ||
              codedBigness 1 ILTableNames.Method

            let iBigness =
              codedBigness 2 ILTableNames.File ||
              codedBigness 2 ILTableNames.AssemblyRef ||
              codedBigness 2 ILTableNames.ExportedType

            let catBigness =
              codedBigness 3 ILTableNames.Method ||
              codedBigness 3 ILTableNames.MemberRef

            let rsBigness =
              codedBigness 2 ILTableNames.Module ||
              codedBigness 2 ILTableNames.ModuleRef ||
              codedBigness 2 ILTableNames.AssemblyRef  ||
              codedBigness 2 ILTableNames.TypeRef

            let rowKindSize (ILRowKind kinds) =
              kinds |> List.sumBy (fun x ->
                    match x with
                    | UShort -> 2
                    | ULong -> 4
                    | Byte -> 1
                    | Data -> 4
                    | GGuid -> (if guidsBigness then 4 else 2)
                    | Blob  -> (if blobsBigness then 4 else 2)
                    | SString  -> (if stringsBigness then 4 else 2)
                    | SimpleIndex tab -> (if tableBigness.[tab.Index] then 4 else 2)
                    | TypeDefOrRefOrSpec -> (if tdorBigness then 4 else 2)
                    | TypeOrMethodDef -> (if tomdBigness then 4 else 2)
                    | HasConstant  -> (if hcBigness then 4 else 2)
                    | HasCustomAttribute -> (if hcaBigness then 4 else 2)
                    | HasFieldMarshal  -> (if hfmBigness then 4 else 2)
                    | HasDeclSecurity  -> (if hdsBigness then 4 else 2)
                    | MemberRefParent  -> (if mrpBigness then 4 else 2)
                    | HasSemantics  -> (if hsBigness then 4 else 2)
                    | MethodDefOrRef -> (if mdorBigness then 4 else 2)
                    | MemberForwarded -> (if mfBigness then 4 else 2)
                    | Implementation  -> (if iBigness then 4 else 2)
                    | CustomAttributeType -> (if catBigness then 4 else 2)
                    | ResolutionScope -> (if rsBigness then 4 else 2))

            let tableRowSizes = tableKinds |> Array.map rowKindSize

            let tablePhysLocations =
                 let res = Array.create 64 0x0
                 let prevTablePhysLoc = ref startOfTables
                 for i = 0 to 63 do
                     res.[i] <- !prevTablePhysLoc;
                     prevTablePhysLoc := !prevTablePhysLoc + (tableRowCount.[i] * tableRowSizes.[i]);
                 res

            // All the caches.  The sizes are guesstimates for the rough sharing-density of the assembly
            let cacheAssemblyRef = mkCacheInt32 lowMem infile "ILAssemblyRef"  (getNumRows ILTableNames.AssemblyRef)
            let cacheMemberRefAsMemberData = mkCacheGeneric lowMem infile "MemberRefAsMemberData" (getNumRows ILTableNames.MemberRef / 20 + 1)
            let cacheTypeRef = mkCacheInt32 lowMem infile "ILTypeRef" (getNumRows ILTableNames.TypeRef / 20 + 1)
            let cacheTypeRefAsType = mkCacheGeneric lowMem infile "TypeRefAsType" (getNumRows ILTableNames.TypeRef / 20 + 1)
            let cacheBlobHeapAsPropertySig = mkCacheGeneric lowMem infile "BlobHeapAsPropertySig" (getNumRows ILTableNames.Property / 20 + 1)
            let cacheBlobHeapAsFieldSig = mkCacheGeneric lowMem infile "BlobHeapAsFieldSig" (getNumRows ILTableNames.Field / 20 + 1)
            let cacheBlobHeapAsMethodSig = mkCacheGeneric lowMem infile "BlobHeapAsMethodSig" (getNumRows ILTableNames.Method / 20 + 1)
            let cacheTypeDefAsType = mkCacheGeneric lowMem infile "TypeDefAsType" (getNumRows ILTableNames.TypeDef / 20 + 1)
            let cacheMethodDefAsMethodData = mkCacheInt32 lowMem infile "MethodDefAsMethodData" (getNumRows ILTableNames.Method / 20 + 1)
            // nb. Lots and lots of cache hits on this cache, hence never optimize cache away
            let cacheStringHeap = mkCacheInt32 false infile "string heap" ( stringsStreamSize / 50 + 1)
            let cacheBlobHeap = mkCacheInt32 lowMem infile "blob heap" ( blobsStreamSize / 50 + 1)

           //-----------------------------------------------------------------------

            let rowAddr (tab:ILTableName) idx = tablePhysLocations.[tab.Index] + (idx - 1) * tableRowSizes.[tab.Index]

            let seekReadUInt16Adv (addr: byref<int>) =
                let res = seekReadUInt16 is addr
                addr <- addr + 2
                res

            let seekReadInt32Adv (addr: byref<int>) =
                let res = seekReadInt32 is addr
                addr <- addr+4
                res

            let seekReadUInt16AsInt32Adv (addr: byref<int>) =
                let res = seekReadUInt16AsInt32 is addr
                addr <- addr+2
                res

            let seekReadTaggedIdx f nbits big (addr: byref<int>) =
                let tok = if big then seekReadInt32Adv &addr else seekReadUInt16AsInt32Adv &addr
                tokToTaggedIdx f nbits tok


            let seekReadIdx big (addr: byref<int>) =
                if big then seekReadInt32Adv &addr else seekReadUInt16AsInt32Adv &addr

            let seekReadUntaggedIdx (tab:ILTableName) (addr: byref<int>) =
                seekReadIdx tableBigness.[tab.Index] &addr


            let seekReadResolutionScopeIdx     (addr: byref<int>) = seekReadTaggedIdx (fun idx -> ResolutionScopeTag idx)    2 rsBigness   &addr
            let seekReadTypeDefOrRefOrSpecIdx  (addr: byref<int>) = seekReadTaggedIdx (fun idx -> TypeDefOrRefOrSpecTag idx)  2 tdorBigness &addr
            let seekReadTypeOrMethodDefIdx     (addr: byref<int>) = seekReadTaggedIdx (fun idx -> TypeOrMethodDefTag idx)    1 tomdBigness &addr
            let seekReadHasConstantIdx         (addr: byref<int>) = seekReadTaggedIdx (fun idx -> HasConstantTag idx)        2 hcBigness   &addr
            let seekReadHasCustomAttributeIdx  (addr: byref<int>) = seekReadTaggedIdx (fun idx -> HasCustomAttributeTag idx)  5 hcaBigness  &addr
            //let seekReadHasFieldMarshalIdx     (addr: byref<int>) = seekReadTaggedIdx (fun idx -> HasFieldMarshalTag idx)    1 hfmBigness &addr
            //let seekReadHasDeclSecurityIdx     (addr: byref<int>) = seekReadTaggedIdx (fun idx -> HasDeclSecurityTag idx)    2 hdsBigness &addr
            let seekReadMemberRefParentIdx     (addr: byref<int>) = seekReadTaggedIdx (fun idx -> MemberRefParentTag idx)    3 mrpBigness &addr
            let seekReadHasSemanticsIdx        (addr: byref<int>) = seekReadTaggedIdx (fun idx -> HasSemanticsTag idx)       1 hsBigness &addr
            let seekReadImplementationIdx      (addr: byref<int>) = seekReadTaggedIdx (fun idx -> ImplementationTag idx)     2 iBigness &addr
            let seekReadCustomAttributeTypeIdx (addr: byref<int>) = seekReadTaggedIdx (fun idx -> CustomAttributeTypeTag idx) 3 catBigness &addr
            let seekReadStringIdx (addr: byref<int>) = seekReadIdx stringsBigness &addr
            let seekReadGuidIdx (addr: byref<int>) = seekReadIdx guidsBigness &addr
            let seekReadBlobIdx (addr: byref<int>) = seekReadIdx blobsBigness &addr

            let seekReadModuleRow idx =
                if idx = 0 then failwith "cannot read Module table row 0";
                let mutable addr = rowAddr ILTableNames.Module idx
                let generation = seekReadUInt16Adv &addr
                let nameIdx = seekReadStringIdx &addr
                let mvidIdx = seekReadGuidIdx &addr
                let encidIdx = seekReadGuidIdx &addr
                let encbaseidIdx = seekReadGuidIdx &addr
                (generation, nameIdx, mvidIdx, encidIdx, encbaseidIdx)

            /// Read Table ILTypeRef
            let seekReadTypeRefRow idx =
                let mutable addr = rowAddr ILTableNames.TypeRef idx
                let scopeIdx = seekReadResolutionScopeIdx &addr
                let nameIdx = seekReadStringIdx &addr
                let namespaceIdx = seekReadStringIdx &addr
                (scopeIdx,nameIdx,namespaceIdx)

            /// Read Table ILTypeDef
            let seekReadTypeDefRow idx =
                let mutable addr = rowAddr ILTableNames.TypeDef idx
                let flags = seekReadInt32Adv &addr
                let nameIdx = seekReadStringIdx &addr
                let namespaceIdx = seekReadStringIdx &addr
                let extendsIdx = seekReadTypeDefOrRefOrSpecIdx &addr
                let fieldsIdx = seekReadUntaggedIdx ILTableNames.Field &addr
                let methodsIdx = seekReadUntaggedIdx ILTableNames.Method &addr
                (flags, nameIdx, namespaceIdx, extendsIdx, fieldsIdx, methodsIdx)

            /// Read Table Field
            let seekReadFieldRow idx =
                let mutable addr = rowAddr ILTableNames.Field idx
                let flags = seekReadUInt16AsInt32Adv &addr
                let nameIdx = seekReadStringIdx &addr
                let typeIdx = seekReadBlobIdx &addr
                (flags,nameIdx,typeIdx)

            /// Read Table Method
            let seekReadMethodRow idx =
                let mutable addr = rowAddr ILTableNames.Method idx
                let codeRVA = seekReadInt32Adv &addr
                let implflags = seekReadUInt16AsInt32Adv &addr
                let flags = seekReadUInt16AsInt32Adv &addr
                let nameIdx = seekReadStringIdx &addr
                let typeIdx = seekReadBlobIdx &addr
                let paramIdx = seekReadUntaggedIdx ILTableNames.Param &addr
                (codeRVA, implflags, flags, nameIdx, typeIdx, paramIdx)

            /// Read Table Param
            let seekReadParamRow idx =
                let mutable addr = rowAddr ILTableNames.Param idx
                let flags = seekReadUInt16AsInt32Adv &addr
                let seq =  seekReadUInt16AsInt32Adv &addr
                let nameIdx = seekReadStringIdx &addr
                (flags,seq,nameIdx)

            let seekReadInterfaceImplRow idx =
                let mutable addr = rowAddr ILTableNames.InterfaceImpl idx
                let tidx = seekReadUntaggedIdx ILTableNames.TypeDef &addr
                let intfIdx = seekReadTypeDefOrRefOrSpecIdx &addr
                (tidx,intfIdx)

            /// Read Table MemberRef
            let seekReadMemberRefRow idx =
                let mutable addr = rowAddr ILTableNames.MemberRef idx
                let mrpIdx = seekReadMemberRefParentIdx &addr
                let nameIdx = seekReadStringIdx &addr
                let typeIdx = seekReadBlobIdx &addr
                (mrpIdx,nameIdx,typeIdx)

            /// Read Table Constant
            let seekReadConstantRow idx =
                let mutable addr = rowAddr ILTableNames.Constant idx
                let kind = seekReadUInt16Adv &addr
                let parentIdx = seekReadHasConstantIdx &addr
                let valIdx = seekReadBlobIdx &addr
                (kind, parentIdx, valIdx)

            /// Read Table CustomAttribute
            let seekReadCustomAttributeRow idx =
                let mutable addr = rowAddr ILTableNames.CustomAttribute idx
                let parentIdx = seekReadHasCustomAttributeIdx &addr
                let typeIdx = seekReadCustomAttributeTypeIdx &addr
                let valIdx = seekReadBlobIdx &addr
                (parentIdx, typeIdx, valIdx)

            /// Read Table EventMap
            let seekReadEventMapRow idx =
                let mutable addr = rowAddr ILTableNames.EventMap idx
                let tidx = seekReadUntaggedIdx ILTableNames.TypeDef &addr
                let eventsIdx = seekReadUntaggedIdx ILTableNames.Event &addr
                (tidx,eventsIdx)

            /// Read Table Event
            let seekReadEventRow idx =
                let mutable addr = rowAddr ILTableNames.Event idx
                let flags = seekReadUInt16AsInt32Adv &addr
                let nameIdx = seekReadStringIdx &addr
                let typIdx = seekReadTypeDefOrRefOrSpecIdx &addr
                (flags,nameIdx,typIdx)

            /// Read Table PropertyMap
            let seekReadPropertyMapRow idx =
                let mutable addr = rowAddr ILTableNames.PropertyMap idx
                let tidx = seekReadUntaggedIdx ILTableNames.TypeDef &addr
                let propsIdx = seekReadUntaggedIdx ILTableNames.Property &addr
                (tidx,propsIdx)

            /// Read Table Property
            let seekReadPropertyRow idx =
                let mutable addr = rowAddr ILTableNames.Property idx
                let flags = seekReadUInt16AsInt32Adv &addr
                let nameIdx = seekReadStringIdx &addr
                let typIdx = seekReadBlobIdx &addr
                (flags,nameIdx,typIdx)

            /// Read Table MethodSemantics
            let seekReadMethodSemanticsRow idx =
                let mutable addr = rowAddr ILTableNames.MethodSemantics idx
                let flags = seekReadUInt16AsInt32Adv &addr
                let midx = seekReadUntaggedIdx ILTableNames.Method &addr
                let assocIdx = seekReadHasSemanticsIdx &addr
                (flags,midx,assocIdx)

            /// Read Table ILModuleRef
            let seekReadModuleRefRow idx =
                let mutable addr = rowAddr ILTableNames.ModuleRef idx
                let nameIdx = seekReadStringIdx &addr
                nameIdx

            /// Read Table ILTypeSpec
            let seekReadTypeSpecRow idx =
                let mutable addr = rowAddr ILTableNames.TypeSpec idx
                let blobIdx = seekReadBlobIdx &addr
                blobIdx

            /// Read Table Assembly
            let seekReadAssemblyRow idx =
                let mutable addr = rowAddr ILTableNames.Assembly idx
                let hash = seekReadInt32Adv &addr
                let v1 = seekReadUInt16Adv &addr
                let v2 = seekReadUInt16Adv &addr
                let v3 = seekReadUInt16Adv &addr
                let v4 = seekReadUInt16Adv &addr
                let flags = seekReadInt32Adv &addr
                let publicKeyIdx = seekReadBlobIdx &addr
                let nameIdx = seekReadStringIdx &addr
                let localeIdx = seekReadStringIdx &addr
                (hash,v1,v2,v3,v4,flags,publicKeyIdx, nameIdx, localeIdx)

            /// Read Table ILAssemblyRef
            let seekReadAssemblyRefRow idx =
                let mutable addr = rowAddr ILTableNames.AssemblyRef idx
                let v1 = seekReadUInt16Adv &addr
                let v2 = seekReadUInt16Adv &addr
                let v3 = seekReadUInt16Adv &addr
                let v4 = seekReadUInt16Adv &addr
                let flags = seekReadInt32Adv &addr
                let publicKeyOrTokenIdx = seekReadBlobIdx &addr
                let nameIdx = seekReadStringIdx &addr
                let localeIdx = seekReadStringIdx &addr
                let hashValueIdx = seekReadBlobIdx &addr
                (v1,v2,v3,v4,flags,publicKeyOrTokenIdx, nameIdx, localeIdx,hashValueIdx)

            /// Read Table File
            let seekReadFileRow idx =
                let mutable addr = rowAddr ILTableNames.File idx
                let flags = seekReadInt32Adv &addr
                let nameIdx = seekReadStringIdx &addr
                let hashValueIdx = seekReadBlobIdx &addr
                (flags, nameIdx, hashValueIdx)

            /// Read Table ILExportedTypeOrForwarder
            let seekReadExportedTypeRow idx =
                let mutable addr = rowAddr ILTableNames.ExportedType idx
                let flags = seekReadInt32Adv &addr
                let tok = seekReadInt32Adv &addr
                let nameIdx = seekReadStringIdx &addr
                let namespaceIdx = seekReadStringIdx &addr
                let implIdx = seekReadImplementationIdx &addr
                (flags,tok,nameIdx,namespaceIdx,implIdx)

            /// Read Table ManifestResource
            let seekReadManifestResourceRow idx =
                let mutable addr = rowAddr ILTableNames.ManifestResource idx
                let offset = seekReadInt32Adv &addr
                let flags = seekReadInt32Adv &addr
                let nameIdx = seekReadStringIdx &addr
                let implIdx = seekReadImplementationIdx &addr
                (offset,flags,nameIdx,implIdx)

            /// Read Table Nested
            let seekReadNestedRow idx =
                let mutable addr = rowAddr ILTableNames.Nested idx
                let nestedIdx = seekReadUntaggedIdx ILTableNames.TypeDef &addr
                let enclIdx = seekReadUntaggedIdx ILTableNames.TypeDef &addr
                (nestedIdx,enclIdx)

            /// Read Table GenericParam
            let seekReadGenericParamRow idx =
                let mutable addr = rowAddr ILTableNames.GenericParam idx
                let seq = seekReadUInt16Adv &addr
                let flags = seekReadUInt16Adv &addr
                let ownerIdx = seekReadTypeOrMethodDefIdx &addr
                let nameIdx = seekReadStringIdx &addr
                (idx,seq,flags,ownerIdx,nameIdx)

            // Read Table GenericParamConstraint
            let seekReadGenericParamConstraintRow idx =
                let mutable addr = rowAddr ILTableNames.GenericParamConstraint idx
                let pidx = seekReadUntaggedIdx ILTableNames.GenericParam &addr
                let constraintIdx = seekReadTypeDefOrRefOrSpecIdx &addr
                (pidx,constraintIdx)

            //let readUserStringHeapUncached idx = seekReadUserString is (userStringsStreamPhysicalLoc + idx)
            //let readUserStringHeap = cacheUserStringHeap readUserStringHeapUncached

            let readStringHeapUncached idx =  seekReadUTF8String is (stringsStreamPhysicalLoc + idx)
            let readStringHeap = cacheStringHeap readStringHeapUncached
            let readStringHeapOption idx = if idx = 0 then UNone else USome (readStringHeap idx)

            let emptyByteArray: byte[] = [||]
            let readBlobHeapUncached idx =
                // valid index lies in range [1..streamSize)
                // NOTE: idx cannot be 0 - Blob\String heap has first empty element that is one byte 0
                if idx <= 0 || idx >= blobsStreamSize then emptyByteArray
                else seekReadBlob is (blobsStreamPhysicalLoc + idx)
            let readBlobHeap = cacheBlobHeap readBlobHeapUncached
            let readBlobHeapOption idx = if idx = 0 then None else Some (readBlobHeap idx)

            //let readGuidHeap idx = seekReadGuid is (guidsStreamPhysicalLoc + idx)

            // read a single value out of a blob heap using the given function
            let readBlobHeapAsBool   vidx = fst (sigptrGetBool   (readBlobHeap vidx) 0)
            let readBlobHeapAsSByte  vidx = fst (sigptrGetSByte  (readBlobHeap vidx) 0)
            let readBlobHeapAsInt16  vidx = fst (sigptrGetInt16  (readBlobHeap vidx) 0)
            let readBlobHeapAsInt32  vidx = fst (sigptrGetInt32  (readBlobHeap vidx) 0)
            let readBlobHeapAsInt64  vidx = fst (sigptrGetInt64  (readBlobHeap vidx) 0)
            let readBlobHeapAsByte   vidx = fst (sigptrGetByte   (readBlobHeap vidx) 0)
            let readBlobHeapAsUInt16 vidx = fst (sigptrGetUInt16 (readBlobHeap vidx) 0)
            let readBlobHeapAsUInt32 vidx = fst (sigptrGetUInt32 (readBlobHeap vidx) 0)
            let readBlobHeapAsUInt64 vidx = fst (sigptrGetUInt64 (readBlobHeap vidx) 0)
            let readBlobHeapAsSingle vidx = fst (sigptrGetSingle (readBlobHeap vidx) 0)
            let readBlobHeapAsDouble vidx = fst (sigptrGetDouble (readBlobHeap vidx) 0)

            //-----------------------------------------------------------------------
            // Read the AbsIL structure (lazily) by reading off the relevant rows.
            // ----------------------------------------------------------------------

            let isSorted (tab:ILTableName) = ((sorted &&& (int64 1 <<< tab.Index)) <> int64 0x0)

            //let subsysversion = (subsysMajor, subsysMinor)
            //let ilMetadataVersion = System.Text.Encoding.UTF8.GetString (ilMetadataVersion, 0, ilMetadataVersion.Length)

            let rec seekReadModule idx =
                let (_generation, nameIdx, _mvidIdx, _encidIdx, _encbaseidIdx) = seekReadModuleRow idx
                let ilModuleName = readStringHeap nameIdx
                //let nativeResources = readNativeResources ctxt

                { Manifest =
                     if getNumRows (ILTableNames.Assembly) > 0 then Some (seekReadAssemblyManifest 1)
                     else None;
                  CustomAttrs = seekReadCustomAttrs (TaggedIndex(HasCustomAttributeTag.Module,idx));
                  Name = ilModuleName;
                  //NativeResources=nativeResources;
                  TypeDefs = ILTypeDefs (lazy (seekReadTopTypeDefs ()));
                  //SubSystemFlags = int32 subsys;
                  //IsILOnly = ilOnly;
                  //SubsystemVersion = subsysversion
                  //UseHighEntropyVA = useHighEntropyVA
                  //Platform = platform;
                  //StackReserveSize = None;
                  //Is32Bit = only32;
                  //Is32BitPreferred = is32bitpreferred;
                  //Is64Bit = only64;
                  //IsDLL=isDll;
                  //VirtualAlignment = alignVirt;
                  //PhysicalAlignment = alignPhys;
                  //ImageBase = imageBaseReal;
                  //MetadataVersion = ilMetadataVersion;
                  Resources = seekReadManifestResources ();
                  }

            and seekReadAssemblyManifest idx =
                let (_hash,v1,v2,v3,v4,flags,publicKeyIdx, nameIdx, localeIdx) = seekReadAssemblyRow idx
                let name = readStringHeap nameIdx
                let pubkey = readBlobHeapOption publicKeyIdx
                { Name= name;
                  //SecurityDecls= seekReadSecurityDecls (TaggedIndex(hds_Assembly,idx));
                  PublicKey= pubkey;
                  Version= Some (Version(int v1,int v2,int v3,int v4));
                  Locale= readStringHeapOption localeIdx;
                  CustomAttrs = seekReadCustomAttrs (TaggedIndex(HasCustomAttributeTag.Assembly,idx));
                  ExportedTypes= seekReadTopExportedTypes ();
                  EntrypointElsewhere=(if fst entryPointToken = ILTableNames.File then Some (seekReadFile (snd entryPointToken)) else None);
                  Retargetable = 0 <> (flags &&& 0x100);
                  //DisableJitOptimizations = 0 <> (flags &&& 0x4000);
                  //JitTracking = 0 <> (flags &&& 0x8000)
                  }

            and seekReadAssemblyRef idx = cacheAssemblyRef  seekReadAssemblyRefUncached idx
            and seekReadAssemblyRefUncached idx =
                let (v1,v2,v3,v4,flags,publicKeyOrTokenIdx, nameIdx, localeIdx,hashValueIdx) = seekReadAssemblyRefRow idx
                let nm = readStringHeap nameIdx
                let publicKey =
                    match readBlobHeapOption publicKeyOrTokenIdx with
                      | None -> None
                      | Some blob -> Some (if (flags &&& 0x0001) <> 0x0 then PublicKey blob else PublicKeyToken blob)

                ILAssemblyRef
                    (name=nm,
                     hash=readBlobHeapOption hashValueIdx,
                     publicKey=publicKey,
                     retargetable=((flags &&& 0x0100) <> 0x0),
                     version=Some(Version(int v1,int v2,int v3,int v4)),
                     locale=readStringHeapOption localeIdx;)

            and seekReadModuleRef idx =
                let nameIdx = seekReadModuleRefRow idx
                ILModuleRef(name=readStringHeap nameIdx, hasMetadata=true, hash=None)

            and seekReadFile idx =
                let (flags, nameIdx, hashValueIdx) = seekReadFileRow idx
                ILModuleRef(name =  readStringHeap nameIdx,
                            hasMetadata= ((flags &&& 0x0001) = 0x0),
                            hash= readBlobHeapOption hashValueIdx)

            //and seekReadClassLayout idx =
            //    match seekReadOptionalIndexedRow (getNumRows ILTableNames.ClassLayout,seekReadClassLayoutRow,(fun (_,_,tidx) -> tidx),simpleIndexCompare idx,isSorted ILTableNames.ClassLayout,(fun (pack,size,_) -> pack,size)) with
            //    | None -> { Size = None; Pack = None }
            //    | Some (pack,size) -> { Size = Some size; Pack = Some pack; }

            and memberAccessOfFlags flags =
                let f = (flags &&& 0x00000007)
                if f = 0x00000001 then  ILMemberAccess.Private
                elif f = 0x00000006 then  ILMemberAccess.Public
                elif f = 0x00000004 then  ILMemberAccess.Family
                elif f = 0x00000002 then  ILMemberAccess.FamilyAndAssembly
                elif f = 0x00000005 then  ILMemberAccess.FamilyOrAssembly
                elif f = 0x00000003 then  ILMemberAccess.Assembly
                else ILMemberAccess.CompilerControlled

            and typeAccessOfFlags flags =
                let f = (flags &&& 0x00000007)
                if f = 0x00000001 then ILTypeDefAccess.Public
                elif f = 0x00000002 then ILTypeDefAccess.Nested ILMemberAccess.Public
                elif f = 0x00000003 then ILTypeDefAccess.Nested ILMemberAccess.Private
                elif f = 0x00000004 then ILTypeDefAccess.Nested ILMemberAccess.Family
                elif f = 0x00000006 then ILTypeDefAccess.Nested ILMemberAccess.FamilyAndAssembly
                elif f = 0x00000007 then ILTypeDefAccess.Nested ILMemberAccess.FamilyOrAssembly
                elif f = 0x00000005 then ILTypeDefAccess.Nested ILMemberAccess.Assembly
                else ILTypeDefAccess.Private

            //and typeLayoutOfFlags flags tidx =
            //    let f = (flags &&& 0x00000018)
            //    if f = 0x00000008 then ILTypeDefLayout.Sequential (seekReadClassLayout tidx)
            //    elif f = 0x00000010 then  ILTypeDefLayout.Explicit (seekReadClassLayout tidx)
            //    else ILTypeDefLayout.Auto

            and typeKindOfFlags nspace nm (super:ILType option) flags =
                if (flags &&& 0x00000020) <> 0x0 then ILTypeDefKind.Interface
                else
                     let isEnum = (match super with None -> false | Some ty -> ty.TypeSpec.Namespace = USome "System" && ty.TypeSpec.Name = "Enum")
                     let isDelegate = (match super with None -> false | Some ty -> ty.TypeSpec.Namespace = USome "System" && ty.TypeSpec.Name = "Delegate")
                     let isMulticastDelegate = (match super with None -> false | Some ty -> ty.TypeSpec.Namespace = USome "System" && ty.TypeSpec.Name = "MulticastDelegate")
                     let selfIsMulticastDelegate = (nspace = USome "System" && nm = "MulticastDelegate")
                     let isValueType = (match super with None -> false | Some ty -> ty.TypeSpec.Namespace = USome "System" && ty.TypeSpec.Name = "ValueType" && not (nspace = USome "System" && nm = "Enum"))
                     if isEnum then ILTypeDefKind.Enum
                     elif  (isDelegate && not selfIsMulticastDelegate) || isMulticastDelegate then ILTypeDefKind.Delegate
                     elif isValueType then ILTypeDefKind.ValueType
                     else ILTypeDefKind.Class

            and typeEncodingOfFlags flags =
                let f = (flags &&& 0x00030000)
                if f = 0x00020000 then ILDefaultPInvokeEncoding.Auto
                elif f = 0x00010000 then ILDefaultPInvokeEncoding.Unicode
                else ILDefaultPInvokeEncoding.Ansi

            and isTopTypeDef flags =
                (typeAccessOfFlags flags =  ILTypeDefAccess.Private) ||
                 typeAccessOfFlags flags =  ILTypeDefAccess.Public

            and seekIsTopTypeDefOfIdx idx =
                let (flags,_,_, _, _,_) = seekReadTypeDefRow idx
                isTopTypeDef flags

            and readStringHeapAsTypeName (nameIdx,namespaceIdx) =
                let name = readStringHeap nameIdx
                let nspace = readStringHeapOption namespaceIdx
                nspace, name

            and seekReadTypeDefRowExtents _info (idx:int) =
                if idx >= getNumRows ILTableNames.TypeDef then
                    getNumRows ILTableNames.Field + 1,
                    getNumRows ILTableNames.Method + 1
                else
                    let (_, _, _, _, fieldsIdx, methodsIdx) = seekReadTypeDefRow (idx + 1)
                    fieldsIdx, methodsIdx

            and seekReadTypeDefRowWithExtents (idx:int) =
                let info= seekReadTypeDefRow idx
                info,seekReadTypeDefRowExtents info idx

            and seekReadTypeDef toponly (idx:int) =
                let (flags, nameIdx, namespaceIdx, _, _, _) = seekReadTypeDefRow idx
                if toponly && not (isTopTypeDef flags) then None
                else

                 let name = readStringHeap nameIdx
                 let nspace = readStringHeapOption namespaceIdx
                 let rest =
                    lazy
                       let ((flags,nameIdx,namespaceIdx, extendsIdx, fieldsIdx, methodsIdx) as info) = seekReadTypeDefRow idx
                       let name = readStringHeap nameIdx
                       let nspace = readStringHeapOption namespaceIdx
                       let (endFieldsIdx, endMethodsIdx) = seekReadTypeDefRowExtents info idx
                       let typars = seekReadGenericParams 0 (TypeOrMethodDefTag.TypeDef,idx)
                       let numtypars = typars.Length
                       let super = seekReadOptionalTypeDefOrRef numtypars AsObject extendsIdx
                       //let layout = typeLayoutOfFlags flags idx
                       //let hasLayout = (match layout with ILTypeDefLayout.Explicit _ -> true | _ -> false)
                       let hasLayout = false
                       let mdefs = seekReadMethods numtypars methodsIdx endMethodsIdx
                       let fdefs = seekReadFields (numtypars,hasLayout) fieldsIdx endFieldsIdx
                       let kind = typeKindOfFlags nspace name super flags
                       let nested = seekReadNestedTypeDefs idx
                       let intfs = seekReadInterfaceImpls numtypars idx
                       //let sdecls =  seekReadSecurityDecls (TaggedIndex(hds_TypeDef,idx))
                       //let mimpls = seekReadMethodImpls numtypars idx
                       let props = seekReadProperties numtypars idx
                       let events = seekReadEvents numtypars idx
                       let cas = seekReadCustomAttrs (TaggedIndex(HasCustomAttributeTag.TypeDef,idx))
                       { Kind= kind
                         Namespace=nspace
                         Name=name
                         GenericParams=typars
                         Attributes = enum<TypeAttributes> flags
                         Access= typeAccessOfFlags flags
                         //Layout = layout
                         Encoding=typeEncodingOfFlags flags
                         NestedTypes= nested
                         Implements =  intfs
                         Extends = super
                         Methods = mdefs
                         //SecurityDecls = sdecls
                         //HasSecurity=(flags &&& 0x00040000) <> 0x0
                         Fields=fdefs
                         //MethodImpls=mimpls
                         InitSemantics=
                             if kind = ILTypeDefKind.Interface then ILTypeInit.OnAny
                             elif (flags &&& 0x00100000) <> 0x0 then ILTypeInit.BeforeField
                             else ILTypeInit.OnAny
                         Events= events
                         Properties=props
                         CustomAttrs=cas
                         Token = idx }
                 Some (nspace, name, rest)

            and seekReadTopTypeDefs () =
                [| for i = 1 to getNumRows ILTableNames.TypeDef do
                      match seekReadTypeDef true i  with
                      | None -> ()
                      | Some td -> yield td |]

            and seekReadNestedTypeDefs tidx =
                ILTypeDefs
                  (lazy
                       let nestedIdxs = seekReadIndexedRows (getNumRows ILTableNames.Nested,seekReadNestedRow,snd,simpleIndexCompare tidx,false,fst)
                       [| for i in nestedIdxs do
                             match seekReadTypeDef false i with
                             | None -> ()
                             | Some td -> yield td |])

            and seekReadInterfaceImpls numtypars tidx =
                seekReadIndexedRows (getNumRows ILTableNames.InterfaceImpl,seekReadInterfaceImplRow ,fst,simpleIndexCompare tidx,isSorted ILTableNames.InterfaceImpl,(snd >> seekReadTypeDefOrRef numtypars AsObject [| |]))

            and seekReadGenericParams numtypars (a,b) : ILGenericParameterDefs =
                let pars =
                    seekReadIndexedRows
                        (getNumRows ILTableNames.GenericParam,seekReadGenericParamRow,
                         (fun (_,_,_,tomd,_) -> tomd),
                         tomdCompare (TaggedIndex(a,b)),
                         isSorted ILTableNames.GenericParam,
                         (fun (gpidx,seq,flags,_,nameIdx) ->
                             let constraints = seekReadGenericParamConstraintsUncached numtypars gpidx
                             let cas = seekReadCustomAttrs (TaggedIndex(HasCustomAttributeTag.GenericParam,gpidx))
                             seq, {Name=readStringHeap nameIdx
                                   Constraints= constraints
                                   CustomAttrs=cas
                                   Attributes = enum (int32 flags) }))
                pars |> Array.sortBy fst |> Array.map snd

            and seekReadGenericParamConstraintsUncached numtypars gpidx =
                seekReadIndexedRows
                    (getNumRows ILTableNames.GenericParamConstraint,
                     seekReadGenericParamConstraintRow,
                     fst,
                     simpleIndexCompare gpidx,
                     isSorted ILTableNames.GenericParamConstraint,
                     (snd >>  seekReadTypeDefOrRef numtypars AsObject (*ok*) [| |]))

            and seekReadTypeDefAsType boxity (ginst:ILTypes) idx = cacheTypeDefAsType seekReadTypeDefAsTypeUncached (TypeDefAsTypIdx (boxity,ginst,idx))

            and seekReadTypeDefAsTypeUncached (TypeDefAsTypIdx (boxity,ginst,idx)) =
                mkILTy boxity (ILTypeSpec(seekReadTypeDefAsTypeRef idx, ginst))

            and seekReadTypeDefAsTypeRef idx =
                 let enc =
                   if seekIsTopTypeDefOfIdx idx then ILTypeRefScope.Top ILScopeRef.Local
                   else
                     let enclIdx = seekReadIndexedRow (getNumRows ILTableNames.Nested,seekReadNestedRow,fst,simpleIndexCompare idx,isSorted ILTableNames.Nested,snd)
                     let tref = seekReadTypeDefAsTypeRef enclIdx
                     ILTypeRefScope.Nested tref
                 let (_, nameIdx, namespaceIdx, _, _, _) = seekReadTypeDefRow idx
                 let nsp, nm = readStringHeapAsTypeName (nameIdx,namespaceIdx)
                 ILTypeRef(enc=enc, nsp = nsp, name = nm )

            and seekReadTypeRef idx = cacheTypeRef seekReadTypeRefUncached idx
            and seekReadTypeRefUncached idx =
                 let scopeIdx,nameIdx,namespaceIdx = seekReadTypeRefRow idx
                 let enc = seekReadTypeRefScope scopeIdx
                 let nsp, nm = readStringHeapAsTypeName (nameIdx,namespaceIdx)
                 ILTypeRef(enc, nsp, nm)

            and seekReadTypeRefAsType boxity ginst idx = cacheTypeRefAsType seekReadTypeRefAsTypeUncached (TypeRefAsTypIdx (boxity,ginst,idx))
            and seekReadTypeRefAsTypeUncached (TypeRefAsTypIdx (boxity,ginst,idx)) =
                 mkILTy boxity (ILTypeSpec(seekReadTypeRef idx, ginst))

            and seekReadTypeDefOrRef numtypars boxity (ginst:ILTypes) (TaggedIndex(tag,idx) ) =
                match tag with
                | tag when tag = TypeDefOrRefOrSpecTag.TypeDef -> seekReadTypeDefAsType boxity ginst idx
                | tag when tag = TypeDefOrRefOrSpecTag.TypeRef -> seekReadTypeRefAsType boxity ginst idx
                | tag when tag = TypeDefOrRefOrSpecTag.TypeSpec -> readBlobHeapAsType numtypars (seekReadTypeSpecRow idx)
                | _ -> failwith "seekReadTypeDefOrRef"

            and seekReadTypeDefOrRefAsTypeRef (TaggedIndex(tag,idx) ) =
                match tag with
                | tag when tag = TypeDefOrRefOrSpecTag.TypeDef -> seekReadTypeDefAsTypeRef idx
                | tag when tag = TypeDefOrRefOrSpecTag.TypeRef -> seekReadTypeRef idx
                | tag when tag = TypeDefOrRefOrSpecTag.TypeSpec -> ilg.typ_Object.TypeRef
                | _ -> failwith "seekReadTypeDefOrRefAsTypeRef_readTypeDefOrRefOrSpec"

            and seekReadMethodRefParent numtypars (TaggedIndex(tag,idx)) =
                match tag with
                | tag when tag = MemberRefParentTag.TypeRef -> seekReadTypeRefAsType AsObject (* not ok - no way to tell if a member ref parent is a value type or not *) [| |] idx
                | tag when tag = MemberRefParentTag.ModuleRef -> mkILTypeForGlobalFunctions (ILScopeRef.Module (seekReadModuleRef idx))
                | tag when tag = MemberRefParentTag.MethodDef ->
                    let (MethodData(enclTyp, cc, nm, argtys, retty, minst)) = seekReadMethodDefAsMethodData idx
                    let mspec = mkILMethSpecInTyRaw(enclTyp, cc, nm, argtys, retty, minst)
                    mspec.EnclosingType
                | tag when tag = MemberRefParentTag.TypeSpec -> readBlobHeapAsType numtypars (seekReadTypeSpecRow idx)
                | _ -> failwith "seekReadMethodRefParent"


            and seekReadCustomAttrType (TaggedIndex(tag,idx) ) =
                match tag with
                | tag when tag = CustomAttributeTypeTag.MethodDef ->
                    let (MethodData(enclTyp, cc, nm, argtys, retty, minst)) = seekReadMethodDefAsMethodData idx
                    mkILMethSpecInTyRaw (enclTyp, cc, nm, argtys, retty, minst)
                | tag when tag = CustomAttributeTypeTag.MemberRef ->
                    let (MethodData(enclTyp, cc, nm, argtys, retty, minst)) = seekReadMemberRefAsMethDataNoVarArgs 0 idx
                    mkILMethSpecInTyRaw (enclTyp, cc, nm, argtys, retty, minst)
                | _ -> failwith "seekReadCustomAttrType"

            and seekReadImplAsScopeRef (TaggedIndex(tag,idx) ) =
                 if idx = 0 then ILScopeRef.Local
                 else
                   match tag with
                   | tag when tag = ImplementationTag.File -> ILScopeRef.Module (seekReadFile idx)
                   | tag when tag = ImplementationTag.AssemblyRef -> ILScopeRef.Assembly (seekReadAssemblyRef idx)
                   | tag when tag = ImplementationTag.ExportedType -> failwith "seekReadImplAsScopeRef"
                   | _ -> failwith "seekReadImplAsScopeRef"

            and seekReadTypeRefScope (TaggedIndex(tag,idx) ) : ILTypeRefScope =
                match tag with
                | tag when tag = ResolutionScopeTag.Module -> ILTypeRefScope.Top(ILScopeRef.Local)
                | tag when tag = ResolutionScopeTag.ModuleRef -> ILTypeRefScope.Top(ILScopeRef.Module (seekReadModuleRef idx))
                | tag when tag = ResolutionScopeTag.AssemblyRef -> ILTypeRefScope.Top(ILScopeRef.Assembly (seekReadAssemblyRef idx))
                | tag when tag = ResolutionScopeTag.TypeRef -> ILTypeRefScope.Nested (seekReadTypeRef idx)
                | _ -> failwith "seekReadTypeRefScope"

            and seekReadOptionalTypeDefOrRef numtypars boxity idx =
                if idx = TaggedIndex(TypeDefOrRefOrSpecTag.TypeDef, 0) then None
                else Some (seekReadTypeDefOrRef numtypars boxity [| |] idx)

            and seekReadField (numtypars, _hasLayout) (idx:int) =
                 let (flags,nameIdx,typeIdx) = seekReadFieldRow idx
                 let nm = readStringHeap nameIdx
                 let isStatic = (flags &&& 0x0010) <> 0
                 { Name = nm
                   FieldType = readBlobHeapAsFieldSig numtypars typeIdx
                   Access = memberAccessOfFlags flags
                   IsStatic = isStatic
                   IsInitOnly = (flags &&& 0x0020) <> 0
                   IsLiteral = (flags &&& 0x0040) <> 0
                   NotSerialized = (flags &&& 0x0080) <> 0
                   IsSpecialName = (flags &&& 0x0200) <> 0 || (flags &&& 0x0400) <> 0 (* REVIEW: RTSpecialName *)
                   LiteralValue = if (flags &&& 0x8000) = 0 then None else Some (seekReadConstant (TaggedIndex(HasConstantTag.FieldDef,idx)))
            (*
                     Marshal =
                         if (flags &&& 0x1000) = 0 then None else
                         Some (seekReadIndexedRow (getNumRows ILTableNames.FieldMarshal,seekReadFieldMarshalRow,
                                                   fst,hfmCompare (TaggedIndex(hfm_FieldDef,idx)),
                                                   isSorted ILTableNames.FieldMarshal,
                                                   (snd >> readBlobHeapAsNativeType ctxt)))
                     Data =
                         if (flags &&& 0x0100) = 0 then None
                         else
                           let rva = seekReadIndexedRow (getNumRows ILTableNames.FieldRVA,seekReadFieldRVARow,
                                                         snd,simpleIndexCompare idx,isSorted ILTableNames.FieldRVA,fst)
                           Some (rvaToData "field" rva)
            *)
                   Attributes = enum<System.Reflection.FieldAttributes>(flags)
                   //Offset =
                   //      if hasLayout && not isStatic then
                   //          Some (seekReadIndexedRow (getNumRows ILTableNames.FieldLayout,seekReadFieldLayoutRow,
                   //                                    snd,simpleIndexCompare idx,isSorted ILTableNames.FieldLayout,fst)) else None
                   CustomAttrs=seekReadCustomAttrs (TaggedIndex(HasCustomAttributeTag.FieldDef,idx)) }

            and seekReadFields (numtypars, hasLayout) fidx1 fidx2 =
                { new ILFieldDefs with
                   member __.Elements =
                       [| for i = fidx1 to fidx2 - 1 do
                           yield seekReadField (numtypars, hasLayout) i |] }

            and seekReadMethods numtypars midx1 midx2 =
                ILMethodDefs
                   (lazy
                       [| for i = midx1 to midx2 - 1 do
                             yield seekReadMethod numtypars i |])

            and sigptrGetTypeDefOrRefOrSpecIdx bytes sigptr =
                let n, sigptr = sigptrGetZInt32 bytes sigptr
                if (n &&& 0x01) = 0x0 then (* Type Def *)
                    TaggedIndex(TypeDefOrRefOrSpecTag.TypeDef,  (n >>>& 2)), sigptr
                else (* Type Ref *)
                    TaggedIndex(TypeDefOrRefOrSpecTag.TypeRef,  (n >>>& 2)), sigptr

            and sigptrGetTy numtypars bytes sigptr =
                let b0,sigptr = sigptrGetByte bytes sigptr
                if b0 = et_OBJECT then ilg.typ_Object , sigptr
                elif b0 = et_STRING then ilg.typ_String, sigptr
                elif b0 = et_I1 then ilg.typ_SByte, sigptr
                elif b0 = et_I2 then ilg.typ_Int16, sigptr
                elif b0 = et_I4 then ilg.typ_Int32, sigptr
                elif b0 = et_I8 then ilg.typ_Int64, sigptr
                elif b0 = et_I then ilg.typ_IntPtr, sigptr
                elif b0 = et_U1 then ilg.typ_Byte, sigptr
                elif b0 = et_U2 then ilg.typ_UInt16, sigptr
                elif b0 = et_U4 then ilg.typ_UInt32, sigptr
                elif b0 = et_U8 then ilg.typ_UInt64, sigptr
                elif b0 = et_U then ilg.typ_UIntPtr, sigptr
                elif b0 = et_R4 then ilg.typ_Single, sigptr
                elif b0 = et_R8 then ilg.typ_Double, sigptr
                elif b0 = et_CHAR then ilg.typ_Char, sigptr
                elif b0 = et_BOOLEAN then ilg.typ_Boolean, sigptr
                elif b0 = et_WITH then
                    let b0,sigptr = sigptrGetByte bytes sigptr
                    let tdorIdx, sigptr = sigptrGetTypeDefOrRefOrSpecIdx bytes sigptr
                    let n, sigptr = sigptrGetZInt32 bytes sigptr
                    let argtys,sigptr = sigptrFold (sigptrGetTy numtypars) n bytes sigptr
                    seekReadTypeDefOrRef numtypars (if b0 = et_CLASS then AsObject else AsValue) argtys tdorIdx,
                    sigptr

                elif b0 = et_CLASS then
                    let tdorIdx, sigptr = sigptrGetTypeDefOrRefOrSpecIdx bytes sigptr
                    seekReadTypeDefOrRef numtypars AsObject [| |] tdorIdx, sigptr
                elif b0 = et_VALUETYPE then
                    let tdorIdx, sigptr = sigptrGetTypeDefOrRefOrSpecIdx bytes sigptr
                    seekReadTypeDefOrRef numtypars AsValue [| |] tdorIdx, sigptr
                elif b0 = et_VAR then
                    let n, sigptr = sigptrGetZInt32 bytes sigptr
                    ILType.Var n,sigptr
                elif b0 = et_MVAR then
                    let n, sigptr = sigptrGetZInt32 bytes sigptr
                    ILType.Var (n + numtypars), sigptr
                elif b0 = et_BYREF then
                    let typ, sigptr = sigptrGetTy numtypars bytes sigptr
                    ILType.Byref typ, sigptr
                elif b0 = et_PTR then
                    let typ, sigptr = sigptrGetTy numtypars bytes sigptr
                    ILType.Ptr typ, sigptr
                elif b0 = et_SZARRAY then
                    let typ, sigptr = sigptrGetTy numtypars bytes sigptr
                    mkILArr1DTy typ, sigptr
                elif b0 = et_ARRAY then
                    let typ, sigptr = sigptrGetTy numtypars bytes sigptr
                    let rank, sigptr = sigptrGetZInt32 bytes sigptr
                    let numSized, sigptr = sigptrGetZInt32 bytes sigptr
                    let sizes, sigptr = sigptrFold sigptrGetZInt32 numSized bytes sigptr
                    let numLoBounded, sigptr = sigptrGetZInt32 bytes sigptr
                    let lobounds, sigptr = sigptrFold sigptrGetZInt32 numLoBounded bytes sigptr
                    let shape =
                        let dim i =
                          (if i <  numLoBounded then Some lobounds.[i] else None),
                          (if i <  numSized then Some sizes.[i] else None)
                        ILArrayShape (Array.init rank dim)
                    ILType.Array (shape, typ), sigptr

                elif b0 = et_VOID then ILType.Void, sigptr
                elif b0 = et_TYPEDBYREF then
                    match ilg.typ_TypedReference with
                    | Some t -> t, sigptr
                    | _ -> failwith "system runtime doesn't contain System.TypedReference"
                elif b0 = et_CMOD_REQD || b0 = et_CMOD_OPT  then
                    let tdorIdx, sigptr = sigptrGetTypeDefOrRefOrSpecIdx bytes sigptr
                    let typ, sigptr = sigptrGetTy numtypars bytes sigptr
                    ILType.Modified((b0 = et_CMOD_REQD), seekReadTypeDefOrRefAsTypeRef tdorIdx, typ), sigptr
                elif b0 = et_FNPTR then
                    let ccByte,sigptr = sigptrGetByte bytes sigptr
                    let generic,cc = byteAsCallConv ccByte
                    if generic then failwith "fptr sig may not be generic"
                    let numparams,sigptr = sigptrGetZInt32 bytes sigptr
                    let retty,sigptr = sigptrGetTy numtypars bytes sigptr
                    let argtys,sigptr = sigptrFold (sigptrGetTy numtypars) ( numparams) bytes sigptr
                    ILType.FunctionPointer (ILCallingSignature(cc, argtys, retty)),sigptr
                elif b0 = et_SENTINEL then failwith "varargs NYI"
                else ILType.Void , sigptr

            and sigptrGetVarArgTys n numtypars bytes sigptr =
                sigptrFold (sigptrGetTy numtypars) n bytes sigptr

            and sigptrGetArgTys n numtypars bytes sigptr acc =
                if n <= 0 then (Array.ofList (List.rev acc),None),sigptr
                else
                  let b0,sigptr2 = sigptrGetByte bytes sigptr
                  if b0 = et_SENTINEL then
                    let varargs,sigptr = sigptrGetVarArgTys n numtypars bytes sigptr2
                    (Array.ofList (List.rev acc),Some( varargs)),sigptr
                  else
                    let x,sigptr = sigptrGetTy numtypars bytes sigptr
                    sigptrGetArgTys (n-1) numtypars bytes sigptr (x::acc)

            and readBlobHeapAsMethodSig numtypars blobIdx = cacheBlobHeapAsMethodSig readBlobHeapAsMethodSigUncached (BlobAsMethodSigIdx (numtypars,blobIdx))

            and readBlobHeapAsMethodSigUncached (BlobAsMethodSigIdx (numtypars,blobIdx)) =
                let bytes = readBlobHeap blobIdx
                let sigptr = 0
                let ccByte,sigptr = sigptrGetByte bytes sigptr
                let generic,cc = byteAsCallConv ccByte
                let genarity,sigptr = if generic then sigptrGetZInt32 bytes sigptr else 0x0,sigptr
                let numparams,sigptr = sigptrGetZInt32 bytes sigptr
                let retty,sigptr = sigptrGetTy numtypars bytes sigptr
                let (argtys,varargs),_sigptr = sigptrGetArgTys  ( numparams) numtypars bytes sigptr []
                generic,genarity,cc,retty,argtys,varargs

            and readBlobHeapAsType numtypars blobIdx =
                let bytes = readBlobHeap blobIdx
                let ty,_sigptr = sigptrGetTy numtypars bytes 0
                ty

            and readBlobHeapAsFieldSig numtypars blobIdx = cacheBlobHeapAsFieldSig readBlobHeapAsFieldSigUncached (BlobAsFieldSigIdx (numtypars,blobIdx))

            and readBlobHeapAsFieldSigUncached (BlobAsFieldSigIdx (numtypars,blobIdx)) =
                let bytes = readBlobHeap blobIdx
                let sigptr = 0
                let _ccByte,sigptr = sigptrGetByte bytes sigptr
                let retty,_sigptr = sigptrGetTy numtypars bytes sigptr
                retty


            and readBlobHeapAsPropertySig numtypars blobIdx = cacheBlobHeapAsPropertySig readBlobHeapAsPropertySigUncached (BlobAsPropSigIdx (numtypars,blobIdx))
            and readBlobHeapAsPropertySigUncached (BlobAsPropSigIdx (numtypars,blobIdx))  =
                let bytes = readBlobHeap blobIdx
                let sigptr = 0
                let ccByte,sigptr = sigptrGetByte bytes sigptr
                let hasthis = byteAsHasThis ccByte
                let numparams,sigptr = sigptrGetZInt32 bytes sigptr
                let retty,sigptr = sigptrGetTy numtypars bytes sigptr
                let argtys,_sigptr = sigptrFold (sigptrGetTy numtypars) ( numparams) bytes sigptr
                hasthis,retty, argtys

            and byteAsHasThis b =
                let hasthis_masked = b &&& 0x60uy
                if hasthis_masked = e_IMAGE_CEE_CS_CALLCONV_INSTANCE then ILThisConvention.Instance
                elif hasthis_masked = e_IMAGE_CEE_CS_CALLCONV_INSTANCE_EXPLICIT then ILThisConvention.InstanceExplicit
                else ILThisConvention.Static

            and byteAsCallConv b =
                let cc =
                    let ccMaxked = b &&& 0x0Fuy
                    if ccMaxked =  e_IMAGE_CEE_CS_CALLCONV_FASTCALL then ILArgConvention.FastCall
                    elif ccMaxked = e_IMAGE_CEE_CS_CALLCONV_STDCALL then ILArgConvention.StdCall
                    elif ccMaxked = e_IMAGE_CEE_CS_CALLCONV_THISCALL then ILArgConvention.ThisCall
                    elif ccMaxked = e_IMAGE_CEE_CS_CALLCONV_CDECL then ILArgConvention.CDecl
                    elif ccMaxked = e_IMAGE_CEE_CS_CALLCONV_VARARG then ILArgConvention.VarArg
                    else  ILArgConvention.Default
                let generic = (b &&& e_IMAGE_CEE_CS_CALLCONV_GENERIC) <> 0x0uy
                generic, Callconv (byteAsHasThis b,cc)

            and seekReadMemberRefAsMethodData numtypars idx : VarArgMethodData =  cacheMemberRefAsMemberData  seekReadMemberRefAsMethodDataUncached (MemberRefAsMspecIdx (numtypars,idx))

            and seekReadMemberRefAsMethodDataUncached (MemberRefAsMspecIdx (numtypars,idx)) =
                let (mrpIdx,nameIdx,typeIdx) = seekReadMemberRefRow idx
                let nm = readStringHeap nameIdx
                let enclTyp = seekReadMethodRefParent numtypars mrpIdx
                let _generic,genarity,cc,retty,argtys,varargs = readBlobHeapAsMethodSig enclTyp.GenericArgs.Length typeIdx
                let minst =  Array.init genarity (fun n -> ILType.Var (numtypars+n))
                (VarArgMethodData(enclTyp, cc, nm, argtys, varargs,retty,minst))

            and seekReadMemberRefAsMethDataNoVarArgs numtypars idx : MethodData =
               let (VarArgMethodData(enclTyp, cc, nm, argtys, _varargs, retty,minst)) =  seekReadMemberRefAsMethodData numtypars idx
               (MethodData(enclTyp, cc, nm, argtys, retty,minst))

            // One extremely annoying aspect of the MD format is that given a
            // ILMethodDef token it is non-trivial to find which ILTypeDef it belongs
            // to.  So we do a binary chop through the ILTypeDef table
            // looking for which ILTypeDef has the ILMethodDef within its range.
            // Although the ILTypeDef table is not "sorted", it is effectively sorted by
            // method-range and field-range start/finish indexes
            and seekReadMethodDefAsMethodData idx = cacheMethodDefAsMethodData seekReadMethodDefAsMethodDataUncached idx
            and seekReadMethodDefAsMethodDataUncached idx =
               let (_code_rva, _implflags, _flags, nameIdx, typeIdx, _paramIdx) = seekReadMethodRow idx
               let nm = readStringHeap nameIdx
               // Look for the method def parent.
               let tidx =
                 seekReadIndexedRow (getNumRows ILTableNames.TypeDef,
                                        (fun i -> i, seekReadTypeDefRowWithExtents i),
                                        (fun r -> r),
                                        (fun (_,((_, _, _, _, _, methodsIdx),
                                                  (_, endMethodsIdx)))  ->
                                                    if endMethodsIdx <= idx then 1
                                                    elif methodsIdx <= idx && idx < endMethodsIdx then 0
                                                    else -1),
                                        true,fst)
               let _generic,_genarity,cc,retty,argtys,_varargs = readBlobHeapAsMethodSig 0 typeIdx
               let finst = mkILFormalGenericArgsRaw (seekReadGenericParams 0 (TypeOrMethodDefTag.TypeDef,tidx))
               let minst = mkILFormalGenericArgsRaw (seekReadGenericParams finst.Length (TypeOrMethodDefTag.MethodDef,idx))
               let enclTyp = seekReadTypeDefAsType AsObject (* not ok: see note *) finst tidx
               MethodData(enclTyp, cc, nm, argtys, retty, minst)

            and seekReadMethod numtypars (idx:int) =
                 let (_codeRVA, implflags, flags, nameIdx, typeIdx, paramIdx) = seekReadMethodRow idx
                 let nm = readStringHeap nameIdx
                 let _generic,_genarity,cc,retty,argtys,_varargs = readBlobHeapAsMethodSig numtypars typeIdx

                 let endParamIdx =
                   if idx >= getNumRows ILTableNames.Method then
                     getNumRows ILTableNames.Param + 1
                   else
                     let (_,_,_,_,_, paramIdx) = seekReadMethodRow (idx + 1)
                     paramIdx

                 let ret,ilParams = seekReadParams (retty,argtys) paramIdx endParamIdx

                 { MetadataToken=idx // This value is not a strict metadata token but it's good enough (if needed we could get the real one pretty easily)
                   Name=nm
                   Access = memberAccessOfFlags flags
                   Attributes = enum<System.Reflection.MethodAttributes>(flags)
                   //SecurityDecls=seekReadSecurityDecls (TaggedIndex(hds_MethodDef,idx))
                   //IsEntryPoint= (fst entryPointToken = ILTableNames.Method && snd entryPointToken = idx)
                   ImplementationFlags= enum<MethodImplAttributes> implflags
                   GenericParams=seekReadGenericParams numtypars (TypeOrMethodDefTag.MethodDef,idx)
                   CustomAttrs=seekReadCustomAttrs (TaggedIndex(HasCustomAttributeTag.MethodDef,idx))
                   Parameters= ilParams
                   CallingConv=cc
                   Return=ret
                   //mdBody=
                   //  if (codetype = 0x01) then
                   //    ILMethodBody.Native
                   //  elif (codetype <> 0x00) then
                   //    ILMethodBody.Abstract
                   //  else
                   //    ILMethodBody.IL   //seekReadMethodRVA (idx,nm,internalcall,noinline,numtypars) codeRVA
                 }


            and seekReadParams (retty,argtys) pidx1 pidx2 =
                let retRes : ILReturn ref =  ref { (* Marshal=None *) Type=retty; CustomAttrs=ILCustomAttrsStatics.Empty }
                let paramsRes =
                    argtys
                    |> Array.map (fun ty ->
                        { Name=UNone
                          Default=None
                          //Marshal=None
                          Attributes= ParameterAttributes.None
                          ParameterType=ty
                          CustomAttrs=ILCustomAttrsStatics.Empty })
                for i = pidx1 to pidx2 - 1 do
                    seekReadParamExtras (retRes,paramsRes) i
                !retRes, paramsRes

            and seekReadParamExtras (retRes,paramsRes) (idx:int) =
               let (flags,seq,nameIdx) = seekReadParamRow idx
               //let _hasMarshal = (flags &&& 0x2000) <> 0x0
               let hasDefault = (flags &&& 0x1000) <> 0x0
               //let fmReader idx = seekReadIndexedRow (getNumRows ILTableNames.FieldMarshal,seekReadFieldMarshalRow,fst,hfmCompare idx,isSorted ILTableNames.FieldMarshal,(snd >> readBlobHeapAsNativeType ctxt))
               let cas = seekReadCustomAttrs (TaggedIndex(HasCustomAttributeTag.ParamDef,idx))
               if seq = 0 then
                   retRes := { !retRes with
                                    //Marshal=(if hasMarshal then Some (fmReader (TaggedIndex(hfm_ParamDef,idx))) else None);
                                    CustomAttrs = cas }
               else
                   paramsRes.[seq - 1] <-
                      { paramsRes.[seq - 1] with
                           //Marshal=(if hasMarshal then Some (fmReader (TaggedIndex(hfm_ParamDef,idx))) else None)
                           Default = (if hasDefault then Some (seekReadConstant (TaggedIndex(HasConstantTag.ParamDef,idx))) else None)
                           Name = readStringHeapOption nameIdx
                           Attributes = enum<ParameterAttributes> flags
                           CustomAttrs = cas }

            //and seekReadMethodImpls numtypars tidx =
            //   { new ILMethodImplDefs with
            //      member x.Elements =
            //          let mimpls = seekReadIndexedRows (getNumRows ILTableNames.MethodImpl,seekReadMethodImplRow,(fun (a,_,_) -> a),simpleIndexCompare tidx,isSorted ILTableNames.MethodImpl,(fun (_,b,c) -> b,c))
            //          mimpls |> Array.map (fun (b,c) ->
            //              { OverrideBy=
            //                  let (MethodData(enclTyp, cc, nm, argtys, retty,minst)) = seekReadMethodDefOrRefNoVarargs numtypars b
            //                  mkILMethSpecInTyRaw (enclTyp, cc, nm, argtys, retty,minst);
            //                Overrides=
            //                  let (MethodData(enclTyp, cc, nm, argtys, retty,minst)) = seekReadMethodDefOrRefNoVarargs numtypars c
            //                  let mspec = mkILMethSpecInTyRaw (enclTyp, cc, nm, argtys, retty,minst)
            //                  OverridesSpec(mspec.MethodRef, mspec.EnclosingType) }) }

            and seekReadMultipleMethodSemantics (flags,id) =
                seekReadIndexedRows
                  (getNumRows ILTableNames.MethodSemantics ,
                   seekReadMethodSemanticsRow,
                   (fun (_flags,_,c) -> c),
                   hsCompare id,
                   isSorted ILTableNames.MethodSemantics,
                   (fun (a,b,_c) ->
                       let (MethodData(enclTyp, cc, nm, argtys, retty, minst)) = seekReadMethodDefAsMethodData b
                       a, (mkILMethSpecInTyRaw (enclTyp, cc, nm, argtys, retty, minst)).MethodRef))
                |> Array.filter (fun (flags2,_) -> flags = flags2)
                |> Array.map snd


            and seekReadOptionalMethodSemantics id =
                match seekReadMultipleMethodSemantics id with
                | [| |] -> None
                | xs -> Some xs.[0]

            and seekReadMethodSemantics id =
                match seekReadOptionalMethodSemantics id with
                | None -> failwith "seekReadMethodSemantics ctxt: no method found"
                | Some x -> x

            and seekReadEvent _numtypars idx =
               let (flags,nameIdx,_typIdx) = seekReadEventRow idx
               { Name = readStringHeap nameIdx
                 //EventHandlerType = seekReadOptionalTypeDefOrRef numtypars AsObject typIdx
                 IsSpecialName = (flags &&& 0x0200) <> 0x0
                 IsRTSpecialName = (flags &&& 0x0400) <> 0x0
                 Attributes = enum<System.Reflection.EventAttributes>(flags)
                 AddMethod= seekReadMethodSemantics (0x0008,TaggedIndex(HasSemanticsTag.Event, idx))
                 RemoveMethod=seekReadMethodSemantics (0x0010,TaggedIndex(HasSemanticsTag.Event,idx))
                 //FireMethod=seekReadOptionalMethodSemantics (0x0020,TaggedIndex(HasSemanticsTag.Event,idx))
                 //OtherMethods = seekReadMultipleMethodSemantics (0x0004, TaggedIndex(HasSemanticsTag.Event, idx))
                 CustomAttrs=seekReadCustomAttrs (TaggedIndex(HasCustomAttributeTag.Event,idx)) }

            and seekReadEvents numtypars tidx =
               { new ILEventDefs with
                    member __.Elements =
                       match seekReadOptionalIndexedRow (getNumRows ILTableNames.EventMap,(fun i -> i, seekReadEventMapRow i),(fun (_,row) -> fst row),compare tidx,false,(fun (i,row) -> (i,snd row))) with
                       | None -> [| |]
                       | Some (rowNum,beginEventIdx) ->
                           let endEventIdx =
                               if rowNum >= getNumRows ILTableNames.EventMap then
                                   getNumRows ILTableNames.Event + 1
                               else
                                   let (_, endEventIdx) = seekReadEventMapRow (rowNum + 1)
                                   endEventIdx

                           [| for i in beginEventIdx .. endEventIdx - 1 do
                               yield seekReadEvent numtypars i |] }

            and seekReadProperty numtypars idx =
               let (flags,nameIdx,typIdx) = seekReadPropertyRow idx
               let cc,retty,argtys = readBlobHeapAsPropertySig numtypars typIdx
               let setter= seekReadOptionalMethodSemantics (0x0001,TaggedIndex(HasSemanticsTag.Property,idx))
               let getter = seekReadOptionalMethodSemantics (0x0002,TaggedIndex(HasSemanticsTag.Property,idx))
               let cc2 =
                   match getter with
                   | Some mref -> mref.CallingConv.ThisConv
                   | None ->
                       match setter with
                       | Some mref ->  mref.CallingConv .ThisConv
                       | None -> cc
               { Name=readStringHeap nameIdx
                 CallingConv = cc2
                 Attributes = enum<System.Reflection.PropertyAttributes>(flags)
                 SetMethod=setter;
                 GetMethod=getter;
                 PropertyType=retty;
                 Init= if (flags &&& 0x1000) = 0 then None else Some (seekReadConstant (TaggedIndex(HasConstantTag.Property,idx)));
                 IndexParameterTypes=argtys;
                 CustomAttrs=seekReadCustomAttrs (TaggedIndex(HasCustomAttributeTag.Property,idx)) }

            and seekReadProperties numtypars tidx =
               { new ILPropertyDefs with
                  member x.Elements =
                       match seekReadOptionalIndexedRow (getNumRows ILTableNames.PropertyMap,(fun i -> i, seekReadPropertyMapRow i),(fun (_,row) -> fst row),compare tidx,false,(fun (i,row) -> (i,snd row))) with
                       | None -> [| |]
                       | Some (rowNum,beginPropIdx) ->
                           let endPropIdx =
                               if rowNum >= getNumRows ILTableNames.PropertyMap then
                                   getNumRows ILTableNames.Property + 1
                               else
                                   let (_, endPropIdx) = seekReadPropertyMapRow (rowNum + 1)
                                   endPropIdx
                           [| for i in beginPropIdx .. endPropIdx - 1 do
                                 yield seekReadProperty numtypars i |] }


            and seekReadCustomAttrs idx =
                { new ILCustomAttrs with
                   member __.Elements =
                       seekReadIndexedRows (getNumRows ILTableNames.CustomAttribute,
                                              seekReadCustomAttributeRow,(fun (a,_,_) -> a),
                                              hcaCompare idx,
                                              isSorted ILTableNames.CustomAttribute,
                                              (fun (_,b,c) -> seekReadCustomAttr (b,c))) }

            and seekReadCustomAttr (catIdx,valIdx) =
                { Method=seekReadCustomAttrType catIdx;
                  Data=
                    match readBlobHeapOption valIdx with
                    | Some bytes -> bytes
                    | None -> [| |] }

            (*
            and seekReadSecurityDecls idx =
               mkILLazySecurityDecls
                (lazy
                     seekReadIndexedRows (getNumRows ILTableNames.Permission,
                                             seekReadPermissionRow,
                                             (fun (_,par,_) -> par),
                                             hdsCompare idx,
                                             isSorted ILTableNames.Permission,
                                             (fun (act,_,ty) -> seekReadSecurityDecl (act,ty))))

            and seekReadSecurityDecl (a,b) =
                ctxt.seekReadSecurityDecl (SecurityDeclIdx (a,b))

            and seekReadSecurityDeclUncached ctxtH (SecurityDeclIdx (act,ty)) =
                PermissionSet ((if List.memAssoc (int act) (Lazy.force ILSecurityActionRevMap) then List.assoc (int act) (Lazy.force ILSecurityActionRevMap) else failwith "unknown security action"),
                               readBlobHeap ty)

            *)

            and seekReadConstant idx =
              let kind,vidx = seekReadIndexedRow (getNumRows ILTableNames.Constant,
                                                  seekReadConstantRow,
                                                  (fun (_,key,_) -> key),
                                                  hcCompare idx,isSorted ILTableNames.Constant,(fun (kind,_,v) -> kind,v))
              match kind with
              | x when x = uint16 et_STRING ->
                let blobHeap = readBlobHeap vidx
                let s = System.Text.Encoding.Unicode.GetString(blobHeap, 0, blobHeap.Length)
                ILFieldInit.String (s)
              | x when x = uint16 et_BOOLEAN -> ILFieldInit.Bool (readBlobHeapAsBool vidx)
              | x when x = uint16 et_CHAR -> ILFieldInit.Char (readBlobHeapAsUInt16 vidx)
              | x when x = uint16 et_I1 -> ILFieldInit.Int8 (readBlobHeapAsSByte vidx)
              | x when x = uint16 et_I2 -> ILFieldInit.Int16 (readBlobHeapAsInt16 vidx)
              | x when x = uint16 et_I4 -> ILFieldInit.Int32 (readBlobHeapAsInt32 vidx)
              | x when x = uint16 et_I8 -> ILFieldInit.Int64 (readBlobHeapAsInt64 vidx)
              | x when x = uint16 et_U1 -> ILFieldInit.UInt8 (readBlobHeapAsByte vidx)
              | x when x = uint16 et_U2 -> ILFieldInit.UInt16 (readBlobHeapAsUInt16 vidx)
              | x when x = uint16 et_U4 -> ILFieldInit.UInt32 (readBlobHeapAsUInt32 vidx)
              | x when x = uint16 et_U8 -> ILFieldInit.UInt64 (readBlobHeapAsUInt64 vidx)
              | x when x = uint16 et_R4 -> ILFieldInit.Single (readBlobHeapAsSingle vidx)
              | x when x = uint16 et_R8 -> ILFieldInit.Double (readBlobHeapAsDouble vidx)
              | x when x = uint16 et_CLASS || x = uint16 et_OBJECT ->  ILFieldInit.Null
              | _ -> ILFieldInit.Null

            and seekReadManifestResources () =
                ILResources
                  (lazy
                     [| for i = 1 to getNumRows ILTableNames.ManifestResource do
                         let (offset,flags,nameIdx,implIdx) = seekReadManifestResourceRow i
                         let scoref = seekReadImplAsScopeRef implIdx
                         let datalab =
                           match scoref with
                           | ILScopeRef.Local ->
                              let start = anyV2P ("resource",offset + resourcesAddr)
                              let len = seekReadInt32 is start
                              ILResourceLocation.Local (fun () -> seekReadBytes is (start + 4) len)
                           | ILScopeRef.Module mref -> ILResourceLocation.File (mref,offset)
                           | ILScopeRef.Assembly aref -> ILResourceLocation.Assembly aref

                         let r =
                           { Name= readStringHeap nameIdx;
                             Location = datalab;
                             Access = (if (flags &&& 0x01) <> 0x0 then ILResourceAccess.Public else ILResourceAccess.Private);
                             CustomAttrs =  seekReadCustomAttrs (TaggedIndex(HasCustomAttributeTag.ManifestResource, i)) }
                         yield r |])

            and seekReadNestedExportedTypes parentIdx =
                ILNestedExportedTypesAndForwarders
                  (lazy
                     [| for i = 1 to getNumRows ILTableNames.ExportedType do
                           let (flags,_tok,nameIdx,namespaceIdx,implIdx) = seekReadExportedTypeRow i
                           if not (isTopTypeDef flags) then
                               let (TaggedIndex(tag,idx) ) = implIdx
                               match tag with
                               | tag when tag = ImplementationTag.ExportedType && idx = parentIdx  ->
                                   let _nsp, nm = readStringHeapAsTypeName (nameIdx,namespaceIdx)
                                   yield
                                     { Name=nm
                                       Access=(match typeAccessOfFlags flags with ILTypeDefAccess.Nested n -> n | _ -> failwith "non-nested access for a nested type described as being in an auxiliary module")
                                       Nested=seekReadNestedExportedTypes i
                                       CustomAttrs=seekReadCustomAttrs (TaggedIndex(HasCustomAttributeTag.ExportedType, i)) }
                               | _ -> () |])

            and seekReadTopExportedTypes () =
                ILExportedTypesAndForwarders
                  (lazy
                     [| for i = 1 to getNumRows ILTableNames.ExportedType do
                         let (flags,_tok,nameIdx,namespaceIdx,implIdx) = seekReadExportedTypeRow i
                         if isTopTypeDef flags then
                           let (TaggedIndex(tag,_idx) ) = implIdx

                           // the nested types will be picked up by their enclosing types
                           if tag <> ImplementationTag.ExportedType then
                               let nsp, nm = readStringHeapAsTypeName (nameIdx,namespaceIdx)

                               let scoref = seekReadImplAsScopeRef implIdx

                               let entry =
                                 { ScopeRef=scoref
                                   Namespace=nsp
                                   Name=nm
                                   IsForwarder =   ((flags &&& 0x00200000) <> 0) }
                                  // Access=typeAccessOfFlags flags
                                  // Nested=seekReadNestedExportedTypes i
                                  // CustomAttrs=seekReadCustomAttrs (TaggedIndex(HasCustomAttributeTag.ExportedType, i)) }
                               yield entry |])


            let ilModule = seekReadModule 1
            let ilAssemblyRefs = [ for i in 1 .. getNumRows ILTableNames.AssemblyRef do yield seekReadAssemblyRef i ]

            member x.Bytes = is.Bytes
            member x.ILGlobals = ilg
            member x.ILModuleDef = ilModule
            member x.ILAssemblyRefs = ilAssemblyRefs

        let sigptr_get_byte (bytes: byte[]) sigptr =
            int bytes.[sigptr], sigptr + 1

        let sigptr_get_u8 bytes sigptr =
            let b0,sigptr = sigptr_get_byte bytes sigptr
            byte b0,sigptr

        let sigptr_get_bool bytes sigptr =
            let b0,sigptr = sigptr_get_byte bytes sigptr
            (b0 = 0x01) ,sigptr

        let sigptr_get_i8 bytes sigptr =
            let i,sigptr = sigptr_get_u8 bytes sigptr
            sbyte i,sigptr

        let sigptr_get_u16 bytes sigptr =
            let b0,sigptr = sigptr_get_byte bytes sigptr
            let b1,sigptr = sigptr_get_byte bytes sigptr
            uint16 (b0 ||| (b1 <<< 8)),sigptr

        let sigptr_get_i16 bytes sigptr =
            let u,sigptr = sigptr_get_u16 bytes sigptr
            int16 u,sigptr

        let sigptr_get_i32 bytes sigptr =
            let b0,sigptr = sigptr_get_byte bytes sigptr
            let b1,sigptr = sigptr_get_byte bytes sigptr
            let b2,sigptr = sigptr_get_byte bytes sigptr
            let b3,sigptr = sigptr_get_byte bytes sigptr
            b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24),sigptr

        let sigptr_get_u32 bytes sigptr =
            let u,sigptr = sigptr_get_i32 bytes sigptr
            uint32 u,sigptr

        let sigptr_get_i64 bytes sigptr =
            let b0,sigptr = sigptr_get_byte bytes sigptr
            let b1,sigptr = sigptr_get_byte bytes sigptr
            let b2,sigptr = sigptr_get_byte bytes sigptr
            let b3,sigptr = sigptr_get_byte bytes sigptr
            let b4,sigptr = sigptr_get_byte bytes sigptr
            let b5,sigptr = sigptr_get_byte bytes sigptr
            let b6,sigptr = sigptr_get_byte bytes sigptr
            let b7,sigptr = sigptr_get_byte bytes sigptr
            int64 b0 ||| (int64 b1 <<< 8) ||| (int64 b2 <<< 16) ||| (int64 b3 <<< 24) |||
            (int64 b4 <<< 32) ||| (int64 b5 <<< 40) ||| (int64 b6 <<< 48) ||| (int64 b7 <<< 56),
            sigptr

        let sigptr_get_u64 bytes sigptr =
            let u,sigptr = sigptr_get_i64 bytes sigptr
            uint64 u,sigptr


        let ieee32_of_bits (x:int32) = System.BitConverter.ToSingle(System.BitConverter.GetBytes(x),0)
        let ieee64_of_bits (x:int64) = System.BitConverter.Int64BitsToDouble(x)

        let sigptr_get_ieee32 bytes sigptr =
            let u,sigptr = sigptr_get_i32 bytes sigptr
            ieee32_of_bits u,sigptr

        let sigptr_get_ieee64 bytes sigptr =
            let u,sigptr = sigptr_get_i64 bytes sigptr
            ieee64_of_bits u,sigptr

        let rec decodeCustomAttrElemType ilg bytes sigptr x =
            match x with
            | x when x =  et_I1 -> ilg.typ_SByte, sigptr
            | x when x = et_U1 -> ilg.typ_Byte, sigptr
            | x when x =  et_I2 -> ilg.typ_Int16, sigptr
            | x when x =  et_U2 -> ilg.typ_UInt16, sigptr
            | x when x =  et_I4 -> ilg.typ_Int32, sigptr
            | x when x =  et_U4 -> ilg.typ_UInt32, sigptr
            | x when x =  et_I8 -> ilg.typ_Int64, sigptr
            | x when x =  et_U8 -> ilg.typ_UInt64, sigptr
            | x when x =  et_R8 -> ilg.typ_Double, sigptr
            | x when x =  et_R4 -> ilg.typ_Single, sigptr
            | x when x = et_CHAR -> ilg.typ_Char, sigptr
            | x when x =  et_BOOLEAN -> ilg.typ_Boolean, sigptr
            | x when x =  et_STRING -> ilg.typ_String, sigptr
            | x when x =  et_OBJECT -> ilg.typ_Object, sigptr
            | x when x =  et_SZARRAY ->
                 let et,sigptr = sigptr_get_u8 bytes sigptr
                 let elemTy,sigptr = decodeCustomAttrElemType ilg bytes sigptr et
                 mkILArr1DTy elemTy, sigptr
            | x when x = 0x50uy -> ilg.typ_Type, sigptr
            | _ ->  failwithf "decodeCustomAttrElemType ilg: unrecognized custom element type: %A" x

        // Parse an IL type signature argument within a custom attribute blob
        type ILTypeSigParser(tstring : string) =

            let mutable startPos = 0
            let mutable currentPos = 0

            //let reset() = startPos <- 0 ; currentPos <- 0
            let nil = '\r' // cannot appear in a type sig

            // take a look at the next value, but don't advance
            let peek() = if currentPos < (tstring.Length-1) then tstring.[currentPos+1] else nil
            let peekN(skip) = if currentPos < (tstring.Length - skip) then tstring.[currentPos+skip] else nil
            // take a look at the current value, but don't advance
            let here() = if currentPos < tstring.Length then tstring.[currentPos] else nil
            // move on to the next character
            let step() = currentPos <- currentPos+1
            // ignore the current lexeme
            let skip() = startPos <- currentPos
            // ignore the current lexeme, advance
            let drop() = skip() ; step() ; skip()
            // return the current lexeme, advance
            let take() =
                let s = if currentPos < tstring.Length then tstring.[startPos..currentPos] else ""
                drop()
                s

            // The format we accept is
            // "<type name>{`<arity>[<type>,+]}{<array rank>}{<scope>}"  E.g.,
            //
            // System.Collections.Generic.Dictionary
            //     `2[
            //         [System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],
            //         dev.virtualearth.net.webservices.v1.search.CategorySpecificPropertySet],
            // mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
            //
            // Note that
            //   • Since we're only reading valid IL, we assume that the signature is properly formed
            //   • For type parameters, if the type is non-local, it will be wrapped in brackets ([])
            member x.ParseType() =

                // Does the type name start with a leading '['?  If so, ignore it
                // (if the specialization type is in another module, it will be wrapped in bracket)
                if here() = '[' then drop()

                // 1. Iterate over beginning of type, grabbing the type name and determining if it's generic or an array
                let typeName =
                    while (peek() <> '`') && (peek() <> '[') && (peek() <> ']') && (peek() <> ',') && (peek() <> nil) do step()
                    take()

                // 2. Classify the type

                // Is the type generic?
                let typeName, specializations =
                    if here() = '`' then
                        drop() // step to the number
                        // fetch the arity
                        let arity =
                            while (int(here()) >= (int('0'))) && (int(here()) <= ((int('9')))) && (int(peek()) >= (int('0'))) && (int(peek()) <= ((int('9')))) do step()
                            System.Int32.Parse(take())

                        // typically types are saturated, i.e. if generic they have arguments. However, assembly metadata for reflectedDefinitions they occur free.
                        // this code takes care of exactly this case.
                        if here () = '[' then
                            // skip the '['
                            drop()
                            // get the specializations
                            typeName+"`"+(arity.ToString()), Some(([| for _i in 0..arity-1 do yield x.ParseType() |]))
                        else
                            typeName+"`"+(arity.ToString()), None
                    else
                        typeName, None

                // Is the type an array?
                let rank =
                    if here() = '[' then
                        let mutable rank = 0

                        while here() <> ']' do
                            rank <- rank + 1
                            step()
                        drop()

                        Some(ILArrayShape(Array.create rank (Some 0, None)))
                    else
                        None

                // Is there a scope?
                let scope =
                    if (here() = ',' || here() = ' ') && (peek() <> '[' && peekN(2) <> '[') then
                        let grabScopeComponent() =
                            if here() = ',' then drop() // ditch the ','
                            if here() = ' ' then drop() // ditch the ' '

                            while (peek() <> ',' && peek() <> ']' && peek() <> nil) do step()
                            take()

                        let scope =
                            [ yield grabScopeComponent() // assembly
                              yield grabScopeComponent() // version
                              yield grabScopeComponent() // culture
                              yield grabScopeComponent() // public key token
                            ] |> String.concat ","
                        ILScopeRef.Assembly(ILAssemblyRef.FromAssemblyName(System.Reflection.AssemblyName(scope)))
                    else
                        ILScopeRef.Local

                // strip any extraneous trailing brackets or commas
                if (here() = ']')  then drop()
                if (here() = ',') then drop()

                // build the IL type
                let tref =
                    let nsp, nm = splitILTypeName typeName
                    ILTypeRef(ILTypeRefScope.Top scope, nsp, nm)

                let genericArgs =
                    match specializations with
                    | None -> [| |]
                    | Some(genericArgs) -> genericArgs
                let tspec = ILTypeSpec(tref,genericArgs)
                let ilty =
                    match tspec.Name with
                    | "System.SByte"
                    | "System.Byte"
                    | "System.Int16"
                    | "System.UInt16"
                    | "System.Int32"
                    | "System.UInt32"
                    | "System.Int64"
                    | "System.UInt64"
                    | "System.Char"
                    | "System.Double"
                    | "System.Single"
                    | "System.Boolean" -> ILType.Value(tspec)
                    | _ -> ILType.Boxed(tspec)

                // if it's an array, wrap it - otherwise, just return the IL type
                match rank with
                | Some(r) -> ILType.Array(r,ilty)
                | _ -> ilty


        let sigptr_get_z_i32 bytes sigptr =
            let b0,sigptr = sigptr_get_byte bytes sigptr
            if b0 <= 0x7F then b0, sigptr
            elif b0 <= 0xbf then
                let b0 = b0 &&& 0x7f
                let b1,sigptr = sigptr_get_byte bytes sigptr
                (b0 <<< 8) ||| b1, sigptr
            else
                let b0 = b0 &&& 0x3f
                let b1,sigptr = sigptr_get_byte bytes sigptr
                let b2,sigptr = sigptr_get_byte bytes sigptr
                let b3,sigptr = sigptr_get_byte bytes sigptr
                (b0 <<< 24) ||| (b1 <<< 16) ||| (b2 <<< 8) ||| b3, sigptr

        let sigptr_get_bytes n (bytes:byte[]) sigptr =
            let res = Array.zeroCreate n
            for i = 0 to n - 1 do
                res.[i] <- bytes.[sigptr + i]
            res, sigptr + n

        let sigptr_get_string n bytes sigptr =
            let intarray,sigptr = sigptr_get_bytes n bytes sigptr
            System.Text.Encoding.UTF8.GetString(intarray , 0, intarray.Length), sigptr

        let sigptr_get_serstring  bytes sigptr =
            let len,sigptr = sigptr_get_z_i32 bytes sigptr
            sigptr_get_string len bytes sigptr

        let sigptr_get_serstring_possibly_null  bytes sigptr =
            let b0,new_sigptr = sigptr_get_byte bytes sigptr
            if b0 = 0xFF then // null case
                None,new_sigptr
            else  // throw away  new_sigptr, getting length & text advance
                let len,sigptr = sigptr_get_z_i32 bytes sigptr
                let s, sigptr = sigptr_get_string len bytes sigptr
                Some(s),sigptr


        let decodeILCustomAttribData ilg (ca: ILCustomAttr) : ILCustomAttrArg list  =
            let bytes = ca.Data
            let sigptr = 0
            let bb0,sigptr = sigptr_get_byte bytes sigptr
            let bb1,sigptr = sigptr_get_byte bytes sigptr
            if not (bb0 = 0x01 && bb1 = 0x00) then failwith "decodeILCustomAttribData: invalid data";

            let rec parseVal argty sigptr =
              match argty with
              | ILType.Value tspec when tspec.Namespace = USome "System" && tspec.Name = "SByte" ->
                  let n,sigptr = sigptr_get_i8 bytes sigptr
                  (argty, box n), sigptr
              | ILType.Value tspec when tspec.Namespace = USome "System" && tspec.Name = "Byte" ->
                  let n,sigptr = sigptr_get_u8 bytes sigptr
                  (argty, box n), sigptr
              | ILType.Value tspec when tspec.Namespace = USome "System" && tspec.Name = "Int16" ->
                  let n,sigptr = sigptr_get_i16 bytes sigptr
                  (argty, box n), sigptr
              | ILType.Value tspec when tspec.Namespace = USome "System" && tspec.Name = "UInt16" ->
                  let n,sigptr = sigptr_get_u16 bytes sigptr
                  (argty, box n), sigptr
              | ILType.Value tspec when tspec.Namespace = USome "System" && tspec.Name = "Int32" ->
                  let n,sigptr = sigptr_get_i32 bytes sigptr
                  (argty, box n), sigptr
              | ILType.Value tspec when tspec.Namespace = USome "System" && tspec.Name = "UInt32" ->
                  let n,sigptr = sigptr_get_u32 bytes sigptr
                  (argty, box n), sigptr
              | ILType.Value tspec when tspec.Namespace = USome "System" && tspec.Name = "Int64" ->
                  let n,sigptr = sigptr_get_i64 bytes sigptr
                  (argty, box n), sigptr
              | ILType.Value tspec when tspec.Namespace = USome "System" && tspec.Name = "UInt64" ->
                  let n,sigptr = sigptr_get_u64 bytes sigptr
                  (argty, box n), sigptr
              | ILType.Value tspec when tspec.Namespace = USome "System" && tspec.Name = "Double" ->
                  let n,sigptr = sigptr_get_ieee64 bytes sigptr
                  (argty, box n), sigptr
              | ILType.Value tspec when tspec.Namespace = USome "System" && tspec.Name = "Single" ->
                  let n,sigptr = sigptr_get_ieee32 bytes sigptr
                  (argty, box n), sigptr
              | ILType.Value tspec when tspec.Namespace = USome "System" && tspec.Name = "Char" ->
                  let n,sigptr = sigptr_get_u16 bytes sigptr
                  (argty, box (char n)), sigptr
              | ILType.Value tspec when tspec.Namespace = USome "System" && tspec.Name = "Boolean" ->
                  let n,sigptr = sigptr_get_byte bytes sigptr
                  (argty, box (not (n = 0))), sigptr
              | ILType.Boxed tspec when tspec.Namespace = USome "System" && tspec.Name = "String" ->
                  let n,sigptr = sigptr_get_serstring_possibly_null bytes sigptr
                  (argty, box (match n with None -> null | Some s -> s)), sigptr
              | ILType.Boxed tspec when tspec.Namespace = USome "System" && tspec.Name = "Type" ->
                  let nOpt,sigptr = sigptr_get_serstring_possibly_null bytes sigptr
                  match nOpt with
                  | None -> (argty, box null) , sigptr // TODO: read System.Type attributes
                  | Some n ->
                    try
                        let parser = ILTypeSigParser(n)
                        parser.ParseType() |> ignore
                        (argty, box null) , sigptr // TODO: read System.Type attributes
                    with e ->
                        failwith (sprintf "decodeILCustomAttribData: error parsing type in custom attribute blob: %s" e.Message)
              | ILType.Boxed tspec when tspec.Namespace = USome "System" && tspec.Name = "Object" ->
                  let et,sigptr = sigptr_get_u8 bytes sigptr
                  if et = 0xFFuy then
                      (argty, null), sigptr
                  else
                      let ty,sigptr = decodeCustomAttrElemType ilg bytes sigptr et
                      parseVal ty sigptr
              | ILType.Array(shape,elemTy) when shape = ILArrayShape.SingleDimensional ->
                  let n,sigptr = sigptr_get_i32 bytes sigptr
                  if n = 0xFFFFFFFF then (argty, null),sigptr else
                  let rec parseElems acc n sigptr =
                    if n = 0 then List.rev acc else
                    let v,sigptr = parseVal elemTy sigptr
                    parseElems (v ::acc) (n-1) sigptr
                  let elems = parseElems [] n sigptr |> List.map snd |> List.toArray
                  (argty, box elems), sigptr
              | ILType.Value _ ->  (* assume it is an enumeration *)
                  let n,sigptr = sigptr_get_i32 bytes sigptr
                  (argty, box n), sigptr
              | _ ->  failwith "decodeILCustomAttribData: attribute data involves an enum or System.Type value"
            let rec parseFixed argtys sigptr =
              match argtys with
                [] -> [],sigptr
              | h::t ->
                  let nh,sigptr = parseVal h sigptr
                  let nt,sigptr = parseFixed t sigptr
                  nh ::nt, sigptr
            let fixedArgs,_sigptr = parseFixed (List.ofArray ca.Method.FormalArgTypes) sigptr
        (*
            let nnamed,sigptr = sigptr_get_u16 bytes sigptr
            let rec parseNamed acc n sigptr =
              if n = 0 then List.rev acc else
              let isPropByte,sigptr = sigptr_get_u8 bytes sigptr
              let isProp = (int isPropByte = 0x54)
              let et,sigptr = sigptr_get_u8 bytes sigptr
              // We have a named value
              let ty,sigptr =
                if (0x50 = (int et) || 0x55 = (int et)) then
                    let qualified_tname,sigptr = sigptr_get_serstring bytes sigptr
                    let unqualified_tname, rest =
                        let pieces = qualified_tname.Split(',')
                        if pieces.Length > 1 then
                            pieces.[0], Some (String.concat "," pieces.[1..])
                        else
                            pieces.[0], None
                    let scoref =
                        match rest with
                        | Some aname -> ILTypeRefScope.Top(ILScopeRef.Assembly(ILAssemblyRef.FromAssemblyName(System.Reflection.AssemblyName(aname))))
                        | None -> ilg.typ_Boolean.TypeSpec.Scope

                    let nsp, nm = splitILTypeName unqualified_tname
                    let tref = ILTypeRef (scoref, nsp, nm)
                    let tspec = mkILNonGenericTySpec tref
                    ILType.Value(tspec),sigptr
                else
                    decodeCustomAttrElemType ilg bytes sigptr et
              let nm,sigptr = sigptr_get_serstring bytes sigptr
              let (_,v),sigptr = parseVal ty sigptr
              parseNamed ((nm,ty,isProp,v) :: acc) (n-1) sigptr
            let named = parseNamed [] (int nnamed) sigptr
            fixedArgs, named
        *)
            fixedArgs


        type CacheValue = ILModuleReader
        let (|CacheValue|_|) (wr: WeakReference) = match wr.Target with null -> None | v -> Some (v :?> CacheValue)
        let CacheValue (reader: CacheValue) = System.WeakReference reader

        // Amortize readers weakly - this is enough that all the type providers in this DLL will at least share
        // resources when all instantiated at the same time.
        let readersWeakCache = ConcurrentDictionary<(string * string), WeakReference>()

        let ILModuleReaderAfterReadingAllBytes  (file:string, ilGlobals: ILGlobals) =
            let bytes = File.ReadAllBytes file
            let key = (file, ilGlobals.systemRuntimeScopeRef.QualifiedName)
            match readersWeakCache.TryGetValue (key) with
            | true, CacheValue mr2  when bytes = mr2.Bytes ->
                mr2 // throw away the bytes we just read and recycle the existing ILModuleReader
            | _ ->
                let mr = ILModuleReader(file, ByteFile(bytes), ilGlobals, true)
                readersWeakCache.[key] <- CacheValue (mr)
                mr


        (* NOTE: ecma_ prefix refers to the standard "mscorlib" *)
        let EcmaPublicKey = PublicKeyToken ([|0xdeuy; 0xaduy; 0xbeuy; 0xefuy; 0xcauy; 0xfeuy; 0xfauy; 0xceuy |])
        let EcmaMscorlibScopeRef = ILScopeRef.Assembly (ILAssemblyRef("mscorlib", None, Some EcmaPublicKey, true, None, UNone))

//====================================================================================================
// AssemblyReaderReflection 
//
// An implementation of reflection objects over on-disk assemblies, sufficient to give
// System.Type, System.MethodInfo, System.ConstructorInfo etc. objects
// that can be referred to in quotations and used as backing information for cross-
// targeting F# type providers.


namespace ProviderImplementation.ProvidedTypes.AssemblyReaderReflection

    //
    // The on-disk assemblies are read by AssemblyReader.
    //
    // Background
    // ----------
    //
    // Provided type/member definitions need to refer to non-provided definitions like "System.Object" and "System.String".
    //
    // For cross-targeting F# type providers, these can be references to assemblies that can't easily be loaded by .NET
    // reflection. For this reason, an implementation of the .NET reflection objects is needed. At minimum this
    // implementation must support the operations used by the F# compiler to interrogate the reflection objects.
    //
    //     For a System.Assembly, the information must be sufficient to allow the Assembly --> ILScopeRef conversion
    //     in ExtensionTyping.fs of the F# compiler. This requires:
    //         Assembly.GetName()
    //
    //     For a System.Type representing a reference to a named type definition, the information must be sufficient
    //     to allow the Type --> ILTypeRef conversion in the F# compiler. This requires:
    //         typ.DeclaringType
    //         typ.Name
    //         typ.Namespace
    //
    //     For a System.Type representing a type expression, the information must be sufficient to allow the Type --> ILType.Var conversion in the F# compiler.
    //        typeof<System.Void>.Equals(typ)
    //        typ.IsGenericParameter
    //           typ.GenericParameterPosition
    //        typ.IsArray
    //           typ.GetElementType()
    //           typ.GetArrayRank()
    //        typ.IsByRef
    //           typ.GetElementType()
    //        typ.IsPointer
    //           typ.GetElementType()
    //        typ.IsGenericType
    //           typ.GetGenericArguments()
    //           typ.GetGenericTypeDefinition()
    //
    //     For a System.MethodBase --> ILType.ILMethodRef conversion:
    //
    //       :?> MethodInfo as minfo
    //
    //          minfo.IsGenericMethod || minfo.DeclaringType.IsGenericType
    //             minfo.DeclaringType.GetGenericTypeDefinition
    //             minfo.DeclaringType.GetMethods().MetadataToken
    //             minfo.MetadataToken
    //          minfo.IsGenericMethod
    //             minfo.GetGenericArguments().Length
    //          minfo.ReturnType
    //          minfo.GetParameters | .ParameterType
    //          minfo.Name
    //
    //       :?> ConstructorInfo as cinfo
    //
    //          cinfo.DeclaringType.IsGenericType
    //             cinfo.DeclaringType.GetGenericTypeDefinition
    //             cinfo.DeclaringType.GetConstructors() GetParameters | .ParameterType
    //

    #nowarn "40"

    open System
    open System.IO
    open System.Collections.Generic
    open System.Reflection
    open ProviderImplementation.ProvidedTypes.AssemblyReader
    open ProviderImplementation.ProvidedTypes.Misc


    [<AutoOpen>]
    module Utils =
        let nullToOption x = match x with null -> None | _ -> Some x
        let optionToNull x = match x with None -> null | Some x -> x
        let uoptionToNull x = match x with UNone -> null | USome x -> x
        let notRequired msg nm =
           failwithf "SHOULD NOT BE REQUIRED! %s for %s. Stack trace:\n%s" msg nm (System.Diagnostics.StackTrace().ToString())

        // A table tracking how wrapped type definition objects are translated to cloned objects.
        // Unique wrapped type definition objects must be translated to unique wrapper objects, based
        // on object identity.
        type TxTable<'T2>() =
            let tab = Dictionary<int, 'T2>()
            member __.Get inp f =
                if tab.ContainsKey inp then
                    tab.[inp]
                else
                    let res = f()
                    tab.[inp] <- res
                    res

            member __.ContainsKey inp = tab.ContainsKey inp

        let lengthsEqAndForall2 (arr1: 'T1[]) (arr2: 'T2[]) f =
            (arr1.Length = arr2.Length) &&
            (arr1,arr2) ||> Array.forall2 f

        // Instantiate a type's generic parameters
        let rec instType inst (ty:Type) =
            if ty.IsGenericType then
                let args = Array.map (instType inst) (ty.GetGenericArguments())
                ty.GetGenericTypeDefinition().MakeGenericType(args)
            elif ty.HasElementType then
                let ety = instType inst (ty.GetElementType())
                if ty.IsArray then
                    let rank = ty.GetArrayRank()
                    if rank = 1 then ety.MakeArrayType()
                    else ety.MakeArrayType(rank)
                elif ty.IsPointer then ety.MakePointerType()
                elif ty.IsByRef then ety.MakeByRefType()
                else ty
            elif ty.IsGenericParameter then
                let pos = ty.GenericParameterPosition
                let (inst1: Type[], inst2: Type[]) = inst
                if pos < inst1.Length then inst1.[pos]
                elif pos < inst1.Length + inst2.Length then inst2.[pos - inst1.Length]
                else ty
            else ty

        let instParameterInfo inst (inp: ParameterInfo) =
            { new ParameterInfo() with
                override __.Name = inp.Name
                override __.ParameterType = inp.ParameterType |> instType inst
                override __.Attributes = inp.Attributes
                override __.RawDefaultValue = inp.RawDefaultValue
                override __.GetCustomAttributesData() = inp.GetCustomAttributesData()
                override x.ToString() = inp.ToString() + "@inst" }

        let rec eqType (ty1:Type) (ty2:Type) =
            if ty1.IsGenericType then ty2.IsGenericType && lengthsEqAndForall2 (ty1.GetGenericArguments()) (ty2.GetGenericArguments()) eqType
            elif ty1.IsArray then ty2.IsArray && ty1.GetArrayRank() = ty2.GetArrayRank() && eqType (ty1.GetElementType()) (ty2.GetElementType())
            elif ty1.IsPointer then ty2.IsPointer && eqType (ty1.GetElementType()) (ty2.GetElementType())
            elif ty1.IsByRef then ty2.IsByRef && eqType (ty1.GetElementType()) (ty2.GetElementType())
            else ty1.Equals(box ty2)

        let hashILParameterTypes (ps: ILParameters) =
           // This hash code doesn't need to be very good as hashing by name is sufficient to give decent hash granularity
           ps.Length

        let eqILScopeRef (_sco1: ILScopeRef) (_sco2: ILScopeRef) =
            true // TODO (though omitting this is not a problem in practice since type equivalence by name is sufficient to bind methods)

        let eqAssemblyAndILScopeRef (_ass1: Assembly) (_sco2: ILScopeRef) =
            true // TODO (though omitting this is not a problem in practice since type equivalence by name is sufficient to bind methods)


        let rec eqILTypeRef (ty1: ILTypeRef) (ty2: ILTypeRef) =
            ty1.Name = ty2.Name && eqILTypeRefScope ty1.Scope ty2.Scope

        and eqILTypeRefScope (ty1: ILTypeRefScope) (ty2: ILTypeRefScope) =
            match ty1, ty2 with
            | ILTypeRefScope.Top scoref1, ILTypeRefScope.Top scoref2 -> eqILScopeRef scoref1 scoref2
            | ILTypeRefScope.Nested tref1, ILTypeRefScope.Nested tref2 -> eqILTypeRef tref1 tref2
            | _ -> false

        and eqILTypes (tys1: ILType[]) (tys2: ILType[]) =
            lengthsEqAndForall2 tys1 tys2 eqILType

        and eqILType (ty1: ILType) (ty2: ILType) =
            match ty1, ty2 with
            | (ILType.Value(tspec1) | ILType.Boxed(tspec1)), (ILType.Value(tspec2) | ILType.Boxed(tspec2))->
                eqILTypeRef tspec1.TypeRef tspec2.TypeRef && eqILTypes tspec1.GenericArgs tspec2.GenericArgs
            | ILType.Array(rank1, arg1), ILType.Array(rank2, arg2) ->
                rank1 = rank2 && eqILType arg1 arg2
            | ILType.Ptr(arg1), ILType.Ptr(arg2) ->
                eqILType arg1 arg2
            | ILType.Byref(arg1), ILType.Byref(arg2) ->
                eqILType arg1 arg2
            | ILType.Var(arg1), ILType.Var(arg2) ->
                arg1 = arg2
            | _ -> false

        let rec eqTypeAndILTypeRef (ty1: Type) (ty2: ILTypeRef) =
            ty1.Name = ty2.Name &&
            ty1.Namespace = (uoptionToNull ty2.Namespace) &&
            match ty2.Scope with
            | ILTypeRefScope.Top scoref2 -> eqAssemblyAndILScopeRef ty1.Assembly scoref2
            | ILTypeRefScope.Nested tref2 -> ty1.IsNested && eqTypeAndILTypeRef ty1.DeclaringType tref2

        let rec eqTypesAndILTypes (tys1: Type[]) (tys2: ILType[]) =
            eqTypesAndILTypesWithInst [| |] tys1 tys2

        and eqTypesAndILTypesWithInst inst2 (tys1: Type[]) (tys2: ILType[]) =
            lengthsEqAndForall2 tys1 tys2 (eqTypeAndILTypeWithInst inst2)

        and eqTypeAndILTypeWithInst inst2 (ty1: Type) (ty2: ILType) =
            match ty2 with
            | (ILType.Value(tspec2) | ILType.Boxed(tspec2))->
                if tspec2.GenericArgs.Length > 0 then
                    ty1.IsGenericType && eqTypeAndILTypeRef (ty1.GetGenericTypeDefinition()) tspec2.TypeRef && eqTypesAndILTypesWithInst inst2 (ty1.GetGenericArguments()) tspec2.GenericArgs
                else
                    not ty1.IsGenericType && eqTypeAndILTypeRef ty1 tspec2.TypeRef
            | ILType.Array(rank2, arg2) ->
                ty1.IsArray && ty1.GetArrayRank() = rank2.Rank && eqTypeAndILTypeWithInst inst2 (ty1.GetElementType()) arg2
            | ILType.Ptr(arg2) ->
                ty1.IsPointer && eqTypeAndILTypeWithInst inst2 (ty1.GetElementType()) arg2
            | ILType.Byref(arg2) ->
                ty1.IsByRef && eqTypeAndILTypeWithInst inst2 (ty1.GetElementType()) arg2
            | ILType.Var(arg2) ->
                if int arg2 < inst2.Length then
                     eqType ty1 inst2.[int arg2]
                else
                     ty1.IsGenericParameter && ty1.GenericParameterPosition = int arg2

            | _ -> false

        let eqParametersAndILParameterTypesWithInst inst2 (ps1: ParameterInfo[])  (ps2: ILParameters) =
            lengthsEqAndForall2 ps1 ps2 (fun p1 p2 -> eqTypeAndILTypeWithInst inst2 p1.ParameterType p2.ParameterType)

        let adjustTypeAttributes isNested attributes =
            let visibilityAttributes =
                match attributes &&& TypeAttributes.VisibilityMask with
                | TypeAttributes.Public when isNested -> TypeAttributes.NestedPublic
                | TypeAttributes.NotPublic when isNested -> TypeAttributes.NestedAssembly
                | TypeAttributes.NestedPublic when not isNested -> TypeAttributes.Public
                | TypeAttributes.NestedAssembly
                | TypeAttributes.NestedPrivate
                | TypeAttributes.NestedFamORAssem
                | TypeAttributes.NestedFamily
                | TypeAttributes.NestedFamANDAssem when not isNested -> TypeAttributes.NotPublic
                | a -> a
            (attributes &&& ~~~TypeAttributes.VisibilityMask) ||| visibilityAttributes



        let convFieldInit x =
            match x with
            | ILFieldInit.String s       -> box s
            | ILFieldInit.Bool bool      -> box bool
            | ILFieldInit.Char u16       -> box (char (int u16))
            | ILFieldInit.Int8 i8        -> box i8
            | ILFieldInit.Int16 i16      -> box i16
            | ILFieldInit.Int32 i32      -> box i32
            | ILFieldInit.Int64 i64      -> box i64
            | ILFieldInit.UInt8 u8       -> box u8
            | ILFieldInit.UInt16 u16     -> box u16
            | ILFieldInit.UInt32 u32     -> box u32
            | ILFieldInit.UInt64 u64     -> box u64
            | ILFieldInit.Single ieee32 -> box ieee32
            | ILFieldInit.Double ieee64 -> box ieee64
            | ILFieldInit.Null            -> (null :> Object)

    /// Represents the type constructor in a provided symbol type.
    [<RequireQualifiedAccess>]
    type ContextTypeSymbolKind =
        | SDArray
        | Array of int
        | Pointer
        | ByRef
        | Generic of ContextTypeDefinition


    /// Represents an array or other symbolic type involving a provided type as the argument.
    /// See the type provider spec for the methods that must be implemented.
    /// Note that the type provider specification does not require us to implement pointer-equality for provided types.
    and ContextTypeSymbol(kind: ContextTypeSymbolKind, args: Type[]) =
        inherit Type()

        override __.FullName =
            match kind,args with
            | ContextTypeSymbolKind.SDArray,[| arg |] -> arg.FullName + "[]"
            | ContextTypeSymbolKind.Array _,[| arg |] -> arg.FullName + "[*]"
            | ContextTypeSymbolKind.Pointer,[| arg |] -> arg.FullName + "*"
            | ContextTypeSymbolKind.ByRef,[| arg |] -> arg.FullName + "&"
            | ContextTypeSymbolKind.Generic gtd, args -> gtd.FullName + "[" + (args |> Array.map (fun arg -> arg.FullName) |> String.concat ",") + "]"
            | _ -> failwith "unreachable"

        override __.DeclaringType =
            match kind,args with
            | ContextTypeSymbolKind.SDArray,[| arg |]
            | ContextTypeSymbolKind.Array _,[| arg |]
            | ContextTypeSymbolKind.Pointer,[| arg |]
            | ContextTypeSymbolKind.ByRef,[| arg |] -> arg.DeclaringType
            | ContextTypeSymbolKind.Generic gtd,_ -> gtd.DeclaringType
            | _ -> failwith "unreachable"

        override __.IsAssignableFrom(otherTy) =
            match kind with
            | ContextTypeSymbolKind.Generic gtd ->
                if otherTy.IsGenericType then
                    let otherGtd = otherTy.GetGenericTypeDefinition()
                    let otherArgs = otherTy.GetGenericArguments()
                    let yes = gtd.Equals(otherGtd) && Seq.forall2 eqType args otherArgs
                    yes
                else
                    base.IsAssignableFrom(otherTy)
            | _ -> base.IsAssignableFrom(otherTy)

        override this.IsSubclassOf(otherTy) =
            base.IsSubclassOf(otherTy) ||
            match kind with
            | ContextTypeSymbolKind.Generic gtd -> gtd.Metadata.IsDelegate && otherTy = typeof<Delegate> // F# quotations implementation
            | _ -> false

        override __.Name =
            match kind,args with
            | ContextTypeSymbolKind.SDArray,[| arg |] -> arg.Name + "[]"
            | ContextTypeSymbolKind.Array _,[| arg |] -> arg.Name + "[*]"
            | ContextTypeSymbolKind.Pointer,[| arg |] -> arg.Name + "*"
            | ContextTypeSymbolKind.ByRef,[| arg |] -> arg.Name + "&"
            | ContextTypeSymbolKind.Generic gtd, _args -> gtd.Name
            | _ -> failwith "unreachable"

        override __.BaseType =
            match kind with
            | ContextTypeSymbolKind.SDArray -> typeof<System.Array>
            | ContextTypeSymbolKind.Array _ -> typeof<System.Array>
            | ContextTypeSymbolKind.Pointer -> typeof<System.ValueType>
            | ContextTypeSymbolKind.ByRef -> typeof<System.ValueType>
            | ContextTypeSymbolKind.Generic gtd  -> instType (args, [| |]) gtd.BaseType

        override this.Assembly =
            match kind, args with
            | ContextTypeSymbolKind.SDArray,[| arg |]
            | ContextTypeSymbolKind.Array _,[| arg |]
            | ContextTypeSymbolKind.Pointer,[| arg |]
            | ContextTypeSymbolKind.ByRef,[| arg |] -> arg.Assembly
            | ContextTypeSymbolKind.Generic gtd, _ -> gtd.Assembly
            | _ -> notRequired "Assembly" this.Name

        override this.Namespace =
            match kind, args with
            | ContextTypeSymbolKind.SDArray,[| arg |]
            | ContextTypeSymbolKind.Array _,[| arg |]
            | ContextTypeSymbolKind.Pointer,[| arg |]
            | ContextTypeSymbolKind.ByRef,[| arg |] -> arg.Namespace
            | ContextTypeSymbolKind.Generic gtd, _ -> gtd.Namespace
            | _ -> failwith "unreachable"

        override __.GetArrayRank() = (match kind with ContextTypeSymbolKind.Array n -> n | ContextTypeSymbolKind.SDArray -> 1 | _ -> invalidOp "non-array type")
        override __.IsValueTypeImpl() = (match kind with ContextTypeSymbolKind.Generic gtd -> gtd.IsValueType | _ -> false)
        override __.IsArrayImpl() = (match kind with ContextTypeSymbolKind.Array _ | ContextTypeSymbolKind.SDArray -> true | _ -> false)
        override __.IsByRefImpl() = (match kind with ContextTypeSymbolKind.ByRef _ -> true | _ -> false)
        override __.IsPointerImpl() = (match kind with ContextTypeSymbolKind.Pointer _ -> true | _ -> false)
        override __.IsPrimitiveImpl() = false
        override __.IsGenericType = (match kind with ContextTypeSymbolKind.Generic _ -> true | _ -> false)
        override __.GetGenericArguments() = (match kind with ContextTypeSymbolKind.Generic _ -> args | _ -> [| |])
        override __.GetGenericTypeDefinition() = (match kind with ContextTypeSymbolKind.Generic e -> (e :> Type) | _ -> invalidOp "non-generic type")
        override __.IsCOMObjectImpl() = false
        override __.HasElementTypeImpl() = (match kind with ContextTypeSymbolKind.Generic _ -> false | _ -> true)
        override __.GetElementType() = (match kind,args with (ContextTypeSymbolKind.Array _  | ContextTypeSymbolKind.SDArray | ContextTypeSymbolKind.ByRef | ContextTypeSymbolKind.Pointer),[| e |] -> e | _ -> invalidOp (sprintf "%A, %A: not an array, pointer or byref type" kind args))

        override this.Module : Module = notRequired "Module" this.Name

        override this.GetHashCode()                                                                    =
            match kind,args with
            | ContextTypeSymbolKind.SDArray,[| arg |] -> 10 + hash arg
            | ContextTypeSymbolKind.Array _,[| arg |] -> 163 + hash arg
            | ContextTypeSymbolKind.Pointer,[| arg |] -> 283 + hash arg
            | ContextTypeSymbolKind.ByRef,[| arg |] -> 43904 + hash arg
            | ContextTypeSymbolKind.Generic gtd,_ -> 9797 + hash gtd + Array.sumBy hash args
            | _ -> failwith "unreachable"

        override this.Equals(other: obj) =
            match other with
            | :? ContextTypeSymbol as otherTy -> (kind, args) = (otherTy.Kind, otherTy.Args)
            | _ -> false

        member this.Kind = kind
        member this.Args = args

        override this.GetConstructors _bindingAttr = notRequired "GetConstructors" this.Name
        override this.GetMethodImpl(name, _bindingAttr, _binderBinder, _callConvention, types, _modifiers) =
            match kind with
            | ContextTypeSymbolKind.Generic gtd ->

                let md =
                    match types with
                    | null ->
                        match gtd.Metadata.Methods.FindByName(name) with
                        | [| md |] -> md
                        | [| |] -> failwith (sprintf "method %s not found" name)
                        | _ -> failwith (sprintf "multiple methods called '%s' found" name)
                    | _ ->
                        match gtd.Metadata.Methods.FindByNameAndArity(name, types.Length) with
                        | [| |] ->  failwith (sprintf "method %s not found with arity %d" name types.Length)
                        | mds ->
                            match mds |> Array.filter (fun md -> eqTypesAndILTypesWithInst args types md.ParameterTypes) with
                            | [| |] ->
                                let md1 = mds.[0]
                                ignore md1
                                failwith (sprintf "no method %s with arity %d found with right types. Comparisons:" name types.Length
                                          + ((types, md1.ParameterTypes) ||> Array.map2 (fun a pt -> eqTypeAndILTypeWithInst args a pt |> sprintf "%A") |> String.concat "\n"))
                            | [| md |] -> md
                            | _ -> failwith (sprintf "multiple methods %s with arity %d found with right types" name types.Length)

                gtd.MakeMethodInfo (this, md)

            | _ -> notRequired "ContextTypeSymbol: GetMethodImpl" this.Name

        override this.GetConstructorImpl(_bindingAttr, _binderBinder, _callConvention, types, _modifiers) =
            match kind with
            | ContextTypeSymbolKind.Generic gtd ->
                let name = ".ctor"
                let md =
                    match types with
                    | null ->
                        match gtd.Metadata.Methods.FindByName(name) with
                        | [| md |] -> md
                        | [| |] -> failwith (sprintf "method %s not found" name)
                        | _ -> failwith (sprintf "multiple methods called '%s' found" name)
                    | _ ->
                        gtd.Metadata.Methods.FindByNameAndArity(name, types.Length)
                        |> Array.find (fun md -> eqTypesAndILTypesWithInst types args md.ParameterTypes)
                gtd.MakeConstructorInfo (this, md)

            | _ -> notRequired "ContextTypeSymbol: GetConstructorImpl" this.Name

        override this.AssemblyQualifiedName = "[" + this.Assembly.FullName + "]" + this.FullName

        override this.GetMembers _bindingAttr = notRequired "GetMembers" this.Name
        override this.GetMethods _bindingAttr = notRequired "GetMethods" this.Name
        override this.GetField(_name, _bindingAttr) = notRequired "GetField" this.Name
        override this.GetFields _bindingAttr = notRequired "GetFields" this.Name
        override this.GetInterface(_name, _ignoreCase) = notRequired "GetInterface" this.Name
        override this.GetInterfaces() = notRequired "GetInterfaces" this.Name
        override this.GetEvent(_name, _bindingAttr) = notRequired "GetEvent" this.Name
        override this.GetEvents _bindingAttr = notRequired "GetEvents" this.Name
        override this.GetProperties _bindingAttr = notRequired "GetProperties" this.Name
        override this.GetPropertyImpl(_name, _bindingAttr, _binder, _returnType, _types, _modifiers) = notRequired "GetPropertyImpl" this.Name
        override this.GetNestedTypes _bindingAttr = notRequired "GetNestedTypes" this.Name
        override this.GetNestedType(_name, _bindingAttr) = notRequired "GetNestedType" this.Name
        override this.GetAttributeFlagsImpl() = notRequired "GetAttributeFlagsImpl" this.Name

        override this.UnderlyingSystemType = (this :> Type)

        override this.GetCustomAttributesData() =  ([| |] :> IList<_>)
        override this.MemberType = notRequired "MemberType" this.Name
        override this.GetMember(_name,_mt,_bindingAttr) = notRequired "GetMember" this.Name
        override this.GUID = notRequired "GUID" this.Name
        override this.InvokeMember(_name, _invokeAttr, _binder, _target, _args, _modifiers, _culture, _namedParameters) = notRequired "InvokeMember" this.Name
        override this.GetCustomAttributes(_inherit) = emptyAttributes
        override this.GetCustomAttributes(_attributeType, _inherit) = emptyAttributes
        override this.IsDefined(_attributeType, _inherit) = false
        override this.MakeArrayType() = ContextTypeSymbol(ContextTypeSymbolKind.SDArray, [| this |]) :> Type
        override this.MakeArrayType arg = ContextTypeSymbol(ContextTypeSymbolKind.Array arg, [| this |]) :> Type
        override this.MakePointerType() = ContextTypeSymbol(ContextTypeSymbolKind.Pointer, [| this |]) :> Type
        override this.MakeByRefType() = ContextTypeSymbol(ContextTypeSymbolKind.ByRef, [| this |]) :> Type

        override this.ToString() = this.FullName

    and ContextMethodSymbol(gmd: MethodInfo, gargs: Type[]) =
        inherit MethodInfo()

        override __.Attributes = gmd.Attributes
        override __.Name = gmd.Name
        override __.DeclaringType = gmd.DeclaringType
        override __.MemberType = gmd.MemberType

        override __.GetParameters() = gmd.GetParameters() |> Array.map (instParameterInfo (gmd.DeclaringType.GetGenericArguments(), gargs))
        override __.CallingConvention = gmd.CallingConvention
        override __.ReturnType = gmd.ReturnType |> instType (gmd.DeclaringType.GetGenericArguments(), gargs)
        override __.IsGenericMethod = true
        override __.GetGenericArguments() = gargs
        override __.MetadataToken = gmd.MetadataToken

        override __.GetCustomAttributesData() = gmd.GetCustomAttributesData()

        override __.GetHashCode() = gmd.GetHashCode()
        override this.Equals(that:obj) =
            match that with
            | :? MethodInfo as thatMI -> thatMI.IsGenericMethod && gmd.Equals(thatMI.GetGenericMethodDefinition()) && lengthsEqAndForall2 (gmd.GetGenericArguments()) (thatMI.GetGenericArguments()) (=)
            | _ -> false

        override this.MethodHandle = notRequired "MethodHandle" this.Name
        override this.ReturnParameter = notRequired "ReturnParameter" this.Name
        override this.IsDefined(_attributeType, _inherited) = notRequired "IsDefined" this.Name
        override this.ReturnTypeCustomAttributes = notRequired "ReturnTypeCustomAttributes" this.Name
        override this.GetBaseDefinition() = notRequired "GetBaseDefinition" this.Name
        override this.GetMethodImplementationFlags() = notRequired "GetMethodImplementationFlags" this.Name
        override this.Invoke(_obj, _invokeAttr, _binder, _parameters, _culture) = notRequired "Invoke" this.Name
        override this.ReflectedType = notRequired "ReflectedType" this.Name
        override this.GetCustomAttributes(_inherited) = emptyAttributes
        override this.GetCustomAttributes(_attributeType, _inherited) = emptyAttributes

        override __.ToString() = gmd.ToString() + "@inst"


    /// Clones namespaces, type providers, types and members provided by tp, renaming namespace nsp1 into namespace nsp2.

    /// Makes a type definition read from a binary available as a System.Type. Not all methods are implemented.
    and ContextTypeDefinition(ilGlobals: ILGlobals, tryBindAssembly : ILAssemblyRef -> Choice<ContextAssembly,exn>, asm: ContextAssembly, declTyOpt: Type option, inp: ILTypeDef) =
        inherit Type()

        // Note: For F# type providers we never need to view the custom attributes
        let rec txCustomAttributesArg ((ty,v): ILCustomAttrArg) =
            CustomAttributeTypedArgument(txILType ([| |], [| |]) ty, v)

        and txCustomAttributesDatum (inp: ILCustomAttr) =
             let args (* , namedArgs *) = decodeILCustomAttribData ilGlobals inp
             { new CustomAttributeData () with
                member __.Constructor =  txILConstructorRef inp.Method.MethodRef
                member __.ConstructorArguments = [| for arg in args -> txCustomAttributesArg arg |] :> IList<_>
                // Note, named arguments of custom attributes are not required by F# compiler on binding context elements.
                member __.NamedArguments = [| |] :> IList<_>
             }

        and txCustomAttributesData (inp: ILCustomAttrs) =
             [| for a in inp.Elements do
                  yield txCustomAttributesDatum a |]
             :> IList<CustomAttributeData>

        /// Makes a parameter definition read from a binary available as a ParameterInfo. Not all methods are implemented.
        and txILParameter gps (inp : ILParameter) =
            { new ParameterInfo() with

                override __.Name = uoptionToNull inp.Name
                override __.ParameterType = inp.ParameterType |> txILType gps
                override __.RawDefaultValue = (match inp.Default with None -> null | Some v -> convFieldInit v)
                override __.Attributes = inp.Attributes
                override __.GetCustomAttributesData() = inp.CustomAttrs  |> txCustomAttributesData

                override x.ToString() = sprintf "ctxt parameter %s" x.Name }

        /// Makes a method definition read from a binary available as a ConstructorInfo. Not all methods are implemented.
        and txILConstructorDef (declTy: Type) (inp: ILMethodDef) =
            let gps = if declTy.IsGenericType then declTy.GetGenericArguments() else [| |]
            { new ConstructorInfo() with

                override __.Name = ".ctor"
                override __.Attributes = inp.Attributes
                override __.MemberType = MemberTypes.Constructor
                override __.DeclaringType = declTy

                override __.GetParameters() = inp.Parameters |> Array.map (txILParameter (gps, [| |]))
                override __.GetCustomAttributesData() = inp.CustomAttrs |> txCustomAttributesData

                override __.GetHashCode() = hashILParameterTypes inp.Parameters
                override __.Equals(that:obj) =
                    match that with
                    | :? ConstructorInfo as that ->
                        eqType declTy that.DeclaringType &&
                        eqParametersAndILParameterTypesWithInst gps (that.GetParameters()) inp.Parameters
                    | _ -> false

                override this.IsDefined(attributeType, inherited) = notRequired "IsDefined"  this.Name
                override this.Invoke(invokeAttr, binder, parameters, culture) = notRequired "Invoke"  this.Name
                override this.Invoke(obj, invokeAttr, binder, parameters, culture) = notRequired "Invoke" this.Name
                override this.ReflectedType = notRequired "ReflectedType" this.Name
                override this.GetMethodImplementationFlags() = notRequired "GetMethodImplementationFlags" this.Name
                override this.MethodHandle = notRequired "MethodHandle" this.Name
                override this.GetCustomAttributes(inherited) = notRequired "GetCustomAttributes" this.Name
                override this.GetCustomAttributes(attributeType, inherited) = notRequired "GetCustomAttributes" this.Name

                override __.ToString() = sprintf "ctxt constructor(...) in type %s" declTy.FullName }

        /// Makes a method definition read from a binary available as a MethodInfo. Not all methods are implemented.
        and txILMethodDef (declTy: Type) (inp: ILMethodDef) =
            let gps = if declTy.IsGenericType then declTy.GetGenericArguments() else [| |]
            let rec gps2 = inp.GenericParams |> Array.mapi (fun i gp -> txILGenericParam (fun () -> gps, gps2) (i + gps.Length) gp)
            { new MethodInfo() with

                override __.Name = inp.Name
                override __.DeclaringType = declTy
                override __.MemberType = MemberTypes.Method
                override __.Attributes = inp.Attributes
                override __.GetParameters() = inp.Parameters |> Array.map (txILParameter (gps, gps2))
                override __.CallingConvention = CallingConventions.HasThis ||| CallingConventions.Standard // Provided types report this by default
                override __.ReturnType = inp.Return.Type |> txILType (gps, gps2)
                override __.GetCustomAttributesData() = inp.CustomAttrs |> txCustomAttributesData
                override __.GetGenericArguments() = gps2
                override __.IsGenericMethod = (gps2.Length <> 0)
                override __.IsGenericMethodDefinition = __.IsGenericMethod

                override __.GetHashCode() = hash inp.Name + hashILParameterTypes inp.Parameters
                override this.Equals(that:obj) =
                    match that with
                    | :? MethodInfo as thatMI ->
                        inp.Name = thatMI.Name &&
                        eqType this.DeclaringType thatMI.DeclaringType &&
                        eqParametersAndILParameterTypesWithInst gps (thatMI.GetParameters()) inp.Parameters
                    | _ -> false

                override this.MakeGenericMethod(args) = ContextMethodSymbol(this, args) :> MethodInfo

                override __.MetadataToken = inp.MetadataToken

                // unused
                override this.MethodHandle = notRequired "MethodHandle" this.Name
                override this.ReturnParameter = notRequired "ReturnParameter" this.Name
                override this.IsDefined(attributeType, inherited) = notRequired "IsDefined" this.Name
                override this.ReturnTypeCustomAttributes = notRequired "ReturnTypeCustomAttributes" this.Name
                override this.GetBaseDefinition() = notRequired "GetBaseDefinition" this.Name
                override this.GetMethodImplementationFlags() = notRequired "GetMethodImplementationFlags" this.Name
                override this.Invoke(obj, invokeAttr, binder, parameters, culture) = notRequired "Invoke" this.Name
                override this.ReflectedType = notRequired "ReflectedType" this.Name
                override this.GetCustomAttributes(inherited) = notRequired "GetCustomAttributes" this.Name
                override this.GetCustomAttributes(attributeType, inherited) = notRequired "GetCustomAttributes" this.Name

                override __.ToString() = sprintf "ctxt method %s(...) in type %s" inp.Name declTy.FullName  }

        /// Makes a property definition read from a binary available as a PropertyInfo. Not all methods are implemented.
        and txPropertyDefinition declTy gps (inp: ILPropertyDef) =
            { new PropertyInfo() with

                override __.Name = inp.Name
                override __.Attributes = inp.Attributes
                override __.MemberType = MemberTypes.Property
                override __.DeclaringType = declTy

                override __.PropertyType = inp.PropertyType |> txILType (gps, [| |])
                override __.GetGetMethod(_nonPublic) = inp.GetMethod |> Option.map txILMethodRef |> optionToNull
                override __.GetSetMethod(_nonPublic) = inp.SetMethod |> Option.map txILMethodRef |> optionToNull
                override __.GetIndexParameters() = inp.IndexParameters |> Array.map (txILParameter (gps, [| |]))
                override __.CanRead = inp.GetMethod.IsSome
                override __.CanWrite = inp.SetMethod.IsSome
                override __.GetCustomAttributesData() = inp.CustomAttrs |> txCustomAttributesData

                override this.GetHashCode() = hash inp.Name
                override this.Equals(that:obj) =
                    match that with
                    | :? PropertyInfo as thatPI ->
                        inp.Name = thatPI.Name  &&
                        eqType this.DeclaringType thatPI.DeclaringType
                    | _ -> false

                override this.GetValue(obj, invokeAttr, binder, index, culture) = notRequired "GetValue" this.Name
                override this.SetValue(obj, _value, invokeAttr, binder, index, culture) = notRequired "SetValue" this.Name
                override this.GetAccessors(nonPublic) = notRequired "GetAccessors" this.Name
                override this.ReflectedType = notRequired "ReflectedType" this.Name
                override this.GetCustomAttributes(inherited) = notRequired "GetCustomAttributes" this.Name
                override this.GetCustomAttributes(attributeType, inherited) = notRequired "GetCustomAttributes" this.Name
                override this.IsDefined(attributeType, inherited) = notRequired "IsDefined" this.Name

                override __.ToString() = sprintf "ctxt property %s(...) in type %s" inp.Name declTy.Name }

        /// Make an event definition read from a binary available as an EventInfo. Not all methods are implemented.
        and txEventDefinition declTy gps (inp: ILEventDef) =
            { new EventInfo() with

                override __.Name = inp.Name
                override __.Attributes = inp.Attributes
                override __.MemberType = MemberTypes.Event
                override __.DeclaringType = declTy

                override __.EventHandlerType = inp.EventHandlerType |> txILType (gps, [| |])
                override __.GetAddMethod(_nonPublic) = inp.AddMethod |> txILMethodRef
                override __.GetRemoveMethod(_nonPublic) = inp.RemoveMethod |> txILMethodRef
                override __.GetCustomAttributesData() = inp.CustomAttrs |> txCustomAttributesData

                override __.GetHashCode() = hash inp.Name
                override this.Equals(that:obj) =
                    match that with
                    | :? EventInfo as thatEI ->
                        inp.Name = thatEI.Name  &&
                        eqType this.DeclaringType thatEI.DeclaringType
                    | _ -> false

                override this.GetRaiseMethod(nonPublic) = notRequired "GetRaiseMethod" this.Name
                override this.ReflectedType = notRequired "ReflectedType" this.Name
                override this.GetCustomAttributes(inherited) = notRequired "GetCustomAttributes" this.Name
                override this.GetCustomAttributes(attributeType, inherited) = notRequired "GetCustomAttributes" this.Name
                override this.IsDefined(attributeType, inherited) = notRequired "IsDefined" this.Name

                override __.ToString() = sprintf "ctxt event %s(...) in type %s" inp.Name declTy.FullName }

        /// Makes a field definition read from a binary available as a FieldInfo. Not all methods are implemented.
        and txFieldDefinition declTy gps (inp: ILFieldDef) =
            { new FieldInfo() with

                override __.Name = inp.Name
                override __.Attributes = FieldAttributes.Static ||| FieldAttributes.Literal ||| FieldAttributes.Public
                override __.MemberType = MemberTypes.Field
                override __.DeclaringType = declTy

                override __.FieldType = inp.FieldType |> txILType (gps, [| |])
                override __.GetRawConstantValue() = match inp.LiteralValue with None -> null | Some v -> convFieldInit v
                override __.GetCustomAttributesData() = inp.CustomAttrs |> txCustomAttributesData

                override __.GetHashCode() = hash inp.Name
                override this.Equals(that:obj) =
                    match that with
                    | :? EventInfo as thatFI ->
                        inp.Name = thatFI.Name  &&
                        eqType this.DeclaringType thatFI.DeclaringType
                    | _ -> false

                override this.ReflectedType = notRequired "ReflectedType" this.Name
                override this.GetCustomAttributes(inherited) = notRequired "GetCustomAttributes" this.Name
                override this.GetCustomAttributes(attributeType, inherited) = notRequired "GetCustomAttributes" this.Name
                override this.IsDefined(attributeType, inherited) = notRequired "IsDefined" this.Name
                override this.SetValue(obj, _value, invokeAttr, binder, culture) = notRequired "SetValue" this.Name
                override this.GetValue(obj) = notRequired "GetValue" this.Name
                override this.FieldHandle = notRequired "FieldHandle" this.Name

                override __.ToString() = sprintf "ctxt literal field %s(...) in type %s" inp.Name declTy.FullName }

        /// Bind a reference to an assembly
        and txScopeRef(sref: ILScopeRef) =
            match sref with
            | ILScopeRef.Assembly aref -> match tryBindAssembly aref with Choice1Of2 asm -> asm | Choice2Of2 exn -> raise exn
            | ILScopeRef.Local -> asm
            | ILScopeRef.Module _ -> asm

        /// Bind a reference to a type
        and txILTypeRef(tref: ILTypeRef) : Type =
            match tref.Scope with
            | ILTypeRefScope.Top scoref -> txScopeRef(scoref).BindType(tref.Namespace, tref.Name)
            | ILTypeRefScope.Nested tref -> txILTypeRef(tref).GetNestedType(tref.Name,BindingFlags.Public ||| BindingFlags.NonPublic)

        /// Bind a reference to a constructor
        and txILConstructorRef(mref: ILMethodRef) =
            let argTypes = Array.map (txILType ([| |], [| |])) mref.ArgTypes
            let declTy = txILTypeRef(mref.EnclosingTypeRef)
            let cons = declTy.GetConstructor(BindingFlags.Public ||| BindingFlags.NonPublic, null, argTypes, null)
            if isNull cons then failwith (sprintf "constructor reference '%A' not resolved" mref)
            cons

        /// Bind a reference to a metehod
        and txILMethodRef(mref: ILMethodRef) =
            let argTypes = mref.ArgTypes |> Array.map (txILType ([| |], [| |]))
            let declTy = mref.EnclosingTypeRef |> txILTypeRef
            let meth = declTy.GetMethod(mref.Name, BindingFlags.Public ||| BindingFlags.NonPublic, null, argTypes, null)
            if isNull meth then failwith (sprintf "method reference '%A' not resolved" mref)
            meth

        /// Convert an ILType read from a binary to a System.Type backed by ContextTypeDefinitions
        and txILType gps (ty: ILType) =

            match ty with
            | ILType.Void -> typeof<System.Void>
            | ILType.Value tspec
            | ILType.Boxed tspec ->
                let tdefR = txILTypeRef tspec.TypeRef
                match tspec.GenericArgs with
                | [| |] -> tdefR
                | args -> tdefR.MakeGenericType(Array.map (txILType gps) args)
            | ILType.Array(rank, arg) ->
                let argR = txILType gps arg
                if rank.Rank = 1 then argR.MakeArrayType()
                else argR.MakeArrayType(rank.Rank)
            | ILType.FunctionPointer _  -> failwith "unexpected function type"
            | ILType.Ptr(arg) -> (txILType gps arg).MakePointerType()
            | ILType.Byref(arg) -> (txILType gps arg).MakeByRefType()
            | ILType.Modified(_,_mod,arg) -> txILType gps arg
            | ILType.Var(n) ->
                let (gps1:Type[]),(gps2:Type[]) = gps
                if n < gps1.Length then gps1.[n]
                elif n < gps1.Length + gps2.Length then gps2.[n - gps1.Length]
                else failwith (sprintf "generic parameter index our of range: %d" n)

        /// Convert an ILGenericParameterDef read from a binary to a System.Type.
        and txILGenericParam gpsf pos (inp: ILGenericParameterDef) =
            { new Type() with
                override __.Name = inp.Name
                override __.Assembly = (asm :> Assembly)
                override __.FullName = inp.Name
                override __.IsGenericParameter = true
                override __.GenericParameterPosition = pos
                override __.GetGenericParameterConstraints() = inp.Constraints |> Array.map (txILType (gpsf()))

                override __.MemberType = enum 0

                override __.Namespace = null //notRequired "Namespace"
                override this.DeclaringType = notRequired "DeclaringType" this.Name
                override this.BaseType = notRequired "BaseType" this.Name
                override this.GetInterfaces() = notRequired "GetInterfaces" this.Name

                override this.GetConstructors(_bindingFlags) = notRequired "GetConstructors" this.Name
                override this.GetMethods(_bindingFlags) = notRequired "GetMethods" this.Name
                override this.GetField(name, _bindingFlags) = notRequired "GetField" this.Name
                override this.GetFields(_bindingFlags) = notRequired "GetFields" this.Name
                override this.GetEvent(name, _bindingFlags) = notRequired "GetEvent" this.Name
                override this.GetEvents(_bindingFlags) = notRequired "GetEvents" this.Name
                override this.GetProperties(_bindingFlags) = notRequired "GetProperties" this.Name
                override this.GetMembers(_bindingFlags) = notRequired "GetMembers" this.Name
                override this.GetNestedTypes(_bindingFlags) = notRequired "GetNestedTypes" this.Name
                override this.GetNestedType(name, _bindingFlags) = notRequired "GetNestedType" this.Name
                override this.GetPropertyImpl(name, _bindingFlags, _binder, _returnType, _types, _modifiers) = notRequired "GetPropertyImpl" this.Name
                override this.MakeGenericType(args) = notRequired "MakeGenericType" this.Name
                override this.MakeArrayType() = ContextTypeSymbol(ContextTypeSymbolKind.SDArray, [| this |]) :> Type
                override this.MakeArrayType arg = ContextTypeSymbol(ContextTypeSymbolKind.Array arg, [| this |]) :> Type
                override this.MakePointerType() = ContextTypeSymbol(ContextTypeSymbolKind.Pointer, [| this |]) :> Type
                override this.MakeByRefType() = ContextTypeSymbol(ContextTypeSymbolKind.ByRef, [| this |]) :> Type

                override __.GetAttributeFlagsImpl() = TypeAttributes.Public ||| TypeAttributes.Class ||| TypeAttributes.Sealed

                override __.IsArrayImpl() = false
                override __.IsByRefImpl() = false
                override __.IsPointerImpl() = false
                override __.IsPrimitiveImpl() = false
                override __.IsCOMObjectImpl() = false
                override __.IsGenericType = false
                override __.IsGenericTypeDefinition = false

                override __.HasElementTypeImpl() = false

                override this.UnderlyingSystemType = this
                override __.GetCustomAttributesData() = inp.CustomAttrs |> txCustomAttributesData

                override this.Equals(that:obj) = System.Object.ReferenceEquals (this, that)

                override __.ToString() = sprintf "ctxt generic param %s" inp.Name

                override this.AssemblyQualifiedName = "[" + this.Assembly.FullName + "]" + this.FullName

                override this.GetGenericArguments() = notRequired "GetGenericArguments" this.Name
                override this.GetGenericTypeDefinition() = notRequired "GetGenericTypeDefinition" this.Name
                override this.GetMember(name,mt,_bindingFlags) = notRequired "txILGenericParam: GetMember" this.Name
                override this.GUID = notRequired "txILGenericParam: GUID" this.Name
                override this.GetMethodImpl(name, _bindingFlags, binder, callConvention, types, modifiers) = notRequired "txILGenericParam: GetMethodImpl" this.Name
                override this.GetConstructorImpl(_bindingFlags, binder, callConvention, types, modifiers) = notRequired "txILGenericParam: GetConstructorImpl" this.Name
                override this.GetCustomAttributes(inherited) = notRequired "txILGenericParam: GetCustomAttributes" this.Name
                override this.GetCustomAttributes(attributeType, inherited) = notRequired "txILGenericParam: GetCustomAttributes" this.Name
                override this.IsDefined(attributeType, inherited) = notRequired "txILGenericParam: IsDefined" this.Name
                override this.GetInterface(name, ignoreCase) = notRequired "txILGenericParam: GetInterface" this.Name
                override this.Module = notRequired "txILGenericParam: Module" this.Name : Module 
                override this.GetElementType() = notRequired "txILGenericParam: GetElementType" this.Name
                override this.InvokeMember(name, invokeAttr, binder, target, args, modifiers, culture, namedParameters) = notRequired "txILGenericParam: InvokeMember" this.Name

            }

        let rec gps = inp.GenericParams |> Array.mapi (fun i gp -> txILGenericParam (fun () -> gps, [| |]) i gp)

        let isNested = declTyOpt.IsSome

        override __.Name = inp.Name
        override __.Assembly = (asm :> Assembly)
        override __.DeclaringType = declTyOpt |> optionToNull
        override __.MemberType = if isNested then MemberTypes.NestedType else MemberTypes.TypeInfo

        override __.FullName =
            match declTyOpt with
            | None ->
                match inp.Namespace with
                | UNone -> inp.Name
                | USome nsp -> nsp + "." + inp.Name
            | Some declTy ->
                declTy.FullName + "+" + inp.Name

        override __.Namespace = inp.Namespace |> uoptionToNull
        override __.BaseType = inp.Extends |> Option.map (txILType (gps, [| |])) |> optionToNull
        override __.GetInterfaces() = inp.Implements |> Array.map (txILType (gps, [| |]))

        override this.GetConstructors(_bindingFlags) =
            inp.Methods.Elements
            |> Array.filter (fun x -> x.Name = ".ctor" || x.Name = ".cctor")
            |> Array.map (txILConstructorDef this)

        override this.GetMethods(_bindingFlags) =
            inp.Methods.Elements |> Array.map (txILMethodDef this)

        override this.GetField(name, _bindingFlags) =
            inp.Fields.Elements
            |> Array.tryPick (fun p -> if p.Name = name then Some (txFieldDefinition this gps p) else None)
            |> optionToNull

        override this.GetFields(_bindingFlags) =
            inp.Fields.Elements
            |> Array.map (txFieldDefinition this gps)

        override this.GetEvent(name, _bindingFlags) =
            inp.Events.Elements
            |> Array.tryPick (fun ev -> if ev.Name = name then Some (txEventDefinition this gps ev) else None)
            |> optionToNull

        override this.GetEvents(_bindingFlags) =
            inp.Events.Elements
            |> Array.map (txEventDefinition this gps)

        override this.GetProperties(_bindingFlags) =
            inp.Properties.Elements
            |> Array.map (txPropertyDefinition this gps)

        override this.GetMembers(_bindingFlags) =
            [| for x in this.GetMethods() do yield (x :> MemberInfo)
               for x in this.GetFields() do yield (x :> MemberInfo)
               for x in this.GetProperties() do yield (x :> MemberInfo)
               for x in this.GetEvents() do yield (x :> MemberInfo)
               for x in this.GetNestedTypes() do yield (x :> MemberInfo) |]

        override this.GetNestedTypes(_bindingFlags) =
            inp.NestedTypes.Elements
            |> Array.map (asm.txILTypeDef (Some (this :> Type)))

        // GetNestedType is used for linking to the binding context
        override this.GetNestedType(name, _bindingFlags) =
            inp.NestedTypes.TryFindByName(UNone, name) |> Option.map (asm.txILTypeDef (Some (this :> Type))) |> optionToNull

        override this.GetPropertyImpl(name, _bindingFlags, _binder, _returnType, _types, _modifiers) =
            inp.Properties.Elements
            |> Array.tryPick (fun p -> if p.Name = name then Some (txPropertyDefinition this gps p) else None)
            |> optionToNull

        override this.GetMethodImpl(name, _bindingFlags, _binder, _callConvention, types, _modifiers)          =
            inp.Methods.FindByNameAndArity(name, types.Length)
            |> Array.find (fun md -> eqTypesAndILTypes types md.ParameterTypes)
            |> txILMethodDef this

        override this.GetConstructorImpl(_bindingFlags, _binder, _callConvention, types, _modifiers)          =
            inp.Methods.FindByNameAndArity(".ctor", types.Length)
            |> Array.find (fun md -> eqTypesAndILTypes types md.ParameterTypes)
            |> txILConstructorDef this

        // Every implementation of System.Type must meaningfully implement these
        override this.MakeGenericType(args) = ContextTypeSymbol(ContextTypeSymbolKind.Generic this, args) :> Type
        override this.MakeArrayType() = ContextTypeSymbol(ContextTypeSymbolKind.SDArray, [| this |]) :> Type
        override this.MakeArrayType arg = ContextTypeSymbol(ContextTypeSymbolKind.Array arg, [| this |]) :> Type
        override this.MakePointerType() = ContextTypeSymbol(ContextTypeSymbolKind.Pointer, [| this |]) :> Type
        override this.MakeByRefType() = ContextTypeSymbol(ContextTypeSymbolKind.ByRef, [| this |]) :> Type

        override __.GetAttributeFlagsImpl() =
            let attr = TypeAttributes.Public ||| TypeAttributes.Class
            let attr = if inp.IsSealed then attr ||| TypeAttributes.Sealed else attr
            let attr = if inp.IsInterface then attr ||| TypeAttributes.Interface else attr
            let attr = if inp.IsSerializable then attr ||| TypeAttributes.Serializable else attr
            if isNested then adjustTypeAttributes isNested attr else attr

        override __.IsValueTypeImpl() = inp.IsStructOrEnum
        override __.IsArrayImpl() = false
        override __.IsByRefImpl() = false
        override __.IsPointerImpl() = false
        override __.IsPrimitiveImpl() = false
        override __.IsCOMObjectImpl() = false
        override __.IsGenericType = (gps.Length <> 0)
        override __.IsGenericTypeDefinition = (gps.Length <> 0)
        override __.HasElementTypeImpl() = false

        override this.UnderlyingSystemType = (this :> Type)
        override __.GetCustomAttributesData() = inp.CustomAttrs |> txCustomAttributesData

        override this.Equals(that:obj) = System.Object.ReferenceEquals (this, that)
        override this.GetHashCode() =  hash (inp.Namespace, inp.Name)

        override this.IsAssignableFrom(otherTy) = base.IsAssignableFrom(otherTy) || this.Equals(otherTy)
        override this.IsSubclassOf(otherTy) = base.IsSubclassOf(otherTy) || inp.IsDelegate && otherTy = typeof<Delegate> // F# quotations implementation

        override this.AssemblyQualifiedName = "[" + this.Assembly.FullName + "]" + this.FullName

        override this.ToString() = sprintf "ctxt type %s" this.FullName

        override __.GetGenericArguments() = gps
        override __.GetGenericTypeDefinition() = notRequired "GetGenericTypeDefinition" inp.Name
        override __.GetMember(_name, _memberType, _bindingFlags) = notRequired "txILTypeDef: GetMember" inp.Name
        override __.GUID = notRequired "txILTypeDef: GUID" inp.Name
        override __.GetCustomAttributes(_inherited) = notRequired "txILTypeDef: GetCustomAttributes" inp.Name
        override __.GetCustomAttributes(_attributeType, _inherited) = notRequired "txILTypeDef: GetCustomAttributes" inp.Name
        override __.IsDefined(_attributeType, _inherited) = notRequired "txILTypeDef: IsDefined" inp.Name
        override __.GetInterface(_name, _ignoreCase) = notRequired "txILTypeDef: GetInterface" inp.Name
        override __.Module = notRequired "txILTypeDef: Module"  inp.Name : Module
        override __.GetElementType() = notRequired "txILTypeDef: GetElementType" inp.Name
        override __.InvokeMember(_name, _invokeAttr, _binder, _target, _args, _modifiers, _culture, _namedParameters) = notRequired "txILTypeDef: InvokeMember" inp.Name

        member x.Metadata: ILTypeDef = inp
        member x.MakeMethodInfo (declTy,md) = txILMethodDef declTy md
        member x.MakeConstructorInfo (declTy,md) = txILConstructorDef declTy md


    and ContextAssembly(ilGlobals, tryBindAssembly: ILAssemblyRef -> Choice<ContextAssembly,exn>, reader: ILModuleReader, location: string) as asm =
        inherit Assembly()

        // A table tracking how type definition objects are translated.
        let txTable = TxTable<Type>()

        member __.txILTypeDef (declTyOpt: Type option) (inp: ILTypeDef) =
            txTable.Get inp.Token (fun () -> ContextTypeDefinition(ilGlobals, tryBindAssembly, asm, declTyOpt, inp) :> System.Type)

        override x.GetTypes () = [| for td in reader.ILModuleDef.TypeDefs.Elements -> x.txILTypeDef None td  |]
        override x.Location = location

        override x.GetType (nm:string) =
            if nm.Contains("+") then
                let i = nm.LastIndexOf("+")
                let enc,nm2 = nm.[0..i-1], nm.[i+1..]
                match x.GetType(enc) with
                | null -> null
                | t -> t.GetNestedType(nm2,BindingFlags.Public ||| BindingFlags.NonPublic)
            elif nm.Contains(".") then
                let i = nm.LastIndexOf(".")
                let nsp,nm2 = nm.[0..i-1], nm.[i+1..]
                x.TryBindType(USome nsp, nm2) |> optionToNull
            else
                x.TryBindType(UNone, nm) |> optionToNull

        override x.GetName () = reader.ILModuleDef.ManifestOfAssembly.GetName()

        override x.FullName = x.GetName().ToString()

        override x.ReflectionOnly = true

        override x.GetManifestResourceStream(resourceName:string) =
            let r = reader.ILModuleDef.Resources.Elements |> Seq.find (fun r -> r.Name = resourceName)
            match r.Location with
            | ILResourceLocation.Local f -> new MemoryStream(f()) :> Stream
            | _ -> notRequired "reading manifest resource %s from non-embedded location" resourceName

        member x.BindType(nsp:string StructOption, nm:string) =
            match x.TryBindType(nsp, nm) with
            | None -> failwithf "failed to bind type %s in assembly %s" nm asm.FullName
            | Some res -> res

        member x.TryBindType(nsp:string StructOption, nm:string) : Type option =
            match reader.ILModuleDef.TypeDefs.TryFindByName(nsp, nm) with
            | Some td -> asm.txILTypeDef None td |> Some
            | None ->
            match reader.ILModuleDef.ManifestOfAssembly.ExportedTypes.TryFindByName(nsp, nm) with
            | Some tref ->
                match tref.ScopeRef with
                | ILScopeRef.Assembly aref2 ->
                    let ass2opt = tryBindAssembly(aref2)
                    match ass2opt with
                    | Choice1Of2 ass2 -> ass2.TryBindType(nsp, nm)
                    | Choice2Of2 _err -> None
                | _ ->
                    printfn "unexpected non-forwarder during binding"
                    None
            | None -> None

        override x.ToString() = "ctxt assembly " + x.FullName


//====================================================================================================
// ProvidedTypeContext
//
// A binding context for cross-targeting type providers

namespace ProviderImplementation.ProvidedTypes


    #nowarn "8796"
    open System
    open System.Diagnostics
    open System.IO
    open System.Collections.Concurrent
    open System.Collections.Generic
    open System.Reflection

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Quotations.ExprShape
    open Microsoft.FSharp.Core.CompilerServices
    open Microsoft.FSharp.Reflection

    open ProviderImplementation.ProvidedTypes
    open ProviderImplementation.ProvidedTypes.AssemblyReader
    open ProviderImplementation.ProvidedTypes.AssemblyReader.Reader
    open ProviderImplementation.ProvidedTypes.AssemblyReaderReflection
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
               p |> isNull |> not

           member x.HasField(nm) =
               let ty = x.GetType()
               let fld = ty.GetField(nm, BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
               fld |> isNull |> not

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
    type private AssemblyReplacer(designTimeAssemblies: Lazy<Assembly[]>, referencedAssemblies: Lazy<Assembly[]>, ?assemblyReplacementMap) =

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
                |> Seq.forall (fun (originalName:string, newName:string) ->
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
          if isNull newP then
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
          if isNull newF then failwithf "Field '%O' of type '%O' not found" f t
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
                if isNull genericMethodT then null else
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

      /// Gets the equivalent target type
      member __.ConvertDesignTimeTypeToTargetType t = t |> replaceType true

      /// Gets an equivalent expression with all the types replaced with runtime equivalents
      member __.ConvertDesignTimeExprToTargetExpr e = e |> replaceExpr true

      /// Gets an equivalent expression with all the types replaced with designTime equivalents
      member __.ConvertTargetExprToDesignTimeExpr e = e |> replaceExpr false



    /// Represents the type binding context for the type provider based on the set of assemblies
    /// referenced by the compilation.
    type ProvidedTypesContext(referencedAssemblyPaths : string list, isForGenerated: bool, assemblyReplacementMap : seq<string*string>) as this =

        /// Find which assembly defines System.Object etc.
        let systemRuntimeScopeRef =
          lazy
            referencedAssemblyPaths |> List.tryPick (fun path ->
              try
                let simpleName = Path.GetFileNameWithoutExtension path
                if simpleName = "mscorlib" || simpleName = "System.Runtime" || simpleName = "netstandard" then
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
               | None -> EcmaMscorlibScopeRef // failwith "no reference to mscorlib.dll or System.Runtime.dll or netstandard.dll found"
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
                                    Choice1Of2(ContextAssembly(ilGlobals.Force(), this.TryBindILAssemblyRef, reader, ref))
                                with err -> Choice2Of2 err) |])
        let readersTable =  lazy ([| for (ref, asm) in readers.Force() do let simpleName = Path.GetFileNameWithoutExtension ref in yield simpleName, asm |] |> Map.ofArray)
        let referencedAssemblies = lazy ([| for (_,asm) in readers.Force() do match asm.Force() with Choice2Of2 _ -> () | Choice1Of2 asm -> yield asm :> Assembly |])

        let tryBindAssemblySimple(simpleName:string) : Choice<ContextAssembly, exn> =
            if readersTable.Force().ContainsKey(simpleName) then readersTable.Force().[simpleName].Force()
            else Choice2Of2 (Exception(sprintf "assembly %s not found" simpleName))

        let designTimeAssemblies =
            lazy
              [| yield Assembly.GetExecutingAssembly()
                 for asm in Assembly.GetExecutingAssembly().GetReferencedAssemblies() do
                    let asm = try Assembly.Load(asm) with _ -> null
                    if not (isNull asm) then
                        yield asm |]

        let replacer = AssemblyReplacer (designTimeAssemblies, referencedAssemblies, assemblyReplacementMap)

        let convToTgt ty = 
            // TODO: cross-targeting generative type providers. Setting isForGenerated=false disables all cross-targeting rewriting
            if isForGenerated then ty 
            else replacer.ConvertDesignTimeTypeToTargetType ty

        let ptb = ZProvidedTypeBuilder(convToTgt)

        let convCode (code: Expr list -> Expr) = 
            // TODO: cross-targeting generative type providers
            if isForGenerated then code 
            else (fun args -> args |> List.map replacer.ConvertTargetExprToDesignTimeExpr |> code |> replacer.ConvertDesignTimeExprToTargetExpr)

        member internal __.TryBindILAssemblyRef(aref: ILAssemblyRef) : Choice<ContextAssembly, exn> = tryBindAssemblySimple(aref.Name)

        member internal __.TryBindAssemblyName(aref: AssemblyName) : Choice<ContextAssembly, exn> = tryBindAssemblySimple(aref.Name)

        member __.TryBindAssembly(aref: AssemblyName) =
            match tryBindAssemblySimple(aref.Name) with 
            | Choice1Of2 asm -> Choice1Of2(asm :> Assembly)
            | Choice2Of2 exn ->  Choice2Of2 exn

        member __.TryBindAssemblyBySimpleName(assemblyName: string) =
            match tryBindAssemblySimple(assemblyName) with 
            | Choice1Of2 asm -> Choice1Of2(asm :> Assembly)
            | Choice2Of2 exn ->  Choice2Of2 exn


        member __.ReferencedAssemblyPaths = referencedAssemblyPaths

        member __.ReferencedAssemblies =  referencedAssemblies.Force()

        member x.FSharpCoreAssemblyVersion = fsharpCoreRefVersion.Force()

        /// Create a new provided static parameter, for use with DefineStaticParamaeters on a provided type definition.
        ///
        /// When making a cross-targeting type provider, use this method instead of the ProvidedParameter constructor from ProvidedTypes
        member __.ProvidedStaticParameter(parameterName, parameterType, ?parameterDefaultValue) =
          ProvidedStaticParameter(parameterName, parameterType, ?parameterDefaultValue=parameterDefaultValue)

        /// Create a new provided field. It is not initially associated with any specific provided type definition.
        ///
        /// When making a cross-targeting type provider, use this method instead of the ProvidedProperty constructor from ProvidedTypes
        member __.ProvidedField(fieldName, fieldType) =
          ProvidedField(fieldName, fieldType |> convToTgt)

        /// Create a new provided literal field. It is not initially associated with any specific provided type definition.
        ///
        /// When making a cross-targeting type provider, use this method instead of the ProvidedProperty constructor from ProvidedTypes
        member __.ProvidedLiteralField(fieldName, fieldType, literalValue:obj) =
          ProvidedLiteralField(fieldName, fieldType |> convToTgt, literalValue)

        /// Create a new provided parameter.
        ///
        /// When making a cross-targeting type provider, use this method instead of the ProvidedProperty constructor from ProvidedTypes
        member __.ProvidedParameter(parameterName, parameterType, ?isOut, ?optionalValue) =
          ProvidedParameter(parameterName, 
                            parameterType |> convToTgt, 
                            isOut = defaultArg isOut false, 
                            optionalValue = optionalValue)


        /// Create a new provided getter/setter property. It is not initially associated with any specific provided type definition.
        ///
        /// When making a cross-targeting type provider, use this method instead of the ProvidedProperty constructor from ProvidedTypes
        member __.ProvidedProperty(propertyName, propertyType, ?isStatic: bool, ?getterCode: Expr list -> Expr, ?setterCode: Expr list -> Expr, ?parameters) =
          ProvidedProperty(propertyName, 
                           propertyType = (propertyType |> convToTgt),
                           isStatic = defaultArg isStatic false,
                           getterCode = (getterCode  |> Option.map convCode),
                           setterCode = (setterCode |> Option.map convCode), 
                           parameters = defaultArg parameters [])

        /// Create a new provided event. It is not initially associated with any specific provided type definition.
        ///
        /// When making a cross-targeting type provider, use this method instead of the ProvidedProperty constructor from ProvidedTypes
        member __.ProvidedEvent(eventName, eventHandlerType, ?isStatic: bool, ?adderCode: Expr list -> Expr, ?removerCode: Expr list -> Expr) =
          ProvidedEvent(eventName, 
                        eventHandlerType = (eventHandlerType |> convToTgt),
                        isStatic = defaultArg isStatic false,
                        adderCode = (adderCode  |> Option.map convCode),
                        removerCode = (removerCode  |> Option.map convCode))

        /// When making a cross-targeting type provider, use this method instead of the ProvidedConstructor constructor from ProvidedTypes
        member __.ProvidedConstructor(parameters, ?invokeCode: Expr list -> Expr) =
          ProvidedConstructor(parameters, 
                              invokeCode = (invokeCode |> Option.map convCode))

        /// When making a cross-targeting type provider, use this method instead of the ProvidedMethod constructor from ProvidedTypes
        member __.ProvidedMethod(methodName, parameters, returnType: Type, ?isStatic: bool, ?invokeCode: Expr list -> Expr) =
          ProvidedMethod(methodName, parameters, 
                         returnType = (returnType |> convToTgt), 
                         isStatic = defaultArg isStatic false, 
                         invokeCode = (invokeCode |> Option.map convCode))

        /// When making a cross-targeting type provider, use this method instead of the corresponding ProvidedTypeDefinition constructor from ProvidedTypes
        member __.ProvidedTypeDefinition(className, baseType: Type option, ?hideObjectMethods: bool, ?nonNullable: bool, ?isErased: bool) =
          let isErased = defaultArg isErased true
          let nonNullable = defaultArg nonNullable false
          let hideObjectMethods = defaultArg hideObjectMethods false
          ProvidedTypeDefinition(className, 
                                 baseType |> Option.map convToTgt, 
                                 convToTgt, 
                                 HideObjectMethods=hideObjectMethods, 
                                 NonNullable=nonNullable, 
                                 IsErased=isErased)

        /// When making a cross-targeting type provider, use this method instead of the corresponding ProvidedTypeDefinition constructor from ProvidedTypes
        member __.ProvidedTypeDefinition(assembly, namespaceName, className, baseType: Type option, ?hideObjectMethods: bool, ?nonNullable: bool, ?isErased: bool) =
          let isErased = defaultArg isErased true
          let nonNullable = defaultArg nonNullable false
          let hideObjectMethods = defaultArg hideObjectMethods false
          ProvidedTypeDefinition(assembly, namespaceName, className, 
                                 baseType |> Option.map convToTgt, 
                                 convToTgt, 
                                 HideObjectMethods=hideObjectMethods, 
                                 NonNullable=nonNullable, 
                                 IsErased=isErased)

        /// When making a cross-targeting type provider, use this method instead of ProvidedTypeBuilder.MakeGenericType
        member __.MakeGenericType(genericTypeDefinition, genericArguments) = ptb.MakeGenericType(genericTypeDefinition, genericArguments)

        /// When making a cross-targeting type provider, use this method instead of ProvidedTypeBuilder.MakeGenericMethod
        member __.MakeGenericMethod(genericMethodDefinition, genericArguments) = ptb.MakeGenericMethod(genericMethodDefinition, genericArguments)

        static member Create (cfg : TypeProviderConfig, isForGenerated: bool, ?assemblyReplacementMap) =

            // Use the reflection hack to determine the set of referenced assemblies by reflecting over the SystemRuntimeContainsType
            // closure in the TypeProviderConfig object.
            let referencedAssemblyPaths =
              try
                if isNull (cfg.GetType().GetField("systemRuntimeContainsType",BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)) then
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


            ProvidedTypesContext(referencedAssemblyPaths, isForGenerated, defaultArg assemblyReplacementMap Seq.empty)


//====================================================================================================
// ProvidedAssembly - the assembly compiler for generative type providers


namespace ProviderImplementation.ProvidedTypes

    open System
    open System.Diagnostics
    open System.IO
    open System.Collections.Concurrent
    open System.Collections.Generic
    open System.Reflection
    open System.Reflection.Emit

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.ExprShape
    open Microsoft.FSharp.Core.CompilerServices
    open Microsoft.FSharp.Reflection

    open ProviderImplementation.ProvidedTypes
    open ProviderImplementation.ProvidedTypes.AssemblyReader
    open ProviderImplementation.ProvidedTypes.AssemblyReaderReflection
    open ProviderImplementation.ProvidedTypes.UncheckedQuotations

    #if !NO_GENERATIVE

    type (*internal*) ExpectedStackState =
        | Empty = 1
        | Address = 2
        | Value = 3

    type CodeGenerator(assemblyMainModule: ModuleBuilder, uniqueLambdaTypeName,
                       implicitCtorArgsAsFields: FieldBuilder list,
                       transType: Type -> Type,
                       transField: FieldInfo -> FieldInfo,
                       transMethod: MethodInfo -> MethodInfo,
                       transCtor: ConstructorInfo -> ConstructorInfo,
                       isLiteralEnumField: FieldInfo -> bool,
                       ilg: ILGenerator, locals:Dictionary<Quotations.Var,LocalBuilder>, parameterVars) =

        // TypeBuilderInstantiation should really be a public type, since we
        // have to use alternative means for various Method/Field/Constructor lookups on this kind of type.
        // Also, on Mono 3.x and 4.x this type had a different name.
        let theTypeBuilderInstantiationType =
            let runningOnMono = try not (isNull (Type.GetType("Mono.Runtime"))) with e-> false
            let ty =
                if runningOnMono then
                    let ty = Type.GetType("System.Reflection.MonoGenericClass")
                    match ty with
                    | null -> Type.GetType("System.Reflection.Emit.TypeBuilderInstantiation")
                    | _ -> ty
                else
                    Type.GetType("System.Reflection.Emit.TypeBuilderInstantiation")

            ty

        // TODO: this works over FSharp.Core 4.4.0.0 types and methods. These types need to be retargeted to the target runtime.

        let getTypeFromHandleMethod() = typeof<Type>.GetMethod("GetTypeFromHandle")
        let languagePrimitivesType() = typedefof<list<_>>.Assembly.GetType("Microsoft.FSharp.Core.LanguagePrimitives")
        let parseInt32Method() = languagePrimitivesType().GetMethod "ParseInt32"
        let decimalConstructor() = typeof<decimal>.GetConstructor([| typeof<int>; typeof<int>; typeof<int>; typeof<bool>; typeof<byte> |])
        let dateTimeConstructor() = typeof<DateTime>.GetConstructor([| typeof<int64>; typeof<DateTimeKind> |])
        let dateTimeOffsetConstructor() = typeof<DateTimeOffset>.GetConstructor([| typeof<int64>; typeof<TimeSpan> |])
        let timeSpanConstructor() = typeof<TimeSpan>.GetConstructor([|typeof<int64>|])

        let isEmpty s = (s = ExpectedStackState.Empty)
        let isAddress s = (s = ExpectedStackState.Address)
        let rec emitLambda(callSiteIlg : ILGenerator, v : Quotations.Var, body : Expr, freeVars : seq<Quotations.Var>, locals : Dictionary<_, LocalBuilder>, parameters) =
            let lambda = assemblyMainModule.DefineType(uniqueLambdaTypeName(), TypeAttributes.Class)
            let baseType = typedefof<FSharpFunc<_, _>>.MakeGenericType(v.Type, body.Type)
            lambda.SetParent(baseType)
            let ctor = lambda.DefineDefaultConstructor(MethodAttributes.Public)
            let decl = baseType.GetMethod "Invoke"
            let paramTypes = [| for p in decl.GetParameters() -> p.ParameterType |]
            let invoke = lambda.DefineMethod("Invoke", MethodAttributes.Virtual ||| MethodAttributes.Final ||| MethodAttributes.Public, decl.ReturnType, paramTypes)
            lambda.DefineMethodOverride(invoke, decl)

            // promote free vars to fields
            let fields = ResizeArray()
            for v in freeVars do
                let f = lambda.DefineField(v.Name, v.Type, FieldAttributes.Assembly)
                fields.Add(v, f)

            let lambdaLocals = Dictionary()

            let ilg = invoke.GetILGenerator()
            for (v, f) in fields do
                let l = ilg.DeclareLocal(v.Type)
                ilg.Emit(OpCodes.Ldarg_0)
                ilg.Emit(OpCodes.Ldfld, f)
                ilg.Emit(OpCodes.Stloc, l)
                lambdaLocals.[v] <- l

            let expectedState = if (invoke.ReturnType = typeof<Void>) then ExpectedStackState.Empty else ExpectedStackState.Value
            let lambadParamVars = [| Quotations.Var("this", lambda); v|]
            let codeGen = CodeGenerator(assemblyMainModule, uniqueLambdaTypeName, implicitCtorArgsAsFields, transType, transField, transMethod, transCtor, isLiteralEnumField, ilg, lambdaLocals, lambadParamVars)
            codeGen.EmitExpr (expectedState, body)
            ilg.Emit(OpCodes.Ret)

            lambda.CreateType() |> ignore

            callSiteIlg.Emit(OpCodes.Newobj, ctor)
            for (v, f) in fields do
                callSiteIlg.Emit(OpCodes.Dup)
                match locals.TryGetValue v with
                | true, loc ->
                    callSiteIlg.Emit(OpCodes.Ldloc, loc)
                | false, _ ->
                    let index = parameters |> Array.findIndex ((=) v)
                    callSiteIlg.Emit(OpCodes.Ldarg, index)
                callSiteIlg.Emit(OpCodes.Stfld, f)

        and emitExpr expectedState expr =
            let pop () = ilg.Emit(OpCodes.Pop)
            let popIfEmptyExpected s = if isEmpty s then pop()
            let emitConvIfNecessary t1 =
                if t1 = typeof<int16> then
                    ilg.Emit(OpCodes.Conv_I2)
                elif t1 = typeof<uint16> then
                    ilg.Emit(OpCodes.Conv_U2)
                elif t1 = typeof<sbyte> then
                    ilg.Emit(OpCodes.Conv_I1)
                elif t1 = typeof<byte> then
                    ilg.Emit(OpCodes.Conv_U1)

            /// emits given expression to corresponding IL
            match expr with
            | ForIntegerRangeLoop(loopVar, first, last, body) ->
                // for(loopVar = first..last) body
                let lb =
                    match locals.TryGetValue loopVar with
                    | true, lb -> lb
                    | false, _ ->
                        let lb = ilg.DeclareLocal(transType loopVar.Type)
                        locals.Add(loopVar, lb)
                        lb

                // loopVar = first
                emitExpr ExpectedStackState.Value first
                ilg.Emit(OpCodes.Stloc, lb)

                let before = ilg.DefineLabel()
                let after = ilg.DefineLabel()

                ilg.MarkLabel before
                ilg.Emit(OpCodes.Ldloc, lb)

                emitExpr ExpectedStackState.Value last
                ilg.Emit(OpCodes.Bgt, after)

                emitExpr ExpectedStackState.Empty body

                // loopVar++
                ilg.Emit(OpCodes.Ldloc, lb)
                ilg.Emit(OpCodes.Ldc_I4_1)
                ilg.Emit(OpCodes.Add)
                ilg.Emit(OpCodes.Stloc, lb)

                ilg.Emit(OpCodes.Br, before)
                ilg.MarkLabel(after)

            | NewArray(elementTy, elements) ->
                ilg.Emit(OpCodes.Ldc_I4, List.length elements)
                ilg.Emit(OpCodes.Newarr, transType elementTy)

                elements
                |> List.iteri (fun i el ->
                    ilg.Emit(OpCodes.Dup)
                    ilg.Emit(OpCodes.Ldc_I4, i)
                    emitExpr ExpectedStackState.Value el
                    ilg.Emit(OpCodes.Stelem, transType elementTy))

                popIfEmptyExpected expectedState

            | WhileLoop(cond, body) ->
                let before = ilg.DefineLabel()
                let after = ilg.DefineLabel()

                ilg.MarkLabel before
                emitExpr ExpectedStackState.Value cond
                ilg.Emit(OpCodes.Brfalse, after)
                emitExpr ExpectedStackState.Empty body
                ilg.Emit(OpCodes.Br, before)

                ilg.MarkLabel after

            | Var v ->
                if isEmpty expectedState then () else

                // Try to interpret this as a method parameter
                let methIdx = parameterVars |> Array.tryFindIndex (fun p -> p = v)
                match methIdx with
                | Some idx ->
                    ilg.Emit((if isAddress expectedState then OpCodes.Ldarga else OpCodes.Ldarg), idx)
                | None ->

                // Try to interpret this as an implicit field in a class
                let implicitCtorArgFieldOpt = implicitCtorArgsAsFields |> List.tryFind (fun f -> f.Name = v.Name)
                match implicitCtorArgFieldOpt with
                | Some ctorArgField ->
                    ilg.Emit(OpCodes.Ldarg_0)
                    ilg.Emit(OpCodes.Ldfld, ctorArgField)
                | None ->

                // Try to interpret this as a local
                match locals.TryGetValue v with
                | true, localBuilder ->
                    ilg.Emit((if isAddress expectedState  then OpCodes.Ldloca else OpCodes.Ldloc), localBuilder.LocalIndex)
                | false, _ ->
                    failwith "unknown parameter/field"

            | Coerce (arg,ty) ->
                // castClass may lead to observable side-effects - InvalidCastException
                emitExpr ExpectedStackState.Value arg
                let argTy = transType arg.Type
                let targetTy = transType ty
                if argTy.IsValueType && not targetTy.IsValueType then
                    ilg.Emit(OpCodes.Box, argTy)
                elif not argTy.IsValueType && targetTy.IsValueType then
                    ilg.Emit(OpCodes.Unbox_Any, targetTy)
                // emit castclass if
                // - targettype is not obj (assume this is always possible for ref types)
                // AND
                // - HACK: targettype is theTypeBuilderInstantiationType
                //   (its implementation of IsAssignableFrom raises NotSupportedException so it will be safer to always emit castclass)
                // OR
                // - not (argTy :> targetTy)
                elif targetTy <> typeof<obj> && (theTypeBuilderInstantiationType.Equals(targetTy.GetType()) || not (targetTy.IsAssignableFrom(argTy))) then
                    ilg.Emit(OpCodes.Castclass, targetTy)

                popIfEmptyExpected expectedState

            | SpecificCall <@ (-) @>(None, [t1; t2; _], [a1; a2]) ->
                assert(t1 = t2)
                emitExpr ExpectedStackState.Value a1
                emitExpr ExpectedStackState.Value a2
                if t1 = typeof<decimal> then
                    ilg.Emit(OpCodes.Call, typeof<decimal>.GetMethod "op_Subtraction")
                else
                    ilg.Emit(OpCodes.Sub)
                    emitConvIfNecessary t1

                popIfEmptyExpected expectedState

            | SpecificCall <@ (/) @> (None, [t1; t2; _], [a1; a2]) ->
                assert (t1 = t2)
                emitExpr ExpectedStackState.Value a1
                emitExpr ExpectedStackState.Value a2
                if t1 = typeof<decimal> then
                    ilg.Emit(OpCodes.Call, typeof<decimal>.GetMethod "op_Division")
                else
                    match Type.GetTypeCode t1 with
                    | TypeCode.UInt32
                    | TypeCode.UInt64
                    | TypeCode.UInt16
                    | TypeCode.Byte
                    | _ when t1 = typeof<unativeint> -> ilg.Emit (OpCodes.Div_Un)
                    | _ -> ilg.Emit(OpCodes.Div)

                    emitConvIfNecessary t1

                popIfEmptyExpected expectedState

            | SpecificCall <@ int @>(None, [sourceTy], [v]) ->
                emitExpr ExpectedStackState.Value v
                match Type.GetTypeCode(sourceTy) with
                | TypeCode.String ->
                    ilg.Emit(OpCodes.Call, parseInt32Method())
                | TypeCode.Single
                | TypeCode.Double
                | TypeCode.Int64
                | TypeCode.UInt64
                | TypeCode.UInt16
                | TypeCode.Char
                | TypeCode.Byte
                | _ when sourceTy = typeof<nativeint> || sourceTy = typeof<unativeint> ->
                    ilg.Emit(OpCodes.Conv_I4)
                | TypeCode.Int32
                | TypeCode.UInt32
                | TypeCode.Int16
                | TypeCode.SByte -> () // no op
                | _ -> failwith "TODO: search for op_Explicit on sourceTy"

            | SpecificCall <@ LanguagePrimitives.IntrinsicFunctions.GetArray @> (None, [ty], [arr; index]) ->
                // observable side-effect - IndexOutOfRangeException
                emitExpr ExpectedStackState.Value arr
                emitExpr ExpectedStackState.Value index
                if isAddress expectedState then
                    ilg.Emit(OpCodes.Readonly)
                    ilg.Emit(OpCodes.Ldelema, transType ty)
                else
                    ilg.Emit(OpCodes.Ldelem, transType ty)

                popIfEmptyExpected expectedState

            | SpecificCall <@ LanguagePrimitives.IntrinsicFunctions.GetArray2D @> (None, _ty, arr::indices)
            | SpecificCall <@ LanguagePrimitives.IntrinsicFunctions.GetArray3D @> (None, _ty, arr::indices)
            | SpecificCall <@ LanguagePrimitives.IntrinsicFunctions.GetArray4D @> (None, _ty, arr::indices) ->

                let meth =
                    let name = if isAddress expectedState then "Address" else "Get"
                    arr.Type.GetMethod(name)

                // observable side-effect - IndexOutOfRangeException
                emitExpr ExpectedStackState.Value arr
                for index in indices do
                    emitExpr ExpectedStackState.Value index

                if isAddress expectedState then
                    ilg.Emit(OpCodes.Readonly)

                ilg.Emit(OpCodes.Call, meth)

                popIfEmptyExpected expectedState


            | FieldGet (None,field) when isLiteralEnumField field ->
                if expectedState <> ExpectedStackState.Empty then
                    emitExpr expectedState (Expr.Value(field.GetRawConstantValue(), field.FieldType.GetEnumUnderlyingType()))

            | FieldGet (objOpt,field) ->
                objOpt |> Option.iter (fun e ->
                    let s = if e.Type.IsValueType then ExpectedStackState.Address else ExpectedStackState.Value
                    emitExpr s e)
                let field = transField field
                if field.IsStatic then
                    ilg.Emit(OpCodes.Ldsfld, field)
                else
                    ilg.Emit(OpCodes.Ldfld, field)

            | FieldSet (objOpt,field,v) ->
                objOpt |> Option.iter (fun e ->
                    let s = if e.Type.IsValueType then ExpectedStackState.Address else ExpectedStackState.Value
                    emitExpr s e)
                emitExpr ExpectedStackState.Value v
                let field = transField field
                if field.IsStatic then
                    ilg.Emit(OpCodes.Stsfld, field)
                else
                    ilg.Emit(OpCodes.Stfld, field)

            | Call (objOpt,meth,args) ->
                objOpt |> Option.iter (fun e ->
                    let s = if e.Type.IsValueType then ExpectedStackState.Address else ExpectedStackState.Value
                    emitExpr s e)

                for pe in args do
                    emitExpr ExpectedStackState.Value pe

                // Handle the case where this is a generic method instantiated at a type being compiled
                let mappedMeth =
                    if meth.IsGenericMethod then
                        let args = meth.GetGenericArguments() |> Array.map transType
                        let gmd = meth.GetGenericMethodDefinition() |> transMethod
                        gmd.GetGenericMethodDefinition().MakeGenericMethod args
                    elif meth.DeclaringType.IsGenericType then
                        let gdty = transType (meth.DeclaringType.GetGenericTypeDefinition())
                        let gdtym = gdty.GetMethods() |> Seq.find (fun x -> x.Name = meth.Name)
                        assert (gdtym |> isNull |> not) // ?? will never happen - if method is not found - KeyNotFoundException will be raised
                        let dtym =
                            match transType meth.DeclaringType with
                            | :? TypeBuilder as dty -> TypeBuilder.GetMethod(dty, gdtym)
                            | dty -> MethodBase.GetMethodFromHandle(meth.MethodHandle, dty.TypeHandle) :?> _

                        assert (dtym |> isNull |> not)
                        dtym
                    else
                        transMethod meth
                match objOpt with
                | Some obj when mappedMeth.IsAbstract || mappedMeth.IsVirtual  ->
                    if obj.Type.IsValueType then ilg.Emit(OpCodes.Constrained, transType obj.Type)
                    ilg.Emit(OpCodes.Callvirt, mappedMeth)
                | _ ->
                    ilg.Emit(OpCodes.Call, mappedMeth)

                let returnTypeIsVoid = mappedMeth.ReturnType = typeof<Void>
                match returnTypeIsVoid, (isEmpty expectedState) with
                | false, true ->
                        // method produced something, but we don't need it
                        pop()
                | true, false when expr.Type = typeof<unit> ->
                        // if we need result and method produce void and result should be unit - push null as unit value on stack
                        ilg.Emit(OpCodes.Ldnull)
                | _ -> ()

            | NewObject (ctor,args) ->
                for pe in args do
                    emitExpr ExpectedStackState.Value pe
                let meth = transCtor ctor
                ilg.Emit(OpCodes.Newobj, meth)

                popIfEmptyExpected expectedState

            | Value (obj, _ty) ->
                let rec emitC (v:obj) =
                    match v with
                    | :? string as x -> ilg.Emit(OpCodes.Ldstr, x)
                    | :? int8 as x -> ilg.Emit(OpCodes.Ldc_I4, int32 x)
                    | :? uint8 as x -> ilg.Emit(OpCodes.Ldc_I4, int32 (int8 x))
                    | :? int16 as x -> ilg.Emit(OpCodes.Ldc_I4, int32 x)
                    | :? uint16 as x -> ilg.Emit(OpCodes.Ldc_I4, int32 (int16 x))
                    | :? int32 as x -> ilg.Emit(OpCodes.Ldc_I4, x)
                    | :? uint32 as x -> ilg.Emit(OpCodes.Ldc_I4, int32 x)
                    | :? int64 as x -> ilg.Emit(OpCodes.Ldc_I8, x)
                    | :? uint64 as x -> ilg.Emit(OpCodes.Ldc_I8, int64 x)
                    | :? char as x -> ilg.Emit(OpCodes.Ldc_I4, int32 x)
                    | :? bool as x -> ilg.Emit(OpCodes.Ldc_I4, if x then 1 else 0)
                    | :? float32 as x -> ilg.Emit(OpCodes.Ldc_R4, x)
                    | :? float as x -> ilg.Emit(OpCodes.Ldc_R8, x)
    #if !FX_NO_GET_ENUM_UNDERLYING_TYPE
                    | :? Enum as x when x.GetType().GetEnumUnderlyingType() = typeof<int32> -> ilg.Emit(OpCodes.Ldc_I4, unbox<int32> v)
    #endif
                    | :? Type as ty ->
                        ilg.Emit(OpCodes.Ldtoken, transType ty)
                        ilg.Emit(OpCodes.Call, getTypeFromHandleMethod())
                    | :? decimal as x ->
                        let bits = Decimal.GetBits x
                        ilg.Emit(OpCodes.Ldc_I4, bits.[0])
                        ilg.Emit(OpCodes.Ldc_I4, bits.[1])
                        ilg.Emit(OpCodes.Ldc_I4, bits.[2])
                        do
                            let sign = (bits.[3] &&& 0x80000000) <> 0
                            ilg.Emit(if sign then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
                        do
                            let scale = byte ((bits.[3] >>> 16) &&& 0x7F)
                            ilg.Emit(OpCodes.Ldc_I4_S, scale)
                        ilg.Emit(OpCodes.Newobj, decimalConstructor())
                    | :? DateTime as x ->
                        ilg.Emit(OpCodes.Ldc_I8, x.Ticks)
                        ilg.Emit(OpCodes.Ldc_I4, int x.Kind)
                        ilg.Emit(OpCodes.Newobj, dateTimeConstructor())
                    | :? DateTimeOffset as x ->
                        ilg.Emit(OpCodes.Ldc_I8, x.Ticks)
                        ilg.Emit(OpCodes.Ldc_I8, x.Offset.Ticks)
                        ilg.Emit(OpCodes.Newobj, timeSpanConstructor())
                        ilg.Emit(OpCodes.Newobj, dateTimeOffsetConstructor())
                    | null -> ilg.Emit(OpCodes.Ldnull)
                    | _ -> failwithf "unknown constant '%A' in generated method" v
                if isEmpty expectedState then ()
                else emitC obj

            | Let(v,e,b) ->
                let lb = ilg.DeclareLocal (transType v.Type)
                locals.Add (v, lb)
                emitExpr ExpectedStackState.Value e
                ilg.Emit(OpCodes.Stloc, lb.LocalIndex)
                emitExpr expectedState b

            | Sequential(e1, e2) ->
                emitExpr ExpectedStackState.Empty e1
                emitExpr expectedState e2

            | IfThenElse(cond, ifTrue, ifFalse) ->
                let ifFalseLabel = ilg.DefineLabel()
                let endLabel = ilg.DefineLabel()

                emitExpr ExpectedStackState.Value cond

                ilg.Emit(OpCodes.Brfalse, ifFalseLabel)

                emitExpr expectedState ifTrue
                ilg.Emit(OpCodes.Br, endLabel)

                ilg.MarkLabel(ifFalseLabel)
                emitExpr expectedState ifFalse

                ilg.Emit(OpCodes.Nop)
                ilg.MarkLabel(endLabel)

            | TryWith(body, _filterVar, _filterBody, catchVar, catchBody) ->

                let stres, ldres =
                    if isEmpty expectedState then ignore, ignore
                    else
                        let local = ilg.DeclareLocal (transType body.Type)
                        let stres = fun () -> ilg.Emit(OpCodes.Stloc, local)
                        let ldres = fun () -> ilg.Emit(OpCodes.Ldloc, local)
                        stres, ldres

                let exceptionVar = ilg.DeclareLocal(transType catchVar.Type)
                locals.Add(catchVar, exceptionVar)

                let _exnBlock = ilg.BeginExceptionBlock()

                emitExpr expectedState body
                stres()

                ilg.BeginCatchBlock(transType  catchVar.Type)
                ilg.Emit(OpCodes.Stloc, exceptionVar)
                emitExpr expectedState catchBody
                stres()
                ilg.EndExceptionBlock()

                ldres()

            | VarSet(v,e) ->
                emitExpr ExpectedStackState.Value e
                match locals.TryGetValue v with
                | true, localBuilder ->
                    ilg.Emit(OpCodes.Stloc, localBuilder.LocalIndex)
                | false, _ ->
                    failwith "unknown parameter/field in assignment. Only assignments to locals are currently supported by TypeProviderEmit"
            | Lambda(v, body) ->
                emitLambda(ilg, v, body, expr.GetFreeVars(), locals, parameterVars)
                popIfEmptyExpected expectedState
            | n ->
                failwith (sprintf "unknown expression '%A' in generated method" n)

        member __.EmitExpr (expectedState, expr) = emitExpr expectedState expr

    //-------------------------------------------------------------------------------------------------
    // The assembly compiler for generative type providers.

    type AssemblyGenerator(assemblyFileName, convToTgt) =
        let assemblyShortName = Path.GetFileNameWithoutExtension assemblyFileName
        let assemblyName = AssemblyName assemblyShortName
        let assembly =
            AppDomain.CurrentDomain.DefineDynamicAssembly(name=assemblyName,access=(AssemblyBuilderAccess.Save ||| AssemblyBuilderAccess.Run),dir=Path.GetDirectoryName assemblyFileName)
        let assemblyMainModule =
            assembly.DefineDynamicModule("MainModule", Path.GetFileName assemblyFileName)
        let typeMap = Dictionary<ProvidedTypeDefinition,TypeBuilder>(HashIdentity.Reference)
        let typeMapExtra = Dictionary<string,TypeBuilder>(HashIdentity.Structural)
        let uniqueLambdaTypeName() =
            // lambda name should be unique across all types that all type provider might contribute in result assembly
            sprintf "Lambda%O" (Guid.NewGuid())

        member __.Assembly = assembly :> Assembly

        /// Emit the given provided type definitions into an assembly and adjust 'Assembly' property of all type definitions to return that
        /// assembly.
        member __.Generate(providedTypeDefinitions:(ProvidedTypeDefinition * string list option) list) =
            let bindAll = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance
            // phase 1 - set assembly fields and emit type definitions
            begin
                let adjustTypeAttributes attributes isNested =
                    let visibilityAttributes =
                        match attributes &&& TypeAttributes.VisibilityMask with
                        | TypeAttributes.Public when isNested -> TypeAttributes.NestedPublic
                        | TypeAttributes.NotPublic when isNested -> TypeAttributes.NestedAssembly
                        | TypeAttributes.NestedPublic when not isNested -> TypeAttributes.Public
                        | TypeAttributes.NestedAssembly
                        | TypeAttributes.NestedPrivate
                        | TypeAttributes.NestedFamORAssem
                        | TypeAttributes.NestedFamily
                        | TypeAttributes.NestedFamANDAssem when not isNested -> TypeAttributes.NotPublic
                        | a -> a
                    (attributes &&& ~~~TypeAttributes.VisibilityMask) ||| visibilityAttributes

                let rec typeMembers (tb:TypeBuilder)  (td : ProvidedTypeDefinition) =
                    for ntd in td.GetNestedTypes(bindAll) do
                        nestedType tb ntd

                and nestedType (tb:TypeBuilder)  (ntd : Type) =
                    match ntd with
                    | :? ProvidedTypeDefinition as pntd ->
                        if pntd.IsErased then invalidOp ("The nested provided type "+pntd.Name+" is marked as erased and cannot be converted to a generated type. Set 'IsErased=false' on the ProvidedTypeDefinition")
                        // Adjust the attributes - we're codegen'ing this type as nested
                        let attributes = adjustTypeAttributes ntd.Attributes true
                        let ntb = tb.DefineNestedType(pntd.Name,attr=attributes)
                        pntd.SetAssemblyInternal null
                        typeMap.[pntd] <- ntb
                        typeMembers ntb pntd
                    | _ -> ()

                for (pt,enclosingGeneratedTypeNames) in providedTypeDefinitions do
                  match enclosingGeneratedTypeNames with
                  | None ->
                    // Filter out the additional TypeProviderTypeAttributes flags
                    let attributes = pt.Attributes &&& ~~~(enum (int32 TypeProviderTypeAttributes.SuppressRelocate))
                                                    &&& ~~~(enum (int32 TypeProviderTypeAttributes.IsErased))
                    // Adjust the attributes - we're codegen'ing as non-nested
                    let attributes = adjustTypeAttributes attributes false
                    let tb = assemblyMainModule.DefineType(name=pt.FullName,attr=attributes)
                    pt.SetAssemblyInternal null
                    typeMap.[pt] <- tb
                    typeMembers tb pt
                  | Some ns ->
                    let otb,_ =
                        ((None,""),ns) ||> List.fold (fun (otb:TypeBuilder option,fullName) n ->
                            let fullName = if fullName = "" then n else fullName + "." + n
                            let priorType = if typeMapExtra.ContainsKey(fullName) then Some typeMapExtra.[fullName]  else None
                            let tb =
                                match priorType with
                                | Some tbb -> tbb
                                | None ->
                                // OK, the implied nested type is not defined, define it now
                                let attributes = TypeAttributes.Public ||| TypeAttributes.Class ||| TypeAttributes.Sealed
                                // Filter out the additional TypeProviderTypeAttributes flags
                                let attributes = adjustTypeAttributes attributes otb.IsSome
                                let tb =
                                    match otb with
                                    | None -> assemblyMainModule.DefineType(name=n,attr=attributes)
                                    | Some (otb:TypeBuilder) -> otb.DefineNestedType(name=n,attr=attributes)
                                typeMapExtra.[fullName] <- tb
                                tb
                            (Some tb, fullName))
                    nestedType otb.Value pt
            end

            let rec transType (ty:Type) =
                match ty with
                | :? ProvidedTypeDefinition as ptd ->
                    if typeMap.ContainsKey ptd then typeMap.[ptd] :> Type else ty
                | _ ->
                    if ty.IsGenericType then ty.GetGenericTypeDefinition().MakeGenericType (Array.map transType (ty.GetGenericArguments()))
                    elif ty.HasElementType then
                        let ety = transType (ty.GetElementType())
                        if ty.IsArray then
                            let rank = ty.GetArrayRank()
                            if rank = 1 then ety.MakeArrayType()
                            else ety.MakeArrayType rank
                        elif ty.IsPointer then ety.MakePointerType()
                        elif ty.IsByRef then ety.MakeByRefType()
                        else ty
                    else ty

            let ctorMap = Dictionary<ProvidedConstructor, ConstructorBuilder>(HashIdentity.Reference)
            let methMap = Dictionary<ProvidedMethod, MethodBuilder>(HashIdentity.Reference)
            let fieldMap = Dictionary<FieldInfo, FieldBuilder>(HashIdentity.Reference)
            let transCtor (f:ConstructorInfo) = match f with :? ProvidedConstructor as pc when ctorMap.ContainsKey pc -> ctorMap.[pc] :> ConstructorInfo  | c -> c
            let transField (f:FieldInfo) = match f with :? ProvidedField as pf when fieldMap.ContainsKey pf -> fieldMap.[pf] :> FieldInfo  | f -> f
            let transMeth (m:MethodInfo) = match m with :? ProvidedMethod as pm when methMap.ContainsKey pm -> methMap.[pm] :> MethodInfo | m -> m
            let isLiteralEnumField (f:FieldInfo) =  match f with :? ProvidedLiteralField as plf -> plf.DeclaringType.IsEnum | _ -> false

            let iterateTypes f =
                let rec typeMembers (ptd : ProvidedTypeDefinition) =
                    let tb = typeMap.[ptd]
                    f tb (Some ptd)
                    for ntd in ptd.GetNestedTypes(bindAll) do
                        nestedType ntd

                and nestedType (ntd : Type) =
                    match ntd with
                    | :? ProvidedTypeDefinition as pntd -> typeMembers pntd
                    | _ -> ()

                for (pt,enclosingGeneratedTypeNames) in providedTypeDefinitions do
                  match enclosingGeneratedTypeNames with
                  | None ->
                    typeMembers pt
                  | Some ns ->
                    let _fullName  =
                        ("",ns) ||> List.fold (fun fullName n ->
                            let fullName = if fullName = "" then n else fullName + "." + n
                            f typeMapExtra.[fullName] None
                            fullName)
                    nestedType pt


            // phase 1b - emit base types
            iterateTypes (fun tb ptd ->
                match ptd with
                | None -> ()
                | Some ptd ->
                match ptd.BaseType with null -> () | bt -> tb.SetParent(transType bt))

            let defineCustomAttrs f (cattrs: IList<CustomAttributeData>) =
                for attr in cattrs do
                    let constructorArgs = [ for x in attr.ConstructorArguments -> x.Value ]
                    let namedProps,namedPropVals = [ for x in attr.NamedArguments do match x.MemberInfo with :? PropertyInfo as pi -> yield (pi, x.TypedValue.Value) | _ -> () ] |> List.unzip
                    let namedFields,namedFieldVals = [ for x in attr.NamedArguments do match x.MemberInfo with :? FieldInfo as pi -> yield (pi, x.TypedValue.Value) | _ -> () ] |> List.unzip
                    let cab = CustomAttributeBuilder(attr.Constructor, Array.ofList constructorArgs, Array.ofList namedProps, Array.ofList namedPropVals, Array.ofList namedFields, Array.ofList namedFieldVals)
                    f cab

            // phase 2 - emit member definitions
            iterateTypes (fun tb ptd ->
                match ptd with
                | None -> ()
                | Some ptd ->
                for cinfo in ptd.GetConstructors(bindAll) do
                    match cinfo with
                    | :? ProvidedConstructor as pcinfo when not (ctorMap.ContainsKey pcinfo)  ->
                        let cb =
                            if pcinfo.IsTypeInitializer then
                                if (cinfo.GetParameters()).Length <> 0 then failwith "Type initializer should not have parameters"
                                tb.DefineTypeInitializer()
                            else
                                let cb = tb.DefineConstructor(cinfo.Attributes, CallingConventions.Standard, [| for p in cinfo.GetParameters() -> transType p.ParameterType |])
                                for (i,p) in cinfo.GetParameters() |> Seq.mapi (fun i x -> (i,x)) do
                                    cb.DefineParameter(i+1, ParameterAttributes.None, p.Name) |> ignore
                                cb
                        ctorMap.[pcinfo] <- cb
                    | _ -> ()

                if ptd.IsEnum then
                    tb.DefineField("value__", ptd.GetEnumUnderlyingType(), FieldAttributes.Public ||| FieldAttributes.SpecialName ||| FieldAttributes.RTSpecialName)
                    |> ignore

                for finfo in ptd.GetFields(bindAll) do
                    let fieldInfo =
                        match finfo with
                            | :? ProvidedField as pinfo ->
                                Some (pinfo.Name, transType finfo.FieldType, finfo.Attributes, pinfo.GetCustomAttributesData(), None)
                            | :? ProvidedLiteralField as pinfo ->
                                Some (pinfo.Name, transType finfo.FieldType, finfo.Attributes, pinfo.GetCustomAttributesData(), Some (pinfo.GetRawConstantValue()))
                            | _ -> None
                    match fieldInfo with
                    | Some (name, ty, attr, cattr, constantVal) when not (fieldMap.ContainsKey finfo) ->
                        let fb = tb.DefineField(name, ty, attr)
                        if constantVal.IsSome then
                            fb.SetConstant constantVal.Value
                        defineCustomAttrs fb.SetCustomAttribute cattr
                        fieldMap.[finfo] <- fb
                    | _ -> ()
                for minfo in ptd.GetMethods(bindAll) do
                    match minfo with
                    | :? ProvidedMethod as pminfo when not (methMap.ContainsKey pminfo)  ->
                        let mb = tb.DefineMethod(minfo.Name, minfo.Attributes, transType minfo.ReturnType, [| for p in minfo.GetParameters() -> transType p.ParameterType |])
                        for (i, p) in minfo.GetParameters() |> Seq.mapi (fun i x -> (i,x :?> ProvidedParameter)) do
                            // TODO: check why F# compiler doesn't emit default value when just p.Attributes is used (thus bad metadata is emitted)
    //                        let mutable attrs = ParameterAttributes.None
    //
    //                        if p.IsOut then attrs <- attrs ||| ParameterAttributes.Out
    //                        if p.HasDefaultParameterValue then attrs <- attrs ||| ParameterAttributes.Optional

                            let pb = mb.DefineParameter(i+1, p.Attributes, p.Name)
                            if p.HasDefaultParameterValue then
                                do
                                    let ctor = typeof<System.Runtime.InteropServices.DefaultParameterValueAttribute>.GetConstructor([|typeof<obj>|])
                                    let builder = CustomAttributeBuilder(ctor, [|p.RawDefaultValue|])
                                    pb.SetCustomAttribute builder
                                do
                                    let ctor = typeof<System.Runtime.InteropServices.OptionalAttribute>.GetConstructor([||])
                                    let builder = CustomAttributeBuilder(ctor, [||])
                                    pb.SetCustomAttribute builder
                                pb.SetConstant p.RawDefaultValue
                        methMap.[pminfo] <- mb
                    | _ -> ()

                for ityp in ptd.GetInterfaceImplementationsInternal() do
                    tb.AddInterfaceImplementation ityp)

            // phase 3 - emit member code
            iterateTypes (fun  tb ptd ->
                match ptd with
                | None -> ()
                | Some ptd ->
                let cattr = ptd.GetCustomAttributesData()
                defineCustomAttrs tb.SetCustomAttribute cattr
                // Allow at most one constructor, and use its arguments as the fields of the type
                let ctors =
                    ptd.GetConstructors(bindAll) // exclude type initializer
                    |> Seq.choose (function :? ProvidedConstructor as pcinfo when not pcinfo.IsTypeInitializer -> Some pcinfo | _ -> None)
                    |> Seq.toList
                let implictCtorArgs =
                    match ctors  |> List.filter (fun x -> x.IsImplicitCtor)  with
                    | [] -> []
                    | [ pcinfo ] -> [ for p in pcinfo.GetParameters() -> p ]
                    | _ -> failwith "at most one implicit constructor allowed"

                let implicitCtorArgsAsFields =
                    [ for ctorArg in implictCtorArgs ->
                          tb.DefineField(ctorArg.Name, transType ctorArg.ParameterType, FieldAttributes.Private) ]



                // Emit the constructor (if any)
                for pcinfo in ctors do
                    assert ctorMap.ContainsKey pcinfo
                    let cb = ctorMap.[pcinfo]
                    let cattr = pcinfo.GetCustomAttributesData()
                    defineCustomAttrs cb.SetCustomAttribute cattr
                    let ilg = cb.GetILGenerator()
                    let locals = Dictionary<Var,LocalBuilder>()
                    let parameterVars =
                        [| yield Var("this", pcinfo.DeclaringType)
                           for p in pcinfo.GetParameters() do
                                yield Var(p.Name, p.ParameterType) |]

                    let codeGen = CodeGenerator(assemblyMainModule, uniqueLambdaTypeName, implicitCtorArgsAsFields, transType, transField, transMeth, transCtor, isLiteralEnumField, ilg, locals, parameterVars)
                    let parameters =
                        [| for v in parameterVars -> Expr.Var v |]
                    match pcinfo.GetBaseConstructorCallInternal (true, convToTgt) with
                    | None ->
                        ilg.Emit(OpCodes.Ldarg_0)
                        let cinfo = ptd.BaseType.GetConstructor(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance, null, [| |], null)
                        ilg.Emit(OpCodes.Call,cinfo)
                    | Some f ->
                        // argExprs should always include 'this'
                        let (cinfo,argExprs) = f (Array.toList parameters)
                        for argExpr in argExprs do
                            codeGen.EmitExpr (ExpectedStackState.Value, argExpr)
                        ilg.Emit(OpCodes.Call,cinfo)

                    if pcinfo.IsImplicitCtor then
                        for ctorArgsAsFieldIdx,ctorArgsAsField in List.mapi (fun i x -> (i,x)) implicitCtorArgsAsFields do
                            ilg.Emit(OpCodes.Ldarg_0)
                            ilg.Emit(OpCodes.Ldarg, ctorArgsAsFieldIdx+1)
                            ilg.Emit(OpCodes.Stfld, ctorArgsAsField)
                    else
                        let exprFun  = pcinfo.GetInvokeCodeInternal (true, convToTgt)
                        let code = exprFun parameters
                        codeGen.EmitExpr (ExpectedStackState.Empty, code)
                    ilg.Emit(OpCodes.Ret)

                match ptd.GetConstructors(bindAll) |> Seq.tryPick (function :? ProvidedConstructor as pc when pc.IsTypeInitializer -> Some pc | _ -> None) with
                | None -> ()
                | Some pc ->
                    let cb = ctorMap.[pc]
                    let ilg = cb.GetILGenerator()
                    let cattr = pc.GetCustomAttributesData()
                    defineCustomAttrs cb.SetCustomAttribute cattr
                    let exprFun = pc.GetInvokeCodeInternal (true, convToTgt)
                    let expr = exprFun [||]
                    let codeGen = CodeGenerator(assemblyMainModule, uniqueLambdaTypeName, implicitCtorArgsAsFields, transType, transField, transMeth, transCtor, isLiteralEnumField, ilg, new Dictionary<_, _>(), [| |])
                    codeGen.EmitExpr (ExpectedStackState.Empty, expr)
                    ilg.Emit OpCodes.Ret

                // Emit the methods
                for minfo in ptd.GetMethods(bindAll) do
                  match minfo with
                  | :? ProvidedMethod as pminfo   ->
                    let mb = methMap.[pminfo]
                    let ilg = mb.GetILGenerator()
                    let cattr = pminfo.GetCustomAttributesData()
                    defineCustomAttrs mb.SetCustomAttribute cattr

                    let parameterVars =
                        [| if not pminfo.IsStatic then
                                yield Var("this", pminfo.DeclaringType)
                           for p in pminfo.GetParameters() do
                                yield Var(p.Name, p.ParameterType) |]
                    let parameters =
                        [| for v in parameterVars -> Expr.Var v |]

                    let exprFun = pminfo.GetInvokeCodeInternal(true, convToTgt)
                    let expr = exprFun parameters

                    let locals = Dictionary<Var,LocalBuilder>()
                    //printfn "Emitting linqCode for %s::%s, code = %s" pminfo.DeclaringType.FullName pminfo.Name (try linqCode.ToString() with _ -> "<error>")


                    let expectedState = if (minfo.ReturnType = typeof<Void>) then ExpectedStackState.Empty else ExpectedStackState.Value
                    let codeGen = CodeGenerator(assemblyMainModule, uniqueLambdaTypeName, implicitCtorArgsAsFields, transType, transField, transMeth, transCtor, isLiteralEnumField, ilg, locals, parameterVars)
                    codeGen.EmitExpr (expectedState, expr)
                    ilg.Emit OpCodes.Ret
                  | _ -> ()

                for (bodyMethInfo,declMethInfo) in ptd.GetMethodOverridesInternal() do
                    let bodyMethBuilder = methMap.[bodyMethInfo]
                    tb.DefineMethodOverride(bodyMethBuilder,declMethInfo)

                for evt in ptd.GetEvents(bindAll) |> Seq.choose (function :? ProvidedEvent as pe -> Some pe | _ -> None) do
                    let eb = tb.DefineEvent(evt.Name, evt.Attributes, evt.EventHandlerType)
                    defineCustomAttrs eb.SetCustomAttribute (evt.GetCustomAttributesData())
                    eb.SetAddOnMethod(methMap.[evt.GetAddMethod(true) :?> _])
                    eb.SetRemoveOnMethod(methMap.[evt.GetRemoveMethod(true) :?> _])
                    // TODO: add raiser

                for pinfo in ptd.GetProperties(bindAll) |> Seq.choose (function :? ProvidedProperty as pe -> Some pe | _ -> None) do
                    let pb = tb.DefineProperty(pinfo.Name, pinfo.Attributes, transType pinfo.PropertyType, [| for p in pinfo.GetIndexParameters() -> transType p.ParameterType |])
                    let cattr = pinfo.GetCustomAttributesData()
                    defineCustomAttrs pb.SetCustomAttribute cattr
                    if  pinfo.CanRead then
                        let minfo = pinfo.GetGetMethod(true)
                        pb.SetGetMethod (methMap.[minfo :?> ProvidedMethod ])
                    if  pinfo.CanWrite then
                        let minfo = pinfo.GetSetMethod(true)
                        pb.SetSetMethod (methMap.[minfo :?> ProvidedMethod ]))

            // phase 4 - complete types

            let resolveHandler = ResolveEventHandler(fun _ args ->
                // On Mono args.Name is full name of the type, on .NET - just name (no namespace)
                typeMap.Values
                |> Seq.filter (fun tb -> tb.FullName = args.Name || tb.Name = args.Name)
                |> Seq.iter (fun tb -> tb.CreateType() |> ignore)

                assemblyMainModule.Assembly)

            try
                AppDomain.CurrentDomain.add_TypeResolve resolveHandler
                iterateTypes (fun tb _ -> tb.CreateType() |> ignore)
            finally
                AppDomain.CurrentDomain.remove_TypeResolve resolveHandler

    //#if !FX_NO_LOCAL_FILESYSTEM
            assembly.Save (Path.GetFileName assemblyFileName)
    //#endif

            let assemblyLoadedInMemory = assemblyMainModule.Assembly

            iterateTypes (fun _tb ptd ->
                match ptd with
                | None -> ()
                | Some ptd -> ptd.SetAssemblyInternal assemblyLoadedInMemory)

    //#if !FX_NO_LOCAL_FILESYSTEM
        member __.GetFinalBytes() =
            let assemblyBytes = File.ReadAllBytes assemblyFileName
            let _assemblyLoadedInMemory = Assembly.Load(assemblyBytes,null,System.Security.SecurityContextSource.CurrentAppDomain)
            //printfn "final bytes in '%s'" assemblyFileName
            File.Delete assemblyFileName
            assemblyBytes
    //#endif

    module GlobalProvidedAssemblyElementsTable =
        let theTable = ConcurrentDictionary<Assembly, Lazy<byte[]>>()

    type ProvidedAssembly(assemblyFileName: string) =
        let theTypes = ResizeArray<_>()
        let convToTgt = id // TODO: Cross-targeting generative type providers
        let assemblyGenerator = AssemblyGenerator(assemblyFileName, convToTgt)
        let assemblyLazy =
            lazy
                assemblyGenerator.Generate(theTypes |> Seq.toList)
                assemblyGenerator.Assembly
    //#if !FX_NO_LOCAL_FILESYSTEM
        let theAssemblyBytesLazy =
          lazy
            assemblyGenerator.GetFinalBytes()

        do
            GlobalProvidedAssemblyElementsTable.theTable.[assemblyGenerator.Assembly] <- theAssemblyBytesLazy

    //#endif

        let add (providedTypeDefinitions:ProvidedTypeDefinition list, enclosingTypeNames: string list option) =
            for pt in providedTypeDefinitions do
                if pt.IsErased then invalidOp ("The provided type "+pt.Name+"is marked as erased and cannot be converted to a generated type. Set 'IsErased=false' on the ProvidedTypeDefinition")
                theTypes.Add(pt,enclosingTypeNames)
                pt.SetAssemblyLazyInternal assemblyLazy

        member x.AddNestedTypes (types, enclosingGeneratedTypeNames) = add (types, Some enclosingGeneratedTypeNames)

        member x.AddTypes (types) = add (types, None)

    //#if !FX_NO_LOCAL_FILESYSTEM
        static member RegisterGenerated (fileName:string) =
            //printfn "registered assembly in '%s'" fileName
            let assemblyBytes = File.ReadAllBytes fileName
            let assembly = Assembly.Load(assemblyBytes,null,System.Security.SecurityContextSource.CurrentAppDomain)
            GlobalProvidedAssemblyElementsTable.theTable.[assembly] <- Lazy<_>.CreateFromValue assemblyBytes
            assembly
    //#endif

#endif // NO_GENERATIVE

//-------------------------------------------------------------------------------------------------
// TypeProviderForNamespaces

namespace ProviderImplementation.ProvidedTypes

    open System
    open System.Diagnostics
    open System.IO
    open System.Collections.Concurrent
    open System.Collections.Generic
    open System.Reflection
    open System.Reflection.Emit

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.ExprShape
    open Microsoft.FSharp.Core.CompilerServices
    open Microsoft.FSharp.Reflection

    open ProviderImplementation.ProvidedTypes
    open ProviderImplementation.ProvidedTypes.AssemblyReader
    open ProviderImplementation.ProvidedTypes.AssemblyReaderReflection
    open ProviderImplementation.ProvidedTypes.UncheckedQuotations

    type TypeProviderForNamespaces(namespacesAndTypes : list<(string * list<ProvidedTypeDefinition>)>) as this =
        let otherNamespaces = ResizeArray<string * list<ProvidedTypeDefinition>>()

        let makeProvidedNamespace (namespaceName:string) (types:ProvidedTypeDefinition list) =
            let types = [| for ty in types -> ty :> Type |]
            {new IProvidedNamespace with
                member __.GetNestedNamespaces() = [| |]
                member __.NamespaceName = namespaceName
                member __.GetTypes() = types |> Array.copy
                member __.ResolveTypeName typeName : Type =
                    match types |> Array.tryFind (fun ty -> ty.Name = typeName) with
                    | Some ty -> ty
                    | None    -> null
            }

        let providedNamespaces =
            lazy [| for (namespaceName,types) in namespacesAndTypes do
                         yield makeProvidedNamespace namespaceName types
                    for (namespaceName,types) in otherNamespaces do
                         yield makeProvidedNamespace namespaceName types |]

        let invalidateE = new Event<EventHandler,EventArgs>()

        let disposing = Event<EventHandler,EventArgs>()

    #if !FX_NO_LOCAL_FILESYSTEM
        let probingFolders = ResizeArray()
        let handler = ResolveEventHandler(fun _ args -> this.ResolveAssembly(args))
        do AppDomain.CurrentDomain.add_AssemblyResolve handler
    #endif

        new (namespaceName:string,types:list<ProvidedTypeDefinition>) = new TypeProviderForNamespaces([(namespaceName,types)])
        new () = new TypeProviderForNamespaces([])

        [<CLIEvent>]
        member __.Disposing = disposing.Publish

    #if FX_NO_LOCAL_FILESYSTEM

        interface IDisposable with
            member x.Dispose() =
                disposing.Trigger(x, EventArgs.Empty)

    #else

        abstract member ResolveAssembly : args : ResolveEventArgs -> Assembly

        default __.ResolveAssembly(args) =
            let expectedName = (AssemblyName(args.Name)).Name + ".dll"
            let expectedLocationOpt =
                probingFolders
                |> Seq.map (fun f -> IO.Path.Combine(f, expectedName))
                |> Seq.tryFind IO.File.Exists
            match expectedLocationOpt with
            | Some f -> Assembly.LoadFrom f
            | None -> null

        member __.RegisterProbingFolder (folder) =
            // use GetFullPath to ensure that folder is valid
            ignore(IO.Path.GetFullPath folder)
            probingFolders.Add folder

        member __.RegisterRuntimeAssemblyLocationAsProbingFolder (config : TypeProviderConfig) =
            config.RuntimeAssembly
            |> IO.Path.GetDirectoryName
            |> this.RegisterProbingFolder

        interface IDisposable with
            member x.Dispose() =
                disposing.Trigger(x, EventArgs.Empty)
                AppDomain.CurrentDomain.remove_AssemblyResolve handler
    #endif

        member __.AddNamespace (namespaceName,types:list<_>) = otherNamespaces.Add (namespaceName,types)

        // FSharp.Data addition: this method is used by Debug.fs
        member __.Namespaces = Seq.readonly otherNamespaces

        member this.Invalidate() = invalidateE.Trigger(this,EventArgs())

        member __.GetStaticParametersForMethod(mb: MethodBase) =
            // printfn "In GetStaticParametersForMethod"
            match mb with
            | :? ProvidedMethod as t -> t.GetStaticParametersInternal()
            | _ -> [| |]

        member __.ApplyStaticArgumentsForMethod(mb: MethodBase, mangledName, objs) =
            // printfn "In ApplyStaticArgumentsForMethod"
            match mb with
            | :? ProvidedMethod as t -> t.ApplyStaticArgumentsInternal(mangledName, objs) :> MethodBase
            | _ -> failwith (sprintf "ApplyStaticArguments: static parameters for method %s are unexpected" mb.Name)

        interface ITypeProvider with

            [<CLIEvent>]
            override __.Invalidate = invalidateE.Publish

            override __.GetNamespaces() = Array.copy providedNamespaces.Value

            member __.GetInvokerExpression(methodBase, parameters) =
                let rec getInvokerExpression (methodBase : MethodBase) parameters =

                    match methodBase with

                    | :? ProvidedMethod as m when (match methodBase.DeclaringType with :? ProvidedTypeDefinition as pt -> pt.IsErased | _ -> true) ->
                        let convToTgt = id // ok to pass 'id' here as only used for generative expressions, see getFastFuncType
                        let exprFun = m.GetInvokeCodeInternal(false, convToTgt) 
                        let expr = exprFun parameters
                        expr |> expand

                    | :? ProvidedConstructor as m when (match methodBase.DeclaringType with :? ProvidedTypeDefinition as pt -> pt.IsErased | _ -> true) ->
                        let convToTgt = id // ok to pass 'id' here as only used for generative expressions, see getFastFuncType
                        let exprFun = m.GetInvokeCodeInternal(false, convToTgt)
                        let expr = exprFun parameters
                        expr |> expand

                    // Otherwise, assume this is a generative assembly and just emit a call to the constructor or method
                    | :?  ConstructorInfo as cinfo ->

                        Expr.NewObjectUnchecked(cinfo, Array.toList parameters)
                    | :? MethodInfo as minfo ->
                        if minfo.IsStatic then
                            Expr.CallUnchecked(minfo, Array.toList parameters)
                        else
                            Expr.CallUnchecked(parameters.[0], minfo, Array.toList parameters.[1..])

                    | _ -> failwith ("TypeProviderForNamespaces.GetInvokerExpression: not a ProvidedMethod/ProvidedConstructor/ConstructorInfo/MethodInfo, name=" + methodBase.Name + " class=" + methodBase.GetType().FullName)

                and expand expr =
                    match expr with
                    | NewObject(ctor, args) -> getInvokerExpression ctor [| for arg in args -> expand arg|]
                    | Call(inst, mi, args) ->
                        let args =
                            [|
                                match inst with
                                | Some inst -> yield expand inst
                                | _ -> ()
                                yield! List.map expand args
                            |]
                        getInvokerExpression mi args
                    | ShapeCombinationUnchecked(shape, args) -> RebuildShapeCombinationUnchecked(shape, List.map expand args)
                    | ShapeVarUnchecked v -> Expr.Var v
                    | ShapeLambdaUnchecked(v, body) -> Expr.Lambda(v, expand body)
                getInvokerExpression methodBase parameters

            override __.GetStaticParameters(ty) =
                match ty with
                | :? ProvidedTypeDefinition as t ->
                    if ty.Name = t.Name then
                        t.GetStaticParametersInternal()
                    else
                        [| |]
                | _ -> [| |]

            override __.ApplyStaticArguments(ty,typePathAfterArguments:string[],objs) =
                let typePathAfterArguments = typePathAfterArguments.[typePathAfterArguments.Length-1]
                match ty with
                | :? ProvidedTypeDefinition as t -> (t.MakeParametricType(typePathAfterArguments,objs) :> Type)
                | _ -> failwith (sprintf "ApplyStaticArguments: static params for type %s are unexpected" ty.FullName)

#if NO_GENERATIVE
            override __.GetGeneratedAssemblyContents(_assembly) =
                failwith "no generative assemblies"
#else
            override __.GetGeneratedAssemblyContents(assembly:Assembly) =
                //printfn "looking up assembly '%s'" assembly.FullName
                match GlobalProvidedAssemblyElementsTable.theTable.TryGetValue assembly with
                | true,bytes -> bytes.Force()
                | _ ->
                    let bytes = File.ReadAllBytes assembly.ManifestModule.FullyQualifiedName
                    GlobalProvidedAssemblyElementsTable.theTable.[assembly] <- Lazy<_>.CreateFromValue bytes
                    bytes
#endif

