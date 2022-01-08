structure TypeCheckingPass = struct
open TypeCheckingAST
open TypeCheckingASTOps

    (* val DEBUG = true *)
    val DEBUG = false


    (*  !!! we assume the context is well formed in the sense that 
    all term type judgments have no free type variables !!! *) 
    (* so before anything is added to context, must perform substitution first ! *)
    (* the context is not a telescope !!! *)
    (* This also applies to type definitions! They must be expanded as well! *)
    (* ^^^ THis is not true !!! *)
    (* this is called the closed-world assumption and in practice, this 
    helps to reduce bugs during type checking *)
    (* also assume no name clash  *)
           
   
                    (* curStructure, curVisibility and mapping *)
  val addToCtxA = appendAbsoluteMappingToCurrentContext (* A for relative *)
  val addToCtxR = appendRelativeMappingToCurrentContext (* R for relative *)
    val addToCtxRL = appendRelativeMappingsToCurrentContext (* L for list *)
    
 
    fun getMapping (c: context ):mapping list = 
        case c of  (Context(cSname, cVis, m)) => m

    exception LookupNotFound of string
    fun lookupMapping (ctx : mapping list) (n : StructureName.t) (curSName : StructureName.t ): (StructureName.t * Type)= 
        case ctx of 
            [] => raise LookupNotFound ("name " ^ StructureName.toStringPlain n ^ " not found in context")
            | TermTypeJ(n1, t1, u)::cs => 
                (case checkRefersTo n1 n curSName 
                of SOME(cname) => (cname, t1)
                | NONE => lookupMapping cs n curSName
                )
            | TypeDef(_) :: cs => lookupMapping cs n curSName

(* require lookup to add name qualification if references local structure *)
    fun lookup (Context(curSName, _, ctx) : context) (n : StructureName.t) : (StructureName.t * Type)= 
        let val ntp = lookupMapping ctx n curSName in ntp end
        handle LookupNotFound s1 => 
            (* (let val tp = lookupMapping ctx (curSName@n)
             (* try both absolute and relative path *)
             in (curSName@n, tp) end *)
            (* handle LookupNotFound s2 =>  *)
            raise TypeCheckingFailure (s1 ^ ", \n " )
            (* ) *)
        
    fun nextContextOfOpenStructure  (curName : StructureName.t) (curVis : bool) (bindings : 'a gmapping list) 
    (openName : StructureName.t)=

     Context(curName, curVis, 
            (* extract all bindings from bindings in order and put them into the current context *)
                    List.mapPartial (fn x => 
                    case x of TermTypeJ(name, t, u) => 
                    if StructureName.isPrefix (curName@openName) name   (* relative path *)
                    then SOME (TermTypeJ(curName@(StructureName.stripPrefix (curName@openName) name), t, u))
                    else 
                    if StructureName.isPrefix openName name   (* absolute path *)
                    then SOME (TermTypeJ((StructureName.stripPrefix openName name), t, u))
                    else  NONE
                    | TypeDef(name, t, u) =>
                    if StructureName.isPrefix (curName@openName) name (* relative path *)
                    then SOME (TypeDef(curName@(StructureName.stripPrefix (curName@openName) name), t, u))
                    else 
                    if StructureName.isPrefix (openName) name (* absolute path *)
                    then SOME (TypeDef((StructureName.stripPrefix openName name), t, u))
                    else NONE   
                    ) bindings @ bindings
                )

        

    fun applyContextTo (ctx : context) (subst : Type -> StructureName.t -> 'a -> 'a) (t : 'a) : 'a = 
    (
        (* print ("apply ctx to gen called"  ^ Int.toString(case ctx of (Context(_, _, l)) => length l)^ "\n") ; *)
        case ctx of Context(curName, curVis, mapl) =>
        (case mapl of
            [] => t
            | TypeDef(n1, t1, u)::cs => (
                let val stepOne = (subst t1 n1 t)
                    val stepTwo = (subst t1 (StructureName.stripPrefixOnAgreedParts curName n1) stepOne)
                    val rest = applyContextTo (Context(curName, curVis, cs)) subst stepTwo
                    in rest end
                )
                (* print "HHHH"; *)
            (* the current subsituting name is a prefix! we need also to perform local subsitution *)
            (* always eagerly perform prefix-stripped substitutions *)
            | TermTypeJ(_) :: cs => applyContextTo (Context(curName, curVis, cs)) subst t)
            (* to get the semantics correct, context need to be applied in reverse order *)
            (* no reverse function is called because context is in reverse order *)
    )
    fun applyContextToType (ctx : context) (t : Type) : Type = 
    (
        (* print "apply ctx to type called\n"; *)
        applyContextTo ctx (fn t => fn  l => fn t1 => 
        (let 
        val _ = 
        if DEBUG then  print (" apply context subsituting "^ PrettyPrint.show_typecheckingType t  
        ^ " for " ^ StructureName.toStringPlain l ^ " in " ^ PrettyPrint.show_typecheckingType t1 ^  "\n") else ()
            val res = substTypeInType t l t1
            val _ =if DEBUG then print (" res is " ^ PrettyPrint.show_typecheckingType res ^ "\n") else ()
            in res end
            )) t
    )
    fun applyContextToExpr (ctx : context) (e : RExpr) : RExpr = 
        applyContextTo ctx (fn t => fn  l => fn e1 => 
        (let 
        val _ = 
        if DEBUG then  print (" apply context subsituting "^ PrettyPrint.show_typecheckingType t  
        ^ " for " ^ StructureName.toStringPlain l ^ " in " ^ PrettyPrint.show_typecheckingRExpr e1 ^  "\n") else ()
            val res = substTypeInRExpr t l e1
            val _ =if DEBUG then print (" res is " ^ PrettyPrint.show_typecheckingRExpr res ^ "\n") else ()
            in res end
            )) e
    fun applyContextToSignature (ctx : context) (s : RSignature) : RSignature = 
        applyContextTo ctx (substituteTypeInRSignature) s


    fun lookupLabel ( ctx : (Label * Type) list) (l : Label) : Type = 
        case ctx of 
            [] => raise TypeCheckingFailure ("label " ^ UTF8String.toString l ^ " not found in prod type")
            | (n1, t1)::cs => if UTF8String.semanticEqual n1 l then t1 else lookupLabel cs l
      fun lookupLabel3 ( ctx : (Label * EVar *Type) list) (l : Label) : Type = 
        case ctx of 
            [] => raise TypeCheckingFailure ("label " ^ UTF8String.toString l ^ " not found in sum type")
            | (n1, _, t1)::cs => if UTF8String.semanticEqual n1 l then t1 else lookupLabel3 cs l


    fun typeUnify (a : Type list) : Type =
        case a of
            [] => raise TypeCheckingFailure ("INternal error: empty sum")
            | [t] =>t
            | (x::y :: xs) => if typeEquiv []  x y then typeUnify (x :: xs)
            else raise TypeCheckingFailure ("Type unify failed")


    fun synthesizeType (ctx : context)(e : RExpr) : (CExpr * Type) =
    (
         let val _ = if DEBUG then print ("synthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e ^ "\n") else ()
         val res = case e of
              RExprVar v => let val (canonicalName, tp)  =  lookup ctx v
                            in (CExprVar canonicalName, tp) end
            | RUnitExpr => (CUnitExpr, UnitType)
            | RProj(e, l) => (case synthesizeType ctx e of
                    (ce, Prod ls) => (CProj(ce, l, Prod ls), lookupLabel ls l)
                    | _ => raise TypeCheckingFailure "Attempt to project out of non product type"
            )
            | RCase(e,cases) => (case (synthesizeType ctx e) of
                    (ce, Sum ls) => let 
                    val checkedCases = (map (fn (l, ev, e) => 
                    synthesizeType (addToCtxA (TermTypeJ([ev], (lookupLabel ls l), ())) ctx)
                     e) cases)
                    val casesTypes = map (#2) checkedCases
                    val checkedExprs = map (#1) checkedCases
                    val returnType = typeUnify casesTypes
                    in 
                        (CCase ((Sum ls, ce), (List.tabulate(length cases, fn i => 
                            let val (label, evar, _) = List.nth(cases, i)
                            in (label, evar, #1 (List.nth(checkedCases, i)))
                            end
                            )), returnType), returnType)
                    end
                    | _ => raise TypeCheckingFailure "Attempt to case on non sum types")
            | RLamWithType (t, ev, e) => let
                val (bodyExpr, returnType) = synthesizeType (addToCtxA (TermTypeJ([ev], t, ())) ctx) e
                in (CLam(ev, bodyExpr, Func (t, returnType)), Func(t, returnType))
                end
            | RApp (e1, e2) => (case synthesizeType ctx e1
                of (ce1, Func (t1, t2)) => 
                (CApp (ce1,(checkType ctx e2 t1), Func(t1,t2)), t2)
                | _ => raise TypeCheckingFailure "Application on nonfunction")
            | RTAbs (tv, e2) => let val (ce2, bodyType) =  synthesizeType ctx  e2
            in (CTAbs(tv, ce2, Forall (tv, bodyType)), Forall (tv, bodyType)) end
            | RTApp (e2, t) => (case synthesizeType ctx e2 of
                (ce2, Forall (tv, tb)) => (CTApp(ce2,t, Forall(tv, tb)), substTypeInType t [tv] tb)
                | _ => raise TypeCheckingFailure "TApp on non universal types"
                )
            (* | Pack (t, e2) => *)
            | ROpen (e1, (tv, ev, e2)) => (case synthesizeType ctx e1 of
                (ce1, Exists (tv', tb)) => 
                (let val (ce2, synthesizedType) = synthesizeType 
                (addToCtxA (TermTypeJ([ev], 
                substTypeInType (TypeVar [tv]) [tv'] tb,())) ctx) e2
                in if List.exists (fn t => t = [tv]) (freeTVar synthesizedType)
                    then raise TypeCheckingFailure "Open's type cannot exit scope"
                    else (COpen((Exists(tv', tb), ce1), (tv, ev, ce2), synthesizedType), synthesizedType)
                    end)
                    | _ => raise TypeCheckingFailure "cannot open non existential types")
            (* | Fold e2 => Fold (substTypeInExpr tS x e2) *)
            | RUnfold e2 => (case synthesizeType ctx e2 of
                (ce2, Rho (tv, tb)) => (CUnfold(ce2, Rho(tv, tb)), substTypeInType (Rho (tv, tb)) [tv] tb)
                | _ => raise TypeCheckingFailure "Cannot unfold non recursive type"
                )
            | RStringLiteral l => (CStringLiteral l, BuiltinType(BIString))
            | RIntConstant i => (CIntConstant i, BuiltinType(BIInt))
            | RRealConstant r => (CRealConstant r, BuiltinType(BIReal))
            

            | RLetIn(decls, e) => (case ctx of 
                Context(curName, curVis, bindings) => 
                    let val (Context(localName, _, newBindings), csig) = typeCheckSignature (Context(curName@StructureName.localName(), curVis, bindings)) decls []
                        (* assume the typeChecking is behaving properly, 
                        no conflicting things will be added to the signature *)
                        (* sub context will be determined by whether the signature is private or not ? *)
                        val (ce, synthesizedType) = synthesizeType (Context(localName,curVis, newBindings)) e
                    in 
                        (CLetIn(csig, ce, synthesizedType), synthesizedType)
                    end
                )
            (* | Fix (ev, e)=> Fix (ev, substTypeInExpr tS x e) *)
            | _ => raise TypeCheckingFailure "Expression does type support type synthesis, please specify type"

            val _ = if DEBUG then print ( "synthesize got result " ^ PrettyPrint.show_typecheckingType (#2 res)^
            " whensynthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e ^ "\n") else ()
            in res
            end )
        handle TypeCheckingFailure s => 
            raise TypeCheckingFailure (s ^ "\n when synthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e 
            ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx)

    and asserTypeEquiv (t1 : Type) (t2 : Type) : unit =
        if typeEquiv [] t1 t2 then () else raise TypeCheckingFailure ("type mismatch \n 1 : " ^ PrettyPrint.show_typecheckingType t1 ^ " \n 2 : " ^ PrettyPrint.show_typecheckingType t2)
    and checkType (ctx : context) (e : RExpr) (tt: Type) (* tt target type *) : CExpr =
        (let 
            val _ = if DEBUG then  print(  "checking the expr " ^ PrettyPrint.show_typecheckingRExpr e ^ 
                " against type " ^ PrettyPrint.show_typecheckingType tt ^ "\n") else ()
            val res = 
            case e of
            RExprVar v => 
            let val (synthExpr, synthType) = (synthesizeType ctx e)
            in if typeEquiv [] (synthType) tt = false 
                        then raise TypeCheckingFailure ("var type mismatch var is " ^ StructureName.toStringPlain v 
                        ^ " synthesized : " ^ PrettyPrint.show_typecheckingType (#2 (synthesizeType ctx e)) ^ " against : " 
                        ^ PrettyPrint.show_typecheckingType tt
                        )
                        else synthExpr
                        end
            | RUnitExpr => if tt = UnitType then CUnitExpr else raise TypeCheckingFailure "unit expr will have unit type"
            | RTuple l => (case tt of 
                Prod ls => if List.length l <> List.length ls
                            then raise TypeCheckingFailure "Prod tuple length mismatch"
                            else (CTuple (List.tabulate(List.length l, (fn i => 
                            checkType ctx (List.nth(l, i)) (#2 (List.nth(ls, i))))),
                                    (Prod ls)))
                | _ => raise TypeCheckingFailure "Expected Prod"
                )
            | RProj(e, l) =>
            (case synthesizeType ctx (RProj(e, l)) of
                (CProj(ce, l, prodType), synthType) => (asserTypeEquiv synthType tt;
                CProj(ce, l, prodType))
                | _ => raise Fail "tcp229")

            | RInj (l, e) => (case tt of
                Sum ls => CInj(l, checkType ctx e (lookupLabel ls l), Sum ls)
                | _ => raise TypeCheckingFailure "Inj encoutnered "
            )
            | RCase(e,cases) => (case (synthesizeType ctx e) of
                    (ce, Sum ls) => 
                    CCase((Sum ls, ce), 
                    (map (fn (l, ev, e) => 
                    (l, ev, checkType (addToCtxA (TermTypeJ([ev], (lookupLabel ls l), ())) ctx) e tt)
                    ) cases), tt)
                    | _ => raise TypeCheckingFailure "Attempt to case on non sum types")
            | RLam(ev, eb) => (case tt of
                Func(t1,t2) => 
                    CLam(ev, checkType (addToCtxA (TermTypeJ([ev], t1,())) ctx) eb t2, tt)
                | _ => raise TypeCheckingFailure "Lambda is not function"
                )
            | RLamWithType (t, ev, eb) => (case tt of
                Func(t1,t2) => (asserTypeEquiv t t1;
                    (CLam(ev, checkType (addToCtxA (TermTypeJ([ev], t1, ())) ctx) eb t2, tt)))
                | _ => raise TypeCheckingFailure "Lambda is not function"
                )
            | RApp (e1, e2) => (case synthesizeType ctx e1
                of (ce1, Func (t1, t2)) => (
                asserTypeEquiv  t2 tt;
                    CApp(ce1, checkType ctx e2 t1, Func(t1, t2))
                )
                | _ => raise TypeCheckingFailure "Application on nonfunction")
            | RTAbs (tv, e2) => (case tt of
                Forall (tv', tb) => 
            CTAbs (tv, checkType ctx e2 (substTypeInType (TypeVar [tv]) [tv'] tb), tt)
                | _ => raise TypeCheckingFailure "Encountered TAbs"
            )
            | RTApp (e2, t) => (case synthesizeType ctx e2 of
                (ce2, Forall (tv, tb)) => (asserTypeEquiv tt (substTypeInType t [tv] tb);
                CTApp(ce2, t, Forall(tv, tb)))
                | _ => raise TypeCheckingFailure "TApp on non universal types"
                )
            | RPack (t, e2) => (case tt of
                Exists (tv, tb) => 
                CPack(t, checkType ctx e2 (substTypeInType t [tv]  tb), tt)
                | _ => raise TypeCheckingFailure "Pack <-> Exists"
            )
            | ROpen (e1, (tv, ev, e2)) => (case synthesizeType ctx e1 of
                (ce1, Exists (tv', tb)) => 
                let 
                    val ce2 =checkType (addToCtxA (TermTypeJ([ev], substTypeInType (TypeVar [tv]) [tv'] tb, ())) ctx) e2 tt
                in COpen((Exists (tv', tb), ce1), (tv, ev, ce2), tt) end
                | _ => raise TypeCheckingFailure "cannot open non existential types")
            | RFold e2 => (case tt
                of 
                Rho (tv ,tb) => 
                let val ce2 =checkType ctx e2 (substTypeInType (Rho(tv, tb)) [tv] tb)
                in CFold(ce2, tt) end
                | _ => raise TypeCheckingFailure "Expected Rho"
                    )
            | RUnfold e2 => (case synthesizeType ctx e2 of
                (ce2, Rho (tv, tb)) =>(
                    asserTypeEquiv tt (substTypeInType (Rho (tv, tb)) [tv] tb);
                    CUnfold(ce2, Rho(tv,tb)))
                | _ => raise TypeCheckingFailure "Cannot unfold non recursive type"
                )
            | RFix (ev, e)=> CFix(ev,checkType (addToCtxA (TermTypeJ([ev] , tt, ())) ctx) e tt, tt)
            | RStringLiteral s => (asserTypeEquiv (BuiltinType(BIString)) tt; CStringLiteral s)
            | RIntConstant i => (asserTypeEquiv (BuiltinType(BIInt)) tt; CIntConstant i)
            | RRealConstant r => (asserTypeEquiv (BuiltinType(BIReal)) tt; CRealConstant r)
            | RFfiCCall (e1, e2) => (
                case e1 of
                    RStringLiteral cfuncName => 
                        let fun elaborateArguments  (args) = 
                            CFfiCCall(cfuncName, args)
                        in
                                    (case e2 of 
                                        RExprVar v => elaborateArguments [v]
                                        | RTuple l => elaborateArguments (map (fn arg => case arg of 
                                            RExprVar v => v
                                            | _ => raise TypeCheckingFailure "ccall arguments must be immediate values"
                                            ) l)
                                    )
                        end
                    | _ => raise TypeCheckingFailure "First argument of the ccall must be string literal"

            )
            | RLetIn(decls, e) => (case ctx of 
        Context(curName, curVis, bindings) => 
            let val (Context(localName, _, newBindings), csig) = typeCheckSignature (Context(curName@StructureName.localName(), curVis, bindings)) decls []
                (* assume the typeChecking is behaving properly, 
                no conflicting things will be added to the signature *)
                (* sub context will be determined by whether the signature is private or not ? *)
            in CLetIn(csig, checkType (Context(localName,curVis, newBindings)) e tt, tt)
            end
        )
        in res
        end 
)
            handle TypeCheckingFailure s =>
            raise TypeCheckingFailure (s ^ "\n when checking the expr " ^ PrettyPrint.show_typecheckingRExpr e ^ 
                " against type " ^ PrettyPrint.show_typecheckingType tt
            ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx
             )
             (* type check signature will return all bindings *)
    and typeCheckSignature(ctx : context) (s : RSignature) (acc : CSignature) : (context * CSignature) =

        (
            if DEBUG then print ("DEBUG " ^ PrettyPrint.show_typecheckingRSig s ^
             " in context " ^ PrettyPrint.show_typecheckingpassctx ctx ^"\n") else (); 

            case s of
            [] => (ctx, acc)
            (* normalize should not change the set of free variables *)
        | RTypeMacro (n, t)::ss => if freeTVar (applyContextToType ctx t) <> [] then 
            raise SignatureCheckingFailure ("Type decl contains free var " ^ PrettyPrint.show_sttrlist (freeTVar (applyContextToType ctx t)) ^" in"  ^ PrettyPrint.show_typecheckingType (applyContextToType ctx t)) 
            else 
            typeCheckSignature (addToCtxR (TypeDef([n], normalizeType (applyContextToType ctx t), ())) ctx) ss (acc)
        | RTermTypeJudgment(n, t):: ss => if freeTVar (applyContextToType ctx t) <> [] 
            then raise SignatureCheckingFailure ("TermType decl contains free var" ^ PrettyPrint.show_sttrlist (freeTVar (applyContextToType ctx t)) ^" in "^ PrettyPrint.show_typecheckingType (applyContextToType ctx t)) 
            else typeCheckSignature (addToCtxR (TermTypeJ([n], (normalizeType (applyContextToType ctx t)), ())) ctx) ss (acc)
        | RTermMacro(n, e) :: ss => 
            let val (transformedExpr , synthesizedType) = synthesizeType ctx (applyContextToExpr ctx e)
            in
                typeCheckSignature (addToCtxR (TermTypeJ([n], synthesizedType, ())) ctx) ss (acc@[CTermDefinition((getCurSName ctx)@[n], transformedExpr)])
            end
        | RTermDefinition(n, e) :: ss => 
        let val transformedExpr = checkType ctx (applyContextToExpr ctx e) (#2 (lookup ctx [n]))
        in typeCheckSignature ctx ss (acc@[CTermDefinition((getCurSName ctx)@[n], transformedExpr)])
        end
        | RStructure (vis, sName, decls) :: ss => 
        (case ctx of 
        Context(curName, curVis, bindings) => 
            let val (Context(_, _, newBindings), checkedSig) = typeCheckSignature (Context(curName@[sName], vis, bindings)) decls []
                (* assume the typeChecking is behaving properly, 
                no conflicting things will be added to the signature *)
                (* sub context will be determined by whether the signature is private or not ? *)
            in typeCheckSignature (Context(curName, curVis, newBindings)) ss (acc@checkedSig)
            end
        )
        | ROpenStructure openName :: ss =>
        (case ctx of 
        Context(curName, curVis, bindings) => 
            let val nextContext = nextContextOfOpenStructure curName curVis bindings openName
                (* assume the typeChecking is behaving properly, 
                no conflicting things will be added to the signature *)
                (* sub context will be determined by whether the signature is private or not ? *)
            in typeCheckSignature nextContext ss (acc)
            end
        )
        | RDirectExpr e :: ss=> 
            let 
            val (checkedExpr, synthesizedType) = (synthesizeType ctx (applyContextToExpr ctx e))
            in typeCheckSignature ctx ss (acc@[CDirectExpr checkedExpr])
            end
        )
        handle SignatureCheckingFailure st =>
        raise TypeCheckingFailure (st ^ "\n when checking the signature " ^ PrettyPrint.show_typecheckingRSig s 
            ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx
            )
end
