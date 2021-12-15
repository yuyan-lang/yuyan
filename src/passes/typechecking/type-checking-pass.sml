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
  val addToCtxR = appendRelativeMappingToCurrentContext (* R for relative *)
    val addToCtxRL = appendRelativeMappingsToCurrentContext (* L for list *)
    
 
    fun getMapping (c: context ):mapping list = 
        case c of  (Context(cSname, cVis, m)) => m

    exception LookupNotFound of string
    fun lookupMapping (ctx : mapping list) (n : StructureName.t) : Type= 
        case ctx of 
            [] => raise LookupNotFound ("name " ^ StructureName.toStringPlain n ^ " not found in context")
            | TermTypeJ(n1, t1, u)::cs => if StructureName.semanticEqual n1 n then t1 else lookupMapping cs n
            | TypeDef(_) :: cs => lookupMapping cs n

    fun lookup (Context(curSName, _, ctx) : context) (n : StructureName.t) : Type= 
        lookupMapping ctx n
        handle LookupNotFound s1 => 
            (lookupMapping ctx (curSName@n) (* try both absolute and relative path *)
            handle LookupNotFound s2 => 
            raise TypeCheckingFailure (s1 ^ ", \n " ^s2)
            )
        
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
                (* print "HHHH"; *)
                applyContextTo (Context(curName, curVis, cs)) subst 
            ((if StructureName.isPrefix curName n1 then 
            (* the current subsituting name is a prefix! we need also to perform local subsitution *)
            (subst t1 (StructureName.stripPrefix curName n1)) else (fn x => x))
                (subst t1 n1 t)))
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
        applyContextTo ctx (substTypeInRExpr) e
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


    fun synthesizeType (ctx : context)(e : RExpr) : Type =
    (
         let val _ = if DEBUG then print ("synthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e ^ "\n") else ()
         val res = case e of
              RExprVar v => lookup ctx v
            | RUnitExpr => UnitType
            | RProj(e, l) => (case synthesizeType ctx e of
                    Prod ls => lookupLabel ls l
                    | _ => raise TypeCheckingFailure "Attempt to project out of non product type"
            )
            | RCase(e,cases) => (case (synthesizeType ctx e) of
                    Sum ls => typeUnify (map (fn (l, ev, e) => 
                    synthesizeType (addToCtxR (TermTypeJ([ev], (lookupLabel ls l), ())) ctx)
                     e) cases)
                    | _ => raise TypeCheckingFailure "Attempt to case on non sum types")
            | RLamWithType (t, ev, e) => Func (t, synthesizeType 
            (addToCtxR (TermTypeJ([ev], t, ())) ctx) e)
            | RApp (e1, e2) => (case synthesizeType ctx e1
                of Func (t1, t2) => (checkType ctx e2 t1; t2)
                | _ => raise TypeCheckingFailure "Application on nonfunction")
            | RTAbs (tv, e2) => Forall (tv, synthesizeType ctx  e2)
            | RTApp (e2, t) => (case synthesizeType ctx e2 of
                Forall (tv, tb) => substTypeInType t [tv] tb
                | _ => raise TypeCheckingFailure "TApp on non universal types"
                )
            (* | Pack (t, e2) => *)
            | ROpen (e1, (tv, ev, e2)) => (case synthesizeType ctx e1 of
                Exists (tv', tb) => 
                (let val synthesizedType = synthesizeType 
                (addToCtxR (TermTypeJ([ev], 
                substTypeInType (TypeVar [tv]) [tv'] tb,())) ctx) e2
                in if List.exists (fn t => t = [tv]) (freeTVar synthesizedType)
                    then raise TypeCheckingFailure "Open's type cannot exit scope"
                    else synthesizedType end)
                    | _ => raise TypeCheckingFailure "cannot open non existential types")
            (* | Fold e2 => Fold (substTypeInExpr tS x e2) *)
            | RUnfold e2 => (case synthesizeType ctx e2 of
                Rho (tv, tb) => substTypeInType (Rho (tv, tb)) [tv] tb
                | _ => raise TypeCheckingFailure "Cannot unfold non recursive type"
                )
            | RStringLiteral l => BuiltinType(BIString)

            | RLetIn(decls, e) => (case ctx of 
        Context(curName, curVis, bindings) => 
            let val Context(localName, _, newBindings) = typeCheckSignature (Context(curName@StructureName.localName(), curVis, bindings)) decls
                (* assume the typeChecking is behaving properly, 
                no conflicting things will be added to the signature *)
                (* sub context will be determined by whether the signature is private or not ? *)
            in synthesizeType (Context(localName,curVis, newBindings)) e 
            end
        )
            (* | Fix (ev, e)=> Fix (ev, substTypeInExpr tS x e) *)
            | _ => raise TypeCheckingFailure "Expression does type support type synthesis, please specify type"

            val _ = if DEBUG then print ( "synthesize got result " ^ PrettyPrint.show_typecheckingType res^
            " whensynthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e ^ "\n") else ()
            in res
            end )
        handle TypeCheckingFailure s => 
            raise TypeCheckingFailure (s ^ "\n when synthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e 
            ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx)

    and asserTypeEquiv (t1 : Type) (t2 : Type) : unit =
        if typeEquiv [] t1 t2 then () else raise TypeCheckingFailure ("type mismatch \n 1 : " ^ PrettyPrint.show_typecheckingType t1 ^ " \n 2 : " ^ PrettyPrint.show_typecheckingType t2)
    and checkType (ctx : context) (e : RExpr) (tt: Type) (* tt target type *) : unit =
        (let 
            val _ = if DEBUG then  print(  "checking the expr " ^ PrettyPrint.show_typecheckingRExpr e ^ 
                " against type " ^ PrettyPrint.show_typecheckingType tt ^ "\n") else ()
            val res = 
            case e of
            RExprVar v => if typeEquiv [] (synthesizeType ctx e) tt = false 
                        then raise TypeCheckingFailure ("var type mismatch var is " ^ StructureName.toStringPlain v 
                        ^ " synthesized : " ^ PrettyPrint.show_typecheckingType (synthesizeType ctx e) ^ " against : " 
                        ^ PrettyPrint.show_typecheckingType tt
                        )
                        else ()
            | RUnitExpr => if tt = UnitType then () else raise TypeCheckingFailure "unit expr will have unit type"
            | RTuple l => (case tt of 
                Prod ls => if List.length l <> List.length ls
                            then raise TypeCheckingFailure "Prod tuple length mismatch"
                            else (List.tabulate(List.length l, (fn i => 
                            checkType ctx (List.nth(l, i)) (#2 (List.nth(ls, i))))); ())
                | _ => raise TypeCheckingFailure "Expected Prod"
                )
            | RProj(e, l) => asserTypeEquiv (synthesizeType ctx (RProj(e, l))) tt 
            | RInj (l, e) => (case tt of
                Sum ls => checkType ctx e (lookupLabel ls l)
                | _ => raise TypeCheckingFailure "Inj encoutnered "
            )
            | RCase(e,cases) => (case (synthesizeType ctx e) of
                    Sum ls => 
                    (map (fn (l, ev, e) => 
                    checkType (addToCtxR (TermTypeJ([ev], (lookupLabel ls l), ())) ctx) e tt) cases; ())
                    | _ => raise TypeCheckingFailure "Attempt to case on non sum types")
            | RLam(ev, eb) => (case tt of
                Func(t1,t2) => 
                    checkType (addToCtxR (TermTypeJ([ev], t1,())) ctx) eb t2
                | _ => raise TypeCheckingFailure "Lambda is not function"
                )
            | RLamWithType (t, ev, eb) => (case tt of
                Func(t1,t2) => (asserTypeEquiv t t1;
                    checkType (addToCtxR (TermTypeJ([ev], t1, ())) ctx) eb t2)
                | _ => raise TypeCheckingFailure "Lambda is not function"
                )
            | RApp (e1, e2) => (case synthesizeType ctx e1
                of Func (t1, t2) => (checkType ctx e2 t1; asserTypeEquiv  t2 tt)
                | _ => raise TypeCheckingFailure "Application on nonfunction")
            | RTAbs (tv, e2) => (case tt of
                Forall (tv', tb) => 
                    checkType ctx e2 (substTypeInType (TypeVar [tv]) [tv'] tb)
                | _ => raise TypeCheckingFailure "Encountered TAbs"
            )
            | RTApp (e2, t) => (case synthesizeType ctx e2 of
                Forall (tv, tb) => asserTypeEquiv tt (substTypeInType t [tv] tb)
                | _ => raise TypeCheckingFailure "TApp on non universal types"
                )
            | RPack (t, e2) => (case tt of
                Exists (tv, tb) => checkType ctx e2 (substTypeInType t [tv]  tb)
                | _ => raise TypeCheckingFailure "Pack <-> Exists"
            )
            | ROpen (e1, (tv, ev, e2)) => (case synthesizeType ctx e1 of
                Exists (tv', tb) => 
                checkType (addToCtxR (TermTypeJ([ev], substTypeInType (TypeVar [tv]) [tv'] tb, ())) ctx) e2 tt
                | _ => raise TypeCheckingFailure "cannot open non existential types")
            | RFold e2 => (case tt
                of 
                Rho (tv ,tb) => checkType ctx e2 (substTypeInType (Rho(tv, tb)) [tv] tb)
                | _ => raise TypeCheckingFailure "Encountered Rho"
                    )
            | RUnfold e2 => (case synthesizeType ctx e2 of
                Rho (tv, tb) => asserTypeEquiv tt (substTypeInType (Rho (tv, tb)) [tv] tb)
                | _ => raise TypeCheckingFailure "Cannot unfold non recursive type"
                )
            | RFix (ev, e)=> checkType (addToCtxR (TermTypeJ([ev] , tt, ())) ctx) e tt
            | RStringLiteral _ => asserTypeEquiv (BuiltinType(BIString)) tt
            | RLetIn(decls, e) => (case ctx of 
        Context(curName, curVis, bindings) => 
            let val Context(localName, _, newBindings) = typeCheckSignature (Context(curName@StructureName.localName(), curVis, bindings)) decls
                (* assume the typeChecking is behaving properly, 
                no conflicting things will be added to the signature *)
                (* sub context will be determined by whether the signature is private or not ? *)
            in checkType (Context(localName,curVis, newBindings)) e tt
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
    and typeCheckSignature(ctx : context) (s : RSignature) : context =

        (
            if DEBUG then print ("DEBUG " ^ PrettyPrint.show_typecheckingSig s ^
             " in context " ^ PrettyPrint.show_typecheckingpassctx ctx ^"\n") else (); 

            case s of
            [] => ctx
        | RTypeMacro (n, t)::ss => if freeTVar (applyContextToType ctx t) <> [] then 
            raise SignatureCheckingFailure ("Type decl contains free var " ^ PrettyPrint.show_sttrlist (freeTVar (applyContextToType ctx t)) ^" in"  ^ PrettyPrint.show_typecheckingType (applyContextToType ctx t)) else 
            typeCheckSignature (addToCtxR (TypeDef([n], t, ())) ctx) ss
        | RTermTypeJudgment(n, t):: ss => if freeTVar (applyContextToType ctx t) <> [] 
            then raise SignatureCheckingFailure ("TermType decl contains free var" ^ PrettyPrint.show_sttrlist (freeTVar (applyContextToType ctx t)) ^" in "^ PrettyPrint.show_typecheckingType (applyContextToType ctx t)) 
            else typeCheckSignature (addToCtxR (TermTypeJ([n], (applyContextToType ctx t), ())) ctx) ss
        | RTermMacro(n, e) :: ss => 
            typeCheckSignature (addToCtxR (TermTypeJ([n], synthesizeType ctx (applyContextToExpr ctx e), ())) ctx) ss
        | RTermDefinition(n, e) :: ss => 
            (checkType ctx (applyContextToExpr ctx e) (lookup ctx [n]); typeCheckSignature ctx ss)
        | RStructure (vis, sName, decls) :: ss => 
        (case ctx of 
        Context(curName, curVis, bindings) => 
            let val Context(_, _, newBindings) = typeCheckSignature (Context(curName@[sName], vis, bindings)) decls
                (* assume the typeChecking is behaving properly, 
                no conflicting things will be added to the signature *)
                (* sub context will be determined by whether the signature is private or not ? *)
            in typeCheckSignature (Context(curName, curVis, newBindings)) ss
            end
        )
        | ROpenStructure openName :: ss =>
        (case ctx of 
        Context(curName, curVis, bindings) => 
            let val nextContext = nextContextOfOpenStructure curName curVis bindings openName
                (* assume the typeChecking is behaving properly, 
                no conflicting things will be added to the signature *)
                (* sub context will be determined by whether the signature is private or not ? *)
            in typeCheckSignature nextContext ss
            end
        )
        | RDirectExpr e :: ss=> (synthesizeType ctx (applyContextToExpr ctx e); typeCheckSignature ctx ss)
        )
        handle SignatureCheckingFailure st =>
        raise TypeCheckingFailure (st ^ "\n when checking the signature " ^ PrettyPrint.show_typecheckingSig s 
            ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx
            )
end
