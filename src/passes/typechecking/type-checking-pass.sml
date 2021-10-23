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
                    
    type context = mapping list

    fun lookup (ctx : context) (n : UTF8String.t) : Type= 
        case ctx of 
            [] => raise TypeCheckingFailure ("name " ^ UTF8String.toString n ^ " not found in context")
            | TermTypeJ(n1, t1)::cs => if n1 = n then t1 else lookup cs n
            | TypeDef(_) :: cs => lookup cs n

    fun applyContextTo (ctx : context) (subst : Type -> Label -> 'a -> 'a) (t : 'a) : 'a = 
        case ctx of
            [] => t
            | TypeDef(n1, t1)::cs => applyContextTo cs subst (subst t1 n1 t)
            | TermTypeJ(_) :: cs => applyContextTo cs subst t
            (* to get the semantics correct, context need to be applied in reverse order *)
            (* no reverse function is called because context is in reverse order *)
    fun applyContextToType (ctx : context) (t : Type) : Type = 
        applyContextTo ctx (fn t => fn  l => fn t1 => 
        (let val _ = if DEBUG then  print (" apply context subsituting "^ PrettyPrint.show_typecheckingType t  
        ^ " for " ^ UTF8String.toString l ^ " in " ^ PrettyPrint.show_typecheckingType t1 ^  "\n") else ()
            val res = substTypeInType t l t1
            val _ =if DEBUG then print (" res is " ^ PrettyPrint.show_typecheckingType res ^ "\n") else ()
            in res end
            )) t
    fun applyContextToExpr (ctx : context) (e : Expr) : Expr = 
        applyContextTo ctx (substTypeInExpr) e
    fun applyContextToSignature (ctx : context) (s : Signature) : Signature = 
        applyContextTo ctx (substituteTypeInSignature) s


    fun lookupLabel ( ctx : (Label * Type) list) (l : Label) : Type = 
        case ctx of 
            [] => raise TypeCheckingFailure ("label " ^ UTF8String.toString l ^ " not found in prod type")
            | (n1, t1)::cs => if n1 = l then t1 else lookupLabel cs l
      fun lookupLabel3 ( ctx : (Label * EVar *Type) list) (l : Label) : Type = 
        case ctx of 
            [] => raise TypeCheckingFailure ("label " ^ UTF8String.toString l ^ " not found in sum type")
            | (n1, _, t1)::cs => if n1 = l then t1 else lookupLabel3 cs l


    fun typeUnify (a : Type list) : Type =
        case a of
            [] => raise TypeCheckingFailure ("INternal error: empty sum")
            | [t] =>t
            | (x::y :: xs) => if typeEquiv []  x y then typeUnify (x :: xs)
            else raise TypeCheckingFailure ("Type unify failed")


    fun synthesizeType (ctx : context)(e : Expr) : Type =
    (
         let val _ = if DEBUG then print ("synthesizing the type for " ^ PrettyPrint.show_typecheckingExpr e ^ "\n") else ()
         in 
         case e of
            ExprVar v => lookup ctx v
            | UnitExpr => UnitType
            | Proj(e, l) => (case synthesizeType ctx e of
                    Prod ls => lookupLabel ls l
                    | _ => raise TypeCheckingFailure "Attempt to project out of non product type"
            )
            | Case(e,cases) => (case (synthesizeType ctx e) of
                    Sum ls => typeUnify (map (fn (l, ev, e) => 
                    synthesizeType (TermTypeJ(ev, (lookupLabel ls l)) :: ctx) e) cases)
                    | _ => raise TypeCheckingFailure "Attempt to case on non sum types")
            | LamWithType (t, ev, e) => Func (t, synthesizeType (TermTypeJ(ev, t)::ctx) e)
            | App (e1, e2) => (case synthesizeType ctx e1
                of Func (t1, t2) => (checkType ctx e2 t1; t2)
                | _ => raise TypeCheckingFailure "Application on nonfunction")
            | TAbs (tv, e2) => Forall (tv, synthesizeType ctx  e2)
            | TApp (e2, t) => (case synthesizeType ctx e2 of
                Forall (tv, tb) => substTypeInType t tv tb
                | _ => raise TypeCheckingFailure "TApp on non universal types"
                )
            (* | Pack (t, e2) => *)
            | Open (e1, (tv, ev, e2)) => (case synthesizeType ctx e1 of
                Exists (tv', tb) => 
                (let val synthesizedType = synthesizeType (TermTypeJ(ev, 
                substTypeInType (TypeVar tv) tv' tb)::ctx) e2
                in if List.exists (fn t => t = tv) (freeTVar synthesizedType)
                    then raise TypeCheckingFailure "Open's type cannot exit scope"
                    else synthesizedType end)
                    | _ => raise TypeCheckingFailure "cannot open non existential types")
            (* | Fold e2 => Fold (substTypeInExpr tS x e2) *)
            | Unfold e2 => (case synthesizeType ctx e2 of
                Rho (tv, tb) => substTypeInType (Rho (tv, tb)) tv tb
                | _ => raise TypeCheckingFailure "Cannot unfold non recursive type"
                )
            | StringLiteral l => BuiltinType(BIString)
            (* | Fix (ev, e)=> Fix (ev, substTypeInExpr tS x e) *)
            | _ => raise TypeCheckingFailure "Expression does type support type synthesis, please specify type"
            end )
        handle TypeCheckingFailure s => 
            raise TypeCheckingFailure (s ^ "\n when synthesizing the type for " ^ PrettyPrint.show_typecheckingExpr e 
            ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx)

    and asserTypeEquiv (t1 : Type) (t2 : Type) : unit =
        if typeEquiv [] t1 t2 then () else raise TypeCheckingFailure ("type mismatch \n 1 : " ^ PrettyPrint.show_typecheckingType t1 ^ " \n 2 : " ^ PrettyPrint.show_typecheckingType t2)
    and checkType (ctx : context) (e : Expr) (tt: Type) (* tt target type *) : unit =
        (let 
            val _ = if DEBUG then  print(  "checking the expr " ^ PrettyPrint.show_typecheckingExpr e ^ 
                " against type " ^ PrettyPrint.show_typecheckingType tt ^ "\n") else ()
        in
            
            case e of
            ExprVar v => if typeEquiv [] (synthesizeType ctx e) tt = false 
                        then raise TypeCheckingFailure ("var type mismatch var is " ^ UTF8String.toString v 
                        ^ " synthesized : " ^ PrettyPrint.show_typecheckingType (synthesizeType ctx e) ^ " against : " 
                        ^ PrettyPrint.show_typecheckingType tt
                        )
                        else ()
            | UnitExpr => if tt = UnitType then () else raise TypeCheckingFailure "unit expr will have unit type"
            | Tuple l => (case tt of 
                Prod ls => if List.length l <> List.length ls
                            then raise TypeCheckingFailure "Prod tuple length mismatch"
                            else (List.tabulate(List.length l, (fn i => 
                            checkType ctx (List.nth(l, i)) (#2 (List.nth(ls, i))))); ())
                | _ => raise TypeCheckingFailure "Expected Prod"
                )
            | Proj(e, l) => asserTypeEquiv (synthesizeType ctx (Proj(e, l))) tt 
            | Inj (l, e) => (case tt of
                Sum ls => checkType ctx e (lookupLabel ls l)
                | _ => raise TypeCheckingFailure "Inj encoutnered "
            )
            | Case(e,cases) => (case (synthesizeType ctx e) of
                    Sum ls => 
                    (map (fn (l, ev, e) => 
                    checkType (TermTypeJ(ev, (lookupLabel ls l)) :: ctx) e tt) cases; ())
                    | _ => raise TypeCheckingFailure "Attempt to case on non sum types")
            | Lam(ev, eb) => (case tt of
                Func(t1,t2) => 
                    checkType (TermTypeJ(ev, t1):: ctx) eb t2
                | _ => raise TypeCheckingFailure "Lambda is not function"
                )
            | LamWithType (t, ev, eb) => (case tt of
                Func(t1,t2) => (asserTypeEquiv t t1;
                    checkType (TermTypeJ(ev, t1):: ctx) eb t2)
                | _ => raise TypeCheckingFailure "Lambda is not function"
                )
            | App (e1, e2) => (case synthesizeType ctx e1
                of Func (t1, t2) => (checkType ctx e2 t1; asserTypeEquiv  t2 tt)
                | _ => raise TypeCheckingFailure "Application on nonfunction")
            | TAbs (tv, e2) => (case tt of
                Forall (tv', tb) => 
                    checkType ctx e2 (substTypeInType (TypeVar tv) tv' tb)
                | _ => raise TypeCheckingFailure "Encountered TAbs"
            )
            | TApp (e2, t) => (case synthesizeType ctx e2 of
                Forall (tv, tb) => asserTypeEquiv tt (substTypeInType t tv tb)
                | _ => raise TypeCheckingFailure "TApp on non universal types"
                )
            | Pack (t, e2) => (case tt of
                Exists (tv, tb) => checkType ctx e2 (substTypeInType t tv  tb)
                | _ => raise TypeCheckingFailure "Pack <-> Exists"
            )
            | Open (e1, (tv, ev, e2)) => (case synthesizeType ctx e1 of
                Exists (tv', tb) => 
                checkType (TermTypeJ(ev, substTypeInType (TypeVar tv) tv' tb)::ctx) e2 tt
                | _ => raise TypeCheckingFailure "cannot open non existential types")
            | Fold e2 => (case tt
                of 
                Rho (tv ,tb) => checkType ctx e2 (substTypeInType (Rho(tv, tb)) tv tb)
                | _ => raise TypeCheckingFailure "Encountered Rho"
                    )
            | Unfold e2 => (case synthesizeType ctx e2 of
                Rho (tv, tb) => asserTypeEquiv tt (substTypeInType (Rho (tv, tb)) tv tb)
                | _ => raise TypeCheckingFailure "Cannot unfold non recursive type"
                )
            | Fix (ev, e)=> checkType (TermTypeJ(ev , tt):: ctx) e tt
            | StringLiteral _ => asserTypeEquiv (BuiltinType(BIString)) tt
        end )
            handle TypeCheckingFailure s =>
            raise TypeCheckingFailure (s ^ "\n when checking the expr " ^ PrettyPrint.show_typecheckingExpr e ^ 
                " against type " ^ PrettyPrint.show_typecheckingType tt
            ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx
             )
    fun typeCheckSignature(ctx : context) (s : Signature) :unit =

        (
            if DEBUG then print ("DEBUG " ^ PrettyPrint.show_typecheckingSig s ^"\n") else (); 

            case s of
            [] => ()
         | TypeMacro (n, t)::ss => if freeTVar (applyContextToType ctx t) <> [] then 
            raise SignatureCheckingFailure ("Type decl contains free var " ^ PrettyPrint.show_strlist (freeTVar (applyContextToType ctx t)) ^" in"  ^ PrettyPrint.show_typecheckingType (applyContextToType ctx t)) else 
            typeCheckSignature (TypeDef(n, t)::ctx) ss
        | TermTypeJudgment(n, t):: ss => if freeTVar (applyContextToType ctx t) <> [] 
            then raise SignatureCheckingFailure ("TermType decl contains free var" ^ PrettyPrint.show_strlist (freeTVar (applyContextToType ctx t)) ^" in "^ PrettyPrint.show_typecheckingType (applyContextToType ctx t)) 
            else typeCheckSignature (TermTypeJ(n, (applyContextToType ctx t)) :: ctx) ss
        | TermMacro(n, e) :: ss => 
            typeCheckSignature (TermTypeJ(n, synthesizeType ctx (applyContextToExpr ctx e)) :: ctx) ss
        | TermDefinition(n, e) :: ss => 
            (checkType ctx (applyContextToExpr ctx e) (lookup ctx n); typeCheckSignature ctx ss)
        | DirectExpr e :: ss=> (synthesizeType ctx (applyContextToExpr ctx e); typeCheckSignature ctx ss))
        handle SignatureCheckingFailure st =>
        raise TypeCheckingFailure (st ^ "\n when checking the signature " ^ PrettyPrint.show_typecheckingSig s 
            ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx
            )
end
