structure TypeCheckingPass = struct
open TypeCheckingAST

    type mapping = UTF8String.t * Type
                    
    type context = mapping list

    fun lookup (ctx : context) (n : UTF8String.t) : Type= 
        case ctx of 
            [] => raise TypeCheckingFailure ("name " ^ UTF8String.toString n ^ " not found in context")
            | (n1, t1)::cs => if n1 = n then t1 else lookup cs n

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
         case e of
            ExprVar v => lookup ctx v
            | UnitExpr => UnitType
            | Proj(e, l) => (case synthesizeType ctx e of
                    Prod ls => lookupLabel ls l
                    | _ => raise TypeCheckingFailure "Attempt to project out of non product type"
            )
            | Case(e,cases) => (case (synthesizeType ctx e) of
                    Sum ls => typeUnify (map (fn (l, ev, e) => 
                    synthesizeType ((ev, (lookupLabel ls l)) :: ctx) e) cases)
                    | _ => raise TypeCheckingFailure "Attempt to case on non sum types")
            | LamWithType (t, ev, e) => Func (t, synthesizeType ((ev, t)::ctx) e)
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
                (let val synthesizedType = synthesizeType ((ev, 
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
            (* | Fix (ev, e)=> Fix (ev, substTypeInExpr tS x e) *)
            | _ => raise TypeCheckingFailure "Expression does type support type synthesis, please specify type"
    and asserTypeEquiv (t1 : Type) (t2 : Type) : unit =
        if typeEquiv [] t1 t2 then () else raise TypeCheckingFailure "type mismatch"
    and checkType (ctx : context) (e : Expr) (tt: Type) (* tt target type *) : unit =
         case e of
            ExprVar v => if typeEquiv [] (synthesizeType ctx e) tt = false 
                        then raise TypeCheckingFailure "var type mismatch"
                        else ()
            | UnitExpr => if tt = UnitType then () else raise TypeCheckingFailure "unit expr will have unit type"
            | Tuple l => (case tt of 
                Prod ls => if List.length l <> List.length ls
                            then raise TypeCheckingFailure "Prod tuple length mismatch"
                            else (List.tabulate(List.length l, (fn i => 
                            checkType ctx (List.nth(l, i)) (#2 (List.nth(ls, i))))); ())
                | _ => raise TypeCheckingFailure "Expected Prod"
                )
            | Proj(e, l) => if typeEquiv [] (synthesizeType ctx (Proj(e, l))) tt then () else raise TypeCheckingFailure "type mismatch"
            | Inj (l, e) => (case tt of
                Sum ls => checkType ctx e (lookupLabel ls l)
                | _ => raise TypeCheckingFailure "Inj encoutnered "
            )
            | Case(e,cases) => (case (synthesizeType ctx e) of
                    Sum ls => 
                    (map (fn (l, ev, e) => 
                    checkType ((ev, (lookupLabel ls l)) :: ctx) e tt) cases; ())
                    | _ => raise TypeCheckingFailure "Attempt to case on non sum types")
            | Lam(ev, eb) => (case tt of
                Func(t1,t2) => 
                    checkType ((ev, t1):: ctx) eb t2
                | _ => raise TypeCheckingFailure "Lambda is not function"
                )
            | LamWithType (t, ev, eb) => (case tt of
                Func(t1,t2) => (asserTypeEquiv t t1;
                    checkType ((ev, t1):: ctx) eb t2)
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
                checkType ((ev, substTypeInType (TypeVar tv) tv' tb)::ctx) e2 tt
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
            | Fix (ev, e)=> checkType ((ev , tt):: ctx) e tt
    fun typeCheckSignature(ctx : context) (s : Signature) :unit =
        case s of
            [] => ()
         | TypeMacro (n, t)::ss => if freeTVar t <> [] then raise TypeCheckingFailure "Type decl contains free var" else 
            typeCheckSignature ctx (substituteTypeInSignature t n ss)
        | TermTypeJudgment(n, t):: ss => if freeTVar t <> [] then raise TypeCheckingFailure "TermType decl contains free var" else 
            typeCheckSignature ((n, t) :: ctx) ss
        | TermMacro(n, e) :: ss => 
            typeCheckSignature ((n, synthesizeType ctx e) :: ctx) ss
        | TermDefinition(n, e) :: ss => 
            (checkType ctx e (lookup ctx n); typeCheckSignature ctx ss)
        | DirectExpr e :: ss=> (synthesizeType ctx e; typeCheckSignature ctx ss)
end
