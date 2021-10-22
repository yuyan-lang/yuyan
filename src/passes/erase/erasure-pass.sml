structure ErasurePass =
struct

(* this is structurally very similar to type checking *)
    open TypeCheckingAST
open TypeCheckingASTOps
    open KMachine
    open TypeCheckingPass


    type mapping = UTF8String.t * Type
                    
    type context = mapping list
    type kcontext = (UTF8String.t * kvalue) list

    fun lookup (ctx : context) (n : UTF8String.t) : Type= 
        case ctx of 
             (n1, t1)::cs => if n1 = n then t1 else lookup cs n

    fun klookup (ctx : kcontext) (n : UTF8String.t) : kvalue= 
        case ctx of 
             (n1, t1)::cs => if n1 = n then t1 else klookup cs n

    fun lookupLabel ( ctx : (Label * Type) list) (l : Label) : Type = 
        case ctx of 
             (n1, t1)::cs => if n1 = l then t1 else lookupLabel cs l
    fun klookupLabel ( ctx : (Label * Type) list) (l : Label) : int = 
        case ctx of 
             (n1, t1)::cs => if n1 = l then 0 else klookupLabel cs l+1

      fun lookupLabel3 ( ctx : (Label * EVar *Type) list) (l : Label) : Type = 
        case ctx of 
             (n1, _, t1)::cs => if n1 = l then t1 else lookupLabel3 cs l

      fun klookupLabel3 ( ctx : (Label * EVar *'a ) list) (l : Label) : int = 
        case ctx of 
             (n1, _, t1)::cs => if n1 = l then 0 else klookupLabel3 cs l+1


    fun kseq (k1 : kcomputation) (k2 : kvalue -> kcomputation):kcomputation= KApp(KRet(KAbs(k2)), k1)

    fun typeUnify (a : Type list) : Type =
        case a of
            [] => raise TypeCheckingFailure ("INternal error: empty sum")
            | [t] =>t
            | (x::y :: xs) => if typeEquiv []  x y then typeUnify (x :: xs)
            else raise TypeCheckingFailure ("Type unify failed")


    fun eraseSynExpr (kctx : kcontext) (ctx : context)(e : Expr) : kcomputation =
    (
         case e of
            ExprVar v => KRet(klookup kctx v)
            | UnitExpr => KRet(KUnit)
            | Proj(e, l) => (case synthesizeType ctx e of
                    Prod ls => KProj(eraseSynExpr kctx ctx e,
                        klookupLabel ls l)
            )
            | Case(e,cases) => (case (synthesizeType ctx e) of
                    Sum ls =>
                     KCases ((eraseSynExpr kctx ctx e),(List.tabulate(List.length ls, 
                    fn index => let val  (label,t) = List.nth(ls, index)
                                    val caseIndex = klookupLabel3 cases label
                                    val (_, ev, e) = List.nth(cases, caseIndex)
                                in (fn v => eraseSynExpr ((ev, v) :: kctx)
                                     ((ev, t):: ctx) e 
                                ) end))))
            | LamWithType (t, ev, e) => KRet(KAbs(fn v => eraseSynExpr ((ev, v) :: kctx) ((ev, t)::ctx) e))
            | App (e1, e2) => (case synthesizeType ctx e1
                of Func (t1, t2) => 
                KApp(eraseSynExpr kctx ctx e1, eraseCkExpr kctx ctx e2 t1))
            | TAbs (tv, e2) => eraseSynExpr kctx ctx e2
            | TApp (e2, t) => eraseSynExpr kctx ctx e2
            | Open (e1, (tv, ev, e2)) => (case synthesizeType ctx e1 of
                Exists (tv', tb) => kseq (eraseSynExpr kctx ctx e1) 
                    (fn v => 
                eraseSynExpr ((ev, v)::kctx) ((ev, substTypeInType (TypeVar tv) tv' tb)::ctx) e2 
                    ))
            (* | Fold e2 => Fold (substTypeInExpr tS x e2) *)
            | Unfold e2 => KUnfold(eraseSynExpr kctx ctx e2) 
            (* (case synthesizeType ctx e2 of
                Rho (tv, tb) => substTypeInType (Rho (tv, tb)) tv tb
                | _ => raise TypeCheckingFailure "Cannot unfold non recursive type"
                ) *)
            (* | Fix (ev, e)=> Fix (ev, substTypeInExpr tS x e) *)
            | _ => raise Fail "ep86"
            )

    and eraseCkExpr (kctx : kcontext) (ctx : context) (e : Expr) (tt: Type) (* tt target type *) : kcomputation =
        (case e of
            ExprVar v => KRet(klookup kctx v)
            | UnitExpr => KRet(KUnit)
            | Tuple l => (case tt of 
                Prod ls => 
                    let fun go i values = 
                        if i = List.length l
                        then KRet(KTuple(values))
                        else kseq (eraseCkExpr kctx ctx (List.nth(l, i)) (#2 (List.nth(ls, i)))) 
                                    (fn v => go (i+1) (values@[v]))
                    in go 0 [] end
                )
            | Proj(e, l) => eraseSynExpr kctx ctx (Proj(e,l))
            | Inj (l, e) => (case tt of
                Sum ls => kseq (eraseCkExpr kctx ctx e (lookupLabel ls l)) (fn v => KRet(KInj(l, klookupLabel ls l, v)))
            )
            | Case(e,cases) => (case (synthesizeType ctx e) of
                    Sum ls => 
                    KCases ((eraseSynExpr kctx ctx e),(List.tabulate(List.length ls, 
                    fn index => let val  (label,t) = List.nth(ls, index)
                                    val caseIndex = klookupLabel3 cases label
                                    val (_, ev, e) = List.nth(cases, caseIndex)
                                in (fn v => eraseCkExpr ((ev, v) :: kctx)
                                     ((ev, t):: ctx) e tt
                                ) end)))
            )
            | Lam(ev, eb) => (case tt of
                Func(t1,t2) => 
                KRet(KAbs(fn v => 
                    eraseCkExpr ((ev, v) :: kctx) ((ev, t1):: ctx) eb t2
                ))
                )
            | LamWithType (t, ev, eb) => (case tt of
                Func(t1,t2) => (
                    KRet(KAbs(fn v => 
                        eraseCkExpr ((ev, v) :: kctx) ((ev, t1):: ctx) eb t2
                    ))
                )
            )
            | App (e1, e2) => (case synthesizeType ctx e1
                of Func (t1, t2) => 
                KApp(eraseSynExpr kctx ctx e1, eraseCkExpr kctx ctx e2 t1)
            )
            | TAbs (tv, e2) => (case tt of
                Forall (tv', tb) => 
                    eraseCkExpr kctx ctx e2 (substTypeInType (TypeVar tv) tv' tb)
            )
            | TApp (e2, t) => eraseSynExpr kctx ctx e2
            (* (case synthesizeType ctx e2 of
                Forall (tv, tb) => asserTypeEquiv tt (substTypeInType t tv tb)
                | _ => raise TypeCheckingFailure "TApp on non universal types"
                ) *)
            | Pack (t, e2) => (case tt of
                Exists (tv, tb) => eraseCkExpr kctx ctx e2 (substTypeInType t tv tb)
            )
            | Open (e1, (tv, ev, e2)) => (case synthesizeType ctx e1 of
                Exists (tv', tb) => kseq (eraseSynExpr kctx ctx e1) 
                    (fn v => 
                eraseCkExpr ((ev, v)::kctx) ((ev, substTypeInType (TypeVar tv) tv' tb)::ctx) e2 tt
                    ))
            | Fold e2 => (case tt
                of 
                Rho (tv ,tb) => kseq (eraseCkExpr kctx ctx e2 (substTypeInType (Rho(tv, tb)) tv tb)) 
                        (fn v => KRet(KFold v))
                    )
            | Unfold e2 => KUnfold (eraseSynExpr kctx ctx e2)
            (* (case synthesizeType ctx e2 of
                Rho (tv, tb) => asserTypeEquiv tt (substTypeInType (Rho (tv, tb)) tv tb)
                | _ => raise TypeCheckingFailure "Cannot unfold non recursive type"
                ) *)
            | Fix (ev, e)=>  KFix(fn v => eraseCkExpr ((ev , v) :: kctx) ((ev, tt)::(ctx)) e tt)
            (* let
             fix F = ((\y. f (y y)) (\y. f (y y)))
             (this one causes infinite look)
                    val compiledF = KAbs (fn v => eraseCkExpr ((ev, v)::kctx) ((ev, tt)::ctx) e tt)
                    fun appVV (v1 : kvalue) (v2:kvalue) : kcomputation = KApp(KRet(v1), KRet(v2))
                    val lamydotfyy = KAbs(fn y => KApp(KRet(compiledF),(appVV y y)))
                in appVV lamydotfyy lamydotfyy
            end *)
            (* `checkType ((ev , tt):: ctx) e tt *)
        )

(* this assumes the signature has been fully checked, so no more checking !*)
(* THIS IS VERY IMPORTANT, as this file just copies from type checking but disregards all 
untyped cases *)
(* the lazy comes from the fact that we used lambdas during kcomputation construction *)
(* we can force evaluation by persisting it back and forth *)
    fun eraseSigLazy  (kctx : kcontext ) (ctx : context) (s : Signature) : KMachine.kcomputation =

        (
            (* print ("DEBUG " ^ PrettyPrint.show_typecheckingSig s ) *)
            (* ; *)
            case s of
            [] => (KRet(KUnit))
         | TypeMacro (n, t)::ss => eraseSigLazy kctx ctx (substituteTypeInSignature t n ss)
        | TermTypeJudgment(n, t):: ss => 
            eraseSigLazy kctx ((n, t) :: ctx)  ss
        | TermMacro(n, e) :: ss => 
            kseq (eraseSynExpr kctx ctx e) (fn v => eraseSigLazy ((n, v) :: kctx) ((n, synthesizeType ctx e) :: ctx)  ss)
        | TermDefinition(n, e) :: ss => 
            kseq (eraseCkExpr kctx ctx e (lookup ctx n)) (fn v => eraseSigLazy ((n, v) :: kctx) ctx ss)  
        | [DirectExpr e] =>  (* special treatment on ending direct exprs *)
            (eraseSynExpr kctx ctx e) 
        | DirectExpr e :: ss=> 
            kseq (eraseSynExpr kctx ctx e) (fn _ => eraseSigLazy kctx ctx ss)
        )
        
    fun eraseSig (s : Signature) : KMachine.kcomputation = 
        PersistentKMachine.toKComp (PersistentKMachine.emptyCtx())
                 (PersistentKMachine.fromKComp (eraseSigLazy [] [] s))
end