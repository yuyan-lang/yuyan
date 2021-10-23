structure ErasurePass =
struct

(* this is structurally very similar to type checking *)
    open TypeCheckingAST
open TypeCheckingASTOps
    open KMachine
    open TypeCheckingPass


    type kmapping = kvalue gmapping
    type kcontext = kvalue gcontext
  fun kaddToCtxR (m : kmapping ) (c : kcontext)= appendRelativeMappingToCurrentContext m c(* R for relative *)
    val kaddToCtxRL = appendRelativeMappingsToCurrentContext (* L for list *)
    
    fun eraseMapping (m : kvalue gmapping) : unit gmapping = 
    case m  of
        TermTypeJ(e, t, v) => TermTypeJ (e, t, ())
        | TypeDef(t, t2, v) => TermTypeJ (t, t2, ())

    fun eraseCtx (ctx : kcontext) : unit gcontext = 
        case ctx of 
            Context(n, v, l) => Context(n, v,map eraseMapping l)
    val n = eraseCtx

    fun klookupMapping (ctx : kmapping list) (n : StructureName.t) : kvalue= 
        case ctx of 
            [] => raise LookupNotFound ("name " ^ StructureName.toStringPlain n ^ " not found in context")
            | TermTypeJ(n1, t1, u)::cs => if n1 = n then u else klookupMapping cs n
            | TypeDef(_) :: cs => klookupMapping cs n

    fun klookup (Context(curSName, _, ctx) : kcontext) (n : StructureName.t) : kvalue= 
        klookupMapping ctx n
        handle LookupNotFound s1 => 
            (klookupMapping ctx (curSName@n) (* try both absolute and relative path *)
            )

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


    fun eraseSynExpr  (ctx : kcontext)(e : Expr) : kcomputation =
    (
         case e of
            ExprVar v => KRet(klookup ctx v)
            | UnitExpr => KRet(KUnit)
            | Proj(e, l) => (case synthesizeType (n ctx) e of
                    Prod ls => KProj(eraseSynExpr ctx e,
                        klookupLabel ls l)
            )
            | Case(e,cases) => (case (synthesizeType (n ctx) e) of
                    Sum ls =>
                     KCases ((eraseSynExpr ctx e),(List.tabulate(List.length ls, 
                    fn index => let val  (label,t) = List.nth(ls, index)
                                    val caseIndex = klookupLabel3 cases label
                                    val (_, ev, e) = List.nth(cases, caseIndex)
                                in (fn v => eraseSynExpr 
                                     (kaddToCtxR (TermTypeJ([ev], t, v)) ctx) e 
                                ) end))))
            | LamWithType (t, ev, e) => KRet(KAbs(fn v => eraseSynExpr (kaddToCtxR (TermTypeJ([ev], t, v)) ctx) e))
            | App (e1, e2) => (case synthesizeType (n ctx) e1
                of Func (t1, t2) => 
                KApp(eraseSynExpr ctx e1, eraseCkExpr ctx e2 t1))
            | TAbs (tv, e2) => eraseSynExpr ctx e2
            | TApp (e2, t) => eraseSynExpr ctx e2
            | Open (e1, (tv, ev, e2)) => (case synthesizeType (n ctx) e1 of
                Exists (tv', tb) => kseq (eraseSynExpr ctx e1) 
                    (fn v => 
                eraseSynExpr (kaddToCtxR (TermTypeJ([ev], substTypeInType (TypeVar [tv]) [tv'] tb, v)) ctx) e2 
                    ))
            (* | Fold e2 => Fold (substTypeInExpr tS x e2) *)
            | Unfold e2 => KUnfold(eraseSynExpr ctx e2) 
            | StringLiteral s => KRet(KBuiltinValue(KbvString s))
            (* (case synthesizeType ctx e2 of
                Rho (tv, tb) => substTypeInType (Rho (tv, tb)) tv tb
                | _ => raise TypeCheckingFailure "Cannot unfold non recursive type"
                ) *)
            (* | Fix (ev, e)=> Fix (ev, substTypeInExpr tS x e) *)
            | _ => raise Fail "ep86"
            )

    and eraseCkExpr  (ctx : kcontext) (e : Expr) (tt: Type) (* tt target type *) : kcomputation =
        (case e of
            ExprVar v => KRet(klookup ctx v)
            | UnitExpr => KRet(KUnit)
            | Tuple l => (case tt of 
                Prod ls => 
                    let fun go i values = 
                        if i = List.length l
                        then KRet(KTuple(values))
                        else kseq (eraseCkExpr ctx (List.nth(l, i)) (#2 (List.nth(ls, i)))) 
                                    (fn v => go (i+1) (values@[v]))
                    in go 0 [] end
                )
            | Proj(e, l) => eraseSynExpr ctx (Proj(e,l))
            | Inj (l, e) => (case tt of
                Sum ls => kseq (eraseCkExpr ctx e (lookupLabel ls l)) (fn v => KRet(KInj(l, klookupLabel ls l, v)))
            )
            | Case(e,cases) => (case (synthesizeType (n ctx) e) of
                    Sum ls => 
                    KCases ((eraseSynExpr ctx e),(List.tabulate(List.length ls, 
                    fn index => let val  (label,t) = List.nth(ls, index)
                                    val caseIndex = klookupLabel3 cases label
                                    val (_, ev, e) = List.nth(cases, caseIndex)
                                in (fn v => eraseCkExpr (
                                     kaddToCtxR (TermTypeJ([ev], t, v)) ctx) e tt
                                ) end)))
            )
            | Lam(ev, eb) => (case tt of
                Func(t1,t2) => 
                KRet(KAbs(fn v => 
                    eraseCkExpr (kaddToCtxR (TermTypeJ([ev], t1, v))ctx) eb t2
                ))
                )
            | LamWithType (t, ev, eb) => (case tt of
                Func(t1,t2) => (
                    KRet(KAbs(fn v => 
                        eraseCkExpr ( kaddToCtxR (TermTypeJ([ev], t1, v))ctx) eb t2
                    ))
                )
            )
            | App (e1, e2) => (case synthesizeType (n ctx) e1
                of Func (t1, t2) => 
                KApp(eraseSynExpr ctx e1, eraseCkExpr ctx e2 t1)
            )
            | TAbs (tv, e2) => (case tt of
                Forall (tv', tb) => 
                    eraseCkExpr ctx e2 (substTypeInType (TypeVar [tv]) [tv'] tb)
            )
            | TApp (e2, t) => eraseSynExpr ctx e2
            (* (case synthesizeType ctx e2 of
                Forall (tv, tb) => asserTypeEquiv tt (substTypeInType t tv tb)
                | _ => raise TypeCheckingFailure "TApp on non universal types"
                ) *)
            | Pack (t, e2) => (case tt of
                Exists (tv, tb) => eraseCkExpr ctx e2 (substTypeInType t [tv] tb)
            )
            | Open (e1, (tv, ev, e2)) => (case synthesizeType (n ctx) e1 of
                Exists (tv', tb) => kseq (eraseSynExpr ctx e1) 
                    (fn v => 
                eraseCkExpr (kaddToCtxR (TermTypeJ([ev], substTypeInType (TypeVar [tv]) [tv'] tb, v))ctx) e2 tt
                    ))
            | Fold e2 => (case tt
                of 
                Rho (tv ,tb) => kseq (eraseCkExpr ctx e2 (substTypeInType (Rho(tv, tb)) [tv] tb)) 
                        (fn v => KRet(KFold v))
                    )
            | Unfold e2 => KUnfold (eraseSynExpr ctx e2)
            (* (case synthesizeType ctx e2 of
                Rho (tv, tb) => asserTypeEquiv tt (substTypeInType (Rho (tv, tb)) tv tb)
                | _ => raise TypeCheckingFailure "Cannot unfold non recursive type"
                ) *)
            | Fix (ev, e)=>  KFix(fn v => eraseCkExpr ( kaddToCtxR (TermTypeJ([ev], tt, v)) ctx) e tt)
            (* let
             fix F = ((\y. f (y y)) (\y. f (y y)))
             (this one causes infinite look)
                    val compiledF = KAbs (fn v => eraseCkExpr ((ev, v)::kctx) ((ev, tt)::ctx) e tt)
                    fun appVV (v1 : kvalue) (v2:kvalue) : kcomputation = KApp(KRet(v1), KRet(v2))
                    val lamydotfyy = KAbs(fn y => KApp(KRet(compiledF),(appVV y y)))
                in appVV lamydotfyy lamydotfyy
            end *)
            | StringLiteral l => (case tt of 
                BuiltinType(BIString) => KRet(KBuiltinValue(KbvString l)))

            (* `checkType ((ev , tt):: ctx) e tt *)
        )

    fun lookAheadForValue (s : Signature) (ename : UTF8String.t) : (UTF8String.t * Expr) * Signature = 
        case s of
         TermDefinition(n, e) :: ss => if n = ename then ((n, e), ss)
                                       else 
                                       let val ((n', e'), ss') = lookAheadForValue ss ename 
                                       in ((n', e'), TermDefinition(n, e)::ss') end
         | (x :: xs) => let val ((n', e'), ss') = lookAheadForValue xs ename 
                                       in ((n', e'), x::ss') end
                                       


(* this assumes the signature has been fully checked, so no more checking !*)
(* THIS IS VERY IMPORTANT, as this file just copies from type checking but disregards all 
untyped cases *)
(* the lazy comes from the fact that we used lambdas during kcomputation construction *)
(* we can force evaluation by persisting it back and forth *)
    fun eraseSigLazy   (ctx : kcontext) (s : Signature) : KMachine.kcomputation =

        (
            (* print ("DEBUG " ^ PrettyPrint.show_typecheckingSig s ) *)
            (* ; *)
            case s of
            [] => (KRet(KUnit))
         | TypeMacro (n, t)::ss => eraseSigLazy (kaddToCtxR (TypeDef([n],t, ())) ctx) ss
        | TermTypeJudgment(n, t):: ss => ( (* todo check all type decls are realized, otherwise 
        erasure is going to fail when type checking passes *)
            let val ((n', e'), ss') = lookAheadForValue ss n
            in 
            kseq (eraseCkExpr ctx (applyContextToExpr (eraseCtx ctx) e') t) 
                (fn v => eraseSigLazy 
            (kaddToCtxR (TermTypeJ([n], (applyContextToType (eraseCtx ctx) t), v)) ctx)  ss')
            end
        )
        | TermMacro(n, e) :: ss => 
            kseq (eraseSynExpr ctx (applyContextToExpr (eraseCtx ctx) e)) 
            (fn v => eraseSigLazy  (addToCtxR (TermTypeJ([n], synthesizeType (eraseCtx ctx) (applyContextToExpr 
                    (eraseCtx ctx) e), v)) ctx)  ss)
        | [DirectExpr e] =>  (* special treatment on ending direct exprs *)
            (eraseSynExpr ctx (applyContextToExpr (eraseCtx ctx) e)) 
        | DirectExpr e :: ss=> 
            kseq (eraseSynExpr ctx (applyContextToExpr (eraseCtx ctx) e)) (fn _ => eraseSigLazy ctx ss)
        )
        

end