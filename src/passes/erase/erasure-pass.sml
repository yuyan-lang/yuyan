structure ErasurePass =
struct

(* this is structurally very similar to type checking *)
    open TypeCheckingAST
open TypeCheckingASTOps
    open KMachine
    open PersistentKMachine
    open TypeCheckingPass


    type kmapping = pkvalue gmapping
    type kcontext = pkvalue gcontext
  fun kaddToCtxR (m : kmapping ) (c : kcontext)= appendRelativeMappingToCurrentContext m c(* R for relative *)
    val kaddToCtxRL = appendRelativeMappingsToCurrentContext (* L for list *)
    
    fun eraseMapping (m : pkvalue gmapping) : unit gmapping = 
    case m  of
        TermTypeJ(e, t, v) => TermTypeJ (e, t, ())
        | TypeDef(t, t2, v) => TypeDef (t, t2, ())

    fun eraseCtx (ctx : kcontext) : unit gcontext = 
        case ctx of 
            Context(n, v, l) => Context(n, v,map eraseMapping l)
    val n = eraseCtx

    fun klookupMapping (ctx : kmapping list) (n : StructureName.t) : pkvalue= 
        case ctx of 
            [] => raise LookupNotFound ("name " ^ StructureName.toStringPlain n ^ " not found in context")
            | TermTypeJ(n1, t1, u)::cs => if StructureName.semanticEqual n1 n then u else klookupMapping cs n
            | TypeDef(_) :: cs => klookupMapping cs n

    fun klookup (Context(curSName, _, ctx) : kcontext) (n : StructureName.t) : pkvalue= 
        klookupMapping ctx n
        handle LookupNotFound s1 => 
            (klookupMapping ctx (curSName@n) (* try both absolute and relative path *)
            )

    fun lookupLabel ( ctx : (Label * Type) list) (l : Label) : Type = 
        case ctx of 
             (n1, t1)::cs => if UTF8String.semanticEqual n1 l then t1 else lookupLabel cs l
    fun klookupLabel ( ctx : (Label * Type) list) (l : Label) : int = 
        case ctx of 
             (n1, t1)::cs => if UTF8String.semanticEqual n1 l then 0 else klookupLabel cs l+1

      fun lookupLabel3 ( ctx : (Label * EVar *Type) list) (l : Label) : Type = 
        case ctx of 
             (n1, _, t1)::cs => if UTF8String.semanticEqual n1 l then t1 else lookupLabel3 cs l

      fun klookupLabel3 ( ctx : (Label * EVar *'a ) list) (l : Label) : int = 
        case ctx of 
             (n1, _, t1)::cs => if UTF8String.semanticEqual n1 l then 0 else klookupLabel3 cs l+1


    fun kseqSig (k1 : pkcomputation) (k2 : int * pkcomputation):pkcomputation= 
    let 
    (* val _ = print ("kseqSig called on k1 = " ^ PrettyPrint.show_pkcomputation k1 ^ 
    " k2 = " ^ Int.toString(#1 k2) ^ " , " ^ 
    PrettyPrint.show_pkcomputation (#2 k2) ^"\n"); *)
            val res = PKApp(PKRet(PKAbs(k2)), k1)
            (* val _ = print ("kseqSig result is " ^ PrettyPrint.show_pkcomputation res ^"\n") *)
        in res end
    fun kseq (k1 : pkcomputation) (k2 : pkvalue -> pkcomputation):pkcomputation= 
    let val boundId = UID.next() in 
        kseqSig k1 (boundId, k2(PKVar(boundId))) end
        
    (* fun kseqSig (k1 : kcomputation) (k2 : (kvalue * kcontext) -> (kcomputation * kcontext)):kcomputation = 
    KApp(KRet(KAbs(fn (kv, kc) => (#1 (k2 v c)) )), k1) *)

    fun typeUnify (a : Type list) : Type =
        case a of
            [] => raise TypeCheckingFailure ("INternal error: empty sum")
            | [t] =>t
            | (x::y :: xs) => if typeEquiv []  x y then typeUnify (x :: xs)
            else raise TypeCheckingFailure ("Type unify failed")


    fun kabs (body :pkvalue -> pkcomputation) : pkvalue = 
        let val boundId = UID.next()
        (* val _ = print "kabs called \n" *)
        in PKAbs(boundId, body(PKVar boundId)) end



    and eraseSigLazy (kont : (kcontext -> PersistentKMachine.pkcomputation) option) (ctx : kcontext) (s : CSignature) : PersistentKMachine.pkcomputation =

        (
            (* print ("eraseSigLazy DEBUG " ^ PrettyPrint.show_typecheckingSig s )
            ; *)
            case s of
            [] => (case kont of SOME f => f(ctx) | NONE => PKRet(PKUnit))
         | TypeMacro (n, t)::ss => eraseSigLazy kont (kaddToCtxR (TypeDef([n],t, ())) ctx) ss
        | TermTypeJudgment(n, t):: ss => ( (* todo check all type decls are realized, otherwise 
        erasure is going to fail when type checking passes *)
            let val ((n', e'), ss') = lookAheadForValue ss n
                val boundId = UID.next()
                val (followingComp ) =
                    (eraseSigLazy kont
            (kaddToCtxR (TermTypeJ([n], (applyContextToType (eraseCtx ctx) t), PKVar(boundId))) ctx)
             ss')
            in 
            (kseqSig (eraseCkExpr ctx (applyContextToExpr (eraseCtx ctx) e') 
            (applyContextToType (eraseCtx ctx) t)
            ) (boundId, followingComp)
            )
            end
        )
        | TermMacro(n, e) :: ss => 
            let val comp1 =(eraseSynExpr ctx (applyContextToExpr (eraseCtx ctx) e))
            val boundId = UID.next()
                val (followingComp ) = 
            (eraseSigLazy kont  (addToCtxR (TermTypeJ([n], synthesizeType (eraseCtx ctx) (applyContextToExpr 
                    (eraseCtx ctx) e), PKVar(boundId))) ctx)  ss)
            in 
            (kseqSig comp1 (boundId, followingComp))
                    end
        | DirectExpr e :: ss=> let
            val (comp1) = (eraseSynExpr ctx (applyContextToExpr (eraseCtx ctx) e))
            val (followingComp ) = (eraseSigLazy kont ctx ss)
            in  if ss = [] andalso not (Option.isSome (kont)) then comp1 else
            (kseqSig comp1 (UID.next(), followingComp) )
            end
        | Structure (vis, sName, decls) :: ss => 
        (case ctx of 
        Context(curName, curVis, bindings) => 
            let val comp2f: kcontext -> PersistentKMachine.pkcomputation = fn Context(_, _, newBindings) => eraseSigLazy kont (Context(curName, curVis, newBindings)) ss
                val (comp1 ) = eraseSigLazy (SOME comp2f) (Context(curName@[sName], vis, bindings)) decls

                (* assume the typeChecking is behaving properly, 
                no conflicting things will be added to the signature *)
                (* sub context will be determined by whether the signature is private or not ? *)
            in  comp1
            end
        )
        | OpenStructure openName :: ss =>
        (case ctx of 
        Context(curName, curVis, bindings) => 
            let val nextContext =  nextContextOfOpenStructure curName curVis bindings openName
                (* assume the typeChecking is behaving properly, 
                no conflicting things will be added to the signature *)
                (* sub context will be determined by whether the signature is private or not ? *)
            in eraseSigLazy kont nextContext ss
            end
        )
        )

    and eraseSynExpr  (ctx : kcontext)(e : CExpr) : pkcomputation =
    (
        let 
        (* val _ = print ("eraseSynExpr on " ^ PrettyPrint.show_typecheckingExpr e ^ " in context " ^ PrettyPrint.show_typecheckingpassctx (eraseCtx ctx) ^ "\n"); *)
         val res = case e of
            ExprVar v => PKRet(klookup ctx v)
            | UnitExpr => PKRet(PKUnit)
            | Proj(e, l) => (case synthesizeType (n ctx) e of
                    Prod ls => PKProj(eraseSynExpr ctx e,
                        klookupLabel ls l)
            )
            | Case(e,cases) => (case (synthesizeType (n ctx) e) of
                    Sum ls => let val boundId = UID.next() in 
                     PKCases ((eraseSynExpr ctx e),(List.tabulate(List.length ls, 
                    fn index => let val  (label,t) = List.nth(ls, index)
                                    val caseIndex = klookupLabel3 cases label
                                    val (_, ev, e) = List.nth(cases, caseIndex)
                                in (boundId, eraseSynExpr 
                                     (kaddToCtxR (TermTypeJ([ev], t, PKVar boundId)) ctx) e 
                                ) end))) end)
            | LamWithType (t, ev, e) => PKRet(kabs(fn v => eraseSynExpr (kaddToCtxR (TermTypeJ([ev], t, v)) ctx) e))
            | App (e1, e2) => (case synthesizeType (n ctx) e1
                of Func (t1, t2) => 
                PKApp(eraseSynExpr ctx e1, eraseCkExpr ctx e2 t1))
            | TAbs (tv, e2) => eraseSynExpr ctx e2
            | TApp (e2, t) => eraseSynExpr ctx e2
            | Open (e1, (tv, ev, e2)) => (case synthesizeType (n ctx) e1 of
                Exists (tv', tb) => kseq (eraseSynExpr ctx e1) 
                    (fn v => 
                eraseSynExpr (kaddToCtxR (TermTypeJ([ev], substTypeInType (TypeVar [tv]) [tv'] tb, v)) ctx) e2 
                    ))
            (* | Fold e2 => Fold (substTypeInExpr tS x e2) *)
            | Unfold e2 => PKUnfold(eraseSynExpr ctx e2) 
            | StringLiteral s => PKRet(PKBuiltinValue(KbvString s))
             | LetIn(decls, e) => (case ctx of 
            Context(curName, curVis, bindings) => 
                let val eF :kcontext -> PersistentKMachine.pkcomputation = fn (Context(localName,_, newBindings)) => 
                                eraseSynExpr (Context(localName, curVis, newBindings)) e 
                    val comp1 :pkcomputation = eraseSigLazy (SOME(eF)) (Context(curName@StructureName.localName(), curVis, bindings)) decls
                    (* assume the typeChecking is behaving properly, 
                    no conflicting things will be added to the signature *)
                    (* sub context will be determined by whether the signature is private or not ? *)
                in comp1
                end
            )
            (* (case synthesizeType ctx e2 of
                Rho (tv, tb) => substTypeInType (Rho (tv, tb)) tv tb
                | _ => raise TypeCheckingFailure "Cannot unfold non recursive type"
                ) *)
            (* | Fix (ev, e)=> Fix (ev, substTypeInExpr tS x e) *)
            | _ => raise Fail "ep86"
        (* val _ = print ("eraseSynExpr result is " ^PrettyPrint.show_pkcomputation res ^
        " on " ^ PrettyPrint.show_typecheckingExpr e ^ " in context " ^ PrettyPrint.show_typecheckingpassctx (eraseCtx ctx) ^ "\n"); *)
        in res end)

    and eraseCkExpr  (ctx : kcontext) (e : Expr) (tt: Type) (* tt target type *) : pkcomputation =
        (let
        (* val _ = print ("eraseCkExpr on " ^ PrettyPrint.show_typecheckingExpr e ^ 
        " against type" ^ PrettyPrint.show_typecheckingType tt ^
        " in context " ^ PrettyPrint.show_typecheckingpassctx (eraseCtx ctx) ^ "\n"); *)
        val res = case e of
            ExprVar v => PKRet(klookup ctx v)
            | UnitExpr => PKRet(PKUnit)
            | Tuple l => (case tt of 
                Prod ls => 
                    let fun go i values = 
                        if i = List.length l
                        then PKRet(PKTuple(values))
                        else kseq (eraseCkExpr ctx (List.nth(l, i)) (#2 (List.nth(ls, i)))) 
                                    (fn v => go (i+1) (values@[v]))
                    in go 0 [] end
                )
            | Proj(e, l) => eraseSynExpr ctx (Proj(e,l))
            | Inj (l, e) => (case tt of
                Sum ls => kseq (eraseCkExpr ctx e (lookupLabel ls l)) (fn v => PKRet(PKInj(l, klookupLabel ls l, v)))
            )
            | Case(e,cases) => (case (synthesizeType (n ctx) e) of
                    Sum ls => 
                    PKCases ((eraseSynExpr ctx e),(List.tabulate(List.length ls, 
                    fn index => let val  (label,t) = List.nth(ls, index)
                                    val caseIndex = klookupLabel3 cases label
                                    val boundId = UID.next()
                                    val (_, ev, e) = List.nth(cases, caseIndex)
                                in (boundId, eraseCkExpr (
                                     kaddToCtxR (TermTypeJ([ev], t, PKVar(boundId))) ctx) e tt
                                ) end)))
            )
            | Lam(ev, eb) => (case tt of
                Func(t1,t2) => 
                PKRet(kabs(fn v => 
                    eraseCkExpr (kaddToCtxR (TermTypeJ([ev], t1, v))ctx) eb t2
                ))
                )
            | LamWithType (t, ev, eb) => (case tt of
                Func(t1,t2) => (
                    PKRet(kabs(fn v => 
                        eraseCkExpr ( kaddToCtxR (TermTypeJ([ev], t1, v))ctx) eb t2
                    ))
                )
            )
            | App (e1, e2) => (case synthesizeType (n ctx) e1
                of Func (t1, t2) => 
                PKApp(eraseSynExpr ctx e1, eraseCkExpr ctx e2 t1)
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
                        (fn v => PKRet(PKFold v))
                    )
            | Unfold e2 => PKUnfold (eraseSynExpr ctx e2)
            (* (case synthesizeType ctx e2 of
                Rho (tv, tb) => asserTypeEquiv tt (substTypeInType (Rho (tv, tb)) tv tb)
                | _ => raise TypeCheckingFailure "Cannot unfold non recursive type"
                ) *)
            | Fix (ev, e)=>  let val boundID = UID.next() in
            PKFix(boundID, eraseCkExpr ( kaddToCtxR (TermTypeJ([ev], tt, PKVar boundID)) ctx) e tt)
            end
            (* let
             fix F = ((\y. f (y y)) (\y. f (y y)))
             (this one causes infinite look)
                    val compiledF = KAbs (fn v => eraseCkExpr ((ev, v)::kctx) ((ev, tt)::ctx) e tt)
                    fun appVV (v1 : kvalue) (v2:kvalue) : kcomputation = KApp(KRet(v1), KRet(v2))
                    val lamydotfyy = KAbs(fn y => KApp(KRet(compiledF),(appVV y y)))
                in appVV lamydotfyy lamydotfyy
            end *)
            | StringLiteral l => (case tt of 
                BuiltinType(BIString) => PKRet(PKBuiltinValue(KbvString l)))
            | LetIn(decls, e) => (case ctx of 
            Context(curName, curVis, bindings) => 
                let val eF :kcontext -> PersistentKMachine.pkcomputation = fn (Context(localName,_, newBindings)) => eraseCkExpr (Context(localName, curVis, newBindings)) e tt
                    val comp1 :pkcomputation = eraseSigLazy (SOME(eF)) (Context(curName@StructureName.localName(), curVis, bindings)) decls
                    (* assume the typeChecking is behaving properly, 
                    no conflicting things will be added to the signature *)
                    (* sub context will be determined by whether the signature is private or not ? *)
                in comp1
                end
            )
            (* `checkType ((ev , tt):: ctx) e tt *)

        (* val _ = print ("eraseCkExpr result is " ^ PrettyPrint.show_pkcomputation res ^ "eraseCkExpr on " ^ PrettyPrint.show_typecheckingExpr e ^ 
        " against type" ^ PrettyPrint.show_typecheckingType tt ^
        " in context " ^ PrettyPrint.show_typecheckingpassctx (eraseCtx ctx) ^ "\n"); *)
        in res end
        )
        handle TypeCheckingFailure s =>
            raise TypeCheckingFailure (s ^ "\n when checking the expr " ^ PrettyPrint.show_typecheckingExpr e ^ 
                " against type " ^ PrettyPrint.show_typecheckingType tt
            ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx 
            ^ "\n [INTERNAL] THIS IS AN INTERNAL ERROR OF THE COMPILER, please file a bug report."
             )

    and lookAheadForValue (s : Signature) (ename : UTF8String.t) : (UTF8String.t * Expr) * CSignature = 
        case s of
         TermDefinition(n, e) :: ss => if UTF8String.semanticEqual n  ename then ((n, e), ss)
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
    


        
    

end