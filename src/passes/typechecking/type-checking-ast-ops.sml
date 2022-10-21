
structure TypeCheckingASTOps = struct
open TypeCheckingAST
open TypeCheckingContext
open StaticErrorStructure
open TypeCheckingErrors
infix 5 >>=
infix 5 =/=


    fun getCurSName (Context(sName, _, _)) = sName
  
    fun ~<> (a, b) = not (StructureName.semanticEqual a b)
    infix 4 ~<>
    fun ~~= (a, b) = (UTF8String.semanticEqual a b)
    infix 4 ~~=
    fun ~~~= (a, b) = (StructureName.semanticEqual a b)
    infix 4 ~~~=

    (* fun freeTVar (t : RType) : StructureName.t list = 
        case t of
              RVar t => [t]
            | RProd (l, soi) => List.concat (map (fn (l, t, soi) => freeTVar t) l)
            | RLazyProd (l, soi) => List.concat (map (fn (l, t, soi) => freeTVar t) l)
            | RSum (l, soi) => List.concat (map (fn (l, t, soi) => freeTVar t) l)
            (* | RFunc (t1,t2, soi) => List.concat (map freeTVar [t1,t2]) *)
            (* | RFunc (t1,t2, soi) => List.concat (map freeTVar [t1,t2]) *)
            | RTypeInst (t1,t2, soi) => List.concat (map freeTVar [t1,t2])
            | RForall (tv,t2, soi) => List.filter (fn t => t ~<> [tv]) (freeTVar t2)
            | RExists (tv,t2, soi) => List.filter (fn t => t ~<> [tv]) (freeTVar t2)
            | RRho (tv,t2, soi) => List.filter (fn t => t ~<> [tv]) (freeTVar t2)
            | RUnitType(s) => []
            | RNullType(s) => []
            | RBuiltinType(b) => []
            | RUniverse(s) => []
            | RLamWithType (t, ev, e, soi) => List.concat [freeTVar t, 
            List.filter (fn t => t ~<> [ev]) (freeTVar e)]
            | _ => raise Fail ("freeTVar " ^ PrettyPrint.show_typecheckingRType t) *)


    fun freeTCVar (t : CType) : StructureName.t list = 
    let
        fun remove v l = List.filter (fn t => t~<> [v]) l
        (* val _ = DebugPrint.p "computing freetcvar" *)
        val res = 
            case t of
                CVar (t, r) => [t]
                | CLabeledProd l => (
                    let fun col l = case l of 
                                    [] => []
                                    | (l1,t1)::tl => freeTCVar t1 @ remove l1 (col tl)
                    in col l end
                )
                | CProd l => List.concat (map (fn ( t) => freeTCVar t) l)
                | CLazyProd l => List.concat (map (fn (l, t) => freeTCVar t) l)
                | CSum l => List.concat (map (fn (l, t) => freeTCVar t) l)
                | CPiType (t1,evop, t2, p) => 
                freeTCVar t1 @ 
                    (case evop of 
                        NONE => freeTCVar t2
                        | SOME(ev) => List.filter (fn t => t ~<> [ev]) (freeTCVar t2)
                    )
                (* | CTypeInst (t1,t2) => List.concat (map freeTCVar [t1,t2])
                | CForall (tv,t2) => List.filter (fn t => t ~<> [tv]) (freeTCVar t2)
                | CExists (tv,t2) => List.filter (fn t => t ~<> [tv]) (freeTCVar t2)
                | CRho (tv,t2) => List.filter (fn t => t ~<> [tv]) (freeTCVar t2) *)
                | CUnitType => []
                | CNullType => []
                | CBuiltinType(b) => []
                | CUniverse => []
                | CLam(ev, eb, _) => List.filter (fn t => t ~<> [ev]) (freeTCVar eb)
                | CLetIn _ => [] (* TODO: *)
                | CFfiCCall _ => [] (* TODO !!! *)
                | CBuiltinFunc (f) => []
                | CApp(e1, e2, u) => freeTCVar e1 @ freeTCVar e2
                | CBuiltinConstant _ => []
                | CUnitExpr => []
                | CIfThenElse (e1, e2, e3) => freeTCVar e1 @ freeTCVar e2 @ freeTCVar e3
                | CMetaVar(v) => [v]
                | CProj(e1, idx, u) => freeTCVar e1
                | CBlockProj(e, lbl, idx) => freeTCVar e
                | CBlock(_) => [] (* free block *)
                | _ => raise Fail ("freeTCVar not implemented for " ^ PrettyPrint.show_typecheckingCType t)
        (* val _ = DebugPrint.p "computed freetcvar" *)
    in
        res
    end



    (* fun freeEVar (e : Expr) : StructureName.t list = 
        case e of
            ExprVar v => [v]
            | UnitExpr => []
            | Tuple l => List.concat (map freeEVar l)
            | Inj (_, e) => freeEVar e
            | Case (e, l) => List.concat (
                (map (fn (l, ev, e) => 
                List.filter (fn ev' => ev' <> [ev]) (freeEVar e)) l)
            )
            | Lam (ev, e)=> List.filter (fn ev' => ev' <> [ev]) (freeEVar e)
            | LamWithType (t, ev, e) => List.filter (fn ev' => ev' <> [ev]) (freeEVar e)
            | App (e1, e2) => List.concat (map freeEVar [e1, e2])
            | TAbs (tv, e2) => freeEVar e2
            | TApp (e2, t) => freeEVar e2
            | Pack (t, e2) => freeEVar e2
            | Open (e1, (tv, ev, e2)) => 
                ((freeEVar e1)  @( List.filter (fn ev' => ev' <> [ev]) (freeEVar e)))
            | Fold e2 => freeEVar e2
            | Unfold e2 => freeEVar e2
            | Fix (ev, e)=> List.filter (fn ev' => ev' <> [ev]) (freeEVar e)
            | StringLiteral l => [] *)



    (* e is the current checking expression, for error reporting *)
    (* todo : change to weak head normalization only *)
    fun weakHeadNormalizeType (e : RExpr) (ctx : context) (t : CType) : CType witherrsoption  = 
    let 
      fun dereferenceIfPossible (ctx : context)(t : CType) : CType option=
        case t of 
            CVar (name, CVTBinder) => NONE
            | CVar(name, CVTDefinition(t')) => SOME(t')
            | CVar(name, CVTConstructor cinfo) => NONE
            | CVar(name, CVTBinderDefinition t') => SOME(CMetaVar(t'))
            | _ => raise Fail "tcastops90"

    val recur = weakHeadNormalizeType e ctx
    val res = 
        case t of
            CVar v => (case dereferenceIfPossible ctx t of 
                NONE => Success(CVar v)
                | SOME(t') => recur t')
            | CMetaVar(name) => ( lookupCtx ctx name >>= (fn (cname, tp, jtp) => 
                case jtp of 
                    JTMetaVarResolved t => 
                    ((case t of 
                    CMetaVar(name') => if StructureName.semanticEqual name name' 
                    then genericError e  ctx ("元变量解析成了自己，解析错误！ "^  StructureName.toStringPlain name)
                    else recur t
                    | _ => recur (t)))
                    | _ => Success(CMetaVar(name))
                )
            )
            | CProd l =>  Success(t)
            | CLazyProd l =>  fmap CLazyProd (collectAll ((map (fn (l, t) => recur t >>= (fn nt => Success(l, nt))) l)))
            | CSum l =>  fmap CSum (collectAll (map (fn (l, t) => recur t >>= (fn nt => Success(l, nt))) l))
            (* | CFunc (t1,t2) => fmap CFunc (recur t1 =/= recur t2 ) *)
            | CPiType (t1, hd, p, t2) => Success(CPiType(t1, hd, p, t2))
            | CSigmaType (t1, hd, t2) => Success(t)
            (* | CTypeInst (t1,t2) => recur t1 >>= (fn nt1 => case nt1 of
                CForall(tv, t1') => (recur t2) >>= (fn nt2 => Success(substTypeInCExpr nt2 ([tv]) t1'))
                | _ => genSingletonError (reconstructFromRExpr e) ("期待通用类型(Expected Forall)，却得到了(got)：" ^
                PrettyPrint.show_typecheckingCType nt1^ "（在检查类型"^ 
                PrettyPrint.show_typecheckingCType t ^ "时）")
                 (showctxSome ctx)
            ) *)
            (* | CForall (tv,t2) => recur t2 >>=(fn nt2 =>  Success(CForall (tv, nt2) )) *)
            (* | CExists (tv,t2) => recur t2 >>=(fn nt2 =>  Success(CExists (tv, nt2) )) *)
            (* | CRho (tv,t2) =>  recur t2 >>=(fn nt2 =>  Success(CRho (tv, nt2) )) *)
            | CUnitType => Success(CUnitType)
            | CNullType => Success(CNullType)
            | CBuiltinType(b) => Success(CBuiltinType(b))
            | CUniverse => Success(CUniverse)
            | CLam(ev, e, u) => 
            fmap CLam(==/=(Success ev, recur e, Success u))
            | CApp(e1, e2, _) =>   
                recur e1 >>= (fn ce1 => 
                    case ce1 of
                        CLam(cev, ce1body, _) => recur e2 >>= (fn ce2 => Success(substTypeInCExpr ce2 ([cev]) ce1body))
                        | _ => (* maybe ce1 is a type constructor *) (Success t)
                )
            | CLetIn _ => Success(t)
            | CFfiCCall _ => Success(t)
            | CBuiltinFunc _ => Success(t)
            | CUnitExpr => Success(t)
            | CIfThenElse _ => Success(t) (* TODO: *)
            | CTuple  _ => Success(t)
            | CProj (e, idx, u) => recur e >>= (fn 
                CTuple (elems, u) => if idx >= length elems
                                     then raise Fail "tcastops176: normalization error"
                                     else recur (List.nth(elems, idx))
                | ne => Success(CProj(ne,  idx, u))
            )
            | CBlock _ => Success (t)
            | CBlockProj(e, lbl, idx) => 
                    recur e >>= (fn 
                        CBlock (csig) => 
                        if idx >= length csig
                        then raise Fail "tcastops176: normalization error"
                        else 
                            (case List.nth(csig, idx) of
                                (* Should we project term or type? *)
                                (* ofcourse we should project tm, because weakhead normalize is 
                                a runtime procedure *)
                                 CTermDefinition(_, tm, _) => recur tm
                                | CConstructorDecl(n, tp, cconsinfo) => Success(CVar([n], CVTConstructor([n], cconsinfo)))
                                | CPureDeclaration(_, tp) => raise Fail "cannot normalize pure decl"
                                | _ => raise Fail "cannot project"
                            )
                        | ne => Success(CBlockProj(ne,  lbl, idx))
                )
            | CLabeledProd _ => Success (t)
            | CBuiltinConstant _ => Success (t)
            | CFix _ => genericError e ctx "Fixpoint expressions 递归表达式不可以用于类型中"
            | _ => raise Fail ("weakHeadNormalizeType not implemented for "  ^ PrettyPrint.show_typecheckingCType t)
    (* val _ = DebugPrint.p ("normalized type " ^ PrettyPrint.show_static_error res PrettyPrint.show_typecheckingCType ^"\n") *)
    in
        res
    end

(* reports an error if not all metavars are resolved *)
    and resolveAllMetaVarsInCExpr  (ctx : context) ( t : CType) : CType witherrsoption = 
        let 

    val recur = resolveAllMetaVarsInCExpr ctx
    val res = 
        case t of
            CVar v => Success(t)
            | CMetaVar(name) => ( lookupCtx ctx name >>= (fn (cname, tp, jtp) => 
                case jtp of 
                    JTMetaVarResolved t => recur (t)
                    | JTMetaVarPendingResolve s => TypeCheckingErrors.genericErrorStr s ctx "存在尚未赋值的元变量(Unresolved Metavariables)"
                    | _ => raise Fail "tcast175: should be a metavar"
                )
            )
            | CProd l =>  fmap CProd (collectAll ((map (fn (t) => recur t >>= (fn nt => Success(nt))) l)))
            | CLabeledProd l =>  fmap CLabeledProd (collectAll ((map (fn (l, t) => recur t >>= (fn nt => Success(l, nt))) l)))
            | CLazyProd l =>  fmap CLazyProd (collectAll ((map (fn (l, t) => recur t >>= (fn nt => Success(l, nt))) l)))
            | CSum l =>  fmap CSum (collectAll (map (fn (l, t) => recur t >>= (fn nt => Success(l, nt))) l))
            | CPiType (t1, hd, t2, p) => recur t1 >>= (fn nt1 =>
                                            recur t2 >>= (fn nt2 => 
                                                Success(CPiType(nt1, hd, nt2, p))
                                            )
                                        ) 
            | CSigmaType (t1, hd, t2) => recur t1 >>= (fn nt1 =>
                                            recur t2 >>= (fn nt2 => 
                                                Success(CSigmaType(nt1, hd, nt2))
                                            )
                                        ) 
            (* | CTypeInst (t1,t2) => recur t1 >>= (fn nt1 => 
                    (recur t2) >>= (fn nt2 =>
                 Success(CTypeInst(nt1,nt2))))
            | CForall (tv,t2) => recur t2 >>=(fn nt2 =>  Success(CForall (tv, nt2) ))
            | CExists (tv,t2) => recur t2 >>=(fn nt2 =>  Success(CExists (tv, nt2) ))
            | CRho (tv,t2) =>  recur t2 >>=(fn nt2 =>  Success(CRho (tv, nt2) )) *)
            | CUnitType => Success(CUnitType)
            | CNullType => Success(CNullType)
            | CBuiltinType(b) => Success(CBuiltinType(b))
            | CUniverse => Success(CUniverse)
            | CLam(ev, e, u) => 
            fmap CLam(==/=(Success ev, recur e, Success u))
            | CApp(e1, e2, u) =>   
                recur e1 >>= (fn ce1 => 
                         recur e2 >>= (fn ce2 => 
                            Success(CApp(ce1, ce2, u))
                         )
                )
            | CLetIn(dl, e, u) => 
                resolveAllMetaVarsInCSig ctx dl >>= (fn cdl => 
                    recur e >>= (fn ce => 
                        Success(CLetIn(cdl, ce, u))
                    )
                )
            | CFfiCCall _ => Success(t)
            | CBuiltinFunc _ => Success(t)
            | CUnitExpr => Success(t)
            | CIfThenElse(e1, e2, e3) => 

                recur e1 >>= (fn ce1 => 
                    recur e2 >>= (fn ce2 => 
                        recur e3 >>= (fn ce3 => 
                            Success(CIfThenElse(ce1, ce2, ce3))
                        )
                    )
                )
            | CBuiltinConstant _ => Success(t)
            | CTuple (l, u) => collectAll (map recur l) >>= (fn cl => 
                Success(CTuple(cl, u))
            )
            | CProj(e,  idx, u) => 
                recur e >>= (fn ce => 
                    Success(CProj(ce,  idx, u))
                )
            | CFix(ev, eb, u) => 
                recur eb >>= (fn ceb => 
                    Success(CFix(ev, ceb, u))
                )
            | CCase((u, e), pel, u2) => 
                recur e >>= (fn ce => 
                    collectAll (map (fn (pat, eb) => 
                        recur eb >>= (fn ceb => Success((pat, ceb)))
                    ) pel) >>= (fn cl => 
                        Success(CCase((u, ce), cl, u2))
                    )
                )
            | CSeqComp(e1, e2, u1, u2) => 
                recur e1 >>= (fn ce1 => 
                    recur e2 >>= (fn ce2 => 
                        Success(CSeqComp(ce1, ce2, u1, u2))
                    )
                )
            (* | CPack(e1, e2, u) => 
                recur e1 >>= (fn ce1 => 
                    recur e2 >>= (fn ce2 => 
                        Success(CPack(ce1, ce2, u))
                    )
                ) *)
            | CBlock(decls) => 
                fmap CBlock (resolveAllMetaVarsInCSig ctx decls)
            | CBlockProj(e, lbl, idx) => 
                fmap (fn e' => CBlockProj(e', lbl, idx)) (recur e)
            | _ => raise Fail ("resolveAllMetaVarsInCExpr not implemented for "  ^ PrettyPrint.show_typecheckingCType t)
    (* val _ = DebugPrint.p ("normalized type " ^ PrettyPrint.show_static_error res PrettyPrint.show_typecheckingCType ^"\n") *)
    in
        res
    end


    and resolveAllMetaVarsInCSig (ctx : context) ( s : CSignature) : CSignature witherrsoption = 
        let fun rme e = resolveAllMetaVarsInCExpr ctx e
        in
        collectAll (map (fn d => 
        case d of 
            CTermDefinition(name, expr, tp) => rme expr >>= 
                (fn cexpr => rme tp >>= 
                    (fn ctp => Success(CTermDefinition(name, cexpr, ctp))))
            | CDirectExpr( expr, tp) => rme expr >>= 
                (fn cexpr => rme tp >>= 
                    (fn ctp => Success(CDirectExpr(cexpr, ctp))))
            | CPureDeclaration( name, tp) => 
                 rme tp >>= (fn ctp => Success(CPureDeclaration(name, ctp)))
            | CConstructorDecl( name, tp, cinfo) => 
                rme tp >>= 
                    (fn ctp => Success(CConstructorDecl(name, ctp, cinfo)))
             | CImport _ => Success(d)
             | COpenStructure _ => Success(d)

        ) s)
        end
    and substTypeInSpine (tS : CType) (x : StructureName.t) (spine : (UTF8String.t * CType) list) = 
        let 
            fun captureAvoid f (tv : UTF8String.t) spine' = 
                    if List.exists (fn t' => t' ~~~= [tv]) (freeTCVar tS)
                    then let val tv' = StructureName.binderName()
                                        in f (tv', substTypeInSpine tS x 
                                            (substTypeInSpine (CVar([tv'], CVTBinder)) [tv] spine')) 
                                            end
                    else  (* No capture, regular *)
                    if [tv] ~~~= x (* do not substitute when the bound variable is the same as substitution *)
                    then f (tv, spine')
                    else f (tv, substTypeInSpine tS x spine')
            (* val _ = DebugPrint.p "SUBST..." *)
        in 
            case spine of 
                [] => []
                | (l1, t1)::tl => captureAvoid (fn (l1', tl') => ((l1', substTypeInCExpr tS x t1):: tl')) l1 (tl)
        end
    and substTypeInCExpr (tS : CType) (x : StructureName.t) (e : CType) = 
        let 
            fun captureAvoid f (tv : UTF8String.t) t2 = 
                    if List.exists (fn t' => t' ~~~= [tv]) (freeTCVar tS)
                    then let val tv' = StructureName.binderName()
                                        in f (tv', substTypeInCExpr tS x 
                                            (substTypeInCExpr (CVar([tv'], CVTBinder)) [tv] t2)) 
                                            end
                    else  (* No capture, regular *)
                    if [tv] ~~~= x (* do not substitute when the bound variable is the same as substitution *)
                    then f (tv, t2)
                    else f (tv, substTypeInCExpr tS x t2)

            val recur = substTypeInCExpr tS x
            fun substTAnn ann = case ann of
                CTypeAnn t => CTypeAnn (substTypeInCExpr tS x t)
                | _ => ann
        in
            case e of
                CVar (name, r) => if name ~~~=x then tS else CVar (name, r) 
                | CMetaVar(name) => if name ~~~=x then tS else e
                | CLabeledProd l => CLabeledProd (substTypeInSpine tS x l)
                | CProd l => CProd  (map (fn ( t) => ( substTypeInCExpr tS x t)) l)
                | CLazyProd l => CLazyProd  (map (fn (l, t) => (l, substTypeInCExpr tS x t)) l)
                | CSum l =>  CSum  (map (fn (l, t) => (l, substTypeInCExpr tS x t)) l)
                (* | CFunc (t1,t2) => CFunc (substTypeInCExpr tS x t1, substTypeInCExpr tS x t2 ) *)
                (* | CTypeInst (t1,t2) => CTypeInst (substTypeInCExpr tS x t1, substTypeInCExpr tS x t2 ) *)
                | CPiType(t1, evop,t2, p) => (case evop of 
                        NONE => CPiType(recur t1, NONE, recur t2, p)
                        | SOME(ev) => let val t1' = recur t1
                                    in captureAvoid (fn (ev', t2') => CPiType(t1', SOME ev', t2', p)) ev t2
                                    end
                    )
                | CSigmaType(t1, evop,t2) => (case evop of 
                        NONE => CSigmaType(recur t1, NONE, recur t2)
                        | SOME(ev) => let val t1' = recur t1
                                    in captureAvoid (fn (ev', t2') => CSigmaType(t1', SOME ev', t2')) ev t2
                                    end
                    )
                (* | CForall (tv,t2) => captureAvoid CForall tv t2
                | CExists (tv,t2) => captureAvoid CExists tv t2
                | CRho (tv,t2) => captureAvoid CRho tv t2 *)
                | CUnitType => CUnitType
                | CNullType => CNullType
                | CBuiltinType(b) => CBuiltinType(b)
                | CUniverse => CUniverse
                | CApp(e1, e2, CTypeAnn(t)) => CApp(recur e1, recur e2, CTypeAnn(recur t))
                | CLam(ev, e2, u) => captureAvoid 
                    (fn (ev', e2') => CLam(ev, e2, substTAnn u)) ev e2
                | CLetIn _ => e (* TODO : do it *)
                | CProj (e1, idx, u) => CProj (recur e1, idx, u)
                | CTuple(e, u) => CTuple (map recur e, u)
                | CFfiCCall(name, args) => CFfiCCall(name, map recur args)
                | CBlock(decls) => CBlock (substituteTypeInCSignature tS x decls)
                | CBlockProj(name, lbl, idx) => CBlockProj(substTypeInCExpr tS x name, lbl, idx)
        | _ => raise Fail ("substTypeInCExpr undefined for " ^ PrettyPrint.show_typecheckingCType e
        ^ " when substituting " ^ PrettyPrint.show_typecheckingCType tS
        ^ " for " ^ StructureName.toStringPlain x)
        end
    and substituteTypeInCSignature (tS : CType) (x : StructureName.t) (decls : CSignature ) : CSignature = 
        let 
            (* performs full substitution on decls, with tv possibly shadowing s, pass the result back to f *)
            fun captureAvoid f (tv : UTF8String.t) decls' = 
                    let val decls = () in
                    if List.exists (fn t' => t' ~~~= [tv]) (freeTCVar tS)
                    then let val tv' = StructureName.binderName()
                                        in f (tv', substituteTypeInCSignature tS x 
                                            (substituteTypeInCSignature (CVar([tv'], CVTBinder)) [tv] decls')) 
                                            end
                    else  (* No capture, regular *)
                    if [tv] ~~~= x (* do not substitute when the bound variable is the same as substitution *)
                    then f (tv, decls')
                    else f (tv, substituteTypeInCSignature tS x decls')
                    end
            (* val _ = DebugPrint.p "SUBST..." *)
        in 
            case decls of 
                [] => []
                | ((CTermDefinition(n, e, t)))::tl => 
                captureAvoid (fn (name', tl') => (CTermDefinition(name', substTypeInCExpr tS x e, substTypeInCExpr tS x t):: tl')) n (tl)
                | ((CPureDeclaration(n, t)))::tl => 
                captureAvoid (fn (name', tl') => (CPureDeclaration(name', substTypeInCExpr tS x t):: tl')) n (tl)
                | ((CConstructorDecl(n, t, cinfo)))::tl => 
                captureAvoid (fn (name', tl') => (CConstructorDecl(name', substTypeInCExpr tS x t, cinfo):: tl')) n (tl)
                | ((CDirectExpr(e, t )))::tl => 
                (CDirectExpr( substTypeInCExpr tS x e, substTypeInCExpr tS x t):: (substituteTypeInCSignature tS x tl))
                | ((h as CImport(_)))::tl => 
                (h:: (substituteTypeInCSignature tS x tl))
                | ((h as COpenStructure(_)))::tl => 
                (h:: (substituteTypeInCSignature tS x tl))
        end
    (* raise Fail "not implemented136" *)
    (* !!! always capture avoiding substitution *)
    (* only used when checking pi types *)
    (* TODO: maybe hereditary substitution *)
    (* and substTypeInRType (tS : RType) (x : StructureName.t) (t : RType) = 
    let 
        fun captureAvoid f (tv : UTF8String.t) t2 = 
    (* capture avoid (ts. t2) performs substitution [tS/tv] t2 
        and necessary renaming of tv in case tv is bound in tS
        it calls f with the subsituted (ts'. t2')
        *)
            if List.exists (fn t' => t' ~~~= [tv]) (freeTVar tS)
             then let val tv' = uniqueName()
                                in f (tv', substTypeInRType tS x 
                                    (substTypeInRType (RVar [tv']) [tv] t2)) 
                                    end
            else  (* No capture, regular *)
            if [tv] ~~~= x (* do not substitute when the boudn variable is the same as substitution *)
            then f (tv, t2)
            else f (tv, substTypeInRType tS x t2)

    in
        case t of
              RVar (t) => if t ~~~=x then tS else RVar t
            | RFunc (t1,t2, soi) => RFunc (substTypeInRType tS x t1, substTypeInRType tS x t2 , soi)
            | RPiType (t1,NONE, t2, soi) => RPiType (substTypeInRType tS x t1, NONE, substTypeInRType tS x t2 , soi)
            | RPiType (t1,SOME tv, t2, soi) => captureAvoid (fn (tv', t2') => 
                RPiType (substTypeInRType tS x t1, SOME tv', t2', soi)
            ) tv t2
            | _ => raise Fail "ni272: unsupported expr in type"
    end *)
              (* 
            | RProd (l, soi) => RProd  (map (fn (l, t, soi) => (l, substTypeInRType tS x t, soi)) l, soi)
            | RLazyProd (l, soi) => RLazyProd  (map (fn (l, t, soi) => (l, substTypeInRType tS x t, soi)) l, soi)
            | RSum (l, soi) =>  RSum  (map (fn (l, t, soi) => (l, substTypeInRType tS x t, soi)) l, soi)
            | RFunc (t1,t2, soi) => RFunc (substTypeInRType tS x t1, substTypeInRType tS x t2 , soi)
            | RTypeInst (t1,t2, soi) => RTypeInst (substTypeInRType tS x t1, substTypeInRType tS x t2 , soi)
            | RForall (tv,t2, soi) => captureAvoid (fn (tv', t2') => RForall (tv', t2', soi)) tv t2
            | RExists (tv,t2, soi) => captureAvoid (fn (tv', t2') => RExists (tv', t2', soi)) tv t2
            | RRho (tv,t2, soi) => captureAvoid (fn (tv', t2') => RRho (tv', t2', soi)) tv t2
            | RUnitType(s) => RUnitType(s)
            | RNullType(s) => RNullType(s)
            | RBuiltinType(b) => RBuiltinType(b)
            | RUniverse(soi) => RUniverse(soi)
            | RUnitExpr(soi) => RUnitExpr (soi)
            | RTuple (l, soi) => RTuple (map (substTypeInRType tS x) l, soi)
            | RLazyTuple (l, soi) => RLazyTuple (map (substTypeInRType tS x) l, soi)
            | RInj (l, e, soi) => RInj (l, substTypeInRType tS x e, soi)
            | RProj (e, l, soi) => RProj (substTypeInRType tS x e, l, soi)
            | RLazyProj (e, l, soi) => RLazyProj (substTypeInRType tS x e, l, soi)
            | RIfThenElse (e, tcase, fcase, soi) => RIfThenElse(substTypeInRType tS x e, 
                substTypeInRType tS x tcase, substTypeInRType tS x fcase,  soi)
            | RCase (e, l, soi) => RCase (substTypeInRType tS x e, (
                (map (fn (l, ev, e) => 
                (l, ev, substTypeInRType tS x e) ) l)
            ), soi)
            | RLam (ev, e, soi)=> RLam (ev, substTypeInRType tS x e, soi)
            | RLamWithType (t, ev, e, soi) => RLamWithType (substTypeInRType tS x t, ev, substTypeInRType tS x e, soi)
            | RApp (e1, e2, soi) => RApp (substTypeInRType tS x e1, substTypeInRType tS x e2, soi)
            | RSeqComp (e1, e2, soi) => RSeqComp (substTypeInRType tS x e1, substTypeInRType tS x e2, soi)
            | RTAbs (tv, e2, soi) => (
                if List.exists (fn t' => t' ~~~= [tv]) (freeTVar tS)
             then let val tv' = uniqueName()
                                in RTAbs (tv', substTypeInRType tS x 
                                    (substTypeInRType (RVar [tv']) [tv] e2), soi) 
                                    end
            else  (* No capture, regular *)
            if [tv] ~~~= x then RTAbs (tv, e2, soi)
             else RTAbs (tv, substTypeInRType tS x e2, soi)
            ) 
            | RTApp (e2, t, soi) => RTApp(substTypeInRType tS x e2, substTypeInRType tS x t , soi)
            | RPack (t, e2, soi) => RPack(substTypeInRType tS x t, substTypeInRType tS x e2, soi)
            | ROpen (e1, (tv, ev, e2), soi) => ROpen(
                substTypeInRType tS x e1, (
                if List.exists (fn t' => t' ~~~= [tv]) (freeTVar tS)
             then let val tv' = uniqueName()
                                in (tv', ev, substTypeInRType tS x 
                                    (substTypeInRType (RVar [tv']) [tv] e2)) 
                                    end
            else  (* No capture, regular *)
             (tv, ev, substTypeInRType tS [tv] e2))
            , soi)
            | RFold (e2, soi) => RFold (substTypeInRType tS x e2, soi)
            | RUnfold (e2, soi) => RUnfold (substTypeInRType tS x e2, soi)
            | RFix (ev, e, soi)=> RFix (ev, substTypeInRType tS x e, soi)
            | RStringLiteral l => RStringLiteral l
            | RIntConstant l => RIntConstant l
            | RRealConstant l => RRealConstant l
            | RBoolConstant l => RBoolConstant l
            | RFfiCCall(e, e2, soi) => RFfiCCall(substTypeInRType tS x e, 
                    substTypeInRType tS x e2, soi
                )
            | RLetIn(decls, e, soi) => 
                RLetIn(substituteTypeInRSignature tS x decls, 
                    substTypeInRType tS x e, soi)
            | RBuiltinFunc(f, s) => RBuiltinFunc(f, s)
            | r => raise Fail ("substTypeInRType Fail on type(RExpr) " ^ PrettyPrint.show_typecheckingRType r)
    end

    and substituteTypeInRDeclaration (tS : RType) (x : StructureName.t) (d : RDeclaration) = 
      case d of
      (* to prevent this from happending by prior checking *)
         (* RTypeMacro (y, t) => if x ~~~= [y] then raise Fail "Cannot have identical types" else 
            RTypeMacro (y, substTypeInRType tS x t) *)
         RTermTypeJudgment(ename, t) => RTermTypeJudgment(ename, substTypeInRType tS x t)
        (* | RTermMacro (n, e) => RTermMacro(n, substTypeInRType tS x e) *)
        | RTermDefinition(n, e) => RTermDefinition (n, substTypeInRType tS x e)
        | RDirectExpr(e) => RDirectExpr (substTypeInRType tS x e)
        | RStructure(v, n, s) => RStructure(v, n, substituteTypeInRSignature tS x s)
        | ROpenStructure(n) => ROpenStructure(n)
        | RReExportStructure(n) => RReExportStructure(n)
        | RImportStructure(n,fp) => RImportStructure(n, fp)
    
    and substituteTypeInRSignature (tS : RType) (x : StructureName.t) (s : RSignature) : RSignature = 
        case s of
            [] => []
            | (d :: ds) => substituteTypeInRDeclaration tS x d :: 
            substituteTypeInRSignature tS x ds *)

(* semantic type equivalence *)
    (* type equiv returns true or false and if an internal error occurs, such as the expression 
    is not well formed, a human readable error is returned *)



    fun judgmentTypeToCVarType (canonicalNameIfConstructor : StructureName.t ) (jt : judgmentType) : cvartype = 
        case jt of 
            JTConstructor cinfo =>  CVTConstructor(canonicalNameIfConstructor, cinfo)
            | JTLocalBinder =>  CVTBinder
            | JTDefinition e =>  CVTDefinition e
            | JTPending => CVTBinder (* to accomodate signature declaration where we only have declarations *)
            (* raise Fail ("tcp271: should not be pending, check circular definitions : " ^ StructureName.toStringPlain canonicalNameIfConstructor) *)
            | JTLocalBinderWithDef metavarname => CVTBinderDefinition metavarname
            | _ => raise Fail "ni427"
    fun countSpineTypeArgs (tp : CType) = 
            case tp of 
                (* CFunc(t1, t2) => 1 + countSpineTypeArgs t2 *)
                 CPiType(t1, _, t2, p) => 1 + countSpineTypeArgs t2 (* todo consider plicity *)
                | _ => 0
            
end