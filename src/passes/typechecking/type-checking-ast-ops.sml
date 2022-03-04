
structure TypeCheckingASTOps = struct
open TypeCheckingAST
open TypeCheckingContext
open StaticErrorStructure
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
        case t of
            CVar (t, r) => [t]
            | CProd l => List.concat (map (fn (l, t) => freeTCVar t) l)
            | CLazyProd l => List.concat (map (fn (l, t) => freeTCVar t) l)
            | CSum l => List.concat (map (fn (l, t) => freeTCVar t) l)
            | CPiType (t1,evop, t2) => 
            freeTCVar t1 @ 
                (case evop of 
                    NONE => freeTCVar t2
                    | SOME(ev) => List.filter (fn t => t ~<> [ev]) (freeTCVar t2)
                )
            | CTypeInst (t1,t2) => List.concat (map freeTCVar [t1,t2])
            | CForall (tv,t2) => List.filter (fn t => t ~<> [tv]) (freeTCVar t2)
            | CExists (tv,t2) => List.filter (fn t => t ~<> [tv]) (freeTCVar t2)
            | CRho (tv,t2) => List.filter (fn t => t ~<> [tv]) (freeTCVar t2)
            | CUnitType => []
            | CNullType => []
            | CBuiltinType(b) => []
            | CUniverse => []
            | CLam(ev, eb, _) => List.filter (fn t => t ~<> [ev]) (freeTCVar eb)
            | CLetIn _ => [] (* TODO: *)
            | CFfiCCall _ => [] (* TODO !!! *)
            | CBuiltinFunc (f) => []
            | CApp(e1, e2, u) => freeTCVar e1 @ freeTCVar e2
            | CIntConstant _ => []
            | CUnitExpr => []
            | CBoolConstant _ => []
            | CIfThenElse (e1, e2, e3) => freeTCVar e1 @ freeTCVar e2 @ freeTCVar e3
            | _ => raise Fail ("freeTCVar not implemented for " ^ PrettyPrint.show_typecheckingCType t)



    (* reconstruct the original string from rexpr, used for generating error information *)
    fun reconstructFromRExpr (e : RExpr) : UTF8String.t = 
    let 
    fun constructWithSep ( args: UTF8String.t list) (sepl : operator list) : UTF8String.t = 
    case args of (h::t) =>
        #1 (foldl (fn (next, (s, y)) => case y of (oph::opt) =>  (reconstructWithArgs oph [s, next], opt)
                                                | _ => raise Fail "tcastops 252"
        ) (h, sepl) t)
        | _ => raise Fail "tcastops 255"
    val tpPlaceHolder =UTF8String.fromString "..." 
    open Operators
    in
    case e of
        RVar v => StructureName.toString v
        | RUnitExpr(soi) => soi
        | RTuple (l, (soil)) => constructWithSep (map reconstructFromRExpr l) (soil)
        | RLazyTuple (l, (soil)) => constructWithSep (map reconstructFromRExpr l) (soil)
        | RProj (e, lbl, soi) => reconstructWithArgs soi [reconstructFromRExpr e, lbl]
        | RLazyProj (e, lbl, soi) => reconstructWithArgs soi [reconstructFromRExpr e, lbl]
        | RIfThenElse (e, tcase, fcase, soi) => reconstructWithArgs soi [reconstructFromRExpr e, reconstructFromRExpr tcase, reconstructFromRExpr fcase]
        | RInj  ( lbl,e, soi) => reconstructWithArgs soi [lbl, reconstructFromRExpr e]
        | RCase (e, l, (soiTop, soiSep, soiClause))=>
                reconstructWithArgs soiTop [reconstructFromRExpr e, 
                    constructWithSep (
                        ListPair.map (fn ((b,c), operClause) => reconstructWithArgs operClause [reconstructFromRExpr b, reconstructFromRExpr c]) (l, soiClause)
                    ) soiSep
                ]
        | RLam (x, e, soi) => reconstructWithArgs soi [x, reconstructFromRExpr e]
        | RLamWithType (t, x, e, soi) => reconstructWithArgs soi [tpPlaceHolder, x, reconstructFromRExpr e]
        | RApp (e1, e2, soi)=> if Operators.eqOpUid soi PreprocessingOperators.appExprOp 
            then reconstructWithArgs soi [reconstructFromRExpr e1, reconstructFromRExpr e2]
            else let
                fun flatten (e : RExpr)  : RExpr list= case e of 
                    RApp(e1', e2', soi') => if Operators.eqOpUid soi soi'
                                            then e1' :: flatten e2'
                                            else [e]
                    | _ => [e]
                in 
                    reconstructWithArgs soi (map reconstructFromRExpr (e1 :: flatten e2))
                end
                                        
        (* | RTAbs (x, e, soi) => reconstructWithArgs soi [x, reconstructFromRExpr e] *)
        | RTApp (e1, e2, (soi,s))=> reconstructWithArgs soi [reconstructFromRExpr e1, s]
        | RPack (t, e, (s, soi))=> reconstructWithArgs soi [s, reconstructFromRExpr e]
        | ROpen (e, (t, x, e2), soi)=> reconstructWithArgs soi [reconstructFromRExpr e, t, x, reconstructFromRExpr e2]
        | RFold (e, soi) => reconstructWithArgs soi [reconstructFromRExpr e]
        | RUnfold (e, soi) => reconstructWithArgs soi [reconstructFromRExpr e]
        | RFix (x, e, soi) => reconstructWithArgs soi [x, reconstructFromRExpr e]
        | RStringLiteral (l, (qil, qir)) => qil :: l @[ qir]
        | RIntConstant (l, soi) => soi
        | RRealConstant (l, soi) => soi
        | RBoolConstant (l, soi) => soi
        | RLetIn (s, e, soi) => reconstructWithArgs soi [tpPlaceHolder, reconstructFromRExpr e]
        | RFfiCCall (s, e, soi) => reconstructWithArgs soi [ reconstructFromRExpr s,  reconstructFromRExpr e ]
        | RBuiltinFunc(f, s) => s
        | RSeqComp(e1, e2, soi) => reconstructWithArgs soi [reconstructFromRExpr e1, reconstructFromRExpr e2]
        | RBuiltinType(b, s) => s
        | RUnitType(s) => s
        | RUniverse(s) => s
        | RPiType(t1, evoption, t2, soi) => 
        (case evoption of 
            SOME v => reconstructWithArgs soi [reconstructFromRExpr t1, v, reconstructFromRExpr t2]
            | NONE => reconstructWithArgs soi [reconstructFromRExpr t1, reconstructFromRExpr t2] )
        | RSigmaType(t1, evoption, t2, soi) =>
        (case evoption of 
            SOME v => reconstructWithArgs soi [reconstructFromRExpr t1, v, reconstructFromRExpr t2]
            | NONE => raise Fail "ui368")
        | RProd(ltsl, sepl) => 
            constructWithSep (List.map (fn (l, t, soi) => 
                reconstructWithArgs soi [l, reconstructFromRExpr t]
            ) ltsl) sepl
        | RLazyProd  (ltsl, sepl) => 
            constructWithSep (List.map (fn (l, t, soi) => 
                reconstructWithArgs soi [l, reconstructFromRExpr t]
            ) ltsl) sepl
        | RSum(ltsl, sepl) => 
            constructWithSep (List.map (fn (l, t, soi) => 
                reconstructWithArgs soi [l, reconstructFromRExpr t]
            ) ltsl) sepl
        | RNullType (s) => s
        (* | RFunc(t1, t2, soi) => reconstructWithArgs soi [reconstructFromRExpr t1, reconstructFromRExpr t2] *)
        | RTypeInst(t1, t2, soi) => reconstructWithArgs soi [reconstructFromRExpr t1, reconstructFromRExpr t2]
        | RForall(tv, t2, soi) => reconstructWithArgs soi [tv, reconstructFromRExpr t2]
        | RExists(tv, t2, soi) => reconstructWithArgs soi [tv, reconstructFromRExpr t2]
        | RRho(tv, t2, soi) => reconstructWithArgs soi [tv, reconstructFromRExpr t2]
        | RPairOfQuotes((ql, qr)) => [ql, qr]
        (* | _ => raise Fail ("reconstruct failed for rexpr " ^ PrettyPrint.show_typecheckingRExpr e) *)
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

    fun uniqueName () =  UTF8String.fromString ("绑定" ^Int.toString (UID.next()))


    (* e is the current checking expression, for error reporting *)
    (* todo : change to weak head normalization only *)
    fun normalizeType (e : RExpr) (ctx : context) (t : CType) : CType witherrsoption  = 
    let 
      fun dereferenceIfPossible (ctx : context)(t : CType) : CType option=
        case t of 
            CVar (name, CVTBinder) => NONE
            | CVar(name, CVTDefinition(t')) => SOME(t')
            | CVar(name, CVTConstructor cinfo) => NONE
            | _ => raise Fail "tcastops90"

    val recur = normalizeType e ctx
    val res = 
        case t of
            CVar v => (case dereferenceIfPossible ctx t of 
                NONE => Success(CVar v)
                | SOME(t') => recur t')
            | CMetaVar(name) => ( lookupCtx ctx name >>= (fn (cname, tp, jtp) => 
                case jtp of 
                    JTMetaVarResolved t => recur (t)
                    | _ => Success(CMetaVar(name))
                )
            )
            | CProd l =>  fmap CProd (collectAll ((map (fn (l, t) => recur t >>= (fn nt => Success(l, nt))) l)))
            | CLazyProd l =>  fmap CLazyProd (collectAll ((map (fn (l, t) => recur t >>= (fn nt => Success(l, nt))) l)))
            | CSum l =>  fmap CSum (collectAll (map (fn (l, t) => recur t >>= (fn nt => Success(l, nt))) l))
            (* | CFunc (t1,t2) => fmap CFunc (recur t1 =/= recur t2 ) *)
            | CPiType (t1, hd, t2) => Success(CPiType(t1, hd, t2))
            | CTypeInst (t1,t2) => recur t1 >>= (fn nt1 => case nt1 of
                CForall(tv, t1') => (recur t2) >>= (fn nt2 => Success(substTypeInCExpr nt2 ([tv]) t1'))
                | _ => genSingletonError (reconstructFromRExpr e) ("期待通用类型(Expected Forall)，却得到了(got)：" ^
                PrettyPrint.show_typecheckingCType nt1^ "（在检查类型"^ 
                PrettyPrint.show_typecheckingCType t ^ "时）")
                 (showctxSome ctx)
            )
            | CForall (tv,t2) => recur t2 >>=(fn nt2 =>  Success(CForall (tv, nt2) ))
            | CExists (tv,t2) => recur t2 >>=(fn nt2 =>  Success(CExists (tv, nt2) ))
            | CRho (tv,t2) =>  recur t2 >>=(fn nt2 =>  Success(CRho (tv, nt2) ))
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
            | CStringLiteral _ => Success(t)
            | CFfiCCall _ => Success(t)
            | CBuiltinFunc _ => Success(t)
            | CUnitExpr => Success(t)
            | CBoolConstant b => Success(t)
            | CIfThenElse _ => Success(t) (* TODO: *)
            | _ => raise Fail ("normalizeType not implemented for "  ^ PrettyPrint.show_typecheckingCType t)
    (* val _ = DebugPrint.p ("normalized type " ^ PrettyPrint.show_static_error res PrettyPrint.show_typecheckingCType ^"\n") *)
    in
        res
    end

    and substTypeInCExpr (tS : CType) (x : StructureName.t) (e : CType) = 
    let fun captureAvoid f (tv : UTF8String.t) t2 = 
            if List.exists (fn t' => t' ~~~= [tv]) (freeTCVar tS)
             then let val tv' = uniqueName()
                                in f (tv', substTypeInCExpr tS x 
                                    (substTypeInCExpr (CVar([tv'], CVTBinder)) [tv] t2)) 
                                    end
            else  (* No capture, regular *)
            if [tv] ~~~= x (* do not substitute when the boudn variable is the same as substitution *)
            then f (tv, t2)
            else f (tv, substTypeInCExpr tS x t2)
        val recur = substTypeInCExpr tS x
        fun substTAnn ann = case ann of
            CTypeAnn t => CTypeAnn (substTypeInCExpr tS x t)
            | _ => ann
    in
        case e of
              CVar (name, r) => if name ~~~=x then tS else CVar (name, r) 
            | CProd l => CProd  (map (fn (l, t) => (l, substTypeInCExpr tS x t)) l)
            | CLazyProd l => CLazyProd  (map (fn (l, t) => (l, substTypeInCExpr tS x t)) l)
            | CSum l =>  CSum  (map (fn (l, t) => (l, substTypeInCExpr tS x t)) l)
            (* | CFunc (t1,t2) => CFunc (substTypeInCExpr tS x t1, substTypeInCExpr tS x t2 ) *)
            | CTypeInst (t1,t2) => CTypeInst (substTypeInCExpr tS x t1, substTypeInCExpr tS x t2 )
            | CPiType(t1, evop, t2) => (case evop of 
                    NONE => CPiType(recur t1, NONE, recur t2)
                    | SOME(ev) => let val t1' = recur t1
                                  in captureAvoid (fn (ev', t2') => CPiType(t1', SOME ev', t2')) ev t2
                                  end
                )
            | CForall (tv,t2) => captureAvoid CForall tv t2
            | CExists (tv,t2) => captureAvoid CExists tv t2
            | CRho (tv,t2) => captureAvoid CRho tv t2
            | CUnitType => CUnitType
            | CNullType => CNullType
            | CBuiltinType(b) => CBuiltinType(b)
            | CUniverse => CUniverse
            | CApp(e1, e2, CTypeAnn(t)) => CApp(recur e1, recur e2, CTypeAnn(recur t))
            | CLam(ev, e2, u) => captureAvoid 
                (fn (ev', e2') => CLam(ev, e2, substTAnn u)) ev e2
            | CLetIn _ => e (* TODO : do it *)
    | _ => raise Fail ("substTypeInCExpr undefined for " ^ PrettyPrint.show_typecheckingCType e
    ^ " when substituting " ^ PrettyPrint.show_typecheckingCType tS
    ^ " for " ^ StructureName.toStringPlain x)
    end
    and substituteTypeInCSignature (tS : CType) (x : StructureName.t) (s : CSignature ) : CSignature = 
    raise Fail "not implemented136"
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
            | JTPending => raise Fail ("tcp271: should not be pending, check circular definitions : " ^ StructureName.toStringPlain canonicalNameIfConstructor)
    fun countSpineTypeArgs (tp : CType) = 
            case tp of 
                (* CFunc(t1, t2) => 1 + countSpineTypeArgs t2 *)
                 CPiType(t1, _, t2) => 1 + countSpineTypeArgs t2
                | _ => 0
            
    (* fun rTypeToCType (ctx: context)(t : RType)  : CType = 
    let 
    val recur = rTypeToCType ctx
    in
        case t of
              RVar t => (case findCtx ctx t of SOME(t', def, jt) => CVar(t', judgmentTypeToCVarType t' jt) | NONE => CVar(t, CVTBinder))
            | RProd (l, soi) => CProd  (map (fn (l, t, soi) => (l, recur t)) l )
            | RLazyProd (l, soi) => CLazyProd  (map (fn (l, t, soi) => (l, recur t)) l )
            | RSum (l,soi) =>  CSum  (map (fn (l, t, soi) => (l, recur t)) l)
            | RFunc (t1,t2, soi) => CFunc (recur t1, recur t2)
            | RTypeInst (t1,t2, soi) => CTypeInst (recur t1, recur t2)
            | RForall (tv,t2, soi) => CForall(tv, recur t2)
            | RExists (tv,t2, soi) => CExists(tv, recur t2)
            | RRho (tv,t2, soi) => CRho(tv, recur t2)
            | RUnitType(soi) => CUnitType
            | RNullType(soi)=> CNullType
            | RBuiltinType(b, soi) => CBuiltinType(b)
            | RUniverse(s) => CUniverse
            | _ => raise Fail ("rTypeToCType not implemented for " ^ PrettyPrint.show_typecheckingRType t)
    end *)

(* 
    fun cTypeToRType (t : CType)  : RType = 
    let 
    in
        case t of
              CVar t => RVar t
            | CProd l => RProd  (map (fn (l, t) => (l, cTypeToRType t)) l)
            | CLazyProd l => RLazyProd  (map (fn (l, t) => (l, cTypeToRType t)) l)
            | CSum l =>  RSum  (map (fn (l, t) => (l, cTypeToRType t)) l)
            | CFunc (t1,t2) => RFunc (cTypeToRType t1, cTypeToRType t2)
            | CTypeInst (t1,t2) => RTypeInst (cTypeToRType t1, cTypeToRType t2)
            | CForall (tv,t2) => RForall(tv, cTypeToRType t2)
            | CExists (tv,t2) => RExists(tv, cTypeToRType t2)
            | CRho (tv,t2) => RRho(tv, cTypeToRType t2)
            | CUnitType => RUnitType
            | CNullType => RNullType
            | CBuiltinType(t) => RBuiltinType(t)
            | CUniverse(s) => RUniverse(s)
    end *)
end