
structure TypeCheckingASTOps = struct
open TypeCheckingAST
open StaticErrorStructure
infix 5 >>=
infix 5 =/=


    fun getCurSName (Context(sName, _, _)) = sName

    fun appendAbsoluteMappingToCurrentContext (m : 'a gmapping) (ctx : 'a gcontext) : 'a gcontext = 
        case ctx of
            Context(curSName, vis, l) => Context(curSName, vis, 
            (case m of 
                TermTypeJ(e, t, u) => TermTypeJ(e, t, u)
                | TypeDef(tname, t, u) => TypeDef(tname, t, u)
                ):: l
            )

   fun appendRelativeMappingToCurrentContext (m : 'a gmapping) (ctx : 'a gcontext) : 'a gcontext = 
        case ctx of
            Context(curSName, vis, l) => Context(curSName, vis, 
            (case m of 
                TermTypeJ(e, t, u) => TermTypeJ(curSName@e, t, u)
                | TypeDef(tname, t, u) => TypeDef(curSName@tname, t, u)
                ):: l
            )

    fun appendAbsoluteMappingsToCurrentContext (m : 'a gmapping list) (ctx : 'a gcontext) : 'a gcontext = 
        foldl (fn (map, acc) => appendAbsoluteMappingToCurrentContext map acc) ctx m

    fun appendRelativeMappingsToCurrentContext (m : 'a gmapping list) (ctx : 'a gcontext) : 'a gcontext = 
        foldl (fn (map, acc) => appendRelativeMappingToCurrentContext map acc) ctx m

  

    fun ~<> (a, b) = not (StructureName.semanticEqual a b)
    infix 4 ~<>
    fun ~~= (a, b) = (UTF8String.semanticEqual a b)
    infix 4 ~~=
    fun ~~~= (a, b) = (StructureName.semanticEqual a b)
    infix 4 ~~~=

    fun freeTVar (t : RType) : StructureName.t list = 
        case t of
              RVar t => [t]
            | RProd (l, soi) => List.concat (map (fn (l, t, soi) => freeTVar t) l)
            | RLazyProd (l, soi) => List.concat (map (fn (l, t, soi) => freeTVar t) l)
            | RSum (l, soi) => List.concat (map (fn (l, t, soi) => freeTVar t) l)
            | RFunc (t1,t2, soi) => List.concat (map freeTVar [t1,t2])
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
            | _ => raise Fail ("freeTVar " ^ PrettyPrint.show_typecheckingRType t)

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

    fun uniqueName () = UTF8String.fromString (Int.toString (UID.next()))


    fun normalizeType (t : CType) : CType witherrsoption  = 
    let 
    val res = 
        case t of
            CVar t => Success(CVar t)
            | CProd l =>  fmap CProd (collectAll ((map (fn (l, t) => normalizeType t >>= (fn nt => Success(l, nt))) l)))
            | CLazyProd l =>  fmap CLazyProd (collectAll ((map (fn (l, t) => normalizeType t >>= (fn nt => Success(l, nt))) l)))
            | CSum l =>  fmap CSum (collectAll (map (fn (l, t) => normalizeType t >>= (fn nt => Success(l, nt))) l))
            | CFunc (t1,t2) => fmap CFunc (normalizeType t1 =/= normalizeType t2 )
            | CTypeInst (t1,t2) => normalizeType t1 >>= (fn nt1 => case nt1 of
                CForall(tv, t1') => (normalizeType t2) >>= (fn nt2 => Success(substTypeInCExpr nt2 ([tv]) t1'))
                | _ => genSingletonError (raise Fail "not implemented") "期待通用类型(Expected Forall)" NONE
            )
            | CForall (tv,t2) => normalizeType t2 >>=(fn nt2 =>  Success(CForall (tv, nt2) ))
            | CExists (tv,t2) => normalizeType t2 >>=(fn nt2 =>  Success(CExists (tv, nt2) ))
            | CRho (tv,t2) =>  normalizeType t2 >>=(fn nt2 =>  Success(CRho (tv, nt2) ))
            | CUnitType => Success(CUnitType)
            | CNullType => Success(CNullType)
            | CBuiltinType(b) => Success(CBuiltinType(b))
            | CUniverse(s) => Success(CUniverse(s))
            | CLam(ev, e, CTypeAnn t) => 
            fmap CLam(==/=(Success ev, normalizeType e, fmap CTypeAnn (normalizeType t)))
            | _ => raise Fail ("normalizeType not implemented for "  ^ PrettyPrint.show_typecheckingCType t)
    (* val _ = DebugPrint.p ("normalized type " ^ PrettyPrint.show_static_error res PrettyPrint.show_typecheckingType ^"\n") *)
    in
        res
    end

    and substTypeInCExpr (tS : CType) (x : StructureName.t) (t : CType) = 
        raise Fail "undefined tcastops118"
    (* !!! always capture avoiding substitution *)
    and substTypeInRExpr (tS : RType) (x : StructureName.t) (t : RType) = 
    let fun captureAvoid f (tv : UTF8String.t) t2 = 
            if List.exists (fn t' => t' ~~~= [tv]) (freeTVar tS)
             then let val tv' = uniqueName()
                                in f (tv', substTypeInRExpr tS x 
                                    (substTypeInRExpr (RVar [tv']) [tv] t2)) 
                                    end
            else  (* No capture, regular *)
            if [tv] ~~~= x (* do not substitute when the boudn variable is the same as substitution *)
            then f (tv, t2)
            else f (tv, substTypeInRExpr tS x t2)

    in
        case t of
              RVar (t) => if t ~~~=x then tS else RVar t
            | RProd (l, soi) => RProd  (map (fn (l, t, soi) => (l, substTypeInRExpr tS x t, soi)) l, soi)
            | RLazyProd (l, soi) => RLazyProd  (map (fn (l, t, soi) => (l, substTypeInRExpr tS x t, soi)) l, soi)
            | RSum (l, soi) =>  RSum  (map (fn (l, t, soi) => (l, substTypeInRExpr tS x t, soi)) l, soi)
            | RFunc (t1,t2, soi) => RFunc (substTypeInRExpr tS x t1, substTypeInRExpr tS x t2 , soi)
            | RTypeInst (t1,t2, soi) => RTypeInst (substTypeInRExpr tS x t1, substTypeInRExpr tS x t2 , soi)
            | RForall (tv,t2, soi) => captureAvoid (fn (tv', t2') => RForall (tv', t2', soi)) tv t2
            | RExists (tv,t2, soi) => captureAvoid (fn (tv', t2') => RExists (tv', t2', soi)) tv t2
            | RRho (tv,t2, soi) => captureAvoid (fn (tv', t2') => RRho (tv', t2', soi)) tv t2
            | RUnitType(s) => RUnitType(s)
            | RNullType(s) => RNullType(s)
            | RBuiltinType(b) => RBuiltinType(b)
            | RUniverse(soi) => RUniverse(soi)
            | RUnitExpr(soi) => RUnitExpr (soi)
            | RTuple (l, soi) => RTuple (map (substTypeInRExpr tS x) l, soi)
            | RLazyTuple (l, soi) => RLazyTuple (map (substTypeInRExpr tS x) l, soi)
            | RInj (l, e, soi) => RInj (l, substTypeInRExpr tS x e, soi)
            | RProj (e, l, soi) => RProj (substTypeInRExpr tS x e, l, soi)
            | RLazyProj (e, l, soi) => RLazyProj (substTypeInRExpr tS x e, l, soi)
            | RIfThenElse (e, tcase, fcase, soi) => RIfThenElse(substTypeInRExpr tS x e, 
                substTypeInRExpr tS x tcase, substTypeInRExpr tS x fcase,  soi)
            | RCase (e, l, soi) => RCase (substTypeInRExpr tS x e, (
                (map (fn (l, ev, e) => 
                (l, ev, substTypeInRExpr tS x e) ) l)
            ), soi)
            | RLam (ev, e, soi)=> RLam (ev, substTypeInRExpr tS x e, soi)
            | RLamWithType (t, ev, e, soi) => RLamWithType (substTypeInRExpr tS x t, ev, substTypeInRExpr tS x e, soi)
            | RApp (e1, e2, soi) => RApp (substTypeInRExpr tS x e1, substTypeInRExpr tS x e2, soi)
            | RSeqComp (e1, e2, soi) => RSeqComp (substTypeInRExpr tS x e1, substTypeInRExpr tS x e2, soi)
            | RTAbs (tv, e2, soi) => (
                if List.exists (fn t' => t' ~~~= [tv]) (freeTVar tS)
             then let val tv' = uniqueName()
                                in RTAbs (tv', substTypeInRExpr tS x 
                                    (substTypeInRExpr (RVar [tv']) [tv] e2), soi) 
                                    end
            else  (* No capture, regular *)
            if [tv] ~~~= x then RTAbs (tv, e2, soi)
             else RTAbs (tv, substTypeInRExpr tS x e2, soi)
            ) 
            | RTApp (e2, t, soi) => RTApp(substTypeInRExpr tS x e2, substTypeInRExpr tS x t , soi)
            | RPack (t, e2, soi) => RPack(substTypeInRExpr tS x t, substTypeInRExpr tS x e2, soi)
            | ROpen (e1, (tv, ev, e2), soi) => ROpen(
                substTypeInRExpr tS x e1, (
                if List.exists (fn t' => t' ~~~= [tv]) (freeTVar tS)
             then let val tv' = uniqueName()
                                in (tv', ev, substTypeInRExpr tS x 
                                    (substTypeInRExpr (RVar [tv']) [tv] e2)) 
                                    end
            else  (* No capture, regular *)
             (tv, ev, substTypeInRExpr tS [tv] e2))
            , soi)
            | RFold (e2, soi) => RFold (substTypeInRExpr tS x e2, soi)
            | RUnfold (e2, soi) => RUnfold (substTypeInRExpr tS x e2, soi)
            | RFix (ev, e, soi)=> RFix (ev, substTypeInRExpr tS x e, soi)
            | RStringLiteral l => RStringLiteral l
            | RIntConstant l => RIntConstant l
            | RRealConstant l => RRealConstant l
            | RBoolConstant l => RBoolConstant l
            | RFfiCCall(e, e2, soi) => RFfiCCall(substTypeInRExpr tS x e, 
                    substTypeInRExpr tS x e2, soi
                )
            | RLetIn(decls, e, soi) => 
                RLetIn(substituteTypeInRSignature tS x decls, 
                    substTypeInRExpr tS x e, soi)
            | RBuiltinFunc(f, s) => RBuiltinFunc(f, s)
            | r => raise Fail ("substTypeInType(RExpr) Fail on type(RExpr) " ^ PrettyPrint.show_typecheckingRType r)
    end

    and substituteTypeInRDeclaration (tS : RType) (x : StructureName.t) (d : RDeclaration) = 
      case d of
      (* to prevent this from happending by prior checking *)
         RTypeMacro (y, t) => if x ~~~= [y] then raise Fail "Cannot have identical types" else 
            RTypeMacro (y, substTypeInRExpr tS x t)
        | RTermTypeJudgment(ename, t) => RTermTypeJudgment(ename, substTypeInRExpr tS x t)
        | RTermMacro (n, e) => RTermMacro(n, substTypeInRExpr tS x e)
        | RTermDefinition(n, e) => RTermDefinition (n, substTypeInRExpr tS x e)
        | RDirectExpr(e) => RDirectExpr (substTypeInRExpr tS x e)
        | RStructure(v, n, s) => RStructure(v, n, substituteTypeInRSignature tS x s)
        | ROpenStructure(n) => ROpenStructure(n)
        | RReExportStructure(n) => RReExportStructure(n)
        | RImportStructure(n,fp) => RImportStructure(n, fp)
    
    and substituteTypeInRSignature (tS : RType) (x : StructureName.t) (s : RSignature) : RSignature = 
        case s of
            [] => []
            | (d :: ds) => substituteTypeInRDeclaration tS x d :: 
            substituteTypeInRSignature tS x ds

(* semantic type equivalence *)
    and typeEquiv (ctx : (RExpr * RExpr) list) (t1:RExpr) (t2:RExpr)  :bool = 
    (let 
    fun typeEquivLst (l1 : (Label * RExpr)list) (l2 : (Label * RExpr) list)= 
            if length l1 <> length l2 then false else
            List.foldr (fn (b1, b2) => b1 andalso b2) true (List.tabulate((List.length l1), (fn i => 
            (#1 (List.nth(l1, i))) ~~= (#1 (List.nth(l2, i)))
            andalso typeEquiv ctx (#2 (List.nth(l1, i))) (#2 (List.nth (l2, i))) )))
    fun unifyBinding tv t2 tv' t2' =
            if tv ~~= tv' then (tv, t2, tv', t2')
            else let val nn = uniqueName() in
            (nn, substTypeInRExpr (RVar [nn]) [tv] t2,
            nn, substTypeInRExpr (RVar [nn]) [tv'] t2')end
        in
        if List.exists (fn ((p1, p2):(RExpr * RExpr)) => p1 = t1 andalso p2 = t2) ctx then true
        else
    (case (t1, t2) of
              (RVar t1, RVar t2) => t1 ~~~= t2
            | (RProd l1, RProd l2) =>  typeEquivLst l1 l2
            | (RLazyProd l1, RLazyProd l2) =>  typeEquivLst l1 l2
            | (RSum l1, RSum l2) =>   typeEquivLst l1 l2
            | (RFunc (t1,t2), RFunc (t1', t2')) => typeEquiv ctx t1 t1' andalso typeEquiv ctx t2 t2'
            | (RForall (tv,t2), RForall (tv', t2')) => let val (_, t1, _, t1') = unifyBinding tv t2 tv' t2' in typeEquiv ctx t1 t1' end
            | (RExists (tv,t2), RExists (tv', t2')) => let val (_, t1, _, t1') = unifyBinding tv t2 tv' t2' in typeEquiv ctx t1 t1' end
            | (RRho (tv,t2), RRho (tv', t2')) => (let val (v, t1, v', t1') = unifyBinding tv t2 tv' t2' 
            in typeEquiv ((RRho(v, t1), RRho(v',t1'))::ctx) 
                (substTypeInRExpr (RRho (v,t1)) [v] t1)
                (substTypeInRExpr (RRho (v',t1')) [v'] t1')
             end)
            | (RUnitType, RUnitType) => true
            | (RNullType, RNullType) => true
            | (RBuiltinType(b1), RBuiltinType(b2)) => b1 = b2
            | _ => false)
    end)



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
        | RUnitExpr(soi) => reconstructWithArgs soi []
        | RTuple (l, (soil)) => constructWithSep (map reconstructFromRExpr l) (soil)
        | RLazyTuple (l, (soil)) => constructWithSep (map reconstructFromRExpr l) (soil)
        | RProj (e, lbl, soi) => reconstructWithArgs soi [reconstructFromRExpr e, lbl]
        | RLazyProj (e, lbl, soi) => reconstructWithArgs soi [reconstructFromRExpr e, lbl]
        | RIfThenElse (e, tcase, fcase, soi) => reconstructWithArgs soi [reconstructFromRExpr e, reconstructFromRExpr tcase, reconstructFromRExpr fcase]
        | RInj  ( lbl,e, soi) => reconstructWithArgs soi [lbl, reconstructFromRExpr e]
        | RCase (e, l, (soiTop, soiSep, soiClause))=>
                reconstructWithArgs soiTop [reconstructFromRExpr e, 
                    constructWithSep (
                        ListPair.map (fn ((a,b,c), operClause) => reconstructWithArgs operClause [a,b, reconstructFromRExpr c]) (l, soiClause)
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
                                        
        | RTAbs (x, e, soi) => reconstructWithArgs soi [x, reconstructFromRExpr e]
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
        | _ => raise Fail ("reconstruct failed for rexpr" ^ PrettyPrint.show_typecheckingRExpr e)
    end
    

    fun rTypeToCType (t : RType)  : CType = 
    let 
    in
        case t of
              RVar t => CVar t
            | RProd (l, soi) => CProd  (map (fn (l, t, soi) => (l, rTypeToCType t)) l )
            | RLazyProd (l, soi) => CLazyProd  (map (fn (l, t, soi) => (l, rTypeToCType t)) l )
            | RSum (l,soi) =>  CSum  (map (fn (l, t, soi) => (l, rTypeToCType t)) l)
            | RFunc (t1,t2, soi) => CFunc (rTypeToCType t1, rTypeToCType t2)
            | RTypeInst (t1,t2, soi) => CTypeInst (rTypeToCType t1, rTypeToCType t2)
            | RForall (tv,t2, soi) => CForall(tv, rTypeToCType t2)
            | RExists (tv,t2, soi) => CExists(tv, rTypeToCType t2)
            | RRho (tv,t2, soi) => CRho(tv, rTypeToCType t2)
            | RUnitType(soi) => CUnitType
            | RNullType(soi)=> CNullType
            | RBuiltinType(b, soi) => CBuiltinType(b)
            | RUniverse(s) => CUniverse(s)
    end
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