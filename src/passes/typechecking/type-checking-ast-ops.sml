
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

    fun freeTVar (t : Type) : StructureName.t list = 
        case t of
            TypeVar t => [t]
            | Prod l => List.concat (map (fn (l, t) => freeTVar t) l)
            | LazyProd l => List.concat (map (fn (l, t) => freeTVar t) l)
            | Sum l => List.concat (map (fn (l, t) => freeTVar t) l)
            | Func (t1,t2) => List.concat (map freeTVar [t1,t2])
            | TypeInst (t1,t2) => List.concat (map freeTVar [t1,t2])
            | Forall (tv,t2) => List.filter (fn t => t ~<> [tv]) (freeTVar t2)
            | Exists (tv,t2) => List.filter (fn t => t ~<> [tv]) (freeTVar t2)
            | Rho (tv,t2) => List.filter (fn t => t ~<> [tv]) (freeTVar t2)
            | UnitType => []
            | NullType => []
            | BuiltinType(b) => []

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

(* !!! always capture avoiding substitution *)
    fun substTypeInType (tS : Type) (x : StructureName.t) (t : Type) = 
    let fun captureAvoid f (tv : UTF8String.t) t2 = 
            if List.exists (fn t' => t' ~~~= [tv]) (freeTVar tS)
             then let val tv' = uniqueName()
                                in f (tv', substTypeInType tS x 
                                    (substTypeInType (TypeVar [tv']) [tv] t2)) 
                                    end
            else  (* No capture, regular *)
            if [tv] ~~~= x (* do not substitute when the boudn variable is the same as substitution *)
            then f (tv, t2)
            else f (tv, substTypeInType tS x t2)

    in
        case t of
            TypeVar t => if t ~~~=x then tS else TypeVar t
            | Prod l => Prod  (map (fn (l, t) => (l, substTypeInType tS x t)) l)
            | LazyProd l => LazyProd  (map (fn (l, t) => (l, substTypeInType tS x t)) l)
            | Sum l =>  Sum  (map (fn (l, t) => (l, substTypeInType tS x t)) l)
            | Func (t1,t2) => Func (substTypeInType tS x t1, substTypeInType tS x t2 )
            | TypeInst (t1,t2) => TypeInst (substTypeInType tS x t1, substTypeInType tS x t2 )
            | Forall (tv,t2) => captureAvoid Forall tv t2
            | Exists (tv,t2) => captureAvoid Exists tv t2
            | Rho (tv,t2) => captureAvoid Rho tv t2
            | UnitType => UnitType
            | NullType => NullType
            | BuiltinType(b) => BuiltinType(b)
    end
    fun normalizeType (t : Type) : Type witherrsoption  = 
    let 
    val res = 
        case t of
            TypeVar t => Success(TypeVar t)
            | Prod l =>  fmap Prod (collectAll ((map (fn (l, t) => normalizeType t >>= (fn nt => Success(l, nt))) l)))
            | LazyProd l =>  fmap LazyProd (collectAll ((map (fn (l, t) => normalizeType t >>= (fn nt => Success(l, nt))) l)))
            | Sum l =>  fmap Sum (collectAll (map (fn (l, t) => normalizeType t >>= (fn nt => Success(l, nt))) l))
            | Func (t1,t2) => fmap Func (normalizeType t1 =/= normalizeType t2 )
            | TypeInst (t1,t2) => normalizeType t1 >>= (fn nt1 => case nt1 of
                Forall(tv, t1') => (normalizeType t2) >>= (fn nt2 => Success(substTypeInType nt2 ([tv]) t1'))
                | _ => genSingletonError (raise Fail "not implemented") "期待通用类型(Expected Forall)" NONE
            )
            | Forall (tv,t2) => normalizeType t2 >>=(fn nt2 =>  Success(Forall (tv, nt2) ))
            | Exists (tv,t2) => normalizeType t2 >>=(fn nt2 =>  Success(Exists (tv, nt2) ))
            | Rho (tv,t2) =>  normalizeType t2 >>=(fn nt2 =>  Success(Rho (tv, nt2) ))
            | UnitType => Success(UnitType)
            | NullType => Success(NullType)
            | BuiltinType(b) => Success(BuiltinType(b))
    (* val _ = DebugPrint.p ("normalized type " ^ PrettyPrint.show_static_error res PrettyPrint.show_typecheckingType ^"\n") *)
    in
        res
    end

(* no capture as we're only interested in types *)
    fun substTypeInRExpr (tS : Type) (x : StructureName.t) (e : RExpr) = 
    let 
    in
        case e of
            RExprVar v => RExprVar v
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
            | RLamWithType (t, ev, e, soi) => RLamWithType (substTypeInType tS x t, ev, substTypeInRExpr tS x e, soi)
            | RApp (e1, e2, soi) => RApp (substTypeInRExpr tS x e1, substTypeInRExpr tS x e2, soi)
            | RSeqComp (e1, e2, soi) => RSeqComp (substTypeInRExpr tS x e1, substTypeInRExpr tS x e2, soi)
            | RTAbs (tv, e2, soi) => (
                if List.exists (fn t' => t' ~~~= [tv]) (freeTVar tS)
             then let val tv' = uniqueName()
                                in RTAbs (tv', substTypeInRExpr tS x 
                                    (substTypeInRExpr (TypeVar [tv']) [tv] e2), soi) 
                                    end
            else  (* No capture, regular *)
            if [tv] ~~~= x then RTAbs (tv, e2, soi)
             else RTAbs (tv, substTypeInRExpr tS x e2, soi)
            ) 
            | RTApp (e2, t, soi) => RTApp(substTypeInRExpr tS x e2, substTypeInType tS x t , soi)
            | RPack (t, e2, soi) => RPack(substTypeInType tS x t, substTypeInRExpr tS x e2, soi)
            | ROpen (e1, (tv, ev, e2), soi) => ROpen(
                substTypeInRExpr tS x e1, (
                if List.exists (fn t' => t' ~~~= [tv]) (freeTVar tS)
             then let val tv' = uniqueName()
                                in (tv', ev, substTypeInRExpr tS x 
                                    (substTypeInRExpr (TypeVar [tv']) [tv] e2)) 
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
    end

    and substituteTypeInRDeclaration (tS : Type) (x : StructureName.t) (d : RDeclaration) = 
      case d of
      (* to prevent this from happending by prior checking *)
         RTypeMacro (y, t) => if x ~~~= [y] then raise Fail "Cannot have identical types" else 
            RTypeMacro (y, substTypeInType tS x t)
        | RTermTypeJudgment(ename, t) => RTermTypeJudgment(ename, substTypeInType tS x t)
        | RTermMacro (n, e) => RTermMacro(n, substTypeInRExpr tS x e)
        | RTermDefinition(n, e) => RTermDefinition (n, substTypeInRExpr tS x e)
        | RDirectExpr(e) => RDirectExpr (substTypeInRExpr tS x e)
        | RStructure(v, n, s) => RStructure(v, n, substituteTypeInRSignature tS x s)
        | ROpenStructure(n) => ROpenStructure(n)
        | RReExportStructure(n) => RReExportStructure(n)
        | RImportStructure(n,fp) => RImportStructure(n, fp)
    
    and substituteTypeInRSignature (tS : Type) (x : StructureName.t) (s : RSignature) : RSignature = 
        case s of
            [] => []
            | (d :: ds) => substituteTypeInRDeclaration tS x d :: 
            substituteTypeInRSignature tS x ds

(* semantic type equivalence *)
    and typeEquiv (ctx : (Type * Type) list) (t1:Type) (t2:Type)  :bool = 
    (let 
    fun typeEquivLst (l1 : (Label * Type)list) (l2 : (Label * Type) list)= 
            if length l1 <> length l2 then false else
            List.foldr (fn (b1, b2) => b1 andalso b2) true (List.tabulate((List.length l1), (fn i => 
            (#1 (List.nth(l1, i))) ~~= (#1 (List.nth(l2, i)))
            andalso typeEquiv ctx (#2 (List.nth(l1, i))) (#2 (List.nth (l2, i))) )))
    fun unifyBinding tv t2 tv' t2' =
            if tv ~~= tv' then (tv, t2, tv', t2')
            else let val nn = uniqueName() in
            (nn, substTypeInType (TypeVar [nn]) [tv] t2,
            nn, substTypeInType (TypeVar [nn]) [tv'] t2')end
        in
        if List.exists (fn p => p =(t1, t2)) ctx then true
        else
    (case (t1, t2) of
              (TypeVar t1, TypeVar t2) => t1 ~~~= t2
            | (Prod l1, Prod l2) =>  typeEquivLst l1 l2
            | (LazyProd l1, LazyProd l2) =>  typeEquivLst l1 l2
            | (Sum l1, Sum l2) =>   typeEquivLst l1 l2
            | (Func (t1,t2), Func (t1', t2')) => typeEquiv ctx t1 t1' andalso typeEquiv ctx t2 t2'
            | (Forall (tv,t2), Forall (tv', t2')) => let val (_, t1, _, t1') = unifyBinding tv t2 tv' t2' in typeEquiv ctx t1 t1' end
            | (Exists (tv,t2), Exists (tv', t2')) => let val (_, t1, _, t1') = unifyBinding tv t2 tv' t2' in typeEquiv ctx t1 t1' end
            | (Rho (tv,t2), Rho (tv', t2')) => (let val (v, t1, v', t1') = unifyBinding tv t2 tv' t2' 
            in typeEquiv ((Rho(v, t1), Rho(v',t1'))::ctx) 
                (substTypeInType (Rho (v,t1)) [v] t1)
                (substTypeInType (Rho (v',t1')) [v'] t1')
             end)
            | (UnitType, UnitType) => true
            | (NullType, NullType) => true
            | (BuiltinType(b1), BuiltinType(b2)) => b1 = b2
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
        RExprVar v => StructureName.toString v
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
    end
    
end