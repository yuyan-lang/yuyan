
structure TypeCheckingASTOps = struct
open TypeCheckingAST

    exception TypeCheckingFailure of string
    exception SignatureCheckingFailure of string

(* these exist here for pretty printing *)
(* g for generic *)
 datatype 'a gmapping = TermTypeJ of StructureName.t * Type  * 'a
                    | TypeDef of StructureName.t * Type * unit
datatype 'a gcontext = Context of StructureName.t * bool * 
    ('a gmapping) list
    type mapping = unit gmapping
    type context = unit gcontext

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
            | Sum l => List.concat (map (fn (l, t) => freeTVar t) l)
            | Func (t1,t2) => List.concat (map freeTVar [t1,t2])
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
            | Sum l =>  Sum  (map (fn (l, t) => (l, substTypeInType tS x t)) l)
            | Func (t1,t2) => Func (substTypeInType tS x t1, substTypeInType tS x t2 )
            | Forall (tv,t2) => captureAvoid Forall tv t2
            | Exists (tv,t2) => captureAvoid Exists tv t2
            | Rho (tv,t2) => captureAvoid Rho tv t2
            | UnitType => UnitType
            | NullType => NullType
            | BuiltinType(b) => BuiltinType(b)
    end

(* no capture as we're only interested in types *)
    fun substTypeInRExpr (tS : Type) (x : StructureName.t) (e : RExpr) = 
    let 
    in
        case e of
            RExprVar v => RExprVar v
            | RUnitExpr => RUnitExpr
            | RTuple l => RTuple (map (substTypeInRExpr tS x) l)
            | RInj (l, e) => RInj (l, substTypeInRExpr tS x e)
            | RProj (e, l) => RProj (substTypeInRExpr tS x e, l)
            | RCase (e, l) => RCase (substTypeInRExpr tS x e, (
                (map (fn (l, ev, e) => 
                (l, ev, substTypeInRExpr tS x e) ) l)
            ))
            | RLam (ev, e)=> RLam (ev, substTypeInRExpr tS x e)
            | RLamWithType (t, ev, e) => RLamWithType (substTypeInType tS x t, ev, substTypeInRExpr tS x e)
            | RApp (e1, e2) => RApp (substTypeInRExpr tS x e1, substTypeInRExpr tS x e2)
            | RTAbs (tv, e2) => (
                if List.exists (fn t' => t' ~~~= [tv]) (freeTVar tS)
             then let val tv' = uniqueName()
                                in RTAbs (tv', substTypeInRExpr tS x 
                                    (substTypeInRExpr (TypeVar [tv']) [tv] e2)) 
                                    end
            else  (* No capture, regular *)
            if [tv] ~~~= x then RTAbs (tv, e2)
             else RTAbs (tv, substTypeInRExpr tS [tv] e2)
            ) 
            | RTApp (e2, t) => RTApp(substTypeInRExpr tS x e2, substTypeInType tS x t )
            | RPack (t, e2) => RPack(substTypeInType tS x t, substTypeInRExpr tS x e2)
            | ROpen (e1, (tv, ev, e2)) => ROpen(
                substTypeInRExpr tS x e1, (
                if List.exists (fn t' => t' ~~~= [tv]) (freeTVar tS)
             then let val tv' = uniqueName()
                                in (tv', ev, substTypeInRExpr tS x 
                                    (substTypeInRExpr (TypeVar [tv']) [tv] e2)) 
                                    end
            else  (* No capture, regular *)
             (tv, ev, substTypeInRExpr tS [tv] e2))
            )
            | RFold e2 => RFold (substTypeInRExpr tS x e2)
            | RUnfold e2 => RUnfold (substTypeInRExpr tS x e2)
            | RFix (ev, e)=> RFix (ev, substTypeInRExpr tS x e)
            | RStringLiteral l => RStringLiteral l
            | RFfiCCall(e, e2) => RFfiCCall(substTypeInRExpr tS x e, 
                    substTypeInRExpr tS x e2
                )
    end

    fun substituteTypeInRDeclaration (tS : Type) (x : StructureName.t) (d : RDeclaration) = 
      case d of
         RTypeMacro (y, t) => if x ~~~= [y] then raise TypeCheckingFailure "Cannot have identical types" else 
            RTypeMacro (y, substTypeInType tS x t)
        | RTermTypeJudgment(ename, t) => RTermTypeJudgment(ename, substTypeInType tS x t)
        | RTermMacro (n, e) => RTermMacro(n, substTypeInRExpr tS x e)
        | RTermDefinition(n, e) => RTermDefinition (n, substTypeInRExpr tS x e)
        | RDirectExpr(e) => RDirectExpr (substTypeInRExpr tS x e)
    
    fun substituteTypeInRSignature (tS : Type) (x : StructureName.t) (s : RSignature) : RSignature = 
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
end