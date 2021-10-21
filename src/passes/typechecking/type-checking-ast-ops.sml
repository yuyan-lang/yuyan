
structure TypeCheckingASTOps = struct
open TypeCheckingAST

    exception TypeCheckingFailure of string
    fun freeTVar (t : Type) : TVar list = 
        case t of
            TypeVar t => [t]
            | Prod l => List.concat (map (fn (l, t) => freeTVar t) l)
            | Sum l => List.concat (map (fn (l, t) => freeTVar t) l)
            | Func (t1,t2) => List.concat (map freeTVar [t1,t2])
            | Forall (tv,t2) => List.filter (fn t => t <> tv) (freeTVar t2)
            | Exists (tv,t2) => List.filter (fn t => t <> tv) (freeTVar t2)
            | Rho (tv,t2) => List.filter (fn t => t <> tv) (freeTVar t2)
            | UnitType => []
            | NullType => []

    fun freeEVar (e : Expr) : EVar list = 
        case e of
            ExprVar v => [v]
            | UnitExpr => []
            | Tuple l => List.concat (map freeEVar l)
            | Inj (_, e) => freeEVar e
            | Case (e, l) => List.concat (
                (map (fn (l, ev, e) => 
                List.filter (fn ev' => ev' <> ev) (freeEVar e)) l)
            )
            | Lam (ev, e)=> List.filter (fn ev' => ev' <> ev) (freeEVar e)
            | LamWithType (t, ev, e) => List.filter (fn ev' => ev' <> ev) (freeEVar e)
            | App (e1, e2) => List.concat (map freeEVar [e1, e2])
            | TAbs (tv, e2) => freeEVar e2
            | TApp (e2, t) => freeEVar e2
            | Pack (t, e2) => freeEVar e2
            | Open (e1, (tv, ev, e2)) => 
                ((freeEVar e1)  @( List.filter (fn ev' => ev' <> ev) (freeEVar e)))
            | Fold e2 => freeEVar e2
            | Unfold e2 => freeEVar e2
            | Fix (ev, e)=> List.filter (fn ev' => ev' <> ev) (freeEVar e)

    fun uniqueName () = UTF8String.fromString (Int.toString (UID.next()))

(* !!! always capture avoiding substitution *)
    fun substTypeInType (tS : Type) (x : TVar) (t : Type) = 
    let fun captureAvoid f tv t2 = 
            if List.exists (fn t' => t' = tv) (freeTVar tS)
                orelse (tv = x)
             then let val tv' = uniqueName()
                                in f (tv', substTypeInType tS x 
                                    (substTypeInType (TypeVar tv') tv t2)) 
                                    end
            else  (* No capture, regular *)
             f (tv, substTypeInType tS x t2)

    in
        case t of
            TypeVar t => if t =x then tS else TypeVar t
            | Prod l => Prod  (map (fn (l, t) => (l, substTypeInType tS x t)) l)
            | Sum l =>  Sum  (map (fn (l, t) => (l, substTypeInType tS x t)) l)
            | Func (t1,t2) => Func (substTypeInType tS x t1, substTypeInType tS x t2 )
            | Forall (tv,t2) => captureAvoid Forall tv t2
            | Exists (tv,t2) => captureAvoid Exists tv t2
            | Rho (tv,t2) => captureAvoid Rho tv t2
            | UnitType => UnitType
            | NullType => NullType
    end

(* no capture as we're only interested in types *)
    fun substTypeInExpr (tS : Type) (x : TVar) (e : Expr) = 
    let 
    in
        case e of
            ExprVar v => ExprVar v
            | UnitExpr => UnitExpr
            | Tuple l => Tuple (map (substTypeInExpr tS x) l)
            | Inj (l, e) => Inj (l, substTypeInExpr tS x e)
            | Proj (e, l) => Proj (substTypeInExpr tS x e, l)
            | Case (e, l) => Case (substTypeInExpr tS x e, (
                (map (fn (l, ev, e) => 
                (l, ev, substTypeInExpr tS x e) ) l)
            ))
            | Lam (ev, e)=> Lam (ev, substTypeInExpr tS x e)
            | LamWithType (t, ev, e) => LamWithType (substTypeInType tS x t, ev, substTypeInExpr tS x e)
            | App (e1, e2) => App (substTypeInExpr tS x e1, substTypeInExpr tS x e2)
            | TAbs (tv, e2) => (
                if List.exists (fn t' => t' = tv) (freeTVar tS)
             then let val tv' = uniqueName()
                                in TAbs (tv', substTypeInExpr tS x 
                                    (substTypeInExpr (TypeVar tv') tv e2)) 
                                    end
            else  (* No capture, regular *)
             TAbs (tv, substTypeInExpr tS tv e2)
            ) 
            | TApp (e2, t) => TApp(substTypeInExpr tS x e2, substTypeInType tS x t )
            | Pack (t, e2) => Pack(substTypeInType tS x t, substTypeInExpr tS x e2)
            | Open (e1, (tv, ev, e2)) => Open(
                substTypeInExpr tS x e1, (
                if List.exists (fn t' => t' = tv) (freeTVar tS)
             then let val tv' = uniqueName()
                                in (tv', ev, substTypeInExpr tS x 
                                    (substTypeInExpr (TypeVar tv') tv e2)) 
                                    end
            else  (* No capture, regular *)
             (tv, ev, substTypeInExpr tS tv e2))
            )
            | Fold e2 => Fold (substTypeInExpr tS x e2)
            | Unfold e2 => Unfold (substTypeInExpr tS x e2)
            | Fix (ev, e)=> Fix (ev, substTypeInExpr tS x e)
    end

    fun substituteTypeInDeclaration (tS : Type) (x : TVar) (d : Declaration) = 
      case d of
         TypeMacro (y, t) => if x = y then raise TypeCheckingFailure "Cannot have identical types" else 
            TypeMacro (y, substTypeInType tS x t)
        | TermTypeJudgment(ename, t) => TermTypeJudgment(ename, substTypeInType tS x t)
        | TermMacro (n, e) => TermMacro(n, substTypeInExpr tS x e)
        | TermDefinition(n, e) => TermDefinition (n, substTypeInExpr tS x e)
        | DirectExpr(e) => DirectExpr (substTypeInExpr tS x e)
    
    fun substituteTypeInSignature (tS : Type) (x : TVar) (s : Signature) : Signature = 
        case s of
            [] => []
            | (d :: ds) => substituteTypeInDeclaration tS x d :: 
            substituteTypeInSignature tS x ds

(* semantic type equivalence *)
    and typeEquiv (ctx : (Type * Type) list) (t1:Type) (t2:Type)  :bool = 
    (let 
    fun typeEquivLst (l1 : (Label * Type)list) (l2 : (Label * Type) list)= 
            if length l1 <> length l2 then false else
            List.foldr (fn (b1, b2) => b1 andalso b2) true (List.tabulate((List.length l1), (fn i => 
            (#1 (List.nth(l1, i))) = (#1 (List.nth(l2, i)))
            andalso typeEquiv ctx (#2 (List.nth(l1, i))) (#2 (List.nth (l2, i))) )))
    fun unifyBinding tv t2 tv' t2' =
            if tv = tv' then (tv, t2, tv', t2')
            else let val nn = uniqueName() in
            (nn, substTypeInType (TypeVar nn) tv t2,
            nn, substTypeInType (TypeVar nn) tv' t2')end
        in
        if List.exists (fn p => p =(t1, t2)) ctx then true
        else
    (case (t1, t2) of
              (TypeVar t1, TypeVar t2) => t1 = t2
            | (Prod l1, Prod l2) =>  typeEquivLst l1 l2
            | (Sum l1, Sum l2) =>   typeEquivLst l1 l2
            | (Func (t1,t2), Func (t1', t2')) => typeEquiv ctx t1 t1' andalso typeEquiv ctx t2 t2'
            | (Forall (tv,t2), Forall (tv', t2')) => let val (_, t1, _, t1') = unifyBinding tv t2 tv' t2' in typeEquiv ctx t1 t1' end
            | (Exists (tv,t2), Exists (tv', t2')) => let val (_, t1, _, t1') = unifyBinding tv t2 tv' t2' in typeEquiv ctx t1 t1' end
            | (Rho (tv,t2), Rho (tv', t2')) => (let val (v, t1, v', t1') = unifyBinding tv t2 tv' t2' 
            in typeEquiv ((Rho(v, t1), Rho(v',t1'))::ctx) 
                (substTypeInType (Rho (v,t1)) v t1)
                (substTypeInType (Rho (v',t1')) v' t1')
             end)
            | (UnitType, UnitType) => true
            | (NullType, NullType) => true
            | _ => false)
    end)
end