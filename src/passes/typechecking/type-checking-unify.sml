
structure TypeCheckingUnify =
struct

    open TypeCheckingAST
    open TypeCheckingASTOps

open StaticErrorStructure
infix 5 >>=
infix 5 =/=
infix 4 ~<>
infix 4 ~~=
infix 4 ~~~=

    fun typeUnify (e : RExpr) (tcctx : context) (eqctx : (CExpr * CExpr) list) (t1:CExpr) (t2:CExpr)  : (constraints * context) witherrsoption = 
    raise Fail "ni16 : type unify"
    (* (let 
        (* TODO: remove the copying, have a dedicated context manager *)
       
        val recur = typeUnify e tcctx

        fun andAlso (b1 : bool witherrsoption) (b2 : bool witherrsoption) = 
            b1 >>= (fn b1' => if b1' then b2 else Success(false))
        
        fun typeEquivLst (l1 : (Label * CExpr)list) (l2 : (Label * CExpr) list) : bool witherrsoption= 
                if length l1 <> length l2 then Success(false) else
                List.foldr (fn (b1, b2) => andAlso b1 b2) (Success true) (List.tabulate((List.length l1), (fn i => 
                andAlso (
                    Success((#1 (List.nth(l1, i))) ~~= (#1 (List.nth(l2, i))))
                )
                    (recur eqctx (#2 (List.nth(l1, i))) (#2 (List.nth (l2, i))) ))
                ))
        fun unifyBinding tv t2 tv' t2' =
                if tv ~~= tv' then (tv, t2, tv', t2')
                else let val nn = uniqueName() in
                (nn, substTypeInCExpr (CVar([nn], CVTBinder)) [tv] t2,
                nn, substTypeInCExpr (CVar([nn], CVTBinder)) [tv'] t2')end

        
        val result = 
        if List.exists (fn ((p1, p2):(CExpr * CExpr)) => p1 = t1 andalso p2 = t2) eqctx then Success(true)
        else
(normalizeType e tcctx t1 =/= normalizeType e tcctx t2) >>=  (fn (t1, t2) => 
    (case (t1, t2) of
              (CVar (t1, CVTBinder), CVar (t2, CVTBinder)) => Success (t1 ~~~= t2)
            | (CVar (t1, CVTConstructor _), CVar (t2, CVTConstructor _)) => Success (t1 ~~~= t2) (* should we care about the arguments ? *)
            | (CVar (t1, _), CVar (t2, _)) => raise Fail "normalize should have replaced variables by their definitions"
            | (CProd l1, CProd l2) =>  typeEquivLst l1 l2
            | (CLazyProd l1, CLazyProd l2) =>  typeEquivLst l1 l2
            | (CSum l1, CSum l2) =>   typeEquivLst l1 l2
            | (CPiType (t1, ev1op, t2), CPiType (t1', ev2op, t2')) => 
                (case (ev1op, ev2op) of 
                    (NONE, NONE) => andAlso (recur eqctx t1 t1') (recur eqctx t2 t2')
                    | (SOME ev1, SOME ev2) => andAlso (recur eqctx t1 t1') 
                        (let val (_, ut2, _, ut2') = unifyBinding ev1 t2 ev2 t2'
                        in recur eqctx ut2 ut2'
                        end
                        )
                    | _ => Success(false)
                )
            | (CForall (tv,t2), CForall (tv', t2')) => let val (_, t1, _, t1') = unifyBinding tv t2 tv' t2' in recur eqctx t1 t1' end
            | (CExists (tv,t2), CExists (tv', t2')) => let val (_, t1, _, t1') = unifyBinding tv t2 tv' t2' in recur eqctx t1 t1' end
            | (CRho (tv,t2), CRho (tv', t2')) => (let val (v, t1, v', t1') = unifyBinding tv t2 tv' t2' 
            in recur ((CRho(v, t1), CRho(v',t1'))::eqctx) 
                (substTypeInCExpr (CRho (v,t1)) [v] t1)
                (substTypeInCExpr (CRho (v',t1')) [v'] t1')
             end)
            | (CUnitType, CUnitType) => Success(true)
            | (CNullType, CNullType) => Success(true)
            | (CBuiltinType(b1), CBuiltinType(b2)) => Success(b1 = b2)
            | (CUniverse, CUniverse) => Success(true)
            | (CApp(e1, e2, _), CApp(e1', e2', _) ) => andAlso (recur eqctx e1 e1') (recur eqctx e2 e2')
            | _ => Success(false))
            )
        (* val _  = DebugPrint.p ("Type equiv result of " ^ PrettyPrint.show_typecheckingCType t1 ^ " and " ^ PrettyPrint.show_typecheckingCType t2
         ^ " is " ^ PrettyPrint.show_static_error result Bool.toString) *)
        in 
        result
    end) *)

    fun tryTypeUnify (ctx : context) (expr: RExpr) (synthesized : CType) (checked : CType) : (context) witherrsoption =
        typeUnify expr ctx []  synthesized checked  >>= (fn (constraints, context) => 
        if length constraints = 0
        then Success( context)
        else raise Fail "ni85: type unify produces constraints"
            (* else TypeCheckingErrors.typeMismatch expr (synthesized) checked ctx *)
        )

end