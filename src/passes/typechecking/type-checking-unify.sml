
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

    val uniqueName = StructureName.binderName


    fun typeUnify (e : RExpr) (failCallback : CExpr * CExpr -> (constraints * context) witherrsoption)
     (ctx : context) (t1:CExpr) (t2:CExpr)  : (constraints * context) witherrsoption = 
    let val recur = typeUnify e failCallback
        fun fail() = failCallback (t1, t2)
        fun unifyBinding tv t2 tv' t2' =
                if tv ~~= tv' then (tv, t2, tv', t2')
                else let val nn = uniqueName() in
                (nn, substTypeInCExpr (CVar([nn], CVTBinder)) [tv] t2,
                nn, substTypeInCExpr (CVar([nn], CVTBinder)) [tv'] t2')end
        fun unifyBindingOption tv t2 tv' t2' =
            case (tv, tv') of 
                (SOME tv, SOME tv') => unifyBinding tv t2 tv' t2'
                | (NONE, SOME tv') => unifyBinding (uniqueName()) t2 tv' t2
                | (SOME tv, NONE ) => unifyBinding tv t2 (uniqueName()) t2
                | (NONE, NONE) => unifyBinding (uniqueName()) t2 (uniqueName()) t2

        (* sigma type equality: bindings must also be equal *)
        fun typeEquivListWithLabel (ctx : context) (l1 : (Label * CExpr)list) (l2 : (Label * CExpr) list) : (constraints * context) witherrsoption= 
        case (l1, l2) of
            ([], []) => Success([], ctx)
            | (((lb1, e1)::l1t), ((lb2, e2)::l2t)) => 
                    if lb1 ~~= lb2 
                    then recur ctx e1 e2 >>= (fn (constraints, ctx) => 
                        case constraints of 
                        [] => typeEquivListWithLabel ctx l1t l2t
                        | _ => raise Fail "ni39: lst unify constraints"
                        )
                    else fail()
            | _ => fail()

        fun typeEquivList (ctx : context) (l1 :  CExpr list) (l2 : CExpr list) : (constraints * context) witherrsoption= 
        case (l1, l2) of
            ([], []) => Success([], ctx)
            | ((( e1)::l1t), (( e2)::l2t)) => 
                    recur ctx e1 e2 >>= (fn (constraints, ctx) => 
                        case constraints of 
                        [] => typeEquivList ctx l1t l2t
                        | _ => raise Fail "ni39: lst unify constraints"
                        )
            | _ => fail()

        fun toHeadSpineForm  (expr : CExpr) : CExpr * CExpr list = 
        case expr of 
            CApp(e1, e2, tp) => (case toHeadSpineForm  e1 of 
                (hd, tl') => (hd, tl'@[e2]))
            | _ => (expr, [])
        fun unifyMetaVar (metavar : StructureName.t) (xbar : CExpr list) (ns : CExpr) : (constraints * context) witherrsoption = 
            (* check each condition *)

        let fun toVarNames(xbar : CExpr) : StructureName.t  = 
                case xbar of
                    CVar(x, _) => x
                    | _ => raise Fail "tcunify66: meta var's args should all be plain"
            fun unique( l : StructureName.t list) =  
                case l of
                    [] => true
                    | (x :: xs) => if List.exists (fn y => StructureName.semanticEqual x y) xs then
                    (DebugPrint.p ("NOT UNIQUE : " ^StructureName.toStringPlain x); false) else unique xs
            
            fun subset(l1 : StructureName.t list) (l2 : StructureName.t list) = 
                case l1 of
                    [] => true
                    | (x :: xs) => if List.exists (fn y => StructureName.semanticEqual x y) l2 
                                   then subset xs l2
                                   else false
            
            fun ctxInScope (ctx : context) (metavar : StructureName.t) (term : CExpr) = 
                true (* TODO: implement scope checking *)
            val sxbar = (map toVarNames xbar)

            fun prependLambdas (xbar : StructureName.t list) (term : CExpr) : CExpr = 
                (foldr (fn (x, acc) => 
                let val name = uniqueName()
                in CLam(name, substTypeInCExpr (CVar ([name], CVTBinder)) x acc, CTypeAnnNotAvailable)
                end) 
                ns sxbar
            )
        in 
        lookupCtx ctx metavar >>= (fn (cname, tp, jtp)=> 
            case jtp of JTMetaVarPendingResolve _ =>
                (case ns of 
                CMetaVar(name) => if StructureName.semanticEqual name cname 
                then Success([], ctx)
                else Success([], modifyCtxResolveMetaVar ctx metavar (ns))
                | _  => 
                    if 
                    (* unique sxbar 
                    andalso *)
                    (* subset (freeTCVar ns) sxbar *)
                    (* andalso ctxInScope ctx metavar ns *)
                    (* TODO add checking *)
                    true
                    (* then Success([], modifyCtxResolveMetaVar ctx metavar (prependLambdas sxbar ns)) *)
                    then Success([], modifyCtxResolveMetaVar ctx metavar (ns))
                    else TypeCheckingErrors.genericError e ctx 
                    ("cannot unify metavar : " 
                    ^ "unique = " ^ Bool.toString (unique sxbar )
                    ^ "subset = " ^ Bool.toString(subset (freeTCVar ns) sxbar)
                    ^ "inscope = " ^ Bool.toString(ctxInScope ctx metavar ns )
                    ^ "names = " ^ String.concatWith "\n" (map StructureName.toStringPlain sxbar))
                )
            | JTMetaVarResolved t => recur ctx t ns
            | _ => raise Fail "ni108"
        )
        end

        
                    

    in

(weakHeadNormalizeType e ctx t1 =/= weakHeadNormalizeType e ctx t2) >>=  (fn ((t1, t2) : CType * CType) => 
            case (toHeadSpineForm t1, toHeadSpineForm t2) of
             ((CMetaVar(mv), xbar), _) => unifyMetaVar mv xbar t2 
             | (_, (CMetaVar(mv), xbar)) => unifyMetaVar mv xbar t1 
             | ((t1, t1l), (t2, t2l)) => 
                if length t1l = length t2l andalso length t1l = 0
                then 
                    (case (t1, t2) of
                        (CUniverse, CUniverse) => Success([], ctx)
                        | (CBuiltinType(b1), CBuiltinType(b2)) => if b1 = b2 then Success([], ctx) else fail()
                        | (CPiType (t1, ev1op,  t2, p), CPiType (t1', ev2op,  t2', p')) => 
                        if p <> p'
                        then fail()
                        else
                            let val (ev1, ut2, ev2',ut2') = unifyBindingOption ev1op t2 ev2op t2'
                            in
                                recur ctx t1 t1' >>= (fn (constraints, ctx) => 
                                    case constraints of 
                                        [] => recur (addToCtxA (TermTypeJ([ev1], t1, JTLocalBinder, NONE)) ctx) ut2 ut2'
                                        | _ => raise Fail "ni42: pi constaints"
                                )
                            end
                        | (CProd l1, CProd l2) => typeEquivList ctx l1 l2
                        | (CLabeledProd l1, CLabeledProd l2) =>  typeEquivListWithLabel ctx l1 l2
                        | (CVar (t1, CVTBinder), CVar (t2, CVTBinder)) => if (t1 ~~~= t2) then Success([], ctx) else fail()
                        | (CVar (t1, CVTConstructor _), CVar (t2, CVTConstructor _)) => if (t1 ~~~= t2) then Success([], ctx) else fail()
                        (* TODO: ^^^ should we care about the arguments ? *)
                        | (CUnitType, CUnitType) => Success([], ctx)
                        | (CProj(t1,  idx1, u1), CProj(t2,  idx2, u2)) => 
                            recur ctx t1 t2 >>= (fn 
                                ([], ctx) => if idx1 = idx2 
                                (* andalso lbl1 = lbl2  *)
                                             then Success([], ctx)
                                             else fail()
                                | _ => raise Fail "ni151"
                            )
                        | (CBlock decl1, CBlock decl2) => 
                        (
                            case (decl1, decl2) of 
                                ([], []) => Success([], ctx) 
                                | ((CPureDeclaration(name1, tp1) :: tl1, CPureDeclaration(name2, tp2) :: tl2) 
                                | (CTermDefinition(name1, _, tp1) :: tl1, CTermDefinition(name2, _, tp2) :: tl2)
                                | (CConstructorDecl(name1, tp1,_) :: tl1, CConstructorDecl(name2, tp2, _) :: tl2)
                                ) => 
                                if UTF8String.semanticEqual name1  name2 
                                then (recur ctx tp1 tp2) >>= (fn (_, ctx) => 
                                (* TODO: remove constraints, use pattern unification *)
                                    recur ctx (CBlock tl1) (CBlock tl2)
                                ) else fail()
                                | (CImport(name1, _) :: tl1, CImport(name2, _) :: tl2) => 
                                    if StructureName.semanticEqual name1 name2 
                                    then (
                                        recur ctx (CBlock tl1) (CBlock tl2)
                                    ) else fail()
                                | (COpenStructure(name1, _) :: tl1, COpenStructure(name2, _) :: tl2) => 
                                    if StructureName.semanticEqual name1 name2 
                                    then (
                                        recur ctx (CBlock tl1) (CBlock tl2)
                                    ) else fail()
                                | _ => fail()
                        )
                        | _ => fail())
                else recur ctx t1 t2 >>= (fn (constraints, ctx) => 
                    case constraints of 
                        [] => typeEquivList ctx t1l t2l
                        | _ => raise Fail "ni42: terms? constaints"
                )
            (* | _  => fail() *)
        )

         (* raise Fail ("ni19: Type unify of " ^ PrettyPrint.show_typecheckingCType t1 ^ " and " ^ PrettyPrint.show_typecheckingCType t2) *)
    end

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
(weakHeadNormalizeType e tcctx t1 =/= weakHeadNormalizeType e tcctx t2) >>=  (fn (t1, t2) => 
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
        typeUnify expr (fn (failedT1, failedT2) => 
             TypeCheckingErrors.typeMismatch expr (synthesized) checked failedT1 failedT2 ctx
        )
        ctx synthesized checked  >>= (fn (constraints, context) => 
        if length constraints = 0
        then Success( context)
        else raise Fail "ni85: type unify produces constraints"
            (* else TypeCheckingErrors.typeMismatch expr (synthesized) checked ctx *)
        )

end