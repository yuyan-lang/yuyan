structure TypeCheckingPass = struct
open TypeCheckingAST
open TypeCheckingASTOps
open StaticErrorStructure
infix 5 >>= 
infix 5 >> 
infix 5 <|>
infix 6 =/=
infix 5 <?>

    (* val DEBUG = true *)
    val DEBUG = false
    val DEBUGSHOWEXPR = true


    (*  !!! we assume the context is well formed in the sense that 
    all term type judgments have no free type variables !!! *) 
    (* so before anything is added to context, must perform substitution first ! *)
    (* the context is not a telescope !!! *)
    (* This also applies to type definitions! They must be expanded as well! *)
    (* ^^^ THis is not true !!! *)
    (* this is called the closed-world assumption and in practice, this 
    helps to reduce bugs during type checking *)
    (* also assume no name clash  *)
           
   
                    (* curStructure, curVisibility and mapping *)
  val addToCtxA = appendAbsoluteMappingToCurrentContext (* A for relative *)
  val addToCtxR = appendRelativeMappingToCurrentContext (* R for relative *)
    val addToCtxAL = appendAbsoluteMappingsToCurrentContext (* L for list *)
    val addToCtxRL = appendRelativeMappingsToCurrentContext (* L for list *)
    

 
    fun getMapping (c: context ):mapping list = 
        case c of  (Context(cSname, cVis, m)) => m

    fun showctx x = SOME(case x of 
    Context(curSName, curVis, m) =>  
    "当前结构名：" ^ StructureName.toStringPlain curSName ^
    "\n当前已定义的值及其类型：\n"  ^
            String.concatWith "；\n" (map (fn x => case x of
    TermTypeJ(e, t,_) => StructureName.toStringPlain e ^ "：" ^ PrettyPrint.show_typecheckingRType t
    | TypeDef(s, t, _) => StructureName.toStringPlain s ^ " = " ^ PrettyPrint.show_typecheckingRType t) m) ^ "\n"
          )

(* require lookup to add name qualification if references local structure *)
    fun lookup (Context(curSName, v, ctx) : context) (n : StructureName.t) : (StructureName.t * RType) witherrsoption= 
        let exception LookupNotFound
            fun lookupMapping (ctx : mapping list) (n : StructureName.t) (curSName : StructureName.t ): (StructureName.t * RType) witherrsoption= 
                case ctx of 
                (* WARNING: toUTF8String discards the separator information, but I guess it is fine because 
                    as long as all components are of the same name, we're fine*)
                    [] => raise LookupNotFound
                    (* ("name " ^ StructureName.toStringPlain n ^ " not found in context") *)
                    | TermTypeJ(n1, t1, u)::cs => 
                        (case StructureName.checkRefersTo n1 n curSName 
                        of SOME(cname) => Success (case u of NONE => cname | SOME(x) => x, t1)
                        | NONE => lookupMapping cs n curSName
                        )
                    | TypeDef(_) :: cs => lookupMapping cs n curSName
            val ntp = lookupMapping ctx n curSName 
                handle LookupNotFound =>
                genSingletonError (StructureName.toString n) ("名称`" ^ StructureName.toStringPlain n ^ "`未找到") (showctx (Context(curSName, v, ctx)))
        in 
            ntp 
        end
        (* handle LookupNotFound s1 => 
            (* (let val tp = lookupMapping ctx (curSName@n)
             (* try both absolute and relative path *)
             in (curSName@n, tp) end *)
            (* handle LookupNotFound s2 =>  *)
            raise TypeCheckingFailure (s1 ^ ", \n " ) *)
            (* ) *)
        
    fun nextContextOfOpenStructure  (curSName : StructureName.t) (curVis : bool) (bindings : (StructureName.t option) gmapping list) 
    (openName : StructureName.t)=

     Context(curSName, curVis, 
            (* extract all bindings from bindings in order and put them into the current context *)
                    List.mapPartial (fn x => 
                    case x of TermTypeJ(name, t, u) => 
                    (case StructureName.checkRefersToScope name openName curSName of
                        SOME(nameStripped) => SOME(TermTypeJ(curSName@nameStripped, t, (case u of SOME x => SOME x | NONE => SOME (name))))
                        | NONE => NONE)
                    | TypeDef(name, t, u) =>
                    (case StructureName.checkRefersToScope name openName curSName of
                        SOME(nameStripped) => SOME(TypeDef(curSName@nameStripped, t, u))
                        | NONE => NONE)
                    ) bindings @ bindings
                )

    fun reExportDecls  (ctx as Context(curSName ,curVis, bindings): context)
    (reexportName : StructureName.t) : CSignature witherrsoption =

            (* extract all bindings from bindings in order and put them into the current context *)
        let val decls = 
        List.mapPartial (fn x => 
            case x of TermTypeJ(name, t, u) => 
            (case StructureName.checkRefersToScope name reexportName curSName of
                SOME(nameStripped) => SOME(CTermDefinition(curSName@nameStripped, (case u of SOME x => CVar(x) | NONE => CVar (name)), rTypeToCType t))
                | NONE => NONE)
            | TypeDef(name, t, u) =>
            (case StructureName.checkRefersToScope name reexportName curSName of
                SOME(nameStripped) => SOME(CTypeMacro(curSName@nameStripped, rTypeToCType t))
                | NONE => NONE)
            ) bindings 
        in if length decls > 0
        then Success(List.rev decls) (* context order are reverse of reexport order *)
        else genSingletonError (StructureName.toString reexportName) "结构未包含任何可导出的值" (showctx ctx)
        end

        

    fun applyContextTo (ctx : context) (subst : RType -> StructureName.t -> 'a -> 'a) (t : 'a) : 'a = 
    (
        (* print ("apply ctx to gen called"  ^ Int.toString(case ctx of (Context(_, _, l)) => length l)^ "\n") ; *)
        case ctx of Context(curName, curVis, mapl) =>
        (case mapl of
            [] => t
            | TypeDef(n1, t1, u)::cs => (
                let val stepOne = (subst t1 n1 t)
                    val stepTwo = (subst t1 (StructureName.stripPrefixOnAgreedParts curName n1) stepOne)
                    val rest = applyContextTo (Context(curName, curVis, cs)) subst stepTwo
                    in rest end
                )
                (* print "HHHH"; *)
            (* the current subsituting name is a prefix! we need also to perform local subsitution *)
            (* always eagerly perform prefix-stripped substitutions *)
            | TermTypeJ(_) :: cs => applyContextTo (Context(curName, curVis, cs)) subst t)
            (* to get the semantics correct, context need to be applied in reverse order *)
            (* no reverse function is called because context is in reverse order *)
    )
    fun applyContextToType (ctx : context) (t : RType) : RType = 
    (
        (* print "apply ctx to type called\n"; *)
        applyContextTo ctx (fn t => fn  l => fn t1 => 
        (let 
        val _ = 
        if DEBUG then  print (" apply context subsituting "^ PrettyPrint.show_typecheckingRType t  
        ^ " for " ^ StructureName.toStringPlain l ^ " in " ^ PrettyPrint.show_typecheckingRType t1 ^  "\n") else ()
            val res = substTypeInRExpr t l t1
            val _ =if DEBUG then print (" res is " ^ PrettyPrint.show_typecheckingRType res ^ "\n") else ()
            in res end
            )) t
    )
    fun applyContextToExpr (ctx : context) (e : RExpr) : RExpr = 
        applyContextTo ctx (fn t => fn  l => fn e1 => 
        (let 
        val _ = 
        if DEBUG then  print (" apply context subsituting "^ PrettyPrint.show_typecheckingRType t  
        ^ " for " ^ StructureName.toStringPlain l ^ " in " ^ PrettyPrint.show_typecheckingRExpr e1 ^  "\n") else ()
            val res = substTypeInRExpr t l e1
            val _ =if DEBUG then print (" res is " ^ PrettyPrint.show_typecheckingRExpr res ^ "\n") else ()
            in res end
            )) e
    fun applyContextToSignature (ctx : context) (s : RSignature) : RSignature = 
        applyContextTo ctx (substituteTypeInRSignature) s


    fun lookupLabel ( ctx : (Label * 'a) list) (l : Label) : 'a witherrsoption = 
        case ctx of 
            [] =>  genSingletonError l ("标签`" ^ UTF8String.toString l ^ "`未找到") NONE
            (* raise TypeCheckingFailure ("label " ^ UTF8String.toString l ^ " not found in prod type") *)
            | (n1, t1)::cs => if UTF8String.semanticEqual n1 l then Success t1 else lookupLabel cs l

      fun lookupLabel3 ( ctx : (Label * EVar *RType) list) (l : Label) : RType witherrsoption = 
        case ctx of 
            [] => genSingletonError l ("标签`" ^ UTF8String.toString l ^ "`未找到") NONE
            (* raise TypeCheckingFailure ("label " ^ UTF8String.toString l ^ " not found in sum type") *)
            | (n1, _, t1)::cs => if UTF8String.semanticEqual n1 l then Success t1 else lookupLabel3 cs l



    fun typeUnify (e : RExpr) (a : RType list) : RType witherrsoption =
        case a of
            [] => raise Fail ("INternal error: empty sum")
            | [t] => Success t
            | (x::y :: xs) => if typeEquiv []  x y then typeUnify e (x :: xs)
            else genSingletonError (reconstructFromRExpr e) "类型不相等"  (SOME 
                ("第一类型：" ^  (PrettyPrint.show_typecheckingRType x) 
                ^ "\n第二类型：" ^  (PrettyPrint.show_typecheckingRType x)
            ))
            (* raise TypeCheckingFailure ("Type unify failed") *)
    
    structure Errors = struct 
        fun typeMismatch e synthesized checked= genSingletonError (reconstructFromRExpr e)
                ((if DEBUGSHOWEXPR then "`" ^ PrettyPrint.show_typecheckingRExpr e ^ "`" else "") ^ "类型不匹配(type mismatch) \n 推断的类型(synthesized type) : " ^ PrettyPrint.show_typecheckingRType synthesized
                ^ " \n 检查的类型(checked type) : " ^ PrettyPrint.show_typecheckingRType checked) NONE
        fun exprTypeError e tt ctx msg= genSingletonError (reconstructFromRExpr e) 
        ((if DEBUGSHOWEXPR then 
        "`" ^ PrettyPrint.show_typecheckingRExpr e ^ "`:" ^
        "`" ^ PrettyPrint.show_typecheckingCType tt ^ "`" 
        else "") ^ msg) (showctx ctx)
        fun exprError e ctx msg= genSingletonError (reconstructFromRExpr e) 
        ((if DEBUGSHOWEXPR then 
        "`" ^ PrettyPrint.show_typecheckingRExpr e ^ "`"
        else "") ^ msg) (showctx ctx)
        fun attemptToProjectNonProd e tt ctx = exprTypeError e tt ctx "试图从非乘积类型中投射(attempt to project out of product type)"
        fun attemptToProjectNonLazyProd e tt ctx = exprTypeError e tt ctx "试图从非乘积类型中投射(attempt to project out of lazy product type)"
        fun attemptToCaseNonSum e tt ctx = exprTypeError e tt ctx "试图对非总和类型进行分析(attempt to case on non-sum types)"
        fun attemptToApplyNonFunction e tt ctx = exprTypeError e tt ctx "试图使用非函数(attempt to apply on nonfunction types)"
        fun attemptToApplyNonUniversal e tt ctx = exprTypeError e tt ctx "试图使用非通用类型(attempt to apply on nonuniversal types)"
        fun openTypeCannotExitScope e tt ctx = exprTypeError e tt ctx "‘打开’的类型不能退出作用域(open's type cannot exit scope)"
        fun attemptToOpenNonExistentialTypes e tt ctx =  exprTypeError e tt ctx "试图打开非存在类型(attempt to open non existential types)"
        fun attemptToUnfoldNonRecursiveTypes e tt ctx =  exprTypeError e tt ctx "试图展开非递归类型(attempt to unfold non recursive type)"
        fun expressionDoesNotSupportTypeSynthesis e ctx =  exprError e ctx "表达式不支持类型合成，请指定类型"
        fun prodTupleLengthMismatch e tt ctx =  exprTypeError e tt ctx "数组长度与类型不匹配( prod tuple length mismatch)"
        fun lazyProdTupleLengthMismatch e tt ctx =  exprTypeError e tt ctx "数组长度与类型不匹配(lazy prod tuple length mismatch)"
        fun expectedProdType e tt ctx =  exprTypeError e tt ctx "期待的类型是乘积类型(expected prod)"
        fun expectedLazyProdType e tt ctx =  exprTypeError e tt ctx "期待的类型是惰性乘积类型(expected lazy prod)"
        fun expectedSumType e tt ctx =  exprTypeError e tt ctx "期待总和类型(expected sum types)"
        fun expectedFunctionType e tt ctx =  exprTypeError e tt ctx "期待函数类型(expected sum types)"
        fun expectedExistentialType e tt ctx =  exprTypeError e tt ctx "期待存在类型(expected existential types)"
        fun expectedUniversalType e tt ctx =  exprTypeError e tt ctx "期待通用类型(expected universal types)"
        fun expectedRecursiveType e tt ctx =  exprTypeError e tt ctx "期待递归类型(expected existential types)"
        fun firstArgumentOfCCallMustBeStringLiteral e ctx =  exprError e ctx "C调用的第一个参数必须是字符串(first argument of ccall must be a string literal)"
        fun ccallArgumentsMustBeImmediate e ctx =  exprError e ctx "C调用的参数必须是直接值(arguments of ccall must be immediate)"
        fun typeDeclContainsFreeVariables s ctx =  genSingletonError s ("类型声明不可以包含未定义的类型(type decl cannot contain free variables)") (showctx ctx)
        fun termTypeDeclContainsFreeVariables s ctx =  genSingletonError s ("值类型声明不可以包含未定义的类型(type decl cannot contain free variables)") (showctx ctx)
        fun importError s ctx = genSingletonError s ("导入模块时出错") (showctx ctx)
    end


    fun configureAndTypeCheckSignature
    (topLevelStructureName : StructureName.t)
    (
        getTypeCheckedAST:  (FileResourceURI.t * StructureName.t) -> TypeCheckingAST.CSignature witherrsoption
    )
    :  RSignature -> CSignature witherrsoption =
    let

            fun synthesizeType (ctx : context)(e : RExpr) : (CExpr * CType) witherrsoption =
            (
                let val _ = if DEBUG then print ("synthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e ^ "\n") else ()
                val originalExpr = e
                val res = case e of
                    RVar v => lookup ctx v >>= (fn (canonicalName, tp) =>
                                        Success (CVar canonicalName, rTypeToCType tp))
                    | RUnitExpr(soi) => Success (CUnitExpr, CUnitType)
                    | RProj(e, l, soi) => synthesizeType ctx e >>= (fn tt =>  case tt of 
                            (ce, CProd ls) => fmap (fn x => (CProj(ce, l, CTypeAnn(CProd ls)),x)) (lookupLabel ls l)
                            | _ => Errors.attemptToProjectNonProd e (#2 tt) ctx
                    )
                    | RLazyProj(e, l, soi) => synthesizeType ctx e >>= (fn t =>  case t of 
                            (ce, CLazyProd ls) => fmap (fn x => (CLazyProj(ce, l, CTypeAnn(CLazyProd ls)),x)) (lookupLabel ls l)
                            | _ => Errors.attemptToProjectNonLazyProd e (#2 t) ctx
                    )
                    | RIfThenElse(e, tcase, fcase, soi)=> checkType ctx e (RBuiltinType BIBool) >>= (fn ce => 
                        (synthesizeType ctx tcase >>= (fn (ctcase, rttp) => 
                                checkType ctx fcase (cTypeToRType rttp) >>= (fn cfcase => 
                                    Success(CIfThenElse(ce, ctcase, cfcase), rttp)
                                )
                            ) 
                        ) <|> (fn () => (* alternative: either branch may synthesize *)
                                    (synthesizeType ctx fcase >>= (fn (cfcase, rttp) => 
                                                checkType ctx tcase (cTypeToRType rttp) >>= (fn ctcase => 
                                                    Success(CIfThenElse(ce, ctcase, cfcase), rttp)
                                                )
                                            ) 
                                        )
                        )
                    )
                    | RCase(e,cases, soi) => (synthesizeType ctx e) >>= (fn t => case t of
                            (ce, CSum ls) => let 
                            val checkedCases = collectAll (map (fn (l, ev, e) => 
                                (lookupLabel ls l) >>= (fn lookedUpType => 
                                        synthesizeType (addToCtxA (TermTypeJ([ev], cTypeToRType lookedUpType, NONE)) ctx) e
                                    )
                                ) 
                            cases)
                            val casesTypes : CType list witherrsoption =  fmap (map (#2)) checkedCases 
                            val checkedExprs : CExpr list witherrsoption =  fmap (map (#1)) checkedCases
                            val returnType : CType witherrsoption = fmap rTypeToCType (casesTypes >>= (fn  ctps => typeUnify originalExpr (map cTypeToRType ctps)))
                            in 
                                (* I don't know how to make these look nice, this is just to unroll the witherrsoption *)
                                checkedCases >>= (fn checkedCases =>
                                casesTypes >>= (fn casesTypes  =>
                                checkedExprs >>= (fn checkedExprs => 
                                returnType >>= (fn returnType =>
                                Success (CCase ((CTypeAnn(CSum ls), ce), (List.tabulate(length cases, fn i => 
                                    let val (label, evar, _) = List.nth(cases, i)
                                    in (label, evar, #1 (List.nth(checkedCases, i)))
                                    end
                                    )), CTypeAnn(returnType)), returnType)
                                )
                                )
                                )
                                )
                            end
                            | _ => Errors.attemptToCaseNonSum e (#2 t) ctx
                            )
                    | RLamWithType (t, ev, e, soi) => 
                        synthesizeType (addToCtxA (TermTypeJ([ev], t, NONE)) ctx) e >>= (fn (bodyExpr, returnType) =>
                        Success(CLam(ev, bodyExpr, CTypeAnn(CFunc (rTypeToCType t, returnType))), CFunc(rTypeToCType t, returnType))
                        )
                    | RApp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn t => case t
                        of (ce1, CFunc (t1, t2)) => 
                        (checkType ctx e2 (cTypeToRType t1)) >>= (fn ce2 => 
                        Success(CApp (ce1,ce2, CTypeAnn(CFunc(t1,t2))), t2)
                        )
                        | (_, t) => Errors.attemptToApplyNonFunction e (t) ctx
                    )
                    | RTAbs (tv, e2, soi) =>   synthesizeType ctx  e2 >>= (fn (ce2, bodyType) => 
                    Success (CTAbs(tv, ce2, CTypeAnn(CForall (tv, bodyType))), CForall (tv, bodyType)) )
                    | RTApp (e2, t, soi) => synthesizeType ctx e2 >>= (fn st => case st of
                        (ce2, CForall (tv, tb)) => 
                            (* important need to normalized before subst *)
                            (normalizeType t >>= (fn nt => 
                                Success(CTApp(ce2, rTypeToCType t, CTypeAnn(CForall(tv, tb))), rTypeToCType (substTypeInRExpr nt [tv] (cTypeToRType tb)))
                            ))
                        | _ => Errors.attemptToApplyNonUniversal e (#2 st) ctx
                        )
                    | ROpen (e1, (tv, ev, e2), soi) => synthesizeType ctx e1 >>= (fn synt => case synt  of
                                (ce1, CExists (tv', tb)) => 
                        synthesizeType (addToCtxA (TermTypeJ([ev], 
                        substTypeInRExpr (RVar [tv]) [tv'] (cTypeToRType tb), NONE)) ctx) e2 >>= (fn (ce2, synthesizedType) =>
                        if List.exists (fn t => t = [tv]) (freeTVar (cTypeToRType synthesizedType))
                            then Errors.openTypeCannotExitScope e synthesizedType ctx
                            else Success(COpen((CTypeAnn(CExists(tv', tb)), ce1), (tv, ev, ce2), CTypeAnn(synthesizedType)), synthesizedType)
                        )
                            | _ => Errors.attemptToOpenNonExistentialTypes e (#2 synt) ctx)
                    | RUnfold (e2, soi) => synthesizeType ctx e2 >>= (fn synt => case synt of
                        (ce2, CRho (tv, tb)) => Success (CUnfold(ce2, CTypeAnn(CRho(tv, tb))),  rTypeToCType (substTypeInRExpr (RRho (tv, cTypeToRType tb)) [tv] (cTypeToRType tb)))
                        | _ => Errors.attemptToUnfoldNonRecursiveTypes e (#2 synt) ctx
                        )
                    | RStringLiteral(l, soi) => Success(CStringLiteral l, CBuiltinType(BIString))
                    | RIntConstant(i, soi) => Success(CIntConstant i, CBuiltinType(BIInt))
                    | RRealConstant (r, soi) => Success(CRealConstant (NumberParser.toRealValue r), CBuiltinType(BIReal))
                    | RBoolConstant (b, soi) => Success(CBoolConstant b, CBuiltinType(BIBool))
                    

                    | RLetIn(decls, e, soi) => (case ctx of 
                        Context(curName, curVis, bindings) => 
                typeCheckSignature (Context(curName@StructureName.localName(), curVis, bindings)) decls [] >>= 
                (fn (Context(localName, _, newBindings), csig) =>
                        synthesizeType (Context(localName,curVis, newBindings)) e >>= 
                        (fn (ce, synthesizedType) =>
                                Success (CLetIn(csig, ce, CTypeAnn(synthesizedType)), synthesizedType)
                        )
                        )
                    )
                    | RBuiltinFunc(f, s) => Success(CBuiltinFunc(f), rTypeToCType (BuiltinFunctions.typeOf f))
                    | RSeqComp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn (ce1, t1) => 
                        synthesizeType ctx e2 >>= (fn (ce2, t2) => 
                            Success(CSeqComp(ce1, ce2, CTypeAnn(t1), CTypeAnn(t2)), t2)
                        ))
                    (* | Fix (ev, e)=> Fix (ev, substTypeInExpr tS x e) *)
                    | _ => Errors.expressionDoesNotSupportTypeSynthesis e ctx
                    

                    val _ = if DEBUG then print ( "synthesize got result " ^
                    PrettyPrint.show_static_error res (fn res => PrettyPrint.show_typecheckingCType (#2 res))^
                    " whensynthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e ^ "\n") else ()
                    in res
                    end )
                (* handle TypeCheckingFailure s => 
                    raise TypeCheckingFailure (s ^ "\n when synthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e 
                    ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx) *)

            and assertTypeEquiv (expr: RExpr) (synthesized : CType) (checked : RType) : unit witherrsoption =
                if typeEquiv [] (cTypeToRType synthesized) checked 
                then Success() 
                else Errors.typeMismatch expr (cTypeToRType synthesized) checked
            and checkType (ctx : context) (e : RExpr) (tt: RType) (* tt target type *) : CExpr witherrsoption =
                (let 
                    val _ = if DEBUG then  print(  "checking the expr " ^ PrettyPrint.show_typecheckingRExpr e ^ 
                        " against type " ^ PrettyPrint.show_typecheckingRType tt ^ "\n") else ()
                    val originalExpr = e
                    val res = 
                    case e of
                    RVar v => 
                    (synthesizeType ctx e) >>= (fn (synthExpr, synthType) =>
                    assertTypeEquiv e (synthType) tt  >> (Success (synthExpr))
                    (* = false 
                                then raise TypeCheckingFailure ("var type mismatch var is " ^ StructureName.toStringPlain v 
                                ^ " synthesized : " ^ PrettyPrint.show_typecheckingType (#2 (synthesizeType ctx e)) ^ " against : " 
                                ^ PrettyPrint.show_typecheckingType tt
                                ) *)
                                (* else synthExpr *)
                    )
                    | RUnitExpr(soi) => assertTypeEquiv e CUnitType  tt >> (Success(CUnitExpr))
                    | RTuple (l, soi) => (case tt of 
                        RProd ls => if List.length l <> List.length ls
                                    then Errors.prodTupleLengthMismatch e (rTypeToCType tt) ctx
                                    else collectAll (List.tabulate(List.length l, (fn i => 
                                    checkType ctx (List.nth(l, i)) (#2 (List.nth(ls, i)))))) >>= (fn checkedElems => 
                                    Success(CTuple ( checkedElems, CTypeAnn(rTypeToCType (RProd ls)))))
                        | _ => Errors.expectedProdType e (rTypeToCType tt) ctx
                        )
                    | RLazyTuple (l, soi) => (case tt of 
                        RLazyProd ls => if List.length l <> List.length ls
                                    then Errors.lazyProdTupleLengthMismatch e (rTypeToCType tt) ctx
                                    else collectAll (List.tabulate(List.length l, (fn i => 
                                    checkType ctx (List.nth(l, i)) (#2 (List.nth(ls, i)))))) >>= (fn checkedElems => 
                                    Success(CLazyTuple ( checkedElems, CTypeAnn(rTypeToCType (RLazyProd ls)))))
                        | _ => Errors.expectedLazyProdType e (rTypeToCType tt) ctx
                        )
                    | RProj(e, l, soi) =>
                    synthesizeType ctx (RProj(e, l, soi)) >>= (fn synt => case synt of
                        (CProj(ce, l, prodType), synthType) => assertTypeEquiv originalExpr synthType tt >> 
                        Success(CProj(ce, l, prodType))
                        | _ => raise Fail "tcp229")
                    | RLazyProj(e, l, soi) =>
                    synthesizeType ctx (RLazyProj(e, l, soi)) >>= (fn synt => case synt of
                        (CLazyProj(ce, l, lazyProdType), synthType) => assertTypeEquiv originalExpr synthType tt >> 
                        Success(CLazyProj(ce, l, lazyProdType))
                        | _ => raise Fail "tcp229")

                    | RInj (l, e, soi) => (case tt of
                        RSum ls => (lookupLabel ls l) >>= (fn lookedupType => 
                                checkType ctx e lookedupType >>= (fn checkedExpr => 
                                    Success(CInj(l, checkedExpr, CTypeAnn(rTypeToCType (RSum ls))))
                                ))
                        | _ => Errors.expectedSumType originalExpr (rTypeToCType tt) ctx
                    )
                    | RIfThenElse(e, tcase, fcase, soi) => (checkType ctx e (RBuiltinType(BIBool))  >>= (fn ce => 
                        checkType ctx tcase tt >>= (fn ctcase => 
                            checkType ctx fcase tt >>= (fn cfcase => 
                                Success(CIfThenElse(ce, ctcase, cfcase))
                            )
                        )
                    ))
                    | RCase(e,cases, soi) => (synthesizeType ctx e) >>= (fn synt => case synt of
                            (ce, CSum ls) => 
                            (collectAll (map (fn (l, ev, e) => 
                                fmap (fn ce => (l, ev, ce)) 
                                    ((lookupLabel ls l) >>= (fn lookedUpType => 
                                        checkType (addToCtxA (TermTypeJ([ev], (cTypeToRType lookedUpType) , NONE)) ctx) e tt))
                                ) cases)) >>= (fn checkedCases  
                                    => Success(CCase((CTypeAnn(CSum ls), ce), checkedCases , CTypeAnn((rTypeToCType tt)))))
                            | _ => Errors.attemptToCaseNonSum originalExpr (#2 synt) ctx)
                    | RLam(ev, eb, soi) => (case tt of
                        RFunc(t1,t2) => 
                            checkType (addToCtxA (TermTypeJ([ev], t1,NONE)) ctx) eb t2
                            >>= (fn checkedExpr => Success(CLam(ev, checkedExpr, CTypeAnn(rTypeToCType tt))))
                        | _ => Errors.expectedFunctionType e (rTypeToCType tt) ctx
                        )
                    | RLamWithType (t, ev, eb, soi) => (case tt of
                        RFunc(t1,t2) => (assertTypeEquiv e (rTypeToCType t) t1 >>
                            (checkType (addToCtxA (TermTypeJ([ev], t1, NONE)) ctx) eb t2 >>= (fn checkedBody => 
                                Success(CLam(ev, checkedBody , CTypeAnn(rTypeToCType tt))))
                                )
                            )
                        | _ => Errors.expectedFunctionType e  (rTypeToCType tt) ctx
                        )
                    | RSeqComp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn (ce1, t1) => 
                        checkType ctx e2 tt >>= (fn ce2 => 
                            Success(CSeqComp(ce1, ce2, CTypeAnn(t1), CTypeAnn(rTypeToCType tt)))
                        ))
                    | RApp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn synt => case synt 
                        of (ce1, CFunc (t1, t2)) => (
                        assertTypeEquiv e t2 tt >> (
                                checkType ctx e2 (cTypeToRType t1) >>= (fn checkedArg => 
                                    Success (CApp(ce1, checkedArg, CTypeAnn(CFunc(t1, t2))))
                                )
                            )
                        )
                        | _ => Errors.attemptToApplyNonFunction e (#2 synt) ctx)
                    | RTAbs (tv, e2, soi) => (case tt of
                        RForall (tv', tb) => 
                                checkType ctx e2 (substTypeInRExpr (RVar [tv]) [tv'] tb) >>= (fn ce2 => 
                                            Success(CTAbs (tv, ce2, CTypeAnn(rTypeToCType tt)))
                                )
                        | _ => Errors.expectedUniversalType e (rTypeToCType tt) ctx
                    )
                    | RTApp (e2, t, soi) => synthesizeType ctx e2  >>= (fn synt => case synt of
                        (ce2, CForall (tv, tb)) => (
                            (* need to normalize type! important! *)
                            (normalizeType t >>= (fn nt => 
                                assertTypeEquiv e (rTypeToCType tt) (substTypeInRExpr t [tv] (cTypeToRType tb))
                            )) >> Success(CTApp(ce2, rTypeToCType t, CTypeAnn(CForall(tv, tb)))))
                        | _ => Errors.attemptToApplyNonUniversal e (#2 synt) ctx
                        )
                    | RPack (t, e2, soi) => (case tt of
                        RExists (tv, tb) => 
                                checkType ctx e2 (substTypeInRExpr t [tv]  tb) >>= (fn ce2 => 
                                                Success(CPack(rTypeToCType t, ce2, CTypeAnn(rTypeToCType tt))))
                        | _ => Errors.expectedExistentialType e (rTypeToCType tt) ctx
                    )
                    | ROpen (e1, (tv, ev, e2), soi) => synthesizeType ctx e1 >>= (fn synt => case synt of
                        (ce1, CExists (tv', tb)) => 
                        checkType (addToCtxA (TermTypeJ([ev], substTypeInRExpr (RVar [tv]) [tv'] (cTypeToRType tb), NONE)) ctx) e2 tt
                        >>= (fn ce2 => 
                        Success(COpen((CTypeAnn(CExists (tv', tb)), ce1), (tv, ev, ce2), CTypeAnn(rTypeToCType tt)))
                        )
                        | _ => Errors.attemptToOpenNonExistentialTypes e (#2 synt) ctx
                    )
                    | RFold (e2, soi) => (case tt
                        of 
                        RRho (tv ,tb) => 
                        checkType ctx e2 (substTypeInRExpr (RRho(tv, tb)) [tv] tb)
                        >>= (fn ce2 => Success (CFold(ce2, CTypeAnn(rTypeToCType tt))))
                        | _ => Errors.expectedRecursiveType e (rTypeToCType tt) ctx
                            )
                    | RUnfold (e2,soi) => synthesizeType ctx e2  >>= (fn synt => case synt of
                        (ce2, CRho (tv, tb)) =>(
                            assertTypeEquiv e (rTypeToCType (substTypeInRExpr (RRho (tv, cTypeToRType tb)) [tv] (cTypeToRType tb))) tt >>
                            Success(CUnfold(ce2, CTypeAnn(CRho(tv,tb)))))
                        | _ => Errors.attemptToUnfoldNonRecursiveTypes e (#2 synt) ctx
                        )
                    | RFix (ev, e, soi)=> checkType (addToCtxA (TermTypeJ([ev] , tt, NONE)) ctx) e tt
                                        >>= (fn ce => Success(CFix(ev,ce, CTypeAnn(rTypeToCType tt))))
                    | RStringLiteral (s, soi) => (assertTypeEquiv e (CBuiltinType(BIString)) (tt) >> (Success (CStringLiteral s)))
                    | RIntConstant (i, soi) => (assertTypeEquiv e (CBuiltinType(BIInt)) tt >> (Success ( CIntConstant i)))
                    | RRealConstant (r, soi) => (assertTypeEquiv e (CBuiltinType(BIReal)) tt >> (Success (CRealConstant (NumberParser.toRealValue r))))
                    | RBoolConstant (r, soi) => (assertTypeEquiv e (CBuiltinType(BIBool)) tt >> (Success (CBoolConstant r)))
                    | RFfiCCall (e1, e2, soi) => (
                        case e1 of
                            RStringLiteral (cfuncName, soi) => 
                                let fun elaborateArguments  (args : StructureName.t list ) : CExpr witherrsoption = 
                                    fmap CFfiCCall(Success cfuncName =/= 
                                    collectAll (map (fn a => fmap (#1) (lookup ctx a)) args))
                                in
                                            (case e2 of 
                                                RVar v => (Success ([v])) >>= elaborateArguments
                                                | RTuple (l, soi) => (collectAll (map (fn arg => case arg of 
                                                    RVar v => Success (v)
                                                    | _ => Errors.ccallArgumentsMustBeImmediate arg ctx
                                                    (* raise TypeCheckingFailure "ccall arguments must be immediate values" *)
                                                    ) l)) >>= elaborateArguments
                                                | RUnitExpr(soi) => elaborateArguments []
                                                | e => raise Fail ("tcp439 : " ^ PrettyPrint.show_typecheckingRExpr e)
                                            )
                                end
                            | _ => Errors.firstArgumentOfCCallMustBeStringLiteral e1 ctx
                            (* raise TypeCheckingFailure "First argument of the ccall must be string literal" *)

                    )
                    | RLetIn(decls, e, soi) => (case ctx of 
                Context(curName, curVis, bindings) => 
                    typeCheckSignature (Context(curName@StructureName.localName(), curVis, bindings)) decls []
                    >>= (fn(Context(localName, _, newBindings), csig) =>
                        (* assume the typeChecking is behaving properly, 
                        no conflicting things will be added to the signature *)
                        (* sub context will be determined by whether the signature is private or not ? *)
                            checkType (Context(localName,curVis, newBindings)) e tt >>= (fn ce => 
                                        Success(CLetIn(csig, ce, CTypeAnn(rTypeToCType tt)))
                            )
                        )
                    
                        )
                    | RBuiltinFunc(f, soi) => (assertTypeEquiv e (rTypeToCType (BuiltinFunctions.typeOf f)) tt >> Success(CBuiltinFunc(f)))
                    | _ => genSingletonError (reconstructFromRExpr e) ("check type failed on " ^ PrettyPrint.show_typecheckingRType e 
                     ^ " <= " ^ PrettyPrint.show_typecheckingRType tt) NONE
                in res
                end 
        )
                    (* handle TypeCheckingFailure s =>
                    raise TypeCheckingFailure (s ^ "\n when checking the expr " ^ PrettyPrint.show_typecheckingRExpr e ^ 
                        " against type " ^ PrettyPrint.show_typecheckingType tt
                    ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx
                    ) *)
                    (* type check signature will return all bindings *)
            and typeCheckSignature(ctx : context) (s : RSignature) (acc : CSignature) : (context * CSignature) witherrsoption =

                (


                    if DEBUG then DebugPrint.p ("DEBUG " ^ PrettyPrint.show_typecheckingRSig s ^
                    " in context " ^ PrettyPrint.show_typecheckingpassctx ctx ^"\n") else (); 
                    (* DebugPrint.p ("TCSig r="  ^ Int.toString (length s) ^   " acc=" ^ Int.toString (length acc) ^
                    "\nDEBUG " ^ PrettyPrint.show_typecheckingRSig s ^
                    "\n"); *)

                    case s of
                    [] => Success(ctx, acc)
                    (* normalize should not change the set of free variables *)
                | RTypeMacro (n, t)::ss => 
                let val freeTVars = freeTVar (applyContextToType ctx t) in if freeTVars <> [] then 
                    Errors.typeDeclContainsFreeVariables (StructureName.toString (hd freeTVars)) ctx
                    else 
                    normalizeType (applyContextToType ctx t) >>= (fn normalizedType => 
                    (
                        (* DebugPrint.p (
                            StructureName.toStringPlain (getCurSName ctx)
                            ^" normlizedType is " ^ PrettyPrint.show_typecheckingType normalizedType ^ "\n")
                        ; *)
                    typeCheckSignature (addToCtxR (TypeDef([n], normalizedType, ())) ctx) ss 
                        (acc@[CTypeMacro((getCurSName ctx)@[n], rTypeToCType normalizedType)])
                    )
                        )
                    end
                | RTermTypeJudgment(n, t):: ss => 
                let val freeTVars = freeTVar (applyContextToType ctx t) in if freeTVars <> [] 
                    then Errors.termTypeDeclContainsFreeVariables (StructureName.toString (hd freeTVars)) ctx
                    (* raise SignatureCheckingFailure ("TermType decl contains free var" ^ PrettyPrint.show_sttrlist (freeTVar (applyContextToType ctx t)) ^" in "^ PrettyPrint.show_typecheckingType (applyContextToType ctx t))  *)
                    else 
                    normalizeType (applyContextToType ctx t) >>= (fn normalizedType => 
                    typeCheckSignature (addToCtxR (TermTypeJ([n], normalizedType, NONE)) ctx) ss (acc))
                end
                | RTermMacro(n, e) :: ss => 
                    synthesizeType ctx (applyContextToExpr ctx e) >>= 
                    (fn (transformedExpr , synthesizedType)  =>
                        typeCheckSignature (addToCtxR (TermTypeJ([n], cTypeToRType synthesizedType, NONE)) ctx) ss 
                            (acc@[CTermDefinition((getCurSName ctx)@[n], transformedExpr, synthesizedType)])
                    )
                | RTermDefinition(n, e) :: ss => 
        (lookup ctx [n]) >>= (fn (_, lookedUpType) => 
                let val transformedExprOrFailure = checkType ctx (applyContextToExpr ctx e) lookedUpType
                in 
                case transformedExprOrFailure of
                Success(transformedExpr) => typeCheckSignature ctx ss 
                                                (acc@[CTermDefinition((getCurSName ctx)@[n], transformedExpr, rTypeToCType lookedUpType)])
                | DErrors(l) => (case typeCheckSignature ctx ss (acc) of 
                            Success _ => DErrors(l)
                            | DErrors l2 => DErrors(l @l2)
                            | _ => raise Fail "tcp458"
                    )
                | _ => raise Fail "tcp457"
                end
        )
                | RStructure (vis, sName, decls) :: ss => 
                (case ctx of 
                Context(curName, curVis, bindings) => 
                    typeCheckSignature (Context(curName@[sName], vis, bindings)) decls [] >>=
                    (fn(Context(_, _, newBindings), checkedSig) =>
                        (* assume the typeChecking is behaving properly, 
                        no conflicting things will be added to the signature *)
                        (* sub context will be determined by whether the signature is private or not ? *)
                    typeCheckSignature (Context(curName, curVis, newBindings)) ss (acc@checkedSig)
                    )
                    
                )
                | ROpenStructure openName :: ss =>
                (case ctx of 
                Context(curName, curVis, bindings) => 
                    let val nextContext = nextContextOfOpenStructure curName curVis bindings openName
                        (* assume the typeChecking is behaving properly, 
                        no conflicting things will be added to the signature *)
                        (* sub context will be determined by whether the signature is private or not ? *)
                    in typeCheckSignature nextContext ss (acc)
                    end
                )
                | RReExportStructure reExportName :: ss =>
                        ((reExportDecls ctx reExportName) <?> ( fn _ =>
                            typeCheckSignature ctx ss (acc) (* we collect remaining possible failures *)
                        )) >>= (fn newBindings => 
                                            typeCheckSignature ctx ss (acc@newBindings)
                        )                
                        (* note that the order of <?> and >>= is important as >>= won't ignore previous error 
                        reverse would have exponential wasted computation *)
                | RImportStructure(importName, path) :: ss => 
                    (getTypeCheckedAST (path, importName)
                    <?> (fn _ => Errors.importError (StructureName.toString importName)  ctx)
                    )
                     >>= (fn csig => 
                        typeCheckSignature 
                        (addToCtxAL (List.mapPartial (fn x => case x of 
                            CTypeMacro(sname, t) => SOME(TypeDef(sname, cTypeToRType t, ()))
                            | CTermDefinition(sname, e, t) => SOME(TermTypeJ(sname, cTypeToRType t,NONE))
                            | CDirectExpr _ => NONE
                            | CImport _ => NONE
                            ) csig) ctx)
                        ss (acc@[CImport(importName, path)])
                    )
                | RDirectExpr e :: ss=> 
                    let 
                    val synthedExprOrFailure = (synthesizeType ctx (applyContextToExpr ctx e))
                    in case synthedExprOrFailure of 
                        Success(checkedExpr, synthesizedType) => typeCheckSignature ctx ss (acc@[CDirectExpr(checkedExpr, synthesizedType)])
                        | DErrors l => (case typeCheckSignature ctx ss (acc) of
                                        Success _ => DErrors l
                                        | DErrors l2 => DErrors (l @ l2)
                                        | _ => raise Fail "tcp 492"
                        )
                        | _ => raise Fail "tcp494"
                    end
                )
                (* handle SignatureCheckingFailure st =>
                raise TypeCheckingFailure (st ^ "\n when checking the signature " ^ PrettyPrint.show_typecheckingRSig s 
                    ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx
                    ) *)
    in 
        fn s => 
        let val res =  (typeCheckSignature 
            (Context (topLevelStructureName, true, 
                    []))
            s [])
                (* val _ = DebugPrint.p "Type checked top level\n"
                val _ = DebugPrint.p (PrettyPrint.show_typecheckingCSig res) *)
        in fmap (#2) res end
    end
end
