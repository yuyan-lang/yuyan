structure TypeCheckingPass = struct
open TypeCheckingAST
open TypeCheckingASTOps
open TypeCheckingContext
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



    fun nextContextOfOpenStructure  (curSName : StructureName.t) (curVis : bool) (bindings : mapping list) 
    (openName : StructureName.t)=

     Context(curSName, curVis, 
            (* extract all bindings from bindings in order and put them into the current context *)
                    List.mapPartial (fn x => 
                    case x of TermTypeJ(name, t, jtp,  u) => 
                    (case StructureName.checkRefersToScope name openName curSName of
                        SOME(nameStripped) => SOME(TermTypeJ(curSName@nameStripped, t, jtp, 
                            (case u of SOME x => SOME x | NONE => SOME (name, jtp))))
                        | NONE => NONE)
                    (* | TermDefJ(name, t, u) =>
                    (case StructureName.checkRefersToScope name openName curSName of
                        SOME(nameStripped) => SOME(TermDefJ(curSName@nameStripped, t, u))
                        | NONE => NONE) *)
                    ) bindings @ bindings
                )

    fun reExportDecls  (ctx as Context(curSName ,curVis, bindings): context)
    (reexportName : StructureName.t) : CSignature witherrsoption =

            (* extract all bindings from bindings in order and put them into the current context *)
        let val decls = 
        List.mapPartial (fn x => 
            case x of TermTypeJ(name, t, jtype,  u) => 
            (case StructureName.checkRefersToScope name reexportName curSName of
                SOME(nameStripped) => SOME(CTermDefinition(curSName@nameStripped,  
                                            (case u of SOME (x, jtp) => CVar(x, (case jtp of JTypeDefinition d => CVarTypeDefinition d 
                                                                                            | _ => CVarTypeBinder))  (* TODO: BIG ISSUE: REExport of type constructors *)
                                            | NONE => CVar (name, (case jtype of JTypeDefinition d => CVarTypeDefinition d 
                                                                                            | _ => CVarTypeBinder))), t))
                                            (* TODO: export of constructors *)
                | NONE => NONE)
            (* | TypeDef(name, t, u) =>
            (case StructureName.checkRefersToScope name reexportName curSName of
                SOME(nameStripped) => SOME(CTypeMacro(curSName@nameStripped, t))
                | NONE => NONE) *)
            ) bindings 
        in if length decls > 0
        then Success(List.rev decls) (* context order are reverse of reexport order *)
        else genSingletonError (StructureName.toString reexportName) "结构未包含任何可导出的值" (showctxSome ctx)
        end

        

    (* fun applyContextTo (ctx : context) (subst : CType -> StructureName.t -> 'a -> 'a) (t : 'a) : 'a = 
    (
        (* print ("apply ctx to gen called"  ^ Int.toString(case ctx of (Context(_, _, l)) => length l)^ "\n") ; *)
        case ctx of Context(curName, curVis, mapl) =>
        (case mapl of
            [] => t
            | TermDefJ(n1, t1, u)::cs => (
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
    fun applyContextToType (ctx : context) (t : CType) : CType = 
    (
        (* print "apply ctx to type called\n"; *)
        applyContextTo ctx (fn t => fn  l => fn t1 => 
        (let 
        val _ = 
        if DEBUG then  print (" apply context subsituting "^ PrettyPrint.show_typecheckingCType t  
        ^ " for " ^ StructureName.toStringPlain l ^ " in " ^ PrettyPrint.show_typecheckingCType t1 ^  "\n") else ()
            val res = substTypeInCExpr t l t1
            val _ =if DEBUG then print (" res is " ^ PrettyPrint.show_typecheckingCType res ^ "\n") else ()
            in res end
            )) t
    ) *)
    (* fun applyContextToExpr (ctx : context) (e : RExpr) : RExpr = 
        applyContextTo ctx (fn t => fn  l => fn e1 => 
        (let 
        val _ = 
        if DEBUG then  print (" apply context subsituting "^ PrettyPrint.show_typecheckingCType t  
        ^ " for " ^ StructureName.toStringPlain l ^ " in " ^ PrettyPrint.show_typecheckingRExpr e1 ^  "\n") else ()
            (* val res = substTypeInCExpr t l e1 *)
            val res = raise Fail "undefined154"
            val _ =if DEBUG then print (" res is " ^ PrettyPrint.show_typecheckingRExpr res ^ "\n") else ()
            in res end
            )) e
    fun applyContextToSignature (ctx : context) (s : CSignature) : CSignature = 
        applyContextTo ctx (substituteTypeInCSignature) s *)


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



    fun typeUnify (ctx : context) (e : RExpr) (a : CType list) : CType witherrsoption =
        case a of
            [] => raise Fail ("INternal error: empty sum")
            | [t] => Success t
            | (x::y :: xs) =>typeEquiv e ctx []  x y  >>= (fn tpequiv => if  tpequiv then typeUnify ctx e (x :: xs)
            else genSingletonError (reconstructFromRExpr e) "类型不相等"  (SOME 
                ("第一类型：" ^  (PrettyPrint.show_typecheckingCType x) 
                ^ "\n第二类型：" ^  (PrettyPrint.show_typecheckingCType x)
            )))
            (* raise TypeCheckingFailure ("Type unify failed") *)
    
    structure Errors = struct 
        fun typeMismatch e synthesized checked ctx= genSingletonError (reconstructFromRExpr e)
                ((if DEBUGSHOWEXPR then "`" ^ PrettyPrint.show_typecheckingRExpr e ^ "`" else "") ^ "类型不匹配(type mismatch) \n 推断的类型(synthesized type) : " ^ PrettyPrint.show_typecheckingCType synthesized
                ^ " \n 检查的类型(checked type) : " ^ PrettyPrint.show_typecheckingCType checked) (showctxSome ctx)
        fun exprTypeError e tt ctx msg= genSingletonError (reconstructFromRExpr e) 
        ((if DEBUGSHOWEXPR then 
        "`" ^ PrettyPrint.show_typecheckingRExpr e ^ "`:" ^
        "`" ^ PrettyPrint.show_typecheckingCType tt ^ "`" 
        else "") ^ msg) (showctxSome ctx)
        fun exprError e ctx msg= genSingletonError (reconstructFromRExpr e) 
        ((if DEBUGSHOWEXPR then 
        "`" ^ PrettyPrint.show_typecheckingRExpr e ^ "`"
        else "") ^ msg) (showctxSome ctx)
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
        fun typeDeclContainsFreeVariables s ctx =  genSingletonError s ("类型声明不可以包含未定义的类型(type decl cannot contain free variables)") (showctxSome ctx)
        fun termTypeDeclContainsFreeVariables s ctx =  genSingletonError s ("值类型声明不可以包含未定义的类型(type decl cannot contain free variables)") (showctxSome ctx)
        fun importError s ctx = genSingletonError s ("导入模块时出错") (showctxSome ctx)
        fun redefinitionError s msg ctx prevDef = genSingletonErrorWithRelatedInfo s ("重复的定义：`" ^  msg ^ "`") (showctxSome ctx) 
            [ (prevDef, "之前的定义")]
        fun notATypeConstructor e ctx = exprError e ctx "不是一个类型构造器"
    end


    


    fun configureAndTypeCheckSignature
    (topLevelStructureName : StructureName.t)
    (
        getTypeCheckedAST:  (FileResourceURI.t * StructureName.t) -> TypeCheckingAST.CSignature witherrsoption
    )
    :  RSignature -> CSignature witherrsoption =
    let
            fun checkConstructorType( ctx : context) (t : RType) : (CType * cconstructorinfo) witherrsoption = 
            let val typeInfo : CType witherrsoption = 
                case t of 
                    RFunc(t1, t2, soi) => checkType ctx t1 (CUniverse) >>= (fn ct1 => 
                        checkType ctx t2 (CUniverse) >>= (fn ct2 => 
                            Success(CFunc(ct1, ct2))
                        )
                    )
                    | _ => checkType ctx t (CUniverse)



                (* only trace one level deep*)
                fun traceVarOnly(errReporting : RExpr) (cexpr : CExpr) =  case cexpr of
                    CUniverse => Success(CConsInfoTypeConstructor)
                    | CVar(v, vinfo) => (case vinfo of 
                        CVarTypeConstructor CConsInfoTypeConstructor =>  Success(CConsInfoElementConstructor v)
                        | CVarTypeDefinition (v') => traceVarOnly errReporting v'
                        | CVarTypeBinder => Errors.notATypeConstructor errReporting ctx
                    )
                    | _ => Errors.notATypeConstructor errReporting ctx

                fun analyzeVariable(v : StructureName.t) = 
                        lookupCtx ctx v  >>= (fn lookedUpJ => 
                                        case  lookedUpJ of
                                            (cname, tp, jinfo) => (case jinfo
                                            of JTypeConstructor (CConsInfoTypeConstructor) => 
                                                Success (CConsInfoElementConstructor cname)
                                                | JTypeDefinition (v') => (traceVarOnly (RVar(v)) v')
                                                | _ => Errors.notATypeConstructor (RVar(v)) ctx
                                            )
                        )
                fun getConsInfo (isCanonical : bool) (t : RType) = 
                if isCanonical
                then
                (case t of 
                    RFunc(t1, t2, soi) => getConsInfo true t2
                    | RPiType(t1, b, t2, soi) => getConsInfo true t2
                    | _ => getConsInfo false t)
                else
                (case t of 
                    RApp(t1, t2, soi) => getConsInfo false t1
                    | RVar(s) => analyzeVariable(s)
                    | RUniverse(s) => Success(CConsInfoTypeConstructor)
                    | _ => Errors.notATypeConstructor t ctx
                    )
                val consInfo = getConsInfo true t
            in 
            (typeInfo =/= consInfo)
            end

            and synthesizeType (ctx : context)(e : RExpr) : (CExpr * CType) witherrsoption =
            (
                let val _ = if DEBUG then print ("synthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e ^ "\n") else ()
                val originalExpr = e
                val res = case e of
                    RVar v => lookupCtxForType ctx v >>= (fn (canonicalName, tp) =>
                                    (case findCtxForDef ctx v of 
                                    SOME (cname2, def) => (if StructureName.semanticEqual cname2 canonicalName
                                    then Success (CVar(canonicalName, CVarTypeDefinition(def)), tp)
                                    else Success(CVar(canonicalName, CVarTypeBinder), tp))
                                    | NONE => Success(CVar(canonicalName, CVarTypeBinder), tp))
                                    )
                    | RUnitExpr(soi) => Success (CUnitExpr, CUnitType)
                    | RProj(e, l, soi) => synthesizeType ctx e >>= (fn tt =>  case tt of 
                            (ce, CProd ls) => fmap (fn x => (CProj(ce, l, CTypeAnn(CProd ls)),x)) (lookupLabel ls l)
                            | _ => Errors.attemptToProjectNonProd e (#2 tt) ctx
                    )
                    | RLazyProj(e, l, soi) => synthesizeType ctx e >>= (fn t =>  case t of 
                            (ce, CLazyProd ls) => fmap (fn x => (CLazyProj(ce, l, CTypeAnn(CLazyProd ls)),x)) (lookupLabel ls l)
                            | _ => Errors.attemptToProjectNonLazyProd e (#2 t) ctx
                    )
                    | RIfThenElse(e, tcase, fcase, soi)=> checkType ctx e (CBuiltinType BIBool) >>= (fn ce => 
                        (synthesizeType ctx tcase >>= (fn (ctcase, rttp) => 
                                checkType ctx fcase (rttp) >>= (fn cfcase => 
                                    Success(CIfThenElse(ce, ctcase, cfcase), rttp)
                                )
                            ) 
                        ) <|> (fn () => (* alternative: either branch may synthesize *)
                                    (synthesizeType ctx fcase >>= (fn (cfcase, rttp) => 
                                                checkType ctx tcase (rttp) >>= (fn ctcase => 
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
                                        synthesizeType (addToCtxA (TermTypeJ([ev], lookedUpType, JTypeLocalBinder, NONE)) ctx) e
                                    )
                                ) 
                            cases)
                            val casesTypes : CType list witherrsoption =  fmap (map (#2)) checkedCases 
                            val checkedExprs : CExpr list witherrsoption =  fmap (map (#1)) checkedCases
                            val returnType : CType witherrsoption = (casesTypes >>= (fn  ctps => typeUnify ctx originalExpr (ctps)))
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
                        synthesizeType (addToCtxA (TermTypeJ([ev], rTypeToCType ctx t, JTypeLocalBinder, NONE)) ctx) e >>= (fn (bodyExpr, returnType) =>
                        Success(CLam(ev, bodyExpr, CTypeAnn(CFunc (rTypeToCType ctx t, returnType))), CFunc(rTypeToCType ctx t, returnType))
                        )
                    | RApp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn t => case t
                        of (ce1, CFunc (t1, t2)) => 
                        (checkType ctx e2 (t1)) >>= (fn ce2 => 
                        Success(CApp (ce1,ce2, CTypeAnn(CFunc(t1,t2))), t2)
                        )
                        | (_, t) => Errors.attemptToApplyNonFunction e (t) ctx
                    )
                    | RTAbs (tv, e2, soi) =>   synthesizeType ctx  e2 >>= (fn (ce2, bodyType) => 
                    Success (CTAbs(tv, ce2, CTypeAnn(CForall (tv, bodyType))), CForall (tv, bodyType)) )
                    | RTApp (e2, t, soi) => synthesizeType ctx e2 >>= (fn st => case st of
                        (ce2, CForall (tv, tb)) => 
                            (* important need to normalized before subst *)
                            (normalizeType t ctx (rTypeToCType ctx t) >>= (fn nt => 
                                Success(CTApp(ce2, rTypeToCType ctx t, CTypeAnn(CForall(tv, tb))), (substTypeInCExpr nt [tv] (tb)))
                            ))
                        | _ => Errors.attemptToApplyNonUniversal e (#2 st) ctx
                        )
                    | ROpen (e1, (tv, ev, e2), soi) => synthesizeType ctx e1 >>= (fn synt => case synt  of
                                (ce1, CExists (tv', tb)) => 
                        synthesizeType (addToCtxA (TermTypeJ([ev], 
                        substTypeInCExpr (CVar([tv], CVarTypeBinder)) [tv'] (tb), JTypeLocalBinder, NONE)) ctx) e2 >>= (fn (ce2, synthesizedType) =>
                        if List.exists (fn t => t = [tv]) (freeTCVar (synthesizedType))
                            then Errors.openTypeCannotExitScope e synthesizedType ctx
                            else Success(COpen((CTypeAnn(CExists(tv', tb)), ce1), (tv, ev, ce2), CTypeAnn(synthesizedType)), synthesizedType)
                        )
                            | _ => Errors.attemptToOpenNonExistentialTypes e (#2 synt) ctx)
                    | RUnfold (e2, soi) => synthesizeType ctx e2 >>= (fn synt => case synt of
                        (ce2, CRho (tv, tb)) => Success (CUnfold(ce2, CTypeAnn(CRho(tv, tb))),  (substTypeInCExpr (CRho (tv, tb)) [tv] (tb)))
                        | _ => Errors.attemptToUnfoldNonRecursiveTypes e (#2 synt) ctx
                        )
                    | RStringLiteral(l, soi) => Success(CStringLiteral l, CBuiltinType(BIString))
                    | RIntConstant(i, soi) => Success(CIntConstant i, CBuiltinType(BIInt))
                    | RRealConstant (r, soi) => Success(CRealConstant  r, CBuiltinType(BIReal))
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
                    | RBuiltinFunc(f, s) => Success(CBuiltinFunc(f), (BuiltinFunctions.typeOf f))
                    | RSeqComp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn (ce1, t1) => 
                        synthesizeType ctx e2 >>= (fn (ce2, t2) => 
                            Success(CSeqComp(ce1, ce2, CTypeAnn(t1), CTypeAnn(t2)), t2)
                        ))
                    (* types *)
                    | RUnitType(s) => Success(CUnitType, CUniverse)
                    | RNullType(s) => Success(CNullType, CUniverse)
                    | RBuiltinType(f, s) => Success(CBuiltinType(f), CUniverse)
                    | RUniverse(s) => Success(CUniverse, CUniverse) (* TODO: maybe universe levels? *)
                    | RPiType(t1, evoption, t2, soi) => 
                        checkType ctx t1 CUniverse >>= (fn ct1 => 
                            synthesizeType (
                                    case evoption of  NONE => ctx | SOME(n) => (addToCtxA (TermTypeJ([n], CUniverse, JTypeLocalBinder, NONE)) ctx)
                                ) t2 >>= (fn (ct2, synT) => 
                                    assertTypeEquiv ctx t2 synT CUniverse >> 
                                        (Success(CPiType(ct1, evoption, ct2), CUniverse))
                            )
                        )
                    | RSigmaType(t1, evoption, t2, soi) =>
                        checkType ctx t1 CUniverse >>= (fn ct1 => 
                            synthesizeType (
                                    case evoption of  NONE => ctx | SOME(n) => (addToCtxA (TermTypeJ([n], CUniverse, JTypeLocalBinder, NONE)) ctx)
                                ) t2 >>= (fn (ct2, synT) => 
                                    assertTypeEquiv ctx t2 synT CUniverse >> 
                                        (Success(CSigmaType(ct1, evoption, ct2), CUniverse))
                                )
                            )
                    | RProd(ltsl, sepl) => 
                         (collectAll (List.map (fn (l, t, soi) => 
                            Success l =/= checkType ctx t CUniverse 
                        ) ltsl)) >>= (fn l => Success(CProd l, CUniverse))
                    | RLazyProd  (ltsl, sepl) => 
                         (collectAll (List.map (fn (l, t, soi) => 
                            Success l =/= checkType ctx t CUniverse 
                        ) ltsl)) >>= (fn l => Success(CLazyProd l, CUniverse))
                    | RSum(ltsl, sepl) => 
                         (collectAll (List.map (fn (l, t, soi) => 
                            Success l =/= checkType ctx t CUniverse 
                        ) ltsl)) >>= (fn l => Success(CSum l, CUniverse))
                    | RFunc(t1, t2, soi) => checkType ctx t1 CUniverse >>= (fn ct1 => 
                        checkType ctx t2 CUniverse >>= (fn ct2 => 
                            Success(CFunc(ct1, ct2), CUniverse)
                        )
                    )
                    | RTypeInst(t1, t2, soi) => checkType ctx t1 CUniverse >>= (fn ct1 => 
                        checkType ctx t2 CUniverse >>= (fn ct2 => 
                            Success(CTypeInst(ct1, ct2), CUniverse)
                        )
                    )
                    | RForall(tv, t2, soi) => 
                        checkType (addToCtxA (TermTypeJ([tv], CUniverse, JTypeLocalBinder, NONE)) ctx) t2 CUniverse >>= (fn ct2 => 
                                Success(CForall(tv, ct2), CUniverse)
                            )
                    | RExists(tv, t2, soi) => 
                        checkType (addToCtxA (TermTypeJ([tv], CUniverse, JTypeLocalBinder, NONE)) ctx) t2 CUniverse >>= (fn ct2 => 
                                Success(CExists(tv, ct2), CUniverse)
                            )
                    | RRho(tv, t2, soi) => 
                        checkType (addToCtxA (TermTypeJ([tv], CUniverse, JTypeLocalBinder, NONE)) ctx) t2 CUniverse >>= (fn ct2 => 
                                Success(CRho(tv, ct2), CUniverse)
                            )
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

            and assertTypeEquiv (ctx : context) (expr: RExpr) (synthesized : CType) (checked : CType) : unit witherrsoption =
                typeEquiv expr ctx []  synthesized checked  >>= (fn tpequiv => if tpequiv
                    then Success() 
                    else Errors.typeMismatch expr (synthesized) checked ctx
                )
            and checkType (ctx : context) (e : RExpr) (ttUnnorm: CType) (* tt target type *) : CExpr witherrsoption =
                     normalizeType e ctx ttUnnorm >>= (fn ttNorm =>
                (let 
                    val tt = ttNorm
                    val _ = if DEBUG then  print(  "checking the expr " ^ PrettyPrint.show_typecheckingRExpr e ^ 
                        " against type " ^ PrettyPrint.show_typecheckingCType tt ^ "\n") else ()
                    val originalExpr = e
                    val res = 
                    case e of
                    RVar v => 
                    (synthesizeType ctx e) >>= (fn (synthExpr, synthType) =>
                    assertTypeEquiv ctx e (synthType) tt  >> (Success (synthExpr))
                    )
                   
                    | RUnitExpr(soi) => assertTypeEquiv ctx e CUnitType  tt >> (Success(CUnitExpr))
                    | RTuple (l, soi) => (case tt of 
                        CProd ls => if List.length l <> List.length ls
                                    then Errors.prodTupleLengthMismatch e (tt) ctx
                                    else collectAll (List.tabulate(List.length l, (fn i => 
                                    checkType ctx (List.nth(l, i)) (#2 (List.nth(ls, i)))))) >>= (fn checkedElems => 
                                    Success(CTuple ( checkedElems, CTypeAnn((CProd ls)))))
                        | _ => Errors.expectedProdType e (tt) ctx
                        )
                    | RLazyTuple (l, soi) => (case tt of 
                        CLazyProd ls => if List.length l <> List.length ls
                                    then Errors.lazyProdTupleLengthMismatch e (tt) ctx
                                    else collectAll (List.tabulate(List.length l, (fn i => 
                                    checkType ctx (List.nth(l, i)) (#2 (List.nth(ls, i)))))) >>= (fn checkedElems => 
                                    Success(CLazyTuple ( checkedElems, CTypeAnn((CLazyProd ls)))))
                        | _ => Errors.expectedLazyProdType e (tt) ctx
                        )
                    | RProj(e, l, soi) =>
                    synthesizeType ctx (RProj(e, l, soi)) >>= (fn synt => case synt of
                        (CProj(ce, l, prodType), synthType) => assertTypeEquiv ctx originalExpr synthType tt >> 
                        Success(CProj(ce, l, prodType))
                        | _ => raise Fail "tcp229")
                    | RLazyProj(e, l, soi) =>
                    synthesizeType ctx (RLazyProj(e, l, soi)) >>= (fn synt => case synt of
                        (CLazyProj(ce, l, lazyProdType), synthType) => assertTypeEquiv ctx originalExpr synthType tt >> 
                        Success(CLazyProj(ce, l, lazyProdType))
                        | _ => raise Fail "tcp229")

                    | RInj (l, e, soi) => (case tt of
                        CSum ls => (lookupLabel ls l) >>= (fn lookedupType => 
                                checkType ctx e lookedupType >>= (fn checkedExpr => 
                                    Success(CInj(l, checkedExpr, CTypeAnn((CSum ls))))
                                ))
                        | _ => Errors.expectedSumType originalExpr (tt) ctx
                    )
                    | RIfThenElse(e, tcase, fcase, soi) => (checkType ctx e (CBuiltinType(BIBool))  >>= (fn ce => 
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
                                        checkType (addToCtxA (TermTypeJ([ev], (lookedUpType) , JTypeLocalBinder, NONE)) ctx) e tt))
                                ) cases)) >>= (fn checkedCases  
                                    => Success(CCase((CTypeAnn(CSum ls), ce), checkedCases , CTypeAnn((tt)))))
                            | _ => Errors.attemptToCaseNonSum originalExpr (#2 synt) ctx)
                    | RLam(ev, eb, soi) => (case tt of
                        CFunc(t1,t2) => 
                            checkType (addToCtxA (TermTypeJ([ev], t1, JTypeLocalBinder, NONE)) ctx) eb t2
                            >>= (fn checkedExpr => Success(CLam(ev, checkedExpr, CTypeAnn(tt))))
                        | _ => Errors.expectedFunctionType e (tt) ctx
                        )
                    | RLamWithType (t, ev, eb, soi) => (case tt of
                        CFunc(t1,t2) => (assertTypeEquiv ctx e (rTypeToCType ctx t) t1 >>
                            (checkType (addToCtxA (TermTypeJ([ev], t1, JTypeLocalBinder, NONE)) ctx) eb t2 >>= (fn checkedBody => 
                                Success(CLam(ev, checkedBody , CTypeAnn(tt))))
                                )
                            )
                        | _ => Errors.expectedFunctionType e  (tt) ctx
                        )
                    | RSeqComp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn (ce1, t1) => 
                        checkType ctx e2 tt >>= (fn ce2 => 
                            Success(CSeqComp(ce1, ce2, CTypeAnn(t1), CTypeAnn(tt)))
                        ))
                    | RApp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn synt => case synt 
                        of (ce1, CFunc (t1, t2)) => (
                        assertTypeEquiv ctx e t2 tt >> (
                                checkType ctx e2 ( t1) >>= (fn checkedArg => 
                                    Success (CApp(ce1, checkedArg, CTypeAnn(CFunc(t1, t2))))
                                )
                            )
                        )
                        | _ => Errors.attemptToApplyNonFunction e (#2 synt) ctx)
                    | RTAbs (tv, e2, soi) => (case tt of
                        CForall (tv', tb) => 
                                checkType ctx e2 (substTypeInCExpr (CVar([tv], CVarTypeBinder)) [tv'] tb) >>= (fn ce2 => 
                                            Success(CTAbs (tv, ce2, CTypeAnn(tt)))
                                )
                        | _ => Errors.expectedUniversalType e (tt) ctx
                    )
                    | RTApp (e2, t, soi) => synthesizeType ctx e2  >>= (fn synt => case synt of
                        (ce2, CForall (tv, tb)) => (
                            (* need to normalize type! important! *)
                            (normalizeType t ctx (rTypeToCType ctx t) >>= (fn nt => 
                                assertTypeEquiv ctx e (tt) (substTypeInCExpr (rTypeToCType ctx t) [tv] ( tb))
                            )) >> Success(CTApp(ce2, rTypeToCType ctx t, CTypeAnn(CForall(tv, tb)))))
                        | _ => Errors.attemptToApplyNonUniversal e (#2 synt) ctx
                        )
                    | RPack (t, e2, soi) => (case tt of
                        CExists (tv, tb) => 
                                checkType ctx e2 (substTypeInCExpr (rTypeToCType ctx t) [tv]  tb) >>= (fn ce2 => 
                                                Success(CPack(rTypeToCType ctx t, ce2, CTypeAnn(tt))))
                        | _ => Errors.expectedExistentialType e (tt) ctx
                    )
                    | ROpen (e1, (tv, ev, e2), soi) => synthesizeType ctx e1 >>= (fn synt => case synt of
                        (ce1, CExists (tv', tb)) => 
                        checkType (addToCtxA (TermTypeJ([ev], substTypeInCExpr (CVar([tv], CVarTypeBinder)) [tv'] ( tb), JTypeLocalBinder, NONE)) ctx) e2 tt
                        >>= (fn ce2 => 
                        Success(COpen((CTypeAnn(CExists (tv', tb)), ce1), (tv, ev, ce2), CTypeAnn(tt)))
                        )
                        | _ => Errors.attemptToOpenNonExistentialTypes e (#2 synt) ctx
                    )
                    | RFold (e2, soi) => (case tt
                        of 
                        CRho (tv ,tb) => 
                        checkType ctx e2 (substTypeInCExpr (CRho(tv, tb)) [tv] tb)
                        >>= (fn ce2 => Success (CFold(ce2, CTypeAnn(tt))))
                        | _ => Errors.expectedRecursiveType e (tt) ctx
                            )
                    | RUnfold (e2,soi) => synthesizeType ctx e2  >>= (fn synt => case synt of
                        (ce2, CRho (tv, tb)) =>(
                            assertTypeEquiv ctx e ((substTypeInCExpr (CRho (tv,  tb)) [tv] ( tb))) tt >>
                            Success(CUnfold(ce2, CTypeAnn(CRho(tv,tb)))))
                        | _ => Errors.attemptToUnfoldNonRecursiveTypes e (#2 synt) ctx
                        )
                    | RFix (ev, e, soi)=> checkType (addToCtxA (TermTypeJ([ev] , tt, JTypeLocalBinder, NONE)) ctx) e tt
                                        >>= (fn ce => Success(CFix(ev,ce, CTypeAnn(tt))))
                    | RStringLiteral (s, soi) => (assertTypeEquiv ctx e (CBuiltinType(BIString)) (tt) >> (Success (CStringLiteral s)))
                    | RIntConstant (i, soi) => (assertTypeEquiv ctx e (CBuiltinType(BIInt)) tt >> (Success ( CIntConstant i)))
                    | RRealConstant (r, soi) => (assertTypeEquiv ctx e (CBuiltinType(BIReal)) tt >> (Success (CRealConstant r)))
                    | RBoolConstant (r, soi) => (assertTypeEquiv ctx e (CBuiltinType(BIBool)) tt >> (Success (CBoolConstant r)))
                    | RFfiCCall (e1, e2, soi) => (
                        case e1 of
                            RStringLiteral (cfuncName, soi) => 
                                let fun elaborateArguments  (args : StructureName.t list ) : CExpr witherrsoption = 
                                    fmap CFfiCCall(Success cfuncName =/= 
                                    collectAll (map (fn a => fmap (#1) (lookupCtxForType ctx a)) args))
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
                                        Success(CLetIn(csig, ce, CTypeAnn(tt)))
                            )
                        )
                    
                        )
                    | RBuiltinFunc(f, soi) => (assertTypeEquiv ctx e ((BuiltinFunctions.typeOf f)) tt >> Success(CBuiltinFunc(f)))
                     (* types *)
                    | RUnitType(s) => assertTypeEquiv ctx e CUniverse tt >> Success(CUnitType)
                    | RNullType(s) => assertTypeEquiv ctx e CUniverse tt >> Success(CNullType )
                    | RBuiltinType(f, s) => assertTypeEquiv ctx e CUniverse tt >> Success(CBuiltinType(f) )
                    | RUniverse(s) => assertTypeEquiv ctx e CUniverse tt >> Success(CUniverse ) (* TODO: maybe universe levels? *)
                    | RPiType(t1, evoption, t2, soi) => 
                        assertTypeEquiv ctx e CUniverse tt >> (
                        checkType ctx t1 CUniverse >>= (fn ct1 => 
                            synthesizeType (
                                    case evoption of  NONE => ctx | SOME(n) => (addToCtxA (TermTypeJ([n], CUniverse, JTypeLocalBinder, NONE)) ctx)
                                ) t2 >>= (fn (ct2, synT) => 
                                    assertTypeEquiv ctx t2 synT CUniverse >> 
                                        (Success(CPiType(ct1, evoption, ct2)))
                            )
                        ))
                    | RSigmaType(t1, evoption, t2, soi) =>
                        assertTypeEquiv ctx e CUniverse tt >> (
                        checkType ctx t1 CUniverse >>= (fn ct1 => 
                            synthesizeType (
                                    case evoption of  NONE => ctx | SOME(n) => (addToCtxA (TermTypeJ([n], CUniverse,JTypeLocalBinder, NONE)) ctx)
                                ) t2 >>= (fn (ct2, synT) => 
                                    assertTypeEquiv ctx t2 synT CUniverse >> 
                                        (Success(CPiType(ct1, evoption, ct2) ))
                                )
                            ))
                    | RProd(ltsl, sepl) => 
                        assertTypeEquiv ctx e CUniverse tt >> 
                         (collectAll (List.map (fn (l, t, soi) => 
                            Success l =/= checkType ctx t CUniverse 
                        ) ltsl)) >>= (fn l => Success(CProd l ))
                    | RLazyProd  (ltsl, sepl) => 
                        assertTypeEquiv ctx e CUniverse tt >> 
                         (collectAll (List.map (fn (l, t, soi) => 
                            Success l =/= checkType ctx t CUniverse 
                        ) ltsl)) >>= (fn l => Success(CLazyProd l ))
                    | RSum(ltsl, sepl) => 
                        assertTypeEquiv ctx e CUniverse tt >> 
                         (collectAll (List.map (fn (l, t, soi) => 
                            Success l =/= checkType ctx t CUniverse 
                        ) ltsl)) >>= (fn l => Success(CSum l ))
                    | RFunc(t1, t2, soi) => 
                        assertTypeEquiv ctx e CUniverse tt >> (
                    checkType ctx t1 CUniverse >>= (fn ct1 => 
                        checkType ctx t2 CUniverse >>= (fn ct2 => 
                            Success(CFunc(ct1, ct2) )
                        )
                    ))
                    | RTypeInst(t1, t2, soi) => 
                        assertTypeEquiv ctx e CUniverse tt >> (
                            checkType ctx t1 CUniverse >>= (fn ct1 => 
                                checkType ctx t2 CUniverse >>= (fn ct2 => 
                                    Success(CTypeInst(ct1, ct2) )
                                )
                            ))
                    | RForall(tv, t2, soi) => 
                        assertTypeEquiv ctx e CUniverse tt >> (
                        checkType (addToCtxA (TermTypeJ([tv], CUniverse,JTypeLocalBinder, NONE)) ctx) t2 CUniverse >>= (fn ct2 => 
                                Success(CForall(tv, ct2) )
                            ))
                    | RExists(tv, t2, soi) => 
                        assertTypeEquiv ctx e CUniverse tt >> (
                        checkType (addToCtxA (TermTypeJ([tv], CUniverse,JTypeLocalBinder, NONE)) ctx) t2 CUniverse >>= (fn ct2 => 
                                Success(CExists(tv, ct2) )
                            ))
                    | RRho(tv, t2, soi) => 
                        assertTypeEquiv ctx e CUniverse tt >> (
                        checkType (addToCtxA (TermTypeJ([tv], CUniverse, JTypeLocalBinder, NONE)) ctx) t2 CUniverse >>= (fn ct2 => 
                                Success(CRho(tv, ct2) )
                            ))
                    (* | _ => genSingletonError (reconstructFromRExpr e) ("check type failed on " ^ PrettyPrint.show_typecheckingRType e 
                     ^ " <= " ^ PrettyPrint.show_typecheckingCType tt) NONE *)
                in res
                end 
        ))
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
                (* | RTypeMacro (n, t)::ss => 
                let val freeTVars = freeTCVar (applyContextToType ctx (rTypeToCType t)) in if freeTVars <> [] then 
                    Errors.typeDeclContainsFreeVariables (StructureName.toString (hd freeTVars)) ctx
                    else 
                    normalizeType (applyContextToType ctx (rTypeToCType t)) >>= (fn normalizedType => 
                    (
                        (* DebugPrint.p (
                            StructureName.toStringPlain (getCurSName ctx)
                            ^" normlizedType is " ^ PrettyPrint.show_typecheckingType normalizedType ^ "\n")
                        ; *)
                    typeCheckSignature (addToCtxR (TypeDef([n], normalizedType, ())) ctx) ss 
                        (acc@[CTypeMacro((getCurSName ctx)@[n],  normalizedType)])
                    )
                        )
                    end *)
                | RTermTypeJudgment(n, t):: ss => 
                let 
                (* val freeTVars = freeTCVar  (rTypeToCType ctx t) *)
                (* (applyContextToType ctx (rTypeToCType ctx t))  *)
                in  (* do not check for free variables, as it will be catched in a later stage? *)
                (* if freeTVars <> [] 
                    then Errors.termTypeDeclContainsFreeVariables (StructureName.toString (hd freeTVars)) ctx
                    (* raise SignatureCheckingFailure ("TermType decl contains free var" ^ PrettyPrint.show_sttrlist (freeTVar (applyContextToType ctx t)) ^" in "^ PrettyPrint.show_typecheckingType (applyContextToType ctx t))  *)
                    else  *)
                    normalizeType t ctx (rTypeToCType ctx t)
                    (* (applyContextToType ctx (rTypeToCType ctx t))  *)
                    >>= (fn normalizedType => 
                    typeCheckSignature (addToCtxR (TermTypeJ([n], normalizedType, JTypeLocalBinder, NONE)) ctx) ss (acc))
                end
                (* | RTermMacro(n, e) :: ss => 
                    synthesizeType ctx (e) >>= 
                    (fn (transformedExpr , synthesizedType)  =>
                        typeCheckSignature (addToCtxR (TermTypeJ([n], synthesizedType, NONE)) ctx) ss 
                            (acc@[CTermDefinition((getCurSName ctx)@[n], transformedExpr, synthesizedType)])
                    ) *)
                | RTermDefinition(n, e) :: ss => 
                (case findCtx ctx ((getCurSName ctx)@[n]) of  (* must find fully qualified name as we allow same name for substructures *)
                    NONE  => synthesizeType ctx (e) >>= 
                            (fn (transformedExpr , synthesizedType)  =>
                                typeCheckSignature 
                                    (addToCtxR (TermTypeJ([n], synthesizedType, JTypeDefinition transformedExpr, NONE)) ctx) ss 
                                    (acc@[CTermDefinition((getCurSName ctx)@[n], transformedExpr, synthesizedType)])
                            ) 
                    | SOME(cname, lookedUpType, lookedUpDef) => 
                        (case lookedUpDef of 
                         JTypePending =>  
                            let val transformedExprOrFailure = checkType ctx (e) lookedUpType
                            in 
                            case transformedExprOrFailure of
                            Success(transformedExpr) => typeCheckSignature (modifyCtxAddDef ctx cname transformedExpr) ss 
                                                            (acc@[CTermDefinition((getCurSName ctx)@[n], transformedExpr, lookedUpType)])
                            | DErrors(l) => (case typeCheckSignature ctx ss (acc) of 
                                        Success _ => DErrors(l)
                                        | DErrors l2 => DErrors(l @l2)
                                        | _ => raise Fail "tcp458"
                                )
                            | _ => raise Fail "tcp457"
                            end
                        | _ => Errors.redefinitionError n (StructureName.toStringPlain cname) ctx (List.last cname) 
                            )
                )
                | RConstructorDecl(name, rtp) :: ss => 
                    checkConstructorType ctx rtp >>= (fn (checkedType, cconsinfo) => 
                    
                        typeCheckSignature (addToCtxR(TermTypeJ([name], checkedType, JTypeConstructor cconsinfo, NONE)) ctx) ss
                        (acc@[CConstructorDecl((getCurSName ctx)@[name], checkedType, cconsinfo)])
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
                        (addToCtxAL (List.concat (List.mapPartial (fn x => case x of 
                            (* CTypeMacro(sname, t) => SOME(TypeDef(sname, t, ())) *)
                             CTermDefinition(sname, e, t) => SOME([TermTypeJ(sname, t, JTypeDefinition(e), NONE)
                                ])
                            | CDirectExpr _ => NONE
                            | CImport _ => NONE
                            | CConstructorDecl(sname, t, consinfo) => SOME([TermTypeJ(sname, t, JTypeConstructor consinfo, NONE)
                                ])
                            ) csig)) ctx)
                        ss (acc@[CImport(importName, path)])
                    )
                | RDirectExpr e :: ss=> 
                    let 
                    val synthedExprOrFailure = (synthesizeType ctx (e))
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
