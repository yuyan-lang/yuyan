structure TypeCheckingErrors =
 struct 
    open StaticErrorStructure
    open TypeCheckingContext
    open OperatorsOps



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
        | RLam (x, e, p, soi) => 
            reconstructWithArgs soi [x, reconstructFromRExpr e]

        | RLamWithType (t, x, e, soi) => reconstructWithArgs soi [tpPlaceHolder, x, reconstructFromRExpr e]
        | RApp (e1, e2, p, soi)=> if Operators.eqOpUid soi PreprocessingOperators.appExprOp 
            then reconstructWithArgs soi [reconstructFromRExpr e1, reconstructFromRExpr e2]
            else 
             let
                (* e.g. ap(ap(〇，〇, 头), 尾) *)
                (* .       ^^ e1          ^^ e2 *)
                val argcount = getNumArguments soi
                fun flatten (e : RExpr) (argsCount : int)  : RExpr list= 
                    if argsCount = 0
                    then []
                    else case e of 
                            RApp(e1', e2', p, soi') => if Operators.eqOpUid soi soi'
                                                    then flatten e1' (argsCount - 1) @[e2']
                                                    else raise Fail "tcerr51: not enough args"
                            | _ =>  
                            (* if argsCount = 1 then [e] else  *)
                            raise Fail ("tcerr52: not enough args, argcount is " ^ 
                            Int.toString argsCount ^ " remaining R is " ^ PrettyPrint.show_typecheckingRExpr e)
                val _ = DebugPrint.p (PrettyPrint.show_op soi ^ " Flattening " ^ Int.toString argcount  ^ " " ^ PrettyPrint.show_typecheckingRExpr e)
                (* when RApp is encountered, argcount is at least 1, otherwise
                RApp would not have been created, the head is assumed to be the thing itself *)
                val priorArgs = flatten e1 (argcount -1)
                in 
                    reconstructWithArgs soi (map reconstructFromRExpr (priorArgs @ [e2]))
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
        | RPiType(t1, evoption, t2, p, soi) => 
        (case (evoption, t1) of 
            (SOME v, SOME t1) => reconstructWithArgs soi [reconstructFromRExpr t1, v, reconstructFromRExpr t2]
            | (NONE, SOME t1) => reconstructWithArgs soi [reconstructFromRExpr t1, reconstructFromRExpr t2] 
            | (SOME v, NONE) => reconstructWithArgs soi [v, reconstructFromRExpr t2] 
            | (NONE, NONE) => raise Fail "at least one must be defined"
            )
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
    
    val DEBUGSHOWEXPR = true
        fun typeMismatch e synthesized checked detailLeft detailRight
        ctx= genSingletonError (reconstructFromRExpr e)
                ((if DEBUGSHOWEXPR then "`" ^ PrettyPrint.show_typecheckingRExpr e ^ "`" else "") ^ "类型不匹配(type mismatch) \n 推断的类型(synthesized type) : " ^ PrettyPrint.show_typecheckingCType synthesized
                ^ " \n 检查的类型(checked type) : " ^ PrettyPrint.show_typecheckingCType checked
                ^ " \n 原因：" ^ PrettyPrint.show_typecheckingCType detailLeft ^ " 不等于 " ^ PrettyPrint.show_typecheckingCType detailRight ) (showctxSome ctx)
        fun exprTypeError e tt ctx msg= genSingletonError (reconstructFromRExpr e) 
        ((if DEBUGSHOWEXPR then 
        "`" ^ PrettyPrint.show_typecheckingRExpr e ^ "`:" ^
        "`" ^ PrettyPrint.show_typecheckingCType tt ^ "`" 
        else "") ^ msg) (showctxSome ctx)
        fun exprError e ctx msg= genSingletonError (reconstructFromRExpr e) 
        ((if DEBUGSHOWEXPR then 
        "`" ^ PrettyPrint.show_typecheckingRExpr e ^ "`"
        else "") ^ msg) (showctxSome ctx)
        fun strError s ctx msg= genSingletonError s 
        ((if DEBUGSHOWEXPR then 
        "`" ^ UTF8String.toString s ^ "`"
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
        fun expectedFunctionType e tt ctx =  exprTypeError e tt ctx "期待函数类型(expected function types)"
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

        fun unboundTermConstructor e ctx = exprError e ctx "元素构造器未找到(unbound term constructor)"
        fun expectedTermConstructor e ctx = exprError e ctx "期待元素构造器(expected element constructor)"
        fun unsupportedPatternType e ctx = exprError e ctx "不支持的模式匹配类型(unsupported pattern type)"
        fun patternArgumentCountMismatch e ctx expected actual = exprError e ctx 
            ("模式匹配参数数量错误，期待" ^ Int.toString expected ^ "个参数，" ^
            "实际" ^ Int.toString actual ^ "个参数")
        fun elementConstructorScopeError e ctx = exprError e ctx "元素构造器必须与其对应的类型构造器处于同一结构中(element constructor must live in the same scope as the corresponding type constructor)"
        fun genericError e ctx msg = exprError e ctx msg
        fun genericErrorStr s ctx msg = strError s ctx msg
    end

