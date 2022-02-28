structure TypeCheckingErrors =
 struct 
    open StaticErrorStructure
    open TypeCheckingASTOps
    open TypeCheckingContext
    val DEBUGSHOWEXPR = true
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
    end

