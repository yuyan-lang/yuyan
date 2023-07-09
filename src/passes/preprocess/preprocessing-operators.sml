structure PreprocessingOperators =
struct
    open Operators
    open StaticErrorStructure

    val enableSimplifiedChineseOperator = true

    val structureRefOp = Operators.parseOperatorStr "〇之〇" true false 710 []
    val structureRefOp2 = Operators.parseOperatorStr "〇中的〇" true false 710 []
    (* t = T *)
    val termDefinitionOp = Operators.parseOperatorStr "〇者〇也" false false 0 []
    val termDefinitionOp2 = Operators.parseOperatorStr "〇的定义是〇" false false 0 []
    val termDefinitionTransparentOp = Operators.parseOperatorStr "〇即〇也" false false 0 []
    val termDefinitionTransparentOp2 = Operators.parseOperatorStr "〇其实就是〇" false false 0 []
    (* e : T *)
    val termTypeJudgmentOp = Operators.parseOperatorStr "〇乃〇也" false false 0 []
    val termTypeJudgmentOp2 = Operators.parseOperatorStr "〇的类型是〇" false false 0 []
    val termTypeJudgmentTransparentOp = Operators.parseOperatorStr "〇号〇也" false false 0 []
    val termTypeJudgmentTransparentOp2 = Operators.parseOperatorStr "〇的类型可以是〇" false false 0 []
    (* cons e : T *)
    val constructorDeclarationOp = Operators.parseOperatorStr "〇立〇也" false false 0 []
    val constructorDeclarationOp2 = Operators.parseOperatorStr "〇是一种〇" false false 0 []
    (* infixl op 232 *)
    val opDeclarationOp = Operators.parseOperatorStr "术〇交〇序〇也" false false 0 []
    val opDeclarationOp2 = Operators.parseOperatorStr "函数〇结合性〇序〇" false false 0 []
    val openStructureOp = Operators.parseOperatorStr "观〇之书" false false 0 []
    val openStructureOp2 = Operators.parseOperatorStr "打开〇" false false 0 []
    val importStructureOp = Operators.parseOperatorStr "寻〇之书" false false 0 []
    val importStructureOp2 = Operators.parseOperatorStr "导入〇" false false 0 []
    val importOpenStructureOp = Operators.parseOperatorStr "寻观〇之书" false false 0 []
    val importOpenStructureOp2 = Operators.parseOperatorStr "导入并打开〇" false false 0 []
    val reexportStructureOp = Operators.parseOperatorStr "诵〇之书" false false 0 []
    val reexportStructureOp2 = Operators.parseOperatorStr "导出〇" false false 0 []
    

    val baseStructureOps = [termTypeJudgmentOp, termTypeJudgmentTransparentOp,
    constructorDeclarationOp, 
     termDefinitionOp, opDeclarationOp, termDefinitionTransparentOp, reexportStructureOp
    ] @ (
        if enableSimplifiedChineseOperator
        then [termTypeJudgmentOp2, termTypeJudgmentTransparentOp2,
        constructorDeclarationOp2, 
        termDefinitionOp2, opDeclarationOp2, termDefinitionTransparentOp2, reexportStructureOp2
        ] else []
    )
    
    val declOpsNoImportOpen = baseStructureOps @ [openStructureOp, importStructureOp]
        @ (
            if enableSimplifiedChineseOperator
            then [openStructureOp2, importOpenStructureOp2]
            else []
        )
    val declOpsWithImportOpen = baseStructureOps @ [importOpenStructureOp ] @(
        if enableSimplifiedChineseOperator
        then [importOpenStructureOp2]
        else []
    )

    exception PreprocessMalformedAssoc of UTF8String.t
    exception PreprocessMalformedPrecedence of UTF8String.t

    val ~= = UTF8Char.~=
    infix 4 ~=


    fun assocErr s = genSingletonError s "关联性(associativity)必须是【左，右，无】中的一种" NONE
    fun parseAssoc (s : UTF8String.t) : associativity witherrsoption = 
        if length s <> 1 
        then assocErr s
        else
        let val c = hd s
        in 
    (
        (* print ("parseAssoc " ^ UTF8String.toString s ^" \n"); *)
        if c ~= UTF8Char.fromString "左" NONE
        then Success(LeftAssoc)
        else 
        if c ~= UTF8Char.fromString "右" NONE
        then Success(RightAssoc)
        else 
        if c ~= UTF8Char.fromString "无" NONE
        then Success(NoneAssoc)
        else assocErr s)
        end

  

    (* Precedence hierachy: 
    
        On Types: (from lowest) Rho < Exists < Forall < Function < Sum < Prod < 0/1
        On Expressons : 
        (from highest)
            () >  Proj > App > Pair = Inj = fold >  unfold  > Case > TApp 
            >  pack > open > Lambda > Lambda/wtype > fix >  TLambda
    
    
    *)

    (* l : e *)
    val unnamedProdTypeOp  = Operators.parseOperatorStr "〇合〇" true false 385 []
    val unnamedProdTypeOp2  = Operators.parseOperatorStr "〇结合〇" true false 385 []
    val functionTypeOp  = Operators.parseOperatorStr "化〇而〇" true false 350 []
    val functionTypeOp2  = Operators.parseOperatorStr "从〇到〇" true false 350 []
    val implicitPiTypeOp  = Operators.parseOperatorStr "承〇者〇而〇" true false 350 [3]
    val implicitPiTypeOp2  = Operators.parseOperatorStr "自〇的〇到〇" true false 350 [3]
    val implicitPiNameOnlyTypeOp  = Operators.parseOperatorStr "承〇而〇" true false 350 [1]
    val implicitPiNameOnlyTypeOp2  = Operators.parseOperatorStr "自〇到〇" true false 350 [1]
    val piTypeOp  = Operators.parseOperatorStr "化〇者〇而〇" true false 350 [3]
    val piTypeOp2  = Operators.parseOperatorStr "从〇的〇到〇" true false 350 [3]

    val typeOpBound = UID.next() (* This is a hack since uid is monotonically increasing *)

    val projExprOp = Operators.parseOperatorStr "〇中〇" true true 700 [] (* only numerical projetion *)
    val projExprOp2 = Operators.parseOperatorStr "〇中的第〇个" true true 700 [] (* only numerical projetion *)
    val appExprOp = Operators.parseOperatorStr "〇于〇" true true 690 []
    val appExprOp2 = Operators.parseOperatorStr "〇使用于〇" true true 690 []
    val pairExprOp = Operators.parseOperatorStr "〇与〇" true false 680 []
    val pairExprOp2 = Operators.parseOperatorStr "〇连结〇" true false 680 []
    val typeAnnotateExprOp = Operators.parseOperatorStr "〇也〇" true false 250 []
    val typeAnnotateExprOp2 = Operators.parseOperatorStr "类型为〇的〇" true false 250 []
    val ifThenElseExprOp = Operators.parseOperatorStr "若〇则〇否则〇" true false 645 []
    val ifThenElseExprOp2 = Operators.parseOperatorStr "如果〇那么〇否则〇" true false 645 []
    val caseClauseOp = Operators.parseOperatorStr "有〇则〇" false false 640 []
    val caseClauseOp2 = Operators.parseOperatorStr "如果是〇那么〇" false false 640 []
    val caseAlternativeOp = Operators.parseOperatorStr "〇或〇" true false 630 []
    val caseAlternativeOp2 = Operators.parseOperatorStr "〇或者〇" true false 630 []
    val caseExprOp = Operators.parseOperatorStr "鉴〇而〇" true false 620 []
    val caseExprOp2 = Operators.parseOperatorStr "分析〇随后〇" true false 620 []
    val implicitAppExprOp = Operators.parseOperatorStr "〇授以〇" true true 690 []
    val implicitAppExprOp2 = Operators.parseOperatorStr "〇给予〇" true true 690 []
    val sequentialCompositionOp = Operators.parseOperatorStr "〇；〇" true false 535 []
    val ffiCCallOp = Operators.parseOperatorStr "《《C调用》》名〇传〇" true false 530 []
    val ffiCCallOp2 = Operators.parseOperatorStr "《《C调用》》函数名是〇参数是〇" true false 530 []
    val lambdaExprOp = Operators.parseOperatorStr "会〇而〇" true false 520 [1]
    val lambdaExprOp2 = Operators.parseOperatorStr "遇到了〇随后〇" true false 520 [1]
    val lambdaExprWithTypeOp = Operators.parseOperatorStr "遇〇者〇而〇" true false 520 [3]
    val lambdaExprWithTypeOp2 = Operators.parseOperatorStr "遇到了〇的〇随后〇" true false 520 [3]
    val fixExprOp = Operators.parseOperatorStr "循〇以〇" true false 510 [1]
    val fixExprOp2 = Operators.parseOperatorStr "递归〇随后〇" true false 510 [1]
    val implicitLambdaExprOp = Operators.parseOperatorStr "受〇而〇" true false 520 [1]
    val implicitLambdaExprOp2 = Operators.parseOperatorStr "得到了〇随后〇" true false 520 [1]
    val letinOp = Operators.parseOperatorStr "虑〇" false false 525 []
    val letinOp2 = Operators.parseOperatorStr "执行如下计算〇" false false 525 []
    val letinSingleOp = Operators.parseOperatorStr "虑〇者〇而〇" true false 526 [1]
    val letinSingleOp2 = Operators.parseOperatorStr "令〇为〇随后〇" true false 526 [1]
    (* val inlineCommentOp = Operators.parseOperatorStr "〇注〇" false false 480 [] *)
    val tpImplOperator = Operators.parseOperatorStr "〇其实〇" false false 480 [] (* made to be compatible with the bs version, which has a much more restricted notion of sealing *)
    val tpImplOperator2 = Operators.parseOperatorStr "〇实际上是〇" false false 480 [] (* made to be compatible with the bs version, which has a much more restricted notion of sealing *)

    val elabAppBound = UID.next() (* This is a hack since uid is monotonically increasing *)

    val allTypeOps = [
        structureRefOp,
        unnamedProdTypeOp,
        functionTypeOp,
        piTypeOp, 
        implicitPiTypeOp,
        implicitPiNameOnlyTypeOp,
        tpImplOperator
        ] @ (
            if enableSimplifiedChineseOperator
            then 
            [
                structureRefOp2,
                unnamedProdTypeOp2,
                functionTypeOp2,
                piTypeOp2, 
                implicitPiTypeOp2,
                implicitPiNameOnlyTypeOp2,
                tpImplOperator2
            ]
            else []
        )
    val allTypeAndExprOps = allTypeOps @ [ 
            projExprOp, appExprOp, pairExprOp,
            caseClauseOp, 
            caseAlternativeOp, caseExprOp, ifThenElseExprOp, implicitAppExprOp, 
            lambdaExprOp,
            implicitLambdaExprOp,
            ffiCCallOp,
            lambdaExprWithTypeOp, fixExprOp, 
            letinOp, letinSingleOp, sequentialCompositionOp, 
            typeAnnotateExprOp
        ] @ (
            if enableSimplifiedChineseOperator
            then 
            [ 
                projExprOp2, appExprOp2, pairExprOp2,
                caseClauseOp2, 
                caseAlternativeOp2, caseExprOp2, ifThenElseExprOp2, implicitAppExprOp2, 
                lambdaExprOp2,
                implicitLambdaExprOp2,
                ffiCCallOp2,
                lambdaExprWithTypeOp2, fixExprOp2, 
                letinOp2, letinSingleOp2, 
                typeAnnotateExprOp2
            ]
            else []
        )
end
