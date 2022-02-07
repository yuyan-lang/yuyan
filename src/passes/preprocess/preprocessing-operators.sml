structure PreprocessingOperators =
struct
    open Operators

    val structureRefOp = Operators.parseOperatorStr "〇之〇" true false 710 []

      (* type t = T *)
    val typeMacroOp = Operators.parseOperatorStr "〇者〇也" false false 0 []
    (* e : T *)
    val termTypeJudgmentOp = Operators.parseOperatorStr "以〇为〇" false false 0 []
    (* #define e = E *)
    val termMacroOp = Operators.parseOperatorStr "设〇为〇" false false 0 []
    (* e = E *)
    val termDefinitionOp = Operators.parseOperatorStr "施〇乃为〇" false false 0 []
    (* infixl op 232 *)
    val opDeclarationOp = Operators.parseOperatorStr "术〇交〇序〇也" false false 0 []
    (* // *)
    val commentOp = Operators.parseOperatorStr "注〇" false false 0 []
    (* structures *)
    val publicStructureOp = Operators.parseOperatorStr "有书〇曰〇" false false 0 []
    val privateStructureOp = Operators.parseOperatorStr "吾书〇曰〇" false false 0 []
    val openStructureOp = Operators.parseOperatorStr "观〇之书" false false 0 []
    val importStructureOp = Operators.parseOperatorStr "览〇之书" false false 0 []
    val reexportStructureOp = Operators.parseOperatorStr "诵〇之书" false false 0 []
    
    val declOps = [typeMacroOp, termTypeJudgmentOp, termMacroOp, termDefinitionOp, opDeclarationOp, commentOp,
    publicStructureOp, privateStructureOp, openStructureOp, importStructureOp, reexportStructureOp]

    exception PreprocessMalformedAssoc of UTF8String.t
    exception PreprocessMalformedPrecedence of UTF8String.t

    val ~= = UTF8Char.~=
    infix 4 ~=


    fun parseAssoc (s : UTF8String.t) : associativity = 
        if length s <> 1 
        then raise PreprocessMalformedAssoc s
        else
        let val c = hd s
        in 
    (
        (* print ("parseAssoc " ^ UTF8String.toString s ^" \n"); *)
        if c ~= UTF8Char.fromString "左" NONE
        then LeftAssoc
        else 
        if c ~= UTF8Char.fromString "右" NONE
        then RightAssoc
        else 
        if c ~= UTF8Char.fromString "无" NONE
        then NoneAssoc
        else raise PreprocessMalformedAssoc s)
        end

  

    (* Precedence hierachy: 
    
        On Types: (from lowest) Rho < Exists < Forall < Function < Sum < Prod < 0/1
        On Expressons : 
        (from highest)
            () >  Proj > App > Pair = Inj = fold >  unfold  > Case > TApp 
            >  pack > open > Lambda > Lambda/wtype > fix >  TLambda
    
    
    *)

    (* l : e *)
    (* val unitTypeOp  = Operators.parseOperatorStr "有" false false 420 []
    val nullTypeOp  = Operators.parseOperatorStr "无" false false 420 [] *)
    val labeledTypeCompOp  = Operators.parseOperatorStr "夫〇表〇" false false 400 [1]
    val prodTypeOp  = Operators.parseOperatorStr "〇合〇" true false 380 []
    val lazyProdTypeOp  = Operators.parseOperatorStr "〇且合〇" true false 370 []
    val sumTypeOp  = Operators.parseOperatorStr "〇亦〇" true false 360 []
    val typeInstantiationOp  = Operators.parseOperatorStr "〇启以〇" true true 355 []
    val functionTypeOp  = Operators.parseOperatorStr "化〇而〇" true false 350 []
    val recursiveTypeOp  = Operators.parseOperatorStr "复〇为〇" true false 345 [1]
    val universalTypeOp  = Operators.parseOperatorStr "承〇而〇" true false 340 [1]
    val existentialTypeOp  = Operators.parseOperatorStr "有〇则〇" true false 320 [1]
    (* val builtinTypeStringOp  = Operators.parseOperatorStr "《《字符串》》" true false 420 [] *)

    val typeOpBound = UID.next() (* This is a hack since uid is monotonically increasing *)

    val unitExprOp = Operators.parseOperatorStr "元" true false 720 []
    val lazyProjExprOp = Operators.parseOperatorStr "〇且中〇" true false 705 []
    val projExprOp = Operators.parseOperatorStr "〇中〇" true false 700 []
    val appExprOp = Operators.parseOperatorStr "〇于〇" true true 690 []
    val pairExprOp = Operators.parseOperatorStr "〇与〇" true false 680 []
    val lazyPairExprOp = Operators.parseOperatorStr "〇且与〇" true false 675 []
    val injExprOp = Operators.parseOperatorStr "〇临〇" false false 670 []
    val foldExprOp = Operators.parseOperatorStr "卷〇" true false 660 []
    val unfoldExprOp = Operators.parseOperatorStr "舒〇" true false 650 []
    val ifThenElseExprOp = Operators.parseOperatorStr "若〇则〇否则〇" true false 645 []
    val caseClauseOp = Operators.parseOperatorStr "曰〇则有〇而〇" true false 640 [3]
    val caseAlternativeOp = Operators.parseOperatorStr "〇或〇" true false 630 []
    val caseExprOp = Operators.parseOperatorStr "鉴〇而〇" true false 620 []
    val typeAppExprOp = Operators.parseOperatorStr "〇授以〇" true true 695 []
    val packExprOp = Operators.parseOperatorStr "入〇合〇" true false 560 []
    val unpackExprOp = Operators.parseOperatorStr "开〇则有〇者〇而〇" true false 540 [3,5]
    val sequentialCompositionOp = Operators.parseOperatorStr "〇；〇" true false 535 []
    val ffiCCallOp = Operators.parseOperatorStr "《《C调用》》名〇传〇" true false 530 []
    val lambdaExprOp = Operators.parseOperatorStr "会〇而〇" true false 520 [1]
    val lambdaExprWithTypeOp = Operators.parseOperatorStr "遇〇者〇而〇" true false 520 [3]
    val fixExprOp = Operators.parseOperatorStr "循〇以〇" true false 510 [1]
    val typeLambdaExprOp = Operators.parseOperatorStr "受〇而〇" true false 500 [1]
    val letinOp = Operators.parseOperatorStr "虑〇以成〇之道" false false 525 []
    val inlineCommentOp = Operators.parseOperatorStr "〇注〇" false false 480 []

    val elabAppBound = UID.next() (* This is a hack since uid is monotonically increasing *)

    val allTypeOps = [
        (* builtinTypeStringOp, *)
        structureRefOp,
        (* unitTypeOp, nullTypeOp,  *)
        labeledTypeCompOp, prodTypeOp, sumTypeOp, functionTypeOp,
        typeInstantiationOp,
        universalTypeOp, existentialTypeOp, recursiveTypeOp
        , inlineCommentOp (* allow comment in types, but not important anyways, as both 
        will soon be merged together *)
        , lazyProdTypeOp
        ]
    val allTypeAndExprOps = allTypeOps @ [ unitExprOp,
        lazyProjExprOp,
        projExprOp, appExprOp, pairExprOp,
        lazyPairExprOp, injExprOp, foldExprOp, unfoldExprOp, caseClauseOp, 
        caseAlternativeOp, caseExprOp, ifThenElseExprOp, typeAppExprOp, packExprOp, unpackExprOp, lambdaExprOp,
        ffiCCallOp,
        lambdaExprWithTypeOp, fixExprOp, typeLambdaExprOp,
        letinOp, sequentialCompositionOp
    ]
end
