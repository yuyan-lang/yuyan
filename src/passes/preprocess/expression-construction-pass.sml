structure ExpressionConstructionPass =
struct
    open PreprocessingAST
    open TypeCheckingAST
    
    (* Precedence hierachy: 
    
        On Types: (from lowest) Rho < Exists < Forall < Function < Sum < Prod < 0/1
        On Expressons : 
        (from highest)
            () >  Proj > App > Pair = Inj = fold >  unfold  > Case > TApp 
            >  pack > open > Lambda > Lambda/wtype > TLambda
    
    
    *)

    (* l : e *)
    val unitTypeOp  = Operators.parseOperatorStr "有" false false 42 []
    val nullTypeOp  = Operators.parseOperatorStr "无" false false 42 []
    val labeledTypeCompOp  = Operators.parseOperatorStr "〇表〇" false false 40 []
    val prodTypeOp  = Operators.parseOperatorStr "〇合〇" true false 38 []
    val sumTypeOp  = Operators.parseOperatorStr "〇亦〇" true false 36 []
    val functionTypeOp  = Operators.parseOperatorStr "化〇而〇" true false 35 []
    val universalTypeOp  = Operators.parseOperatorStr "承〇而〇" true false 34 [1]
    val existentialTypeOp  = Operators.parseOperatorStr "有〇则〇" true false 32 [1]
    val recursiveTypeOp  = Operators.parseOperatorStr "复〇为〇" true false 30 [1]

    val unitExprOp = Operators.parseOperatorStr "生" true false 72 []
    val projExprOp = Operators.parseOperatorStr "〇之〇" true false 70 []
    val appExprOp = Operators.parseOperatorStr "〇于〇" true false 68 []
    val pairExprOp = Operators.parseOperatorStr "〇与〇" true false 66 []
    val injExprOp = Operators.parseOperatorStr "〇临〇" false false 66 []
    val foldExprOp = Operators.parseOperatorStr "卷〇" true false 66 []
    val unfoldExprOp = Operators.parseOperatorStr "舒〇" true false 65 []
    val caseClauseOp = Operators.parseOperatorStr "曰〇则有〇而〇" true false 64 [3]
    val caseAlternativeOp = Operators.parseOperatorStr "〇或〇" true false 63 []
    val caseExprOp = Operators.parseOperatorStr "鉴〇而〇" true false 62 []
    val typeAppExprOp = Operators.parseOperatorStr "授〇以〇" true false 60 []
    val packExprOp = Operators.parseOperatorStr "入〇合〇" true false 56 []
    val unpackExprOp = Operators.parseOperatorStr "开〇则有〇者〇而〇" true false 54 [3,5]
    val lambdaExprOp = Operators.parseOperatorStr "会〇而〇" true false 52 [1]
    val lambdaExprWithTypeOp = Operators.parseOperatorStr "遇〇者〇而〇" true false 52 [3]
    val typeLambdaExprOp = Operators.parseOperatorStr "受〇而〇" true false 50 [1]

    val allTypeOps = [
        unitTypeOp, nullTypeOp, labeledTypeCompOp, prodTypeOp, sumTypeOp, functionTypeOp,
        universalTypeOp, existentialTypeOp, recursiveTypeOp]
    val allTypeAndExprOps = allTypeOps @ [ unitExprOp,
        projExprOp, appExprOp, pairExprOp, injExprOp, foldExprOp, unfoldExprOp, caseClauseOp, 
        caseAlternativeOp, caseExprOp, typeAppExprOp, packExprOp, unpackExprOp, lambdaExprOp,
        lambdaExprWithTypeOp, typeLambdaExprOp
    ]


    fun elaborateOpASTtoType (ast : Operators.OpAST) : TypeCheckingAST.Type = 
        (print (PrettyPrint.show_opast ast);
        raise Fail "fail")

    fun elaborateOpASTtoExpr (ast : Operators.OpAST) : TypeCheckingAST.Expr = 
        (print (PrettyPrint.show_opast ast);
        raise Fail "fail")

    structure PrecParser = MixFixParser(structure Options = struct 
        val enableBracketedExpression = true
        end)

    fun parseType (tbody : UTF8String.t)(addedOps : Operators.operator list) : TypeCheckingAST.Type = 
        elaborateOpASTtoType (PrecParser.parseMixfixExpression allTypeOps tbody) 
    fun parseExpr (ebody : UTF8String.t)(addedOps : Operators.operator list) : TypeCheckingAST.Expr
    = elaborateOpASTtoExpr (PrecParser.parseMixfixExpression (allTypeOps@addedOps) ebody) 
    

    fun constructOpAST (ast : PreprocessingAST.t) (addedOps : Operators.operator list) 
        : TypeCheckingAST.Signature = 
        case ast of 
            [] => []
            | (x :: xs) => 
                let fun trailingNoOps() = constructOpAST xs addedOps
                fun trailingWithOps(addedOp:Operators.operator): TypeCheckingAST.Signature 
                    = constructOpAST xs (addedOps@[addedOp])
                in
                (case x of 
                    PTypeMacro(tname, tbody) => TypeMacro(tname, parseType tbody addedOps) :: trailingNoOps()
                    | PTermTypeJudgment(ename, tbody) => TermTypeJudgment(ename, parseType tbody addedOps) :: trailingNoOps()
                    | PTermMacro(ename, ebody) => TermMacro(ename, parseExpr ebody addedOps) :: trailingNoOps()
                    | PTermDefinition(ename, ebody) => TermDefinition(ename, parseExpr ebody addedOps) :: trailingNoOps()
                    | POpDeclaration(opName, assoc, pred) => trailingWithOps(Operators.parseOperator 
                            opName (assoc = Operators.NoneAssoc) (assoc = Operators.LeftAssoc) pred [])
                    | PDirectExpr(ebody) => DirectExpr(parseExpr ebody addedOps) :: trailingNoOps()
                )
                end
                

    fun constructTypeCheckingAST ( ast : PreprocessingAST.t) : TypeCheckingAST.Signature = 
        constructOpAST ast []
        
end
