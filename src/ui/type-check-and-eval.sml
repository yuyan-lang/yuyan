structure TypeCheckAndEval =
struct
    fun typeCheckAndEval (input : string) =
        let val stmtAST = StatementDivisionPass.parseStatementAST (UTF8String.fromString input)
            val _ = print "----------------- Statement AST Constructed -------------- \n"
            val _ = print (PrettyPrint.show_statementast stmtAST)
            val preprocessAST = PreprocessingPass.preprocessAST(stmtAST)
            val _ = print "----------------- Preprocess AST Constructed -------------- \n"
            val _ = print (PrettyPrint.show_preprocessaast preprocessAST)
            val typeCheckingAST = ExpressionConstructionPass.constructTypeCheckingAST(preprocessAST)
        in 
            ""
        end
    
end