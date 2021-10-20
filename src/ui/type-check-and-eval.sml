structure TypeCheckAndEval =
struct
    fun typeCheckAndEval (input : string) =
        let val stmtAST = MixedStr.makeDecl (UTF8String.fromString input)
            val _ = print "----------------- Lexical Analysis Complete -------------- \n"
            val _ = print (MixedStr.toString [stmtAST] ^ "\n")
            val preprocessAST = PreprocessingPass.preprocessAST([stmtAST])
            val _ = print "----------------- Preprocess AST Constructed -------------- \n"
            val _ = print (PrettyPrint.show_preprocessaast preprocessAST)
            val typeCheckingAST = ExpressionConstructionPass.constructTypeCheckingAST(preprocessAST)
            val _ = print "----------------- Type Checking AST Constructed -------------- \n"
            val _ = print (PrettyPrint.show_typecheckingSig typeCheckingAST)
        in 
            ""
        end
    
end