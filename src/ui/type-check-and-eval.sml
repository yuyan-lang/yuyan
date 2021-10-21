structure TypeCheckAndEval =
struct
    fun typeCheckAndEval (input : string) =
        let val stmtAST = MixedStr.makeDecl (UTF8String.fromString input)
            val _ = print "----------------- Lexical Analysis Complete -------------- \n"
            val _ = print (PrettyPrint.show_mixedstrs stmtAST ^ "\n")
            val preprocessAST = PreprocessingPass.preprocessAST(stmtAST)
            val _ = print "----------------- Preprocess AST Constructed -------------- \n"
            val _ = print (PrettyPrint.show_preprocessaast preprocessAST)
            val typeCheckingAST = ExpressionConstructionPass.constructTypeCheckingAST(preprocessAST)
            val _ = print "----------------- Type Checking AST Constructed -------------- \n"
            val _ = print (PrettyPrint.show_typecheckingSig typeCheckingAST)
            val _ = print "----------------- Type Checking in Progress -------------------- \n"
            val _ = TypeCheckingPass.typeCheckSignature [] typeCheckingAST
            val _ = print "----------------- Type Checking OK! -------------------- \n"
            val erasedAST = ErasurePass.eraseSig typeCheckingAST
            val _ = print "----------------- Byte Code Generated ! -------------------- \n"
            val _ = print (PrettyPrint.show_pkcomputation (PersistentKMachine.fromKComp erasedAST))
        in 
            ""
        end
    
end