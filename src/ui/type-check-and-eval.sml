structure TypeCheckAndEval =
struct
    fun typeCheckAndEval (input : string)(verbose : int) =
        (let fun cprint x s = if x <= verbose then print s else () 
            val whitespaceRemoved = UTF8String.removeAllWhitespace input
            val stmtAST = MixedStr.makeDecl (UTF8String.fromString whitespaceRemoved)
            val _ = cprint 1 "----------------- Lexical Analysis Complete -------------- \n"
            val _ = cprint 2 (PrettyPrint.show_mixedstrs stmtAST ^ "\n")
            val preprocessAST = PreprocessingPass.preprocessAST(stmtAST)
            val _ = cprint 1 "----------------- Preprocess AST Constructed -------------- \n"
            val _ = cprint 2 (PrettyPrint.show_preprocessaast preprocessAST)
            val typeCheckingAST = ExpressionConstructionPass.constructTypeCheckingAST(preprocessAST)
            val _ = cprint 1 "----------------- Type Checking AST Constructed -------------- \n"
            val _ = cprint 2 (PrettyPrint.show_typecheckingSig typeCheckingAST)
            val _ = cprint 1 "----------------- Type Checking in Progress -------------------- \n"
            val _ = TypeCheckingPass.typeCheckSignature [] typeCheckingAST
            val _ = cprint 1 "----------------- Type Checking OK! -------------------- \n"
            val erasedAST = ErasurePass.eraseSig typeCheckingAST
            val _ = cprint 1 "----------------- Byte Code Generated ! -------------------- \n"
            val _ = cprint 2 (PrettyPrint.show_pkcomputation (PersistentKMachine.fromKComp erasedAST) ^ "\n")
            val _ = cprint 1 "----------------- Executing ---------------------- \n"
            val result = KMachine.runUntilCompletion (KMachine.Run([],erasedAST)) (fn km => print (PrettyPrint.show_kmachine km ^ "\n"))
            val _ = cprint 1 "----------------- Execution Completed ! -------------------- \n"
            val _ = print (UTF8String.toString (KMachine.kvalueToString 0 result) ^ "\n")
        in 
            ()
        end)
        handle TypeCheckingASTOps.TypeCheckingFailure s => (print "Type checking failed\n"; print s)
    | ElaboratePrecedence.ElaborationFail s => (print "elaboration prec failed (perhaps internal error (bug))\n"; print (PrettyPrint.show_parseopast s))
      | ExpressionConstructionPass.ElaborateFailure s => (print "elaboration econs failed (perhaps internal error(bug))\n"; print s )
      |  ExpressionConstructionPass.ECPNoPossibleParse s=> (print "parse failed\n"; print (MixedStr.toString s))
      |  MixedStr.InternalFailure s=> print ( "\n\n" ^ MixedStr.toString s)

    
end