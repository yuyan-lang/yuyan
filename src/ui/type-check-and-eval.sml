structure TypeCheckAndEval =
struct
    fun typeCheckAndEval (input : string)(verbose : int) =
        (let fun cprint x s = if x <= verbose then print s else () 
            val startTime = Time.now()
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
            val _ = cprint 2 (PrettyPrint.show_pkcomputation  erasedAST ^ "\n")
            val _ = cprint 1 "----------------- Executing ---------------------- \n"
            val executeTime = Time.now()
            val result = PersistentKMachine.runUntilCompletion (PersistentKMachine.Run([],erasedAST)) 
                                    (fn km => print (PrettyPrint.show_pkmachine km ^ "\n"))
            val endTime = Time.now()
            val compileDuration : Time.time = Time.-(executeTime,startTime)
            val runDuration : Time.time = Time.-(endTime,executeTime)
            val _ = cprint 1 "----------------- Execution Completed ! -------------------- \n"
            val _ = print (UTF8String.toString (PersistentKMachine.pkvalueToString 0 result) ^ "\n")
            val _ = cprint 1 "------------------------------------------- \n"
            val _ = cprint 1 ("compilation took " ^ (LargeInt.toString(Time.toMilliseconds(compileDuration))) ^ "ms; execution took "^
            (LargeInt.toString(Time.toMilliseconds(runDuration))) ^ "ms\n")
        in 
            ()
        end)
        handle TypeCheckingASTOps.TypeCheckingFailure s => (print "Type checking failed\n"; print s)
    | ElaboratePrecedence.ElaborationFail s => (print "elaboration prec failed (perhaps internal error (bug))\n"; print (PrettyPrint.show_parseopast s))
      | ExpressionConstructionPass.ElaborateFailure s => (print "elaboration econs failed (perhaps internal error(bug))\n"; print s )
      |  ExpressionConstructionPass.ECPNoPossibleParse s=> (print "parse failed\n"; print s)
      |  MixedStr.InternalFailure s=> print ( "\n\n" ^ MixedStr.toString s)

    
end