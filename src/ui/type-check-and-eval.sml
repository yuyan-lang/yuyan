structure TypeCheckAndEval =
struct

    fun printErr (s : string) = (TextIO.output(TextIO.stdErr,s) ; TextIO.flushOut TextIO.stdErr)

    fun typeCheckAndEval (input : string)(options : ReplOptions.t) (filename: string)  =
        (let fun cprint x s = if x <= (#verbose options) then printErr s else () 
            val startTime = Time.now()
            val cm = CompilationManager.initWithWorkingDirectory (FileResourceURI.make (OS.FileSys.getDir()))
            (* val content  = (UTF8String.fromStringAndFile input filename) *)
            (* val _ = cprint 1 (PrettyPrint.show_utf8string content) *)
            (* val whitespaceRemoved = UTF8String.removeAllWhitespace content
            val _ = cprint 1 "----------------- White Space Removal Complete -------------- \n"
            val _ = cprint 2 (UTF8String.toString whitespaceRemoved^"\n") *)
            (* val stmtAST = MixedStr.makeDecl  whitespaceRemoved *)
            (* val stmtAST = MixedStr.makeDecl content *)
            (* val _ = cprint 1 "----------------- Lexical Analysis Complete -------------- \n" *)
            (* val _ = cprint 2 (PrettyPrint.show_mixedstrs stmtAST ^ "\n") *)
            (* val preprocessAST = ExpressionConstructionPass.preprocessAST(stmtAST) *)
            (* val _ = cprint 1 "----------------- Preprocess AST Constructed -------------- \n" *)
            (* val _ = cprint 2 (PrettyPrint.show_preprocessaast preprocessAST) *)
            val absFp = (FileResourceURI.make (PathUtil.makeAbsolute filename (#pwd cm)))
            val _ = CompilationManager.findOrAddFile absFp NONE cm
            val _ = CompilationManager.requestFileProcessing absFp CompilationStructure.UpToLevelLLVMInfo cm
            val CompilationStructure.CompilationFile cfile = CompilationManager.lookupFileByPath absFp cm
            val preExecuteTime = Time.now()
            (* val (typeCheckingAST, tokens) = StrDict.lookup (! (#currentModule cm)) filename
            val data = SyntaxHighlight.getDataFromTokens tokens
            val _ = cprint 1 "----------------- TOKENS: -------------- \n"
            (* val _ = cprint 2 (PrettyPrint.show_tokens tokens) *)
            val _ = cprint 1 "----------------- TOKENS DATA (LSP): -------------- \n"
            (* val _ = cprint 2 (PrettyPrint.show_intlist data ^ "\n") *)
            val _ = cprint 1 "----------------- Type Checking AST Constructed -------------- \n"
            (* val _ = cprint 2 (PrettyPrint.show_typecheckingRSig typeCheckingAST) *)
            val _ = cprint 1 "----------------- Type Checking in Progress -------------------- \n"
            val _ = TypeCheckingEntry.typeCheckSignatureTopLevel typeCheckingAST
            val _ = cprint 1 "----------------- Type Checking OK! -------------------- \n" *)
            val executeTime =  (* removed the use of pk machines due to foreign functions *)
            if #usekmachine options
            then
                (let 
                val erasedASTK = ErasureEntry.eraseSigK (#1 (Option.valOf (#typeCheckingInfo cfile)))
                val _ = cprint 1 "----------------- Erasure Complete ! -------------------- \n"
                val kastK = (PersistentKMachine.fromKComp erasedASTK)
                val _ = cprint 1 "----------------- Byte Code Generated ! -------------------- \n"
                (* val _ = cprint 2 (PrettyPrint.show_pkcomputation kastK ^ "\n") *)
                val _ = cprint 1 "----------------- Executing ---------------------- \n"
                val executeTime = Time.now()
                val result = KMachine.runUntilCompletion (KMachine.Run([],erasedASTK)) 
                                            (fn x => ())
                                        (* (fn km => print (PrettyPrint.show_pkmachine (PersistentKMachine.fromKComp km) ^ "\n")) *)
                val _ = cprint 1 "----------------- Execution Completed ! -------------------- \n"
                val _ = print (UTF8String.toString (KMachine.kvalueToString 0 result) ^ "\n")
                in executeTime end)
            else 
                let val _ = CompilationManager.makeExecutable absFp cm
                val executeTime = Time.now()
                val _ = OS.Process.system "./.yybuild/yyexe"
                in 
                executeTime
                end
            val endTime = Time.now()
            val codeGenDuration : Time.time = Time.-(preExecuteTime,startTime)
            val compileDuration : Time.time = Time.-(executeTime,preExecuteTime)
            val runDuration : Time.time = Time.-(endTime,executeTime)
            val _ = cprint 1 "------------------------------------------- \n"
            val _ = cprint 1 ("codegen took " ^ LargeInt.toString (Time.toMilliseconds codeGenDuration) ^ 
                "ms; (clang) [or kmachine] compilation took " ^ (LargeInt.toString(Time.toMilliseconds(compileDuration))) ^ "ms; execution took "^
            (LargeInt.toString(Time.toMilliseconds(runDuration))) ^ "ms\n")
        in 
            ()
        end)
        handle TypeCheckingASTOps.TypeCheckingFailure s => (print "Type checking failed\n"; print s)
    | ElaboratePrecedence.ElaborationFail s => (print "elaboration prec failed (perhaps internal error (bug))\n"; print (PrettyPrint.show_parseopast s))
      | ExpressionConstructionPass.ElaborateFailure s => (print "elaboration econs failed (perhaps internal error(bug), correction: perhaps not. Check whether you have type inside expression?)\n"; print s )
      |  ExpressionConstructionPass.ECPNoPossibleParse s=> (print "ecp parse failed\n"; print s)
      |  ExpressionConstructionPass.ECPAmbiguousParse s=> (print "ecp parse failed\n"; print s)
      |  MixedStr.InternalFailure s=> print ( "\n\n" ^ MixedStr.toString s)
      |  KMachineOps.InternalFailure s=> print ( "internal failure  (bug) \n\n" ^ s)

    
end