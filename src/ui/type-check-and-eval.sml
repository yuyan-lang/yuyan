structure TypeCheckAndEval =
struct
open StaticErrorStructure

    fun printErr (s : string) = (TextIO.output(TextIO.stdErr,s) ; TextIO.flushOut TextIO.stdErr)

    fun typeCheckAndEval (options : ReplOptions.t) (filename: string) (debugFunc : (CompilationStructure.compilationfile 
        -> CompilationStructure.compilationmanager
    -> 'a) option) : unit=
        (let fun cprint x s = if x <= (#verbose options) then printErr s else () 
            val startTime = Time.now()
            val cm = CompilationManager.initWithWorkingDirectory (FileResourceURI.make (OS.FileSys.getDir()))
            val absFp = (FileResourceURI.make (PathUtil.makeAbsolute filename (#pwd cm)))
            val _ = CompilationManager.findOrAddFile absFp NONE cm
            val _ = CompilationManager.requestFileProcessing absFp CompilationStructure.UpToLevelLLVMInfo cm []
            val CompilationStructure.CompilationFile cfile = CompilationManager.lookupFileByPath absFp cm
            val preExecuteTime = Time.now()
            val (executeTime, exitSt) =  
                    let 
                    (* val _ = DebugPrint.p (PrettyPrint.show_compilationfile (CompilationStructure.CompilationFile cfile) ^ "\n") *)
                    val (res) = case debugFunc of 
                            SOME f =>  (Time.now(), (f (CompilationStructure.CompilationFile cfile) cm; OS.Process.success))
                            | NONE => 
                            let
                                val exec = CompilationManager.makeExecutable absFp cm
                                val executeTime = Time.now()
                                val exitSt = case exec of 
                                    Success _ => (OS.Process.system "./.yybuild/yyexe")
                                    | DErrors l => (DebugPrint.p (PrintDiagnostics.showErrs l cm);OS.Process.failure)
                                    | NotAvailable => raise Fail "tcev30"
                            in 
                                (executeTime, exitSt)
                            end
                    in res end
            val endTime = Time.now()
            val codeGenDuration : Time.time = Time.-(preExecuteTime,startTime)
            val compileDuration : Time.time = Time.-(executeTime,preExecuteTime)
            val runDuration : Time.time = Time.-(endTime,executeTime)
            val _ = cprint 1 "------------------------------------------- \n"
            val _ = cprint 1 ("codegen took " ^ LargeInt.toString (Time.toMilliseconds codeGenDuration) ^ 
                "ms; (clang) [or kmachine] compilation took " ^ (LargeInt.toString(Time.toMilliseconds(compileDuration))) ^ "ms; execution took "^
            (LargeInt.toString(Time.toMilliseconds(runDuration))) ^ "ms\n")
        in 
        if (#exitOnFailure options) 
        then OS.Process.exit exitSt
        else ()
        end)
        handle 
     ElaboratePrecedence.ElaborationFail s => (DebugPrint.p "elaboration prec failed (perhaps internal error (bug))\n"; print (PrettyPrint.show_parseopast s))
      |  MixedStr.InternalFailure s=> DebugPrint.p ( "\n\n" ^ MixedStr.toString s)
      |  KMachineOps.InternalFailure s=> DebugPrint.p ( "internal failure  (bug) \n\n" ^ s)
      (* | DeclarationParser.DeclAmbiguousParse ls => DebugPrint.p ("decl ambiguous parse " ^ String.concatWith "\n possible parse : " 
            (map (fn (oper, args) => PrettyPrint.show_op oper ^ " args: " ^ PrettyPrint.show_mixedstrs args) ls)) *)

    
end