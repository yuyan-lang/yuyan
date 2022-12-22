structure TypeCheckAndEval =
struct
open StaticErrorStructure
open ReplOptions

    fun printErr (s : string) = (TextIO.output(TextIO.stdErr,s) ; TextIO.flushOut TextIO.stdErr)

    fun typeCheckAndEval (options : ReplOptions.t) : OS.Process.status =
        (let fun cprint x s = if x <= (#verbose options) then printErr s else () 
            (* val _ = DebugPrint.p ("Input files:" ^ Int.toString (length (getInputFiles options))) *)
            val startTime = Time.now()
            val cm = CompilationManager.initWithWorkingDirectory (FileResourceURI.make (OS.FileSys.getDir()))
            val absFps = map (fn filename => (FileResourceURI.make (PathUtil.makeAbsolute filename (#pwd cm)))) (getInputFiles options)
            val _ = map (fn absFp => CompilationManager.findOrAddFile absFp NONE cm) absFps
            val _ = map (fn absFp =>  
            (if (getTypeCheckOnly options) 
            then CompilationManager.requestFileProcessing absFp CompilationStructure.UpToLevelTypeCheckedInfo cm []
            else CompilationManager.requestFileProcessing absFp CompilationStructure.UpToLevelLLVMInfo cm [])) absFps
            val entryFileAbsFp = if length absFps > 0 then List.last absFps else raise Fail "tcae13: Should have at least one input file"
            val CompilationStructure.CompilationFile cfile = CompilationManager.lookupFileByPath entryFileAbsFp cm
            val outputFilePath = case getOutputFilePath options of SOME f => (FileResourceURI.make (PathUtil.makeAbsolute f (#pwd cm))) 
                                                                | NONE => (FileResourceURI.make (PathUtil.makeAbsolute ".yybuild.nosync/yyexe" (#pwd cm)))
            val preExecuteTime = Time.now()
            val (executeTime, exitSt) =  
                    let 
                    (* val _ = DebugPrint.p (PrettyPrint.show_compilationfile (CompilationStructure.CompilationFile cfile) ^ "\n") *)
                    val (res) = 
                    if getGenDocs options 
                    then (Time.now(), (DocsGeneration.generateDocs 
                            (FileResourceURI.make (PathUtil.concat [(#pwd cm), "yylib"]))
                            (FileResourceURI.make (PathUtil.concat [(#pwd cm), ".yybuild.nosync", "docs"]))
                            cm ; OS.Process.success))
                    else if getTypeCheckOnly options
                    then ( let val allErrors = (List.concat (List.map (fn (x, l) => l) (CompilationManager.collectAllDiagnostics cm)))
                    val _ = if length allErrors > 0 then DebugPrint.p (PrintDiagnostics.showErrs allErrors cm) else 
                        if getVerbose options > 0 then 
                            (map (fn absFp => 
                            let val CompilationStructure.CompilationFile f =  CompilationManager.lookupFileByPath absFp cm
                            in DebugPrint.p (
                                (FileResourceURI.access absFp) ^ " : \n" ^
                                PrettyPrint.show_typecheckingCSig (valOf (#typeCheckedInfo f))
                                ^ " \n\n\n"
                                )
                            end
                            ) absFps; ())
                        else ()
                    in
                        (Time.now(), if length allErrors = 0 then OS.Process.success else OS.Process.failure)
                    end)
                    else
                            let
                                val exec = CompilationManager.makeExecutable entryFileAbsFp cm (#optimize options) (getEnableProfiling options) 
                                    (getUselocallib options) 
                                outputFilePath
                                (case getClangOptions options of NONE => "" | SOME opt => opt)
                                val executeTime = Time.now()
                                val exitSt = case exec of 
                                    Success _ => (
                                        if (#compileOnly) options 
                                        then OS.Process.success
                                        else (if OS.Process.isSuccess (OS.Process.system (FileResourceURI.access outputFilePath))
                                            then OS.Process.success else OS.Process.failure  (* mlton will fail for nonconfomant exit status*)
                                        ))
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
        exitSt
        end)
        (* handle 
     ElaboratePrecedence.ElaborationFail s => (DebugPrint.p "elaboration prec failed (perhaps internal error (bug))\n"; print (PrettyPrint.show_parseopast s))
      |  MixedStr.InternalFailure s=> DebugPrint.p ( "\n\n" ^ MixedStr.toString s)
      |  KMachineOps.InternalFailure s=> DebugPrint.p ( "internal failure  (bug) \n\n" ^ s) *)
      (* | DeclarationParser.DeclAmbiguousParse ls => DebugPrint.p ("decl ambiguous parse " ^ String.concatWith "\n possible parse : " 
            (map (fn (oper, args) => PrettyPrint.show_op oper ^ " args: " ^ PrettyPrint.show_mixedstrs args) ls)) *)

    
end