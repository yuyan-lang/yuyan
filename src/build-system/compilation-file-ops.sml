
structure CompilationFileOps =
struct

open CompilationStructure
open CompilationTokens

open StaticErrorStructure
    (* val >>= = StaticErrorStructure.>>= *)
    infix 5 >>= 

    (* val DEBUG = true *)
    val DEBUG = false



    fun updateFileContent ( newContent : string) (CompilationFile file : compilationfile) : compilationfile = 
        CompilationFile {
                                            fp=(#fp file)
                                        , content=Success((UTF8String.fromStringAndFile newContent (#fp file), Time.now()))
                                        , tokensInfo = []
                                        , preprocessingInfo = NotAvailable
                                        , typeCheckingInfo=NotAvailable
                                        , dependencyInfo=NotAvailable
                                        , typeCheckedInfo=NotAvailable
                                        , cpsInfo=NotAvailable
                                        , llvmInfo=NotAvailable
                                        }

    open FileResourceURI
    fun initWithFilePathAndContent(fp : FileResourceURI.t) ( content : string) = 
        updateFileContent content (CompilationFile {
                                            fp=access fp
                                        , content=NotAvailable
                                        , tokensInfo = []
                                        , preprocessingInfo = NotAvailable
                                        , typeCheckingInfo=NotAvailable
                                        , dependencyInfo=NotAvailable
                                        , typeCheckedInfo=NotAvailable
                                        , cpsInfo=NotAvailable
                                        , llvmInfo=NotAvailable
                                        })

    fun initWithFilePath(fp : FileResourceURI.t) = 
        CompilationFile {
                                            fp=access fp
                                        , content=NotAvailable
                                        , tokensInfo = []
                                        , preprocessingInfo = NotAvailable
                                        , typeCheckingInfo=NotAvailable
                                        , dependencyInfo=NotAvailable
                                        , typeCheckedInfo=NotAvailable
                                        , cpsInfo=NotAvailable
                                        , llvmInfo=NotAvailable
                                        }


    fun constructPreprocessingAST
        (content : UTF8String.t) (topLevelStructureName : StructureName.t)
        (getPreprocessingAST : StructureName.t -> (PreprocessingAST.t * FileResourceURI.t) witherrsoption) 
        : (PreprocessingAST.t) witherrsoption  * token list= 
        let 
            val tokensInfo : token list ref = ref []
                (* val _ = if DEBUG then DebugPrint.p (PrettyPrint.show_mixedstrs stmtAST) else () *)
                (* (updateUsefulTokensFromOpAST tokensInfo)
                (updateUsefulTokensFromDeclarationParser tokensInfo)
                (fn x => ()) *)
                (* (stmtAST) *)
                (* val _ = DebugPrint.p (PrettyPrint.show_typecheckingRSig typeCheckingAST) *)
                    (* (typeCheckingAST >>= (fn t => Success (t))) *)
            val result = PreprocessingPass.configureAndConstructPreprocessingASTTopLevel 
                getPreprocessingAST
                (updateUsefulTokensFromOpAST tokensInfo)
                (updateUsefulTokensFromPreprocessingAST tokensInfo)
                topLevelStructureName 
                content  >>= (fn (t : PreprocessingAST.t) => 
                let
                    (* val _ = DebugPrint.p ("DEBUG78: "  ^ PrettyPrint.show_preprocessaast t) *)
                in 
                    Success(t) 
                end
                )

            val sortedTokens = ListMergeSort.sort 
                    (* true if gt *)
                    (fn (Token(s1,_), Token(s2, _))
                    => 
                    let val SourceRange.StartEnd(_, l1, c1, _, _) = UTF8String.getSourceRange s1 "cfops81"
                        val SourceRange.StartEnd(_, l2, c2, _, _) = UTF8String.getSourceRange s2 "cfops82"
                    in
                        if l1 > l2 then true else if l1 < l2 then false else if c1 > c2 then true else false
                    end
                    )
                    (!tokensInfo) 
        in (result, sortedTokens)
        end

    fun constructFileDependencyInfo (tc : TypeCheckingAST.CSignature) : dependency list = 
        FileDependencyResolution.constructFileDependencyInfo tc

    fun constructCPSInfo (tckedAST : TypeCheckingAST.CSignature) (curFp : FileResourceURI.t) (curFDependencies : dependency list)
    (helperFuncs : cmhelperfuncs) :
     (( CPSAst.cpsvar (* the global cps location that stores the compiled module *) * CPSAst.cpscomputation)
                * CPSAst.cpscomputation * LLVMAst.llvmsignature )
      witherrsoption=
        (
            (* DebugPrint.p ("entering construct cps"); *)
    ((#getDependencyInfo helperFuncs) curFp curFDependencies ) >>= (fn orderedDeps => 
        (
            (* DebugPrint.p ("got ordered Dep"); *)
      mapM (fn d as(fp,sname) => fmap (fn info=> (info,sname)) (#getCPSInfo helperFuncs d)) orderedDeps  >>= (fn orderedDeps =>
        let
        (* val grandTypeCheckedAST = List.concat(csigs@[tckedAST]) *)
        (* val _ = DebugPrint.p ("The dependency of " ^ FileResourceURI.access curFp ^ " is " ^ 
        String.concatWith ", " (map (fn (_, sname) =>StructureName.toStringPlain sname ) orderedDeps))  *)
        val initialCtx =map (fn (((varname, _), _,_), sname)  => (sname, CPSAst.GlobalVar varname)) orderedDeps
        val gResultLoc = CPSAst.CPSVarGlobal (UID.next())
         val cpsAST  = (CPSPass.configureAndCpsTransformSigTopLevel
            (fn fp => 
            case (#getCPSInfo helperFuncs (fp, [UTF8String.fromString ("cfops120: when retrieving cpsinfo from" ^ FileResourceURI.access curFp)]))
            of Success ((v, _), _,_) => v
            | _ => raise Fail "CPSinfo retrieval failure"
            )
            initialCtx tckedAST gResultLoc

            handle CPSPass.CPSInternalError => 
                (DebugPrint.p (" when contructing cps info for " ^ FileResourceURI.access curFp);
                    raise CPSPass.CPSInternalError
                )
         )
        (* val _ = DebugPrint.p "----------------- CPS Done -------------------- \n" *)
        (* val _ = DebugPrint.p (PrettyPrint.show_cpscomputation cpsAST) *)
        val closureAST = ClosureConvert.closureConvertTopLevel (cpsAST)
        (* val _ = DebugPrint.p "----------------- ClosureConvert Done -------------------- \n" *)
        (* val _ = DebugPrint.p (PrettyPrint.show_cpscomputation closureAST) *)
        val (llvmsig) = LLVMConvert.genLLVMSignatureTopLevel closureAST
        in 
            Success ((gResultLoc, cpsAST), closureAST, llvmsig)
        end
      )
    ))
        )
        

    fun constructLLVMInfo (llvmsig : LLVMAst.llvmsignature) 
    (curFp : FileResourceURI.t)
    (curFDependencies : dependency list)
    (helperFuncs : cmhelperfuncs)  (* TODO: dont need to be this complicated , since we now don't need to calculate 
    dependencies, just follow structure *)
    (pwd : string) : {llfilepath :string} witherrsoption
                 =
        (
            (* DebugPrint.p ("entering construct cps"); *)
    ((#getDependencyInfo helperFuncs) curFp curFDependencies ) >>= (fn orderedDeps => 
        (
            (* DebugPrint.p ("got ordered Dep"); *)
      mapM (fn d as(fp,sname) => fmap (fn info=> (info,sname)) (#getCPSInfo helperFuncs d)) orderedDeps  >>= (fn orderedDeps =>
        let
        (* val grandTypeCheckedAST = List.concat(csigs@[tckedAST]) *)
        (* val _ = DebugPrint.p ("The dependency of " ^ FileResourceURI.access curFp ^ " is " ^ 
        String.concatWith ", " (map (fn (_, sname) =>StructureName.toStringPlain sname ) orderedDeps))  *)
        val prevLLVMStmts =map (fn (((_, _), _,llvmsig), sname)  => 
        (
            let 
            (* val _ = DebugPrint.p ("DEBUG : " ^ StructureName.toStringPlain sname ^ " mapped to " ^ 
            Int.toString (#1 llvmsig)) *)
            in
        llvmsig
        end)) orderedDeps

        val allLLVMSig = prevLLVMStmts @ [llvmsig]

        (* TODO use aboslute path for filenames *)
            val filename  = PathUtil.makeAbsolute (".yybuild/yy" ^ Int.toString (UID.next())^ ".ll") pwd
            val statements = LLVMCodegen.genLLVMSignatureWithMainFunction allLLVMSig 
            val filehandle = TextIO.openOut filename
            val _ = TextIO.output (filehandle,(String.concatWith "\n" statements))
            val _ = TextIO.flushOut (filehandle)
        in 
            Success {llfilepath = filename}
        end

      ))))

    fun getFileDiagnostics(CompilationFile file : compilationfile) : errlist option = 
        case #content file of DErrors l => SOME l
        | _ => case #preprocessingInfo file of DErrors l => SOME l
        | _ => case #typeCheckingInfo file of DErrors l => SOME l
        | _ => case #typeCheckedInfo file of DErrors l => SOME l
        | _ => case #dependencyInfo file of DErrors l => SOME l
        | _ => case #cpsInfo file of DErrors l => SOME l
        | _ => case #llvmInfo file of DErrors l => SOME l
        | _ => NONE
    
    fun getPreprocessingAST(CompilationFile file : compilationfile) : PreprocessingAST.t witherrsoption = 
        case #content file of DErrors l => DErrors l
        | _ =>  #preprocessingInfo file 

    fun getTypeCheckedAST(CompilationFile file : compilationfile) : TypeCheckingAST.CSignature witherrsoption = 
        case #content file of DErrors l => DErrors l
        | _ => case #preprocessingInfo file of DErrors l => DErrors l
        | _ => case #typeCheckingInfo file of DErrors l => DErrors l
        | _ => #typeCheckedInfo file 

    fun getDependencyInfo(CompilationFile file : compilationfile) : dependency list witherrsoption = 
        case #content file of DErrors l => DErrors l
        | _ => case #preprocessingInfo file of DErrors l => DErrors l
        | _ => case #typeCheckingInfo file of DErrors l => DErrors l
        | _ => case #typeCheckedInfo file of DErrors l => DErrors l
        | _ => #dependencyInfo file

    fun getCPSInfo(CompilationFile file : compilationfile) : cpsinfo witherrsoption = 
        case #content file of DErrors l => DErrors l
        | _ => case #preprocessingInfo file of DErrors l => DErrors l
        | _ => case #typeCheckingInfo file of DErrors l => DErrors l
        | _ => case #typeCheckedInfo file of DErrors l => DErrors l
        | _ => case #dependencyInfo file of DErrors l => DErrors l
        | _ => #cpsInfo file
    
end