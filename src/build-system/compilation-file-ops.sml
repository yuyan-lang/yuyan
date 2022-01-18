
structure CompilationFileOps =
struct

open CompilationStructure

open StaticErrorStructure
    (* val >>= = StaticErrorStructure.>>= *)
    infix 5 >>= 

    (* val DEBUG = true *)
    val DEBUG = false

    fun fromSome (SOME v) = v


    fun updateFileContent ( newContent : string) (CompilationFile file : compilationfile) : compilationfile = 
        CompilationFile {
                                            fp=(#fp file)
                                        , content=Success((UTF8String.fromStringAndFile newContent (#fp file), Time.now()))
                                        , tokensInfo = []
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
                                        , typeCheckingInfo=NotAvailable
                                        , dependencyInfo=NotAvailable
                                        , typeCheckedInfo=NotAvailable
                                        , cpsInfo=NotAvailable
                                        , llvmInfo=NotAvailable
                                        }


    fun constructTypeCheckingAST 
        (content : UTF8String.t) : (TypeCheckingAST.RSignature 
              ) witherrsoption  * token list= 
        let 
            val tokensInfo : token list ref = ref []
            val tcast = MixedStr.makeDecl content >>=
            (fn stmtASTwithEi =>
            let
                val stmtAST =  (map (fn (x, ei) => x) stmtASTwithEi)
                val _ = if DEBUG then DebugPrint.p (PrettyPrint.show_mixedstrs stmtAST) else ()
                val typeCheckingAST = ExpressionConstructionPass.configureAndConstructTypeCheckingASTTopLevel
                (updateUsefulTokensFromOpAST tokensInfo)
                (updateUsefulTokensFromDeclarationParser tokensInfo)
                (fn x => ())
                (stmtAST)
                (* val _ = DebugPrint.p (PrettyPrint.show_typecheckingRSig typeCheckingAST) *)
                in 
                    (typeCheckingAST >>= (fn t => Success (t)))
                end
            )
            val sortedTokens = ListMergeSort.sort 
                    (* true if gt *)
                    (fn (Token(SourceRange.StartEnd(_, l1, c1, _, _),_,_), Token(SourceRange.StartEnd(_, l2, c2, _, _),_, _))
                    => if l1 > l2 then true else if l1 < l2 then false else if c1 > c2 then true else false)
                    (!tokensInfo) 
        open CompilationTokens
        in (tcast, sortedTokens)
        end

    fun constructCPSInfo (typeCheckedAST : TypeCheckingAST.CSignature) : (CPSAst.cpscomputation * CPSAst.cpscomputation * 
                LLVMAst.llvmsignature ) witherrsoption=
        let
         val cpsAST = CPSPass.cpsTransformSigTopLevel typeCheckedAST 
        (* val _ = DebugPrint.p "----------------- CPS Done -------------------- \n" *)
        (* val _ = DebugPrint.p (PrettyPrint.show_cpscomputation cpsAST) *)
        val closureAST = ClosureConvert.closureConvertTopLevel cpsAST
        (* val _ = DebugPrint.p "----------------- ClosureConvert Done -------------------- \n" *)
        (* val _ = DebugPrint.p (PrettyPrint.show_cpscomputation closureAST) *)
        val (llvmsig) = LLVMConvert.genLLVMSignatureTopLevel closureAST
        in 
            Success (cpsAST, closureAST, llvmsig)
        end
    
    fun constructLLVMInfo (llvmsig : LLVMAst.llvmsignature) (pwd : string) : {llfilepath :string} witherrsoption
                 =
        let  (* TODO use aboslute path for filenames *)
            val filename  = PathUtil.makeAbsolute (".yybuild/yy" ^ Int.toString (UID.next())^ ".ll") pwd
            val statements = LLVMCodegen.genLLVMSignatureWithMainFunction llvmsig 
            val filehandle = TextIO.openOut filename
            val _ = TextIO.output (filehandle,(String.concatWith "\n" statements))
            val _ = TextIO.flushOut (filehandle)
        in 
            Success {llfilepath = filename}
        end


    fun getFileDiagnostics(CompilationFile file : compilationfile) : errlist option = 
        case #content file of DErrors l => SOME l
        | _ => case #typeCheckingInfo file of DErrors l => SOME l
        | _ => case #dependencyInfo file of DErrors l => SOME l
        | _ => case #typeCheckedInfo file of DErrors l => SOME l
        | _ => case #cpsInfo file of DErrors l => SOME l
        | _ => case #llvmInfo file of DErrors l => SOME l
        | _ => NONE
    
    
end