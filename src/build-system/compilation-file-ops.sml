
structure CompilationFileOps =
struct

open CompilationStructure

    fun fromSome (SOME v) = v


    fun updateFileContent ( newContent : string) (CompilationFile file : compilationfile) : compilationfile = 
        CompilationFile {
                                            fp=(#fp file)
                                        , content=SOME((UTF8String.fromStringAndFile newContent (#fp file), Time.now()))
                                        , typeCheckingInfo=NONE
                                        , dependencyInfo=NONE
                                        , typeCheckedInfo=NONE
                                        , cpsInfo=NONE
                                        , llvmInfo=NONE
                                        }

    open FileResourceURI
    fun initWithFilePathAndContent(fp : FileResourceURI.t) ( content : string) = 
        updateFileContent content (CompilationFile {
                                            fp=access fp
                                        , content=NONE
                                        , typeCheckingInfo=NONE
                                        , dependencyInfo=NONE
                                        , typeCheckedInfo=NONE
                                        , cpsInfo=NONE
                                        , llvmInfo=NONE
                                        })

    fun initWithFilePath(fp : FileResourceURI.t) = 
        CompilationFile {
                                            fp=access fp
                                        , content=NONE
                                        , typeCheckingInfo=NONE
                                        , dependencyInfo=NONE
                                        , typeCheckedInfo=NONE
                                        , cpsInfo=NONE
                                        , llvmInfo=NONE
                                        }


    fun constructTypeCheckingAST 
        (content : UTF8String.t) : TypeCheckingAST.RSignature 
              * token list = 
        let 
        open CompilationTokens
        val stmtAST = MixedStr.makeDecl content
        val tokensInfo : token list ref = ref []
        val typeCheckingAST = ExpressionConstructionPass.configureAndConstructTypeCheckingASTTopLevel
        (updateUsefulTokensFromOpAST tokensInfo)
        (updateUsefulTokensFromDeclarationParser tokensInfo)
        (fn x => ())
        (stmtAST)
        val sortedTokens = ListMergeSort.sort 
                (* true if gt *)
                (fn (Token(SourceRange.StartEnd(_, l1, c1, _, _),_,_), Token(SourceRange.StartEnd(_, l2, c2, _, _),_, _))
                => if l1 > l2 then true else if l1 < l2 then false else if c1 > c2 then true else false)
                (!tokensInfo) 
        in 
            (typeCheckingAST, sortedTokens)
        end

    fun constructCPSInfo (typeCheckedAST : TypeCheckingAST.CSignature) : CPSAst.cpscomputation * CPSAst.cpscomputation * 
                LLVMAst.llvmsignature =
        let
         val cpsAST = CPSPass.cpsTransformSigTopLevel typeCheckedAST 
        val _ = DebugPrint.p "----------------- CPS Done -------------------- \n"
        val _ = DebugPrint.p (PrettyPrint.show_cpscomputation cpsAST)
        val closureAST = ClosureConvert.closureConvertTopLevel cpsAST
        val _ = DebugPrint.p "----------------- ClosureConvert Done -------------------- \n"
        val _ = DebugPrint.p (PrettyPrint.show_cpscomputation closureAST)
        val (llvmsig) = LLVMConvert.genLLVMSignatureTopLevel closureAST
        in 
            (cpsAST, closureAST, llvmsig)
        end
    
    fun constructLLVMInfo (llvmsig : LLVMAst.llvmsignature) (pwd : string) : {llfilepath :string}
                 =
        let  (* TODO use aboslute path for filenames *)
            val filename  = PathUtil.makeAbsolute (".yybuild/yy" ^ Int.toString (UID.next())^ ".ll") pwd
            val statements = LLVMCodegen.genLLVMSignatureWithMainFunction llvmsig 
            val _ = TextIO.output (TextIO.openOut filename,(String.concatWith "\n" statements))
        in 
            {llfilepath = filename}
        end


    
    
end