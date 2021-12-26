
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


    
    fun levelToInt (upToLevel : uptolevel) : int = 
    case upToLevel of
        UpToLevelContent  => 1
        | UpToLevelTypeCheckingInfo => 2
        | UpToLevelDependencyInfo => 3
        | UpToLevelTypeCheckedInfo => 4
        | UpToLevelCPSInfo => 5
        | UpToLevelLLVMInfo => 6


    fun processFileUpTo  (upToLevel: uptolevel)(pwd : string)
        (file : compilationfile)  : compilationfile = 
        let 
            open CompilationTokens
            val CompilationFile{
                                    fp=fp
                                , content=content
                                , typeCheckingInfo=typecheckingast
                                , dependencyInfo=dependencyInfo
                                , typeCheckedInfo=typecheckedast
                                , cpsInfo=cpsInfo
                                , llvmInfo=llvmInfo
                                } = file
            val levelInt = levelToInt upToLevel
        in
            if levelInt < (levelToInt UpToLevelContent)
            then file
            else 
            let val (newContent, updatedContent) = (case content of 
                        SOME c => (c, false)
                        | NONE => ((UTF8String.fromStringAndFile (TextIO.inputAll (TextIO.openIn fp)) fp, Time.now()), true))
            in 
                if levelInt <= (levelToInt UpToLevelContent)
                then (if updatedContent
                      then CompilationFile {
                                    fp=fp
                                , content=SOME(newContent)
                                , typeCheckingInfo=NONE
                                , dependencyInfo=NONE
                                , typeCheckedInfo=NONE
                                , cpsInfo=NONE
                                , llvmInfo=NONE
                                }
                      else file (* return the original file if content are not updated *)
                    )
                else 
                let val (newTypeCheckingInfo, updatedTCkingInfo) = 
                        if Option.isSome typecheckingast andalso not updatedContent
                        then (Option.valOf typecheckingast, false)
                        else (constructTypeCheckingAST (#1 newContent), true)
                in
                    if levelInt <= (levelToInt UpToLevelTypeCheckingInfo)
                    then (if updatedTCkingInfo
                          then CompilationFile {
                                    fp=fp
                                , content=SOME(newContent)
                                , typeCheckingInfo=SOME(newTypeCheckingInfo)
                                , dependencyInfo=NONE
                                , typeCheckedInfo=NONE
                                , cpsInfo=NONE
                                , llvmInfo=NONE
                                }
                        else file  
                    )
                    else
                    let val (newDependencyInfo, updatedDependencyInfo) = 
                            if Option.isSome dependencyInfo andalso not updatedTCkingInfo
                            then (Option.valOf dependencyInfo, false)
                            else (FileDependencyResolution.findFileDependenciesTopLevel (#1 newTypeCheckingInfo), true)
                    in
                        if levelInt <= (levelToInt UpToLevelDependencyInfo)
                        then (if updatedDependencyInfo
                            then CompilationFile {
                                        fp=fp
                                    , content=SOME(newContent)
                                    , typeCheckingInfo=SOME(newTypeCheckingInfo)
                                    , dependencyInfo=SOME(newDependencyInfo)
                                    , typeCheckedInfo=NONE
                                    , cpsInfo=NONE
                                    , llvmInfo=NONE
                                    }
                            else file  
                        )
                        else
                        let val (newTypeCheckedInfo, updatedTypeCheckedInfo) = 
                                if Option.isSome typecheckedast andalso not updatedDependencyInfo
                                then (Option.valOf typecheckedast, false)
                                else (TypeCheckingEntry.typeCheckSignatureTopLevel (#1 newTypeCheckingInfo), true)
                        in 
                            if levelInt <= (levelToInt UpToLevelTypeCheckedInfo)
                            then (if updatedTypeCheckedInfo
                                then CompilationFile {
                                            fp=fp
                                        , content=SOME(newContent)
                                        , typeCheckingInfo=SOME(newTypeCheckingInfo)
                                        , dependencyInfo=SOME(newDependencyInfo)
                                        , typeCheckedInfo=SOME(newTypeCheckedInfo)
                                        , cpsInfo=NONE
                                        , llvmInfo=NONE
                                        }
                                else file  
                            )
                            else
                            let val (newCPSInfo, updatedCPSInfo) = 
                                if Option.isSome cpsInfo andalso not updatedTypeCheckedInfo
                                then (Option.valOf cpsInfo, false)
                                else (constructCPSInfo newTypeCheckedInfo, true)
                            in 
                                if levelInt <= (levelToInt UpToLevelCPSInfo)
                                then (if updatedCPSInfo
                                    then CompilationFile {
                                                fp=fp
                                            , content=SOME(newContent)
                                            , typeCheckingInfo=SOME(newTypeCheckingInfo)
                                            , dependencyInfo=SOME(newDependencyInfo)
                                            , typeCheckedInfo=SOME(newTypeCheckedInfo)
                                            , cpsInfo=SOME(newCPSInfo)
                                            , llvmInfo=NONE
                                            }
                                    else file  
                                )
                                else
                                let val (newLLVMInfo, updatedLLVMInfo) = 
                                    if Option.isSome llvmInfo andalso not updatedCPSInfo
                                    then (Option.valOf llvmInfo, false)
                                    else (constructLLVMInfo (#3 newCPSInfo) pwd, true)
                                in 
                                    if levelInt <= (levelToInt UpToLevelLLVMInfo)
                                    then (if updatedCPSInfo
                                        then CompilationFile {
                                                    fp=fp
                                                , content=SOME(newContent)
                                                , typeCheckingInfo=SOME(newTypeCheckingInfo)
                                                , dependencyInfo=SOME(newDependencyInfo)
                                                , typeCheckedInfo=SOME(newTypeCheckedInfo)
                                                , cpsInfo=SOME(newCPSInfo)
                                                , llvmInfo=SOME(newLLVMInfo)
                                                }
                                        else file  
                                    )
                                    else
                                    raise Fail "cfops217"
                                end
                            end
                        end
                    end
                end
            end
        end

    
end