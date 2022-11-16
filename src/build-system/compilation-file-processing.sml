
structure CompilationFileProcessing =
struct



open CompilationFileOps

val DEBUG = true
(* val DEBUG = false *)

    fun levelToInt (upToLevel : uptolevel) : int = 
    case upToLevel of
        UpToLevelContent  => 1
        | UpToLevelPreprocessingInfo  => 2
        | UpToLevelTypeCheckingInfo => 3
        | UpToLevelTypeCheckedInfo => 4
        | UpToLevelDependencyInfo => 5
        | UpToLevelCPSInfo => 6
        | UpToLevelLLVMInfo => 7


    fun processFileUpTo  (upToLevel: uptolevel)(cm : compilationmanager)
        (helperFuncs : cmhelperfuncs)
        (file : compilationfile)  : compilationfile = 
        let 
            open CompilationTokens
            val CompilationFile{
                                    fp=fp
                                , content=content
                                , tokensInfo = tokensInfo
                                , preprocessingInfo=preprocessingInfo
                                , typeCheckingInfo=typecheckingast
                                , typeCheckedInfo=typecheckedast
                                , dependencyInfo=dependencyInfo
                                , cpsInfo=cpsInfo
                                , llvmInfo=llvmInfo
                                } = file
            val levelInt = levelToInt upToLevel
            fun debugPrint s = DebugPrint.p ("cfp : ["  ^ PathUtil.makeRelative fp (#pwd cm) ^ "] " ^ s)
        in
            if levelInt < (levelToInt UpToLevelContent)
            then file
            else 
            let val (newContent, updatedContent) = (case content of 
                        Success c => (Success c, false)
                        | NotAvailable => (Success (UTF8String.fromStringAndFile (TextIO.inputAll (TextIO.openIn fp)) fp, Time.now()), true)
                        | DErrors l => (Success (UTF8String.fromStringAndFile (TextIO.inputAll (TextIO.openIn fp)) fp, Time.now()), true)
            )
                val _ = if DEBUG andalso updatedContent then debugPrint 
                ("Updated Content success: " ^ Bool.toString (StaticErrorStructure.isSuccess newContent)
                ^ " updated: " ^ Bool.toString updatedContent
                ^ "\n") else ()
            in 
                if levelInt <= (levelToInt UpToLevelContent) 
                        orelse StaticErrorStructure.isNotSuccess newContent
                        (* if content has error, early return *)
                then (if updatedContent
                      then CompilationFile {
                                    fp=fp
                                , content=newContent
                                , tokensInfo = []
                                , preprocessingInfo=NotAvailable
                                , typeCheckingInfo=NotAvailable
                                , typeCheckedInfo=NotAvailable
                                , dependencyInfo=NotAvailable
                                , cpsInfo=NotAvailable
                                , llvmInfo=NotAvailable
                                }
                      else file (* return the original file if content are not updated *)
                    )
                else 
                let val ((newPreprocessingInfo, newTokensInfo), updatedPreprocessingInfo) = 
                    if StaticErrorStructure.isSuccess preprocessingInfo andalso not updatedContent
                    then ((preprocessingInfo, tokensInfo), false)
                    else (
                        constructPreprocessingAST 
                            (#1 (StaticErrorStructure.valOf newContent))
                            (#getTopLevelStructureName helperFuncs (FileResourceURI.make fp))
                            (#getPreprocessingAST helperFuncs)
                        , true)
                    val _ = if DEBUG andalso updatedPreprocessingInfo then debugPrint 
                    ("Updated Preprocessing success: " ^ Bool.toString (StaticErrorStructure.isSuccess newPreprocessingInfo)
                    ^ " updated: " ^ Bool.toString updatedPreprocessingInfo ^ "\n") else ()
                in
                    if levelInt <= (levelToInt UpToLevelPreprocessingInfo)
                        orelse StaticErrorStructure.isNotSuccess newPreprocessingInfo
                    then (if updatedPreprocessingInfo
                          then CompilationFile {
                                        fp=fp
                                    , content=newContent
                                    , tokensInfo=newTokensInfo
                                    , preprocessingInfo =newPreprocessingInfo
                                    , typeCheckingInfo=NotAvailable
                                    , typeCheckedInfo=NotAvailable
                                    , dependencyInfo=NotAvailable
                                    , cpsInfo=NotAvailable
                                    , llvmInfo=NotAvailable
                                    }
                        else file   
                    )
                    else
                    let val (newTypeCheckingInfo , updatedTCkingInfo) = 
                            if StaticErrorStructure.isSuccess typecheckingast andalso not updatedPreprocessingInfo
                            then ( ( typecheckingast), false)
                            else (ExpressionConstructionPass.constructTypeCheckingASTTopLevel 
                            (StaticErrorStructure.valOf newPreprocessingInfo), true)
                        val _ = if DEBUG andalso updatedTCkingInfo then debugPrint 
                        ("Computed TypeChecking success: " ^ Bool.toString (StaticErrorStructure.isSuccess newTypeCheckingInfo)^ 
                        " updated: " ^ Bool.toString updatedTCkingInfo ^
                        (* (if (StaticErrorStructure.isSuccess newTypeCheckingInfo) then 
                            " DEBUG105: " ^ PrettyPrint.show_typecheckingRSig (StaticErrorStructure.valOf newTypeCheckingInfo)
                             else "") ^ *)
                        "\n") else ()
                    in
                        if levelInt <= (levelToInt UpToLevelTypeCheckingInfo)
                            orelse StaticErrorStructure.isNotSuccess newTypeCheckingInfo
                        then (if updatedTCkingInfo
                            then CompilationFile {
                                        fp=fp
                                    , content=newContent
                                    , tokensInfo=newTokensInfo
                                    , preprocessingInfo =newPreprocessingInfo
                                    , typeCheckingInfo=newTypeCheckingInfo
                                    , typeCheckedInfo=NotAvailable
                                    , dependencyInfo=NotAvailable
                                    , cpsInfo=NotAvailable
                                    , llvmInfo=NotAvailable
                                    }
                            else file  
                        )
                        else
                        let val (newTypeCheckedInfo, updatedTypeCheckedInfo) = 
                                if StaticErrorStructure.isSuccess typecheckedast andalso not updatedTCkingInfo
                                then ( typecheckedast, false)
                                else (TypeCheckingPass.configureAndTypeCheckSignature
                                        (#getTopLevelStructureName helperFuncs (FileResourceURI.make fp))
                                        (#getTypeCheckedAST helperFuncs)
                                        ( (StaticErrorStructure.valOf newTypeCheckingInfo)), true)
                            val _ = if DEBUG andalso updatedTypeCheckedInfo then debugPrint ("Computed TypeChecked success : " ^ 
                                    Bool.toString (StaticErrorStructure.isSuccess newTypeCheckedInfo) 
                                    ^ " updated: " ^ Bool.toString updatedTypeCheckedInfo 
                                    ^ "\n") else ()
                        in 
                            if levelInt <= (levelToInt UpToLevelTypeCheckedInfo)
                                    orelse StaticErrorStructure.isNotSuccess newTypeCheckedInfo
                            then (if updatedTypeCheckedInfo
                                then CompilationFile {
                                            fp=fp
                                        , content=newContent
                                        , tokensInfo=newTokensInfo
                                        , preprocessingInfo =newPreprocessingInfo
                                        , typeCheckingInfo=newTypeCheckingInfo
                                        , typeCheckedInfo=newTypeCheckedInfo
                                        , dependencyInfo=NotAvailable
                                        , cpsInfo=NotAvailable
                                        , llvmInfo=NotAvailable
                                        }
                                else file  
                            )
                            else
                            let val (newDependencyInfo, updatedDependencyInfo) = 
                                    if StaticErrorStructure.isSuccess dependencyInfo andalso not updatedTypeCheckedInfo
                                    then ( dependencyInfo, false)
                                    else ((#findFileDependenciesTopLevel helperFuncs) ( 
                                    (StaticErrorStructure.valOf newTypeCheckedInfo)) , true)
                                val _ = if DEBUG andalso updatedDependencyInfo then debugPrint ("Computed Dependency success : " ^ 
                                    Bool.toString (StaticErrorStructure.isSuccess newDependencyInfo) ^
                                    " updated: " ^ Bool.toString updatedDependencyInfo ^
                                     "\n")else ()
                            in
                                if levelInt <= (levelToInt UpToLevelDependencyInfo)
                                    orelse StaticErrorStructure.isNotSuccess newDependencyInfo
                                then (if updatedDependencyInfo
                                    then CompilationFile {
                                                fp=fp
                                            , content=newContent
                                            , tokensInfo=newTokensInfo
                                            , preprocessingInfo =newPreprocessingInfo
                                            , typeCheckingInfo=newTypeCheckingInfo
                                            , typeCheckedInfo=newTypeCheckedInfo
                                            , dependencyInfo=newDependencyInfo
                                            , cpsInfo=NotAvailable
                                            , llvmInfo=NotAvailable
                                            }
                                    else file  
                                )
                                else
                                let val (newCPSInfo, updatedCPSInfo) = 
                                    if StaticErrorStructure.isSuccess cpsInfo andalso not updatedDependencyInfo
                                    then ( cpsInfo, false)
                                    else (constructCPSInfo (StaticErrorStructure.valOf newTypeCheckedInfo) 
                                    (FileResourceURI.make fp)
                                    (StaticErrorStructure.valOf newDependencyInfo)  
                                     helperFuncs, true)
                                    val _ = if DEBUG andalso updatedCPSInfo then debugPrint ("Computed CPSInfo success : " ^ 
                                    Bool.toString (StaticErrorStructure.isSuccess newCPSInfo) 
                                    ^ " updated: " ^ Bool.toString updatedCPSInfo 
                                    ^ "\n") else ()
                                in 
                                    if levelInt <= (levelToInt UpToLevelCPSInfo)
                                        orelse StaticErrorStructure.isNotSuccess newCPSInfo
                                    then (if updatedCPSInfo
                                        then CompilationFile {
                                                    fp=fp
                                                , content=newContent
                                                , tokensInfo=newTokensInfo
                                                , preprocessingInfo =newPreprocessingInfo
                                                , typeCheckingInfo=newTypeCheckingInfo
                                                , typeCheckedInfo=newTypeCheckedInfo
                                                , dependencyInfo=newDependencyInfo
                                                , cpsInfo=newCPSInfo
                                                , llvmInfo=NotAvailable
                                                }
                                        else file  
                                    )
                                    else
                                    let val (newLLVMInfo, updatedLLVMInfo) = 
                                        if StaticErrorStructure.isSuccess llvmInfo andalso not updatedCPSInfo
                                        then ( llvmInfo, false)
                                        else (constructLLVMInfo (#3 (StaticErrorStructure.valOf newCPSInfo)) 
                                        (FileResourceURI.make fp)
                                        (StaticErrorStructure.valOf newDependencyInfo) 
                                        helperFuncs
                                        (#pwd cm), true)
                                        val _ = if DEBUG andalso updatedLLVMInfo then debugPrint ("Computed LLVMInfo succes : " ^
                                    Bool.toString (StaticErrorStructure.isSuccess newLLVMInfo) 
                                    ^ " updated: " ^ Bool.toString updatedLLVMInfo
                                    ^ "\n") else ()
                                    in 
                                        if levelInt <= (levelToInt UpToLevelLLVMInfo)
                                            orelse StaticErrorStructure.isNotSuccess newLLVMInfo
                                        then (if updatedCPSInfo
                                            then CompilationFile {
                                                        fp=fp
                                                    , content=newContent
                                                    , tokensInfo=newTokensInfo
                                                    , preprocessingInfo =newPreprocessingInfo
                                                    , typeCheckingInfo=newTypeCheckingInfo
                                                    , typeCheckedInfo=newTypeCheckedInfo
                                                    , dependencyInfo=newDependencyInfo
                                                    , cpsInfo=newCPSInfo
                                                    , llvmInfo=newLLVMInfo
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


end