
structure CompilationFileProcessing =
struct

open CompilationFileOps
    fun levelToInt (upToLevel : uptolevel) : int = 
    case upToLevel of
        UpToLevelContent  => 1
        | UpToLevelTypeCheckingInfo => 2
        | UpToLevelDependencyInfo => 3
        | UpToLevelTypeCheckedInfo => 4
        | UpToLevelCPSInfo => 5
        | UpToLevelLLVMInfo => 6


    fun processFileUpTo  (upToLevel: uptolevel)(cm : compilationmanager)
    (findFileDependenciesTopLevel: TypeCheckingAST.RSignature -> StructureName.t list StrDict.dict )
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
                            else (findFileDependenciesTopLevel (#1 newTypeCheckingInfo) , true)
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
                                    else (constructLLVMInfo (#3 newCPSInfo) (#pwd cm), true)
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