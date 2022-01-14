
structure CompilationFileProcessing =
struct


val DEBUG = true

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
    (findFileDependenciesTopLevel: TypeCheckingAST.RSignature -> StructureName.t list StrDict.dict witherrsoption)
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
                        Success c => (Success c, false)
                        | NotAvailable => (Success (UTF8String.fromStringAndFile (TextIO.inputAll (TextIO.openIn fp)) fp, Time.now()), true))
                val _ = if DEBUG then DebugPrint.p 
                ("cfp: Updated Content success: " ^ Bool.toString (StaticErrorStructure.isSuccess newContent)^ "\n") else ()
            in 
                if levelInt <= (levelToInt UpToLevelContent) 
                        orelse StaticErrorStructure.isNotSuccess newContent
                        (* if content has error, early return *)
                then (if updatedContent
                      then CompilationFile {
                                    fp=fp
                                , content=newContent
                                , typeCheckingInfo=NotAvailable
                                , dependencyInfo=NotAvailable
                                , typeCheckedInfo=NotAvailable
                                , cpsInfo=NotAvailable
                                , llvmInfo=NotAvailable
                                }
                      else file (* return the original file if content are not updated *)
                    )
                else 
                let val (newTypeCheckingInfo, updatedTCkingInfo) = 
                        if StaticErrorStructure.isSuccess typecheckingast andalso not updatedContent
                        then ( typecheckingast, false)
                        else (constructTypeCheckingAST (#1 (StaticErrorStructure.valOf newContent)), true)
                    val _ = if DEBUG then DebugPrint.p 
                    ("cfp: Computed TypeChecking success: " ^ Bool.toString (StaticErrorStructure.isSuccess newTypeCheckingInfo)^ "\n") else ()
                in
                    if levelInt <= (levelToInt UpToLevelTypeCheckingInfo)
                        orelse StaticErrorStructure.isNotSuccess newTypeCheckingInfo
                    then (if updatedTCkingInfo
                          then CompilationFile {
                                    fp=fp
                                , content=newContent
                                , typeCheckingInfo=newTypeCheckingInfo
                                , dependencyInfo=NotAvailable
                                , typeCheckedInfo=NotAvailable
                                , cpsInfo=NotAvailable
                                , llvmInfo=NotAvailable
                                }
                        else file  
                    )
                    else
                    let val (newDependencyInfo, updatedDependencyInfo) = 
                            if StaticErrorStructure.isSuccess dependencyInfo andalso not updatedTCkingInfo
                            then ( dependencyInfo, false)
                            else (findFileDependenciesTopLevel (#1 
                            (StaticErrorStructure.valOf newTypeCheckingInfo)) , true)
                        val _ = if DEBUG then DebugPrint.p "Computed Dependency\n" else ()
                    in
                        if levelInt <= (levelToInt UpToLevelDependencyInfo)
                            orelse StaticErrorStructure.isNotSuccess newDependencyInfo
                        then (if updatedDependencyInfo
                            then CompilationFile {
                                        fp=fp
                                    , content=newContent
                                    , typeCheckingInfo=newTypeCheckingInfo
                                    , dependencyInfo=newDependencyInfo
                                    , typeCheckedInfo=NotAvailable
                                    , cpsInfo=NotAvailable
                                    , llvmInfo=NotAvailable
                                    }
                            else file  
                        )
                        else
                        let val (newTypeCheckedInfo, updatedTypeCheckedInfo) = 
                                if StaticErrorStructure.isSuccess typecheckedast andalso not updatedDependencyInfo
                                then ( typecheckedast, false)
                                else (TypeCheckingEntry.typeCheckSignatureTopLevel 
                                    (#1 (StaticErrorStructure.valOf newTypeCheckingInfo)), true)
                            val _ = if DEBUG then DebugPrint.p "Computed TypeCheck\n" else ()
                        in 
                            if levelInt <= (levelToInt UpToLevelTypeCheckedInfo)
                                    orelse StaticErrorStructure.isNotSuccess newTypeCheckedInfo
                            then (if updatedTypeCheckedInfo
                                then CompilationFile {
                                            fp=fp
                                        , content=newContent
                                        , typeCheckingInfo=newTypeCheckingInfo
                                        , dependencyInfo=newDependencyInfo
                                        , typeCheckedInfo=newTypeCheckedInfo
                                        , cpsInfo=NotAvailable
                                        , llvmInfo=NotAvailable
                                        }
                                else file  
                            )
                            else
                            let val (newCPSInfo, updatedCPSInfo) = 
                                if StaticErrorStructure.isSuccess cpsInfo andalso not updatedTypeCheckedInfo
                                then ( cpsInfo, false)
                                else (constructCPSInfo (StaticErrorStructure.valOf newTypeCheckedInfo), true)
                            in 
                                if levelInt <= (levelToInt UpToLevelCPSInfo)
                                    orelse StaticErrorStructure.isNotSuccess newCPSInfo
                                then (if updatedCPSInfo
                                    then CompilationFile {
                                                fp=fp
                                            , content=newContent
                                            , typeCheckingInfo=newTypeCheckingInfo
                                            , dependencyInfo=newDependencyInfo
                                            , typeCheckedInfo=newTypeCheckedInfo
                                            , cpsInfo=newCPSInfo
                                            , llvmInfo=NotAvailable
                                            }
                                    else file  
                                )
                                else
                                let val (newLLVMInfo, updatedLLVMInfo) = 
                                    if StaticErrorStructure.isSuccess llvmInfo andalso not updatedCPSInfo
                                    then ( llvmInfo, false)
                                    else (constructLLVMInfo (#3 (StaticErrorStructure.valOf newCPSInfo)) (#pwd cm), true)
                                in 
                                    if levelInt <= (levelToInt UpToLevelLLVMInfo)
                                        orelse StaticErrorStructure.isNotSuccess newLLVMInfo
                                    then (if updatedCPSInfo
                                        then CompilationFile {
                                                    fp=fp
                                                , content=newContent
                                                , typeCheckingInfo=newTypeCheckingInfo
                                                , dependencyInfo=newDependencyInfo
                                                , typeCheckedInfo=newTypeCheckedInfo
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