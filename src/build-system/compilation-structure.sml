structure CompilationStructure =
struct
    open StaticErrorStructure

    datatype tokenType = TkTpStructureKeyword
                       | TkTpTypeKeyword
                       | TkTpExprKeyword
                       | TkTpIdentifierBinder
                       | TkTpIdentifierReference
                       | TkTpCustomOperatorName
                       | TkTpStringLiteral
                       | TkTpLabel (* not implemented Yet *)
                       | TkTpComment(* not implemented Yet *)
    datatype tokenInfo = TokenInfo of tokenType
    datatype token = Token of 
        UTF8String.t * (* the original text from which source range can be computed*)
         tokenInfo (* for syntax highlighting *) 



    datatype uptolevel = 
        UpToLevelContent 
        | UpToLevelPreprocessingInfo
        | UpToLevelTypeCheckingInfo
        | UpToLevelDependencyInfo
        | UpToLevelTypeCheckedInfo
        | UpToLevelCPSInfo
        | UpToLevelLLVMInfo

    type dependency = FileResourceURI.t * StructureName.t
    type filepath = FileResourceURI.t
    type cpsinfo = (( CPSAst.cpsvar * CPSAst.cpscomputation) * CPSAst.cpscomputation * 
                LLVMAst.llvmsignature)
    datatype compilationfile = 
        CompilationFile  of   
            {fp: string (* file path *)
            , content: (UTF8String.t * Time.time) witherrsoption (* file content  and accessed time *)
            , tokensInfo : token list 
            , preprocessingInfo : PreprocessingAST.t witherrsoption
            , typeCheckingInfo: TypeCheckingAST.RSignature witherrsoption (* parsed *)
            (* , dependencyInfo: StructureName.t list StrDict.dict witherrsoption list of file paths that this file depends on, for dependency resolution *)
            , dependencyInfo: dependency list witherrsoption 
            , typeCheckedInfo: (TypeCheckingAST.CSignature ) witherrsoption  (* type checked *)
            , cpsInfo: cpsinfo witherrsoption  (* cps transformed, closure converted, and codegened *)
            , llvmInfo: {llfilepath :string} witherrsoption  (* the actual generated ll file *)
            }

    (* type moduleinfo =  {
        name : StructureName.t,
        sourceFolder : string option,
        dependencies : UTF8String.t list option,
        autoOpens : UTF8String.t list option,
        submodules : UTF8String.t list option
    } *)
    
    type cmhelperfuncs = {
        getPreprocessingAST: StructureName.t -> (PreprocessingAST.t * FileResourceURI.t) witherrsoption
                                                (* use to signal errors *)
        ,getTypeCheckedAST:  (dependency) -> TypeCheckingAST.CSignature witherrsoption
        ,getCPSInfo:  (dependency) -> cpsinfo witherrsoption
        (* we also request recursive computation of all file dependencies info when this function is invoked *)
        ,findFileDependenciesTopLevel: TypeCheckingAST.CSignature -> dependency list witherrsoption
        (* returns the topological ordering in the reachable dependency subgraph from the input argument, 
        assumes all dependencies have been computed (function does not request computation) 
        the second argument is the current file's dependency as it cannot be queried from cm because the file might have just 
        computed its dependencies and updates are not in cm yet
        *)
        ,getDependencyInfo:  FileResourceURI.t  -> dependency list -> dependency list witherrsoption
        ,getTopLevelStructureName:  filepath -> StructureName.t
    }


(* we maintain the invariant that all files under a module must be a 
subdirectory of rootDir *)
    type compilationmodule = 
            {files: (
            compilationfile
            ) StrDict.dict ref
            , rootPath: string
            (* , name : StructureName.t *)
             } (* a list of files, mappings from filepath *)

    type compilationmanager = 
        {
        importedModules: compilationmodule list ref  (* imported, a module is basically a search path *)
        , pwd : string (* pwd *)
    }

end
