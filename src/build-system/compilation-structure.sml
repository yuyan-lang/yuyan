structure CompilationStructure =
struct
    open StaticErrorStructure
    open CompilationTokens

    datatype uptolevel = 
        UpToLevelContent 
        | UpToLevelPreprocessingInfo
        | UpToLevelTypeCheckingInfo
        | UpToLevelDependencyInfo
        | UpToLevelTypeCheckedInfo
        | UpToLevelCPSInfo
        | UpToLevelLLVMInfo

    datatype compilationfile = 
        CompilationFile  of   
            {fp: string (* file path *)
            , content: (UTF8String.t * Time.time) witherrsoption (* file content  and accessed time *)
            , tokensInfo : token list 
            , preprocessingInfo : PreprocessingAST.t witherrsoption
            , typeCheckingInfo: TypeCheckingAST.RSignature witherrsoption (* parsed *)
            , dependencyInfo: StructureName.t list StrDict.dict witherrsoption (* list of file paths that this file depends on, for dependency resolution *)
            , typeCheckedInfo: (TypeCheckingAST.CSignature ) witherrsoption  (* type checked *)
            , cpsInfo: (CPSAst.cpscomputation * CPSAst.cpscomputation * 
                LLVMAst.llvmsignature) witherrsoption  (* cps transformed, closure converted, and codegened *)
            , llvmInfo: {llfilepath :string} witherrsoption  (* the actual generated ll file *)
            }

    type moduleinfo =  {
        name : StructureName.t,
        sourceFolder : string option,
        dependencies : UTF8String.t list option,
        autoOpens : UTF8String.t list option,
        submodules : UTF8String.t list option
    }
    
    type cmhelperfuncs = {
        findFileDependenciesTopLevel: TypeCheckingAST.RSignature -> StructureName.t list StrDict.dict witherrsoption,
        getPreprocessingAST: StructureName.t -> (PreprocessingAST.t * FileResourceURI.t) witherrsoption
    }


(* we maintain the invariant that all files under a module must be a 
subdirectory of rootDir *)
    type compilationmodule = 
            {files: (
            compilationfile
            ) StrDict.dict ref
            , rootPath: string
            , moduleInfo : moduleinfo
             } (* a list of files, mappings from filepath *)

    type compilationmanager = 
        {
        importedModules: (StructureName.t * compilationmodule ) list ref  (* imported, and current modules *)
        , pwd : string (* pwd *)
    }

end
