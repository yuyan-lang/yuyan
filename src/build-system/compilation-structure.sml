structure CompilationStructure =
struct
    open CompilationTokens

    datatype uptolevel = 
        UpToLevelContent 
        | UpToLevelTypeCheckingInfo
        | UpToLevelDependencyInfo
        | UpToLevelTypeCheckedInfo
        | UpToLevelCPSInfo
        | UpToLevelLLVMInfo

    datatype compilationfile = 
        CompilationFile  of   
            {fp: string (* file path *)
            , content: (UTF8String.t * Time.time) option (* file content  and accessed time *)
            , typeCheckingInfo: (TypeCheckingAST.RSignature 
              (* list of parse tokens, for LSP *)
              (* TODO: optimize, no need to generate tokens info when compiling on command line*)
              * token list ) option (* parsed *)
            , dependencyInfo: StructureName.t list StrDict.dict option (* list of file paths that this file depends on, for dependency resolution *)
            , typeCheckedInfo: (TypeCheckingAST.CSignature ) option  (* type checked *)
            , cpsInfo: (CPSAst.cpscomputation * CPSAst.cpscomputation * 
                LLVMAst.llvmsignature) option  (* cps transformed, closure converted, and codegened *)
            , llvmInfo: {llfilepath :string} option  (* the actual generated ll file *)
            }

    type moduleinfo =  {
        name : StructureName.t,
        sourceFolder : string option,
        dependencies : UTF8String.t list option,
        autoOpens : UTF8String.t list option,
        submodules : UTF8String.t list option
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
