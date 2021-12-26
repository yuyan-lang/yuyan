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
            , dependencyInfo: string list option (* list of file paths that this file depends on, for dependency resolution *)
            , typeCheckedInfo: (TypeCheckingAST.CSignature ) option  (* type checked *)
            , cpsInfo: (CPSAst.cpscomputation * CPSAst.cpscomputation * 
                LLVMAst.llvmsignature) option  (* cps transformed, closure converted, and codegened *)
            , llvmInfo: {llfilepath :string} option  (* the actual generated ll file *)
            }

(* we maintain the invariant that all files under a module must be a 
subdirectory of rootDir *)
    type compilationmodule = 
            {files: (
            compilationfile
            ) StrDict.dict ref
            , rootPath: string
             } (* a list of files, mappings from filepath *)

    type compilationmanager = 
        {
        importedModules: (StructureName.t * compilationmodule ) list  (* imported, and current modules *)
        , pwd : string (* pwd *)
    }

end
