structure CompilationManager = struct


  
  open CompilationTokens
  open CompilationModule
  open CompilationStructure
    
    (* the components of compilation manager contain ref types, 
    and they can change across method calls *)


    open Operators
    open OpAST

    type filepath = FileResourceURI.t

    open FileResourceURI

    fun findModuleForFilePath (filepath : filepath) (cm : compilationmanager) : compilationmodule option
        = 
        case List.filter (fn (_,m) => String.isPrefix (#rootPath m) (access filepath)) (#importedModules cm) of 
            [(s,m)] => SOME m
            | _ => NONE

    fun lookupModuleForFilePath (filepath : filepath) (cm : compilationmanager) : compilationmodule 
        = case findModuleForFilePath filepath cm
        of SOME m => m
        | NONE => raise Fail "cm25: not found"

    fun performFileUpdate (filepath : filepath) (updateFun : compilationfile -> compilationfile) (cm : compilationmanager) : unit = 
        let val module = lookupModuleForFilePath filepath cm
          val file =  StrDict.lookup (!(#files module)) (access filepath)
          val newFile = updateFun file
          val _ = (#files module) := StrDict.insert (!(#files module)) (access filepath) newFile
        in () end

    fun requestFileProcessing(filepath : filepath) (level : uptolevel) (cm : compilationmanager) = 
        performFileUpdate filepath ( CompilationFileOps.processFileUpTo level (#pwd cm)) cm 

    
    fun lookupFileByPath(filepath : filepath) (cm : compilationmanager) : compilationfile
        = let val module = lookupModuleForFilePath filepath cm
          in StrDict.lookup (!(#files module)) (access filepath)
          end
 
    fun addModule (rootPath : filepath) (cm : compilationmanager) (name : StructureName.t) : compilationmanager =
        {importedModules=(#importedModules cm)@[(name, {files=ref (StrDict.empty), rootPath=(access rootPath)})],
        pwd=(#pwd cm)}
(* add a new file to the compilation manager, if the file's module is not found, 
a new module is added with root Path being the file's residing directory *)
    fun addFile(filepath: filepath) (cm : compilationmanager) : compilationmanager =
         case findModuleForFilePath filepath cm of 
            NONE => (addFile filepath (addModule (make (#dir (OS.Path.splitDirFile (access filepath)))) cm (StructureName.localName())))
            | SOME m => let 
                val _ = (#files m) := StrDict.insert (!(#files m)) (access filepath) (CompilationFileOps.initWithFilePath filepath)
                in cm end

    fun updateContentForFilepath (filepath : filepath) (content : string) (cm : compilationmanager) : unit = 
        performFileUpdate filepath (CompilationFileOps.updateFileContent content) cm

    fun makeExecutable(entryFilePath : filepath) (cm : compilationmanager)  = 
        let val CompilationStructure.CompilationFile cfile = lookupFileByPath entryFilePath cm
        val cmd =  "clang "
        ^ String.concatWith " " (map (fn i => (#pwd cm) ^"/runtime/" ^ i) 
        ["allocation.c", "entry.c", "exception.c"]) ^
        " " ^ (#llfilepath (Option.valOf (#llvmInfo cfile)))
        ^ " -save-temps=obj -g -o "  ^ OS.Path.concat(((#pwd cm), ".yybuild/yyexe"))
        ^ " -I /usr/local/include"
        val _ = DebugPrint.p (cmd ^ "\n")
        val _ = OS.Process.system (cmd)
        in 
            ()
        end

    (*
    and getContentForFilepath (filepath : string ) (cm : compilationmanager) :  UTF8String.t = 
        case StrDict.find (!(#fileBuffer cm)) filepath of
            SOME(content) => content
            | NONE =>  let 
            val content = UTF8String.fromStringAndFile (TextIO.inputAll (TextIO.openIn filepath)) filepath
            val _ = updateContentForFilepath filepath content cm
            in content end *)



    

    (* fun compileFile (filepath : string) (cm : compilationmanager ) : unit =
    let val content = getContentForFilepath filepath cm
        val _ = DebugPrint.p ("[compileFile] The content for "^ filepath ^ " is now " ^ UTF8String.toString (getContentForFilepath filepath cm))
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
        val _ = DebugPrint.p "----------------- Type Checking AST Constructed -------------- \n"
        val _ = DebugPrint.p (PrettyPrint.show_typecheckingRSig typeCheckingAST)
        val _ = DebugPrint.p "----------------- Type Checking in Progress -------------------- \n"
        val typeCheckedAST = TypeCheckingEntry.typeCheckSignatureTopLevel typeCheckingAST
        val _ = DebugPrint.p "----------------- Type Checking OK! -------------------- \n"
        val _ = DebugPrint.p (PrettyPrint.show_typecheckingCSig typeCheckedAST)
        val _ = DebugPrint.p "----------------- CPS in Progress -------------------- \n"
        val cpsAST = CPSPass.cpsTransformSigTopLevel typeCheckedAST 
        val _ = DebugPrint.p "----------------- CPS Done -------------------- \n"
        val _ = DebugPrint.p (PrettyPrint.show_cpscomputation cpsAST)
        val closureAST = ClosureConvert.closureConvertTopLevel cpsAST
        val _ = DebugPrint.p "----------------- ClosureConvert Done -------------------- \n"
        val _ = DebugPrint.p (PrettyPrint.show_cpscomputation closureAST)
        val (entryFuncName, llvmsig) = LLVMConvert.genLLVMSignatureTopLevel closureAST
        val _ = DebugPrint.p "----------------- LLVMConvert Done -------------------- \n"
        val statements = LLVMCodegen.genLLVMSignatureWithMainFunction (entryFuncName, llvmsig)
        val _ = DebugPrint.p "----------------- LLVM IR Codegen Done -------------------- \n"
        val _ = DebugPrint.p (String.concatWith "\n" statements)
        val _ = TextIO.output (TextIO.openOut "debug.ll",(String.concatWith "\n" statements))
        val cmd =  "clang "
        ^ String.concatWith " " (map (fn i => OS.FileSys.getDir() ^"/runtime/" ^ i) 
        ["allocation.c", "entry.c", "exception.c"]) ^
        " debug.ll -save-temps=obj -g -o debug -I /usr/local/include"
        val _ = DebugPrint.p (cmd ^ "\n")
        val _ = OS.Process.system (cmd)
    in 
        (* (#currentModule cm) := StrDict.insert (! (#currentModule cm)) filepath (typeCheckingAST, sortedTokens)  *)
        ()
    end *)

    fun initWithWorkingDirectory (pwd : filepath) : compilationmanager =  
    let val _ =  OS.FileSys.mkDir (OS.Path.concat (access pwd, ".yybuild"))
        handle OS.SysErr s => () (* assume creation successful *)
    in
        {
            importedModules = [(StructureName.topLevelName, {files =ref (StrDict.empty), rootPath=(access pwd)})]
            , pwd=(access pwd)(* pwd *)
        }
    end
        (* check if the current directory has a package.yyon file *)
        
        
end
