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
    open StaticErrorStructure 
    infix 5 >>=

    fun findModuleForFilePath (filepath : filepath) (cm : compilationmanager) : compilationmodule option
        = 
        let val candidates = List.filter (fn (m) => String.isPrefix (#rootPath m) (access filepath)) (!(#importedModules cm)) 
        in if length candidates = 0
            then NONE
            else SOME(hd (ListMergeSort.sort (* descending, using the reverse of the usual gt order *)
                (fn (x,y) => String.size (#rootPath x) < String.size (#rootPath y)) candidates))
        end

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


    
    fun lookupFileByPath(filepath : filepath) (cm : compilationmanager) : compilationfile
        = let val module = lookupModuleForFilePath filepath cm
          in StrDict.lookup (!(#files module)) (access filepath)
          end
        
    

    (* exception ModuleSpecMalformed of string
    fun getModuleInfo (rootPath : filepath) (name : StructureName.t) : moduleinfo = 
        let val packageFp = PathUtil.concat [access rootPath, "package.yyon"]
        in 
        if OS.FileSys.access (packageFp, []) 
        then
            let
                val yyon = YYONUtil.loadYYONFromFile packageFp 
                open YYON
                open YYONUtil
                val yyonObject = asObject yyon
                fun findInObj (s : string) =
                    ListSearchUtil.findUTF8Str yyonObject (UTF8String.fromString s)
                val _ = case findInObj "名称" of
                    SOME (STRING s) => if [s] <> name then raise ModuleSpecMalformed "name not identical" else ()
                    | _ => ()
                val sourceFolder = case findInObj "源文件夹" of 
                    SOME (STRING s) => SOME(PathUtil.concat[access rootPath, UTF8String.toString s])
                    | _ => NONE
                val dependencies = case findInObj "依赖" of 
                    SOME (ARRAY arr) => SOME(map asString arr)
                    | _ => NONE
                val autoOpens = case findInObj "自动导入" of 
                    SOME (ARRAY arr) => SOME(map asString arr)
                    | _ => NONE
                val submodules = case findInObj "分属包" of 
                    SOME (ARRAY arr) => SOME(map asString arr)
                    | _ => NONE
            in 
                {
            name=name,
            sourceFolder= sourceFolder,
            dependencies= dependencies,
            autoOpens= autoOpens,
            submodules= submodules
                }
            end
        else {
        name=name,
        sourceFolder=NONE,
        dependencies=NONE,
        autoOpens=NONE,
        submodules=NONE
            }
        end *)
 
    fun addModule (rootPath : filepath) (cm : compilationmanager)   : compilationmodule =
        let val newModule =( {files=ref (StrDict.empty), rootPath=(access rootPath)
        (* , moduleInfo=getModuleInfo rootPath name *)
        (* name =(if isTopLevel then StructureName.topLevelName else StructureName.searchPathName()) *)
        })
            val _ = (#importedModules cm) := (!(#importedModules cm))@[( newModule)]
        in newModule end
        
    (* fun findOrImportModule (moduleName: StructureName.t) (cm : compilationmanager) : compilationmodule =
        case ListSearchUtil.findSName (!(#importedModules cm)) moduleName of
            SOME(m) => m
            | NONE => addModule (make(OS.Path.concat (OS.Path.concat (#pwd cm, "/yylib/"), StructureName.toStringPlain moduleName))) cm moduleName *)

(* add a new file to the compilation manager, if the file's module is not found, 
a new module is added with root Path being the file's residing directory *)
    fun findOrAddFile(filepath: filepath) (content : string option) (cm : compilationmanager) : compilationfile =
         case findModuleForFilePath filepath cm of 
            NONE => ((addModule (make (#dir (OS.Path.splitDirFile (access filepath)))) cm;
                      findOrAddFile filepath content cm))
            | SOME m => case StrDict.find (!(#files m)) (access filepath) of 
                    SOME f => f
                    | NONE => let
                            val newFile= case content of SOME s => CompilationFileOps.initWithFilePathAndContent filepath s
                                                        | NONE =>  CompilationFileOps.initWithFilePath filepath
                            val _ = (#files m) := StrDict.insert (!(#files m)) (access filepath) newFile
                            in newFile end

    fun updateContentForFilepath (filepath : filepath) (content : string) (cm : compilationmanager) : unit = 
        performFileUpdate filepath (CompilationFileOps.updateFileContent content) cm

    fun getOSSpecificClangOption() : string = 
        if SMLofNJ.SysInfo.getOSName() = "Linux"
        then "-l bsd"
        else ""

    fun makeExecutable(entryFilePath : filepath) (cm : compilationmanager)
    (optimize : bool) 
    (profile : bool) 
    (uselocallib : bool) 
    (outputFilePath : filepath)
    (additionalClangOptions : string)
    : unit witherrsoption  = 
        let val CompilationStructure.CompilationFile cfile = lookupFileByPath entryFilePath cm
        in case CompilationFileOps.getFileDiagnostics (CompilationStructure.CompilationFile cfile) of 
        SOME errs => DErrors errs
        | NONE =>
            let 
            val cmd =  "(" ^
            (if uselocallib then "make -C runtime/ " ^ (if optimize then "opt" else "debug_old") ^ ";" else "") ^
            "clang " ^
             (#llfilepath (StaticErrorStructure.valOf (#llvmInfo cfile)))
            ^
            " " ^
            (if uselocallib then (#pwd cm)^ "/runtime/libyyrt" ^ (if optimize then "opt" else "debug") ^ "_old.a" else 
            "-l yuyanlangruntime" ^ (if optimize then "" else "debug"))
            ^ " "
            ^ (if optimize then "-O3 " else "-g ") 
            ^ (if profile then "-pg " else "") 
            ^ ""
            ^ (getOSSpecificClangOption())
            ^" -o "  
            ^ (access outputFilePath)
            ^ " " ^ additionalClangOptions
            (* ^ " -save-temps=obj -l matplot " *)
            ^ " -L /usr/local/lib -l gc -l uv "
            (* ^ "-l stdc++ " *) (* do not include c++ as this causes a bug on linux *)
            ^ "-Wno-override-module)"
            val _ = DebugPrint.p (cmd ^ "\n")
            val ret = OS.Process.system (cmd)
            in 
            (if not (OS.Process.isSuccess ret) then DebugPrint.p "ERROR in Making Executable\n" else 
            DebugPrint.p "Made Executable!\n";
                Success ())
            end
        end
(* 
    fun listAllFilesInModule (m : compilationmodule) : filepath list =
    if Option.isSome (#sourceFolder (#moduleInfo m))
    then
    map make (
        PathUtil.globDir (Option.valOf (#sourceFolder (#moduleInfo m))) (fn f => 
            UTF8String.isSuffix (UTF8String.fromString ".yuyan") (UTF8String.fromString f) 
            orelse
            UTF8String.isSuffix (UTF8String.fromString "。豫") (UTF8String.fromString f) 
        ))
    else []


    fun listAllAutoOpenModules (m : compilationmodule) (cm : compilationmanager) : compilationmodule list =
    if Option.isSome (#autoOpens (#moduleInfo m))
    then let val moduleNames = Option.valOf (#autoOpens (#moduleInfo m))
             val modules = map (fn x => findOrImportModule ([x]) cm) moduleNames
         in modules end
    else [] *)

    exception AmbiguousStructureReference of StructureName.t
    exception UnresolvedReference of StructureName.t

    (* fun resolveName (sname : StructureName.t) 
    (inmodule : compilationmodule)
    (resolutionStack : (filepath* StructureName.t) list) (* require information *)
    (cm : compilationmanager) : string (* resolved file name *)
    = "Not Implemented" *)
        (* let val allFilePathsInModule = listAllFilesInModule inmodule 
            fun findUniqueReferenceAmongFiles (filepaths : filepath list) : string option = 
                let
                    val _ = List.map (fn x => findOrAddFile x NONE cm) filepaths
                    val _ = List.map (fn x => requestFileProcessing x UpToLevelTypeCheckingInfo cm) allFilePathsInModule
                    val allFiles = List.map (fn x => lookupFileByPath x cm) allFilePathsInModule
                    val filesHavingReference = List.mapPartial (fn CompilationFile f => 
                    let val foundNameOption = IdentifierNameResolution.findIdentifierInSignature 
                        (#1 (StaticErrorStructure.valOf (#typeCheckingInfo f))) sname
                    in case foundNameOption of SOME _ => SOME (#fp f) | NONE => NONE
                    end
                    ) allFiles
                in case filesHavingReference of
                    [] => NONE
                    | [x] => SOME(x)
                    | _ => raise AmbiguousStructureReference sname
                end
        in case findUniqueReferenceAmongFiles allFilePathsInModule of
            SOME s => s
            | NONE => let val openModules = listAllAutoOpenModules inmodule cm
                          val allOpenFiles = List.concat (List.map (fn m => listAllFilesInModule m) openModules)
                      in case findUniqueReferenceAmongFiles allOpenFiles of 
                            SOME s => s
                           | NONE => 
                                (case sname of  (* must be a structural reference at least two components *)
                                    (x::y::xs) => let val importedModule = findOrImportModule ([x]) cm
                                                  in resolveName (y::xs) (importedModule) (resolutionStack) cm
                                                  end
                                    | _ => 
                                    (DebugPrint.p ("Unresolved Ref "  ^ StructureName.toStringPlain sname)
                                    
                                    (* TODO FIX THIS *)
                                    (* raise UnresolvedReference sname *)
                                    ;"<<ERR: UNRESOLVED REFERENCE>>"))
                      end
        end *)
                            


    (* and findFileDependenciesTopLevel 
    (fp : filepath)
    (tcast: TypeCheckingAST.RSignature)
    (cm : compilationmanager) : StructureName.t list StrDict.dict witherrsoption= 
let 
val module = lookupModuleForFilePath fp cm 
(* val _ = DebugPrint.p "got module" *)
(* val unresolvedNames = IdentifierNameResolution.getUnresolvedIdentifiersSignatureTopLevel tcast (#name (#moduleInfo module)) *)
(* val _ = DebugPrint.p ("got unresolved names " ^ PrettyPrint.show_sttrlist unresolvedNames) *)
in 
Success (StrDict.empty)
(* TODO FIx this bug *)
    (* Success (foldl (fn (name, acc) => 
        let val inFile = resolveName name module [(fp, name)] cm
        in case StrDict.find acc inFile of 
            SOME (s) => StrDict.insert acc inFile (s@[name])
            | NONE => StrDict.insert acc inFile ([name])
        end
        ) StrDict.empty unresolvedNames) *)
end *)

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
    fun resolveStructureReference(fromFile: filepath) (sname : StructureName.t) (cm : compilationmanager) : filepath witherrsoption = 
        let 
            val moduleSearchPath : string list = [PathUtil.concat([#pwd cm]), PathUtil.concat([#pwd cm, "yylib"])]
            fun findFileAmongCandidates(otherCandidates : string list) (errInfo : UTF8String.t) : filepath witherrsoption = 
                            let
                                val res = (case (foldl (fn (candidate, acc) => 
                                case acc of 
                                    SOME(x) => SOME(x)
                                    | NONE => if PathUtil.exists candidate
                                            then SOME(Success(make candidate))
                                            else NONE
                                ) NONE otherCandidates) of 
                                    SOME (x) => x
                                    | NONE =>  genSingletonError (errInfo) ("导入的模块未找到(cannot find module)(3) "
                                    ^ " candidates : " 
                                    ^ String.concatWith ", " otherCandidates) NONE)
                            in res
                            end
            fun resolveRec(currentDir : string)(remainingName : StructureName.t) : filepath witherrsoption = 
                case remainingName of 
                    [] => raise Fail "cm324"
                    | [onlyName] => let val otherCandiates = 
                                (fn x => [PathUtil.concat [x, StructureName.toStringPlain remainingName  ^ ".yuyan"],
                                        PathUtil.concat [x, StructureName.toStringPlain remainingName ^"。豫"]]
                                ) (currentDir)
                                in findFileAmongCandidates otherCandiates onlyName end
                    | (firstName ::  rest) => 
                        let val nextFolder = PathUtil.concat [currentDir, UTF8String.toString firstName]
                        in if PathUtil.exists nextFolder
                        then resolveRec nextFolder rest
                        else  genSingletonError (firstName) "导入的模块未找到(cannot find module)(1)" NONE
                        end
        in
        if length sname = 0
            then raise Fail "unexpected empty structure name"
            else case sname of 
                 [] => raise Fail "cm339"
                | [onlyName] => let 
                            val otherCandiates = List.concat (map 
                            (fn x => [PathUtil.concat [x, StructureName.toStringPlain sname  ^ ".yuyan"],
                                    PathUtil.concat [x, StructureName.toStringPlain sname ^"。豫"]]
                            ) (PathUtil.getBaseDir (access fromFile) :: moduleSearchPath))
                            val res = findFileAmongCandidates otherCandiates onlyName
                        in res
                        end
                | (firstName :: rest) => let 
                            val otherCandiates = (map 
                                (fn x => PathUtil.concat ([x, UTF8String.toString firstName]))
                                ) (PathUtil.getBaseDir (access fromFile) :: moduleSearchPath)
                            val res = case (foldl (fn (candidate, acc) => 
                            case acc of 
                                SOME(x) => SOME(x)
                                | NONE => if PathUtil.exists candidate
                                        then SOME(candidate)
                                        else NONE
                            ) NONE otherCandiates) of 
                                SOME (x) => resolveRec x rest
                                | NONE =>  genSingletonError (firstName) "导入的模块未找到(cannot find module)(2)" NONE
                        in res
                        end
        end

(* by the function invoked, dependency info for current filepath cannot be queried from cm because it has not been updated *)
    fun getDependencyOrder(fromFilepath: filepath)(fromFileDependencies : dependency list) (cm : compilationmanager) : dependency list witherrsoption = 
        let 
            val reverseDependencyGraph : dependency list StrDict.dict ref = ref (StrDict.empty)
            val nameDepMapping : dependency list ref = ref fromFileDependencies
            (* do not include self in the dependency *)
            (* val reverseDependencyGraph : dependency list StrDict.dict ref = ref (StrDict.singleton (access fromFilepath) fromFileDependencies) *)
            fun computeReachableGraph((endNode, dep) : dependency) : unit witherrsoption = 
                if (access endNode) = (access fromFilepath) then raise Fail "cyclic dependencies"
                else
                CompilationFileOps.getDependencyInfo (lookupFileByPath endNode cm) >>= (fn dfpl => 
                let val (newDict, present) = 
                        (StrDict.insert' (! reverseDependencyGraph)  (access endNode) (dfpl)) 
                    val _ = reverseDependencyGraph := newDict
                    val _ = nameDepMapping := (endNode, dep) :: (! nameDepMapping)
                in if present 
                    then Success()
                    else mapM (fn dfp => 
                            computeReachableGraph dfp
                            ) dfpl  >>= (fn _ => Success ())
                end
                )
            val r = map computeReachableGraph fromFileDependencies (* do not include self *)
            val resultList : dependency list ref = ref []
            fun computeUntilEmpty () = 
                if StrDict.isEmpty (! reverseDependencyGraph) then 
                    ()
                else (* find an empty key first *)
                    let val emptyNames =  List.filter (fn (_, y) => length y = 0) (StrDict.toList (! reverseDependencyGraph))
                    val currentGraphStr =  "Current Graph: " ^ String.concatWith "; " (map (fn (x,y) => 
                    x ^ " -> " ^ String.concatWith ", "
                    (map (fn y => (FileResourceURI.access (#1 y))) y)) (StrDict.toList (! reverseDependencyGraph)))
                    (* val _ = DebugPrint.p ("\n\n" ^ currentGraphStr) *)
                    in if length emptyNames = 0 then raise Fail ("cyclic dependency : " ^ currentGraphStr)
                    else (resultList :=  (!resultList)@ (map (fn x => (make (#1 x), ListSearchUtil.lookupGeneric (! nameDepMapping) (make (#1 x)) (fn x => fn y => access x= access y)))
                     emptyNames); (* todo: we could reverse all arrows and reverse the concat here *)
                          reverseDependencyGraph := foldr (fn ((name, _), d) => 
                            StrDict.remove d name
                          ) (! reverseDependencyGraph) emptyNames;
                          reverseDependencyGraph := StrDict.map (fn (l) => 
                            List.filter (fn (x, _) => not (List.exists (fn y =>  y = (FileResourceURI.access x)) (map (#1) emptyNames))) l
                          ) (! reverseDependencyGraph) ;
                          computeUntilEmpty()
                          )
                    end
            val _ = computeUntilEmpty()
            (* remove itself from the dependencies *)
            (* val _ = if length (!resultList) = 0 then raise Fail "internal error cm390" else () *)
            (* val _ = DebugPrint.p ("Result list is " ^ String.concatWith ", " (map FileResourceURI.access (! resultList))) *)
            (* val _ = resultList := List.take((!resultList), length (!resultList) -1) *)
        in 
        (* r >>= (fn x =>  *)
        Success(!resultList)
        (* ) *)
        end

    fun getRelativeStructureName(moduleRootPath : filepath) (actualFilePath : filepath) : UTF8String.t list = 
        let
            val relativepath = UTF8String.fromString (PathUtil.makeRelative  (access actualFilePath) (access moduleRootPath))
            val removedExtension = if UTF8String.isSuffix (UTF8String.fromString ".yuyan") (relativepath)
                                   then UTF8String.stripSuffix (UTF8String.fromString ".yuyan") relativepath
                                   else if UTF8String.isSuffix (UTF8String.fromString "。豫") (relativepath)
                                   then UTF8String.stripSuffix (UTF8String.fromString "。豫") relativepath
                                   else relativepath
            val sNameStr = UTF8String.fields (fn x => UTF8Char.semanticEqual x (SpecialChars.pathSeparator)) removedExtension
            val res = sNameStr
        in res
        end


    fun getTopLevelStructureName(filepath : filepath) (cm : compilationmanager) : StructureName.t = 
        let val  module = lookupModuleForFilePath filepath cm
            val moduleRootPath = #rootPath module
        in getRelativeStructureName (make moduleRootPath) filepath
        end


    exception CircularReference of dependency list
    fun showCircularReferenceDependencyList (ds : dependency list) : string = 
        String.concatWith "\n; " (map (fn (fruri, sname) => 
            FileResourceURI.access fruri ^ " with SName " ^ (StructureName.toStringPlain sname)
            ) ds)


    fun requestFileProcessing(filepath : filepath) (level : uptolevel) (cm : compilationmanager) (requestingStack : dependency list) :unit = 
        performFileUpdate filepath ( 
            CompilationFileProcessing.processFileUpTo level (cm)
                {findFileDependenciesTopLevel=
                    (fn csig => 
                        let val deps = CompilationFileOps.constructFileDependencyInfo csig
                        (* no need to consider stack here as the funciton does not query other files *)
                        val _ = map (fn (d, _) => requestFileProcessing d UpToLevelDependencyInfo cm (requestingStack)) deps (* ignore this part *)
                        in Success(deps) end
                    ),
                getPreprocessingAST=(fn structureName => 
                        (* assumes that structureName is issued from the file *)
                        (resolveStructureReference filepath structureName cm >>= (fn fp => 
                            (findOrAddFile (fp) NONE cm;
                            (* if requested file in requesting stack *)
                            if List.exists(fn (x, _) => (access x) = (access fp)) requestingStack
                            then raise CircularReference (( fp, structureName) :: requestingStack)
                            else (requestFileProcessing (fp) UpToLevelPreprocessingInfo cm ((fp, structureName) :: requestingStack)
                            ; CompilationFileOps.getPreprocessingAST (lookupFileByPath (fp) cm) >>= (fn tree => 
                                    Success (tree, fp)
                                )
                            ) handle CircularReference fpl => genSingletonError (StructureName.toString structureName) 
                                ("循环引用 (1) " ^ showCircularReferenceDependencyList fpl) NONE
                        )
                    )
                )),
                getTypeCheckedAST = (fn (fp, errName) => 
                    (findOrAddFile (fp) NONE cm;
                        if List.exists(fn (x,_) => (access x) = (access fp)) requestingStack
                            then raise CircularReference ((fp, errName) :: requestingStack)
                            else (
                    requestFileProcessing (fp) UpToLevelTypeCheckedInfo cm ((fp, errName) :: requestingStack);
                    CompilationFileOps.getTypeCheckedAST (lookupFileByPath fp cm)
                            ) handle CircularReference fpl => genSingletonError (StructureName.toString errName) "循环引用 (2)" NONE
                    ))
                , getDependencyInfo = (fn fp =>  fn dfpl =>
                    getDependencyOrder fp dfpl cm
                )
                , getTopLevelStructureName = (fn fp => 
                    getTopLevelStructureName fp cm
                ),
                 getCPSInfo = (fn (fp, errName) => 
                    (findOrAddFile (fp) NONE cm;
                        if List.exists(fn (x,_) => (access x) = (access fp)) requestingStack
                            then raise CircularReference ((fp, errName) :: requestingStack)
                            else (
                    requestFileProcessing (fp) UpToLevelCPSInfo cm ((fp, errName) :: requestingStack);
                    CompilationFileOps.getCPSInfo (lookupFileByPath fp cm)
                            ) handle CircularReference fpl => genSingletonError (StructureName.toString errName) "循环引用 (3)" NONE
                    ))
                }
            ) cm 

    fun initWithWorkingDirectory (pwd : filepath) : compilationmanager =  
    let val _ =  OS.FileSys.mkDir (OS.Path.concat (access pwd, ".yybuild.nosync"))
        handle OS.SysErr s => () (* assume creation successful *)
        val cm = {
            importedModules = ref []
            , pwd=(access pwd)(* pwd *)
        }
        val _ = addModule pwd cm 
        val _ = addModule (make (PathUtil.concat [access pwd, "yylib"])) cm 
    in
        cm
    end
        (* check if the current directory has a package.yyon file *)

    fun findAllFiles( cm :compilationmanager) : (string * compilationfile) list = 
    let
        val allFiles = List.concat (map (fn (m) => (StrDict.toList (!(#files m)))) ((!(#importedModules cm))))
        in allFiles end

    
    (* returns errors one element per file *)
    fun collectAllDiagnostics(cm : compilationmanager) : (string * StaticErrorStructure.errlist) list = 
    let
        val allFiles = findAllFiles cm
        val diagnostics = (List.map(fn (s, f) =>  case  (CompilationFileOps.getFileDiagnostics f) of 
            SOME l => (s, l) | NONE => (s, [])) allFiles)
    in
        diagnostics
    end


        
        
end
