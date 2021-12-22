structure CompilationManager = struct

      structure FileTable : TABLE =
                HashTable
                (structure Key =  StringHashable)
      structure AllTokensTable : TABLE =
                HashTable
                (structure Key = ProductHashable(structure X = StringHashable;
                 structure Y = ProductHashable(structure X = IntHashable;
                  structure Y = IntHashable)))
  
  open CompilationTokens
  open CompilationModule
    
    (* the components of compilation manager contain ref types, 
    and they can change across method calls *)
    type compilationmanager = 
         {
            importedModules: (UTF8String.t * yymodule) list  (* imported modules *)
            , currentModule : yymodule ref (* current module (pwd) *)
            , pwd : string (* pwd *)
            , fileBuffer: UTF8String.t StrDict.dict ref (* file content buffer, mainly for LSP *)
        }

    type t = compilationmanager

    open Operators
    open OpAST
    fun updateUsefulTokensFromOperator(tokensInfo : token list ref)
        (ast : operator ) (info :tokenInfo) : unit = (
            (* print "update called"; *)
        case ast of 
        Operator (_, _, _, comps, _) => (map (fn comp => 
        case comp of 
            OpCompString s => let val sourceRange = UTF8String.getSourceRange  s
            in (tokensInfo := Token (sourceRange, s, info) :: (!tokensInfo) 
            (* print ("Tokens Info Length = " ^ Int.toString (length (!tokensInfo))^ " added " ^ UTF8String.toString s 
            ^ "comps length" ^ Int.toString (length comps)
            ^"\n") *)
            )
            end
            | _ => ()
            ) comps; ())
        )


    fun updateUsefulTokensFromOpAST(tokensInfo : token list ref)
        (ast : OpAST.t) : unit  = 
        case ast of 
            OpAST (oper as Operator(_, _, _, _, uid), lst) => 
                ( (* operator itself *)
                    if uid >= PreprocessingOperators.elabAppBound
                then (* user defined ops *) 
                    updateUsefulTokensFromOperator tokensInfo oper (TokenInfo TkTpCustomOperatorName)
                else if uid <= PreprocessingOperators.typeOpBound
                then updateUsefulTokensFromOperator tokensInfo oper (TokenInfo TkTpTypeKeyword)
                else updateUsefulTokensFromOperator tokensInfo oper (TokenInfo TkTpExprKeyword)
                ; (* children *)
                map (updateUsefulTokensFromOpAST tokensInfo)lst
                ;
                ())
            | UnknownOpName s => 
                ((tokensInfo := Token (UTF8String.getSourceRange s,s, (TokenInfo TkTpIdentifierReference)) :: (!tokensInfo)); ())
            | NewOpName s => 
                ((tokensInfo := Token (UTF8String.getSourceRange s,s, (TokenInfo TkTpIdentifierBinder)) :: (!tokensInfo)  ); ())
            | OpStrLiteral  s =>
                ((tokensInfo := Token (UTF8String.getSourceRange s,s, (TokenInfo TkTpStringLiteral)) :: (!tokensInfo)  ); ())
            | _ => ()
            (* | OpUnparsedExpr of MixedStr.t (* not used *)
            | OpUnparsedDecl of MixedStr.t list not used *)
        

    fun updateUsefulTokensFromDeclarationParser(tokensInfo : token list ref)
        ((oper, _) : operator * MixedStr.t list) : unit = 
            updateUsefulTokensFromOperator tokensInfo oper (TokenInfo TkTpStructureKeyword)

    fun updateContentForFilepath (filepath : string) (content : UTF8String.t) (cm : compilationmanager) : unit = 
             (
                 (#fileBuffer cm) := StrDict.insert (!(#fileBuffer cm)) filepath content
                 ;
                 DebugPrint.p ("The content for "^ filepath ^ " is now " ^ UTF8String.toString (getContentForFilepath filepath cm))
             )

    and getContentForFilepath (filepath : string ) (cm : compilationmanager) :  UTF8String.t = 
        case StrDict.find (!(#fileBuffer cm)) filepath of
            SOME(content) => content
            | NONE =>  let 
            val content = UTF8String.fromStringAndFile (TextIO.inputAll (TextIO.openIn filepath)) filepath
            val _ = updateContentForFilepath filepath content cm
            in content end



    

    fun compileFile (filepath : string) (cm : compilationmanager ) : unit =
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
        (#currentModule cm) := StrDict.insert (! (#currentModule cm)) filepath (typeCheckingAST, sortedTokens) 
    end

    fun initWithWorkingDirectory (pwd : string) : compilationmanager =  
        {
            importedModules = []
            , currentModule = ref(StrDict.empty)
            , pwd=pwd(* pwd *)
            , fileBuffer=  ref StrDict.empty  (* file content buffer, mainly for LSP *)
        }
        (* check if the current directory has a package.yyon file *)
        
        
end
