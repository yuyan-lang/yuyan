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
    datatype yymodule = YYModule of 
             (string * (TypeCheckingAST.Signature
              (* * token AllTokensTable.table *)
              * token list
              ) ) list
    datatype compilationmanager = 
        YYCM of (UTF8String.t * yymodule) list  * yymodule * string
        (* importedModules , currentModule, pwd *)

    type t = compilationmanager

    open Operators
    open OpAST
    fun updateUsefulTokensFromOperator(tokensInfo : token list ref)
        (ast : operator ) (info :tokenInfo) : unit = 
        case ast of 
        Operator (_, _, _, comps, _) => (map (fn comp => 
        case comp of 
            OpCompString s => let val sourceRange = UTF8String.getSourceRange  s
            in tokensInfo := Token (sourceRange, s, info) :: (!tokensInfo) end
            | _ => ()
            ) comps; ())


    fun updateUsefulTokensFromOpAST(tokensInfo : token list ref)
        (ast : OpAST.t) : unit  = 
        case ast of 
            OpAST (oper as Operator(_, _, _, _, uid), lst) => 
                if uid >= PreprocessingOperators.elabAppBound
                then (* user defined ops *) 
                    updateUsefulTokensFromOperator tokensInfo oper (TokenInfo TkTpCustomOperatorName)
                else if uid <= PreprocessingOperators.typeOpBound
                then updateUsefulTokensFromOperator tokensInfo oper (TokenInfo TkTpTypeKeyword)
                else updateUsefulTokensFromOperator tokensInfo oper (TokenInfo TkTpExprKeyword)
            | UnknownOpName s => 
                ((tokensInfo := Token (UTF8String.getSourceRange s,s, (TokenInfo TkTpIdentifierBinder)) :: (!tokensInfo)); ())
            | NewOpName s => 
                ((tokensInfo := Token (UTF8String.getSourceRange s,s, (TokenInfo TkTpIdentifierReference)) :: (!tokensInfo)  ); ())
            | OpStrLiteral  s =>
                ((tokensInfo := Token (UTF8String.getSourceRange s,s, (TokenInfo TkTpStringLiteral)) :: (!tokensInfo)  ); ())
            | _ => ()
            (* | OpUnparsedExpr of MixedStr.t (* not used *)
            | OpUnparsedDecl of MixedStr.t list not used *)
        

    fun updateUsefulTokensFromDeclarationParser(tokensInfo : token list ref)
        ((oper, _) : operator * MixedStr.t list) : unit = 
            updateUsefulTokensFromOperator tokensInfo oper (TokenInfo TkTpStructureKeyword)


    

    fun compileFile (filepath : string) (cm : compilationmanager ) : compilationmanager =
    let val content = UTF8String.fromStringAndFile (TextIO.inputAll (TextIO.openIn filepath)) filepath
        val stmtAST = MixedStr.makeDecl content
        val tokensInfo : token list ref = ref []
        val typeCheckingAST = ExpressionConstructionPass.configureAndConstructTypeCheckingASTTopLevel
        (updateUsefulTokensFromOpAST tokensInfo)
        (updateUsefulTokensFromDeclarationParser tokensInfo)
        (stmtAST)
         val sortedTokens = ListMergeSort.sort 
                (* true if gt *)
                (fn (Token(SourceRange.StartEnd(_, l1, c1, _, _),_,_), Token(SourceRange.StartEnd(_, l2, c2, _, _),_, _))
                => if l1 > l2 then true else if l1 < l2 then false else if c1 > c2 then true else false)
                (!tokensInfo)
    in case cm of
        YYCM(importModules, YYModule curModule, pwd) => YYCM(importModules, YYModule (((filepath, (typeCheckingAST, sortedTokens)) :: curModule)), pwd)
    end

    fun initWithWorkingDirectory (pwd : string) : compilationmanager =  
        YYCM([], YYModule [], pwd)
        (* check if the current directory has a package.yyon file *)
        
        
end
