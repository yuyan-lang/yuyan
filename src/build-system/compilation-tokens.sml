structure CompilationTokens = struct 
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
        SourceRange.t *
        UTF8String.t * (* the original text *)
         tokenInfo (* for syntax highlighting *) 


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
            | OpStrLiteral  (s, qi) =>
                ((tokensInfo := Token (UTF8String.getSourceRange s,s, (TokenInfo TkTpStringLiteral)) :: (!tokensInfo)  ); ())
            | _ => ()
            (* | OpUnparsedExpr of MixedStr.t (* not used *)
            | OpUnparsedDecl of MixedStr.t list not used *)
        

    (* fun updateUsefulTokensFromDeclarationParser(tokensInfo : token list ref)
        ((oper, _) : operator * MixedStr.t list) : unit = 
            updateUsefulTokensFromOperator tokensInfo oper (TokenInfo TkTpStructureKeyword) *)


    fun updateUsefulTokensFromPJudgment(tokensInfo : token list ref)(x : OpAST.pJudgment * MixedStr.endinginfo) : unit =
        raise Fail "to do"
    fun updateUsefulTokensFromPreprocessingAST(tokensInfo : token list ref)(x : PreprocessingAST.t) : unit =
        (map (updateUsefulTokensFromPJudgment tokensInfo) x; ())


end
