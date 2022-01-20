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
        UTF8String.t * (* the original text from which source range can be computed*)
         tokenInfo (* for syntax highlighting *) 


    open Operators
    open OpAST
    open OpASTOps

    fun addTokenStr(tokensInfo : token list ref) (s: UTF8String.t) (tktype : tokenType) : unit = 
        tokensInfo := (Token(s, TokenInfo tktype)) :: (!tokensInfo)
    fun updateUsefulTokensFromOperator(tokensInfo : token list ref)
        (ast : operator ) (tp :tokenType) : unit = (
            (* print "update called"; *)
        case ast of 
        Operator (_, _, _, comps, _) => (map (fn comp => 
        case comp of 
            OpCompString s => let 
            (* val sourceRange = UTF8String.getSourceRange  s *)
            in (addTokenStr tokensInfo s tp
            (* (tokensInfo := Token (s, info) :: (!tokensInfo)  *)
            (* print ("Tokens Info Length = " ^ Int.toString (length (!tokensInfo))^ " added " ^ UTF8String.toString s 
            ^ "comps length" ^ Int.toString (length comps)
            ^"\n") *)
            )
            end
            | _ => ()
            ) comps; ())
        )

    fun updateUsefulTokensFromEndingInfo(tokensInfo : token list ref) 
    (ei : MixedStr.endinginfo) 
    (tp : tokenType)= 
        case ei of 
            SOME (s) => addTokenStr tokensInfo ([s]) tp
            | NONE => ()
    fun updateUsefulTokensFromQuoteInfo(tokensInfo : token list ref) 
    ((ql, qr) : MixedStr.quoteinfo) 
    (tp : tokenType)= 
             (addTokenStr tokensInfo ([ql]) tp ; addTokenStr tokensInfo ([qr]) tp)



    fun updateUsefulTokensFromOpAST(tokensInfo : token list ref)
        (ast : OpAST.t) : unit  = 
        let 
        val add = addTokenStr tokensInfo
        in 
        case ast of 
            OpAST (oper as Operator(_, _, _, _, uid), lst) => 
                ( (* operator itself *)
                    if uid >= PreprocessingOperators.elabAppBound
                then (* user defined ops *) 
                    updateUsefulTokensFromOperator tokensInfo oper (TkTpCustomOperatorName)
                else if uid <= PreprocessingOperators.typeOpBound
                then updateUsefulTokensFromOperator tokensInfo oper (TkTpTypeKeyword)
                else updateUsefulTokensFromOperator tokensInfo oper (TkTpExprKeyword)
                ; (* children *)
                map (updateUsefulTokensFromOpAST tokensInfo)lst
                ;
                ())
            | UnknownOpName s => 
                add s TkTpIdentifierReference
                (* ((tokensInfo := Token (s, (TokenInfo TkTpIdentifierReference)) :: (!tokensInfo)); ()) *)
            | NewOpName s => 
                add s TkTpIdentifierBinder
                (* ((tokensInfo := Token (s, (TokenInfo TkTpIdentifierBinder)) :: (!tokensInfo)  ); ()) *)
            | OpStrLiteral  (s, qi) =>
                (add s TkTpStringLiteral;
                    updateUsefulTokensFromQuoteInfo tokensInfo qi TkTpStringLiteral)

                (* ((tokensInfo := Token (s, (TokenInfo TkTpStringLiteral)) :: (!tokensInfo)  ); ()) *)
            | OpParsedDecl (l, qi) => 
                (updateUsefulTokensFromPreprocessingAST tokensInfo l;
                    updateUsefulTokensFromQuoteInfo tokensInfo qi TkTpStructureKeyword)
            | OpParsedQuotedExpr (ast, qi) => 
                (updateUsefulTokensFromOpAST tokensInfo ast;
                    updateUsefulTokensFromQuoteInfo tokensInfo qi TkTpCustomOperatorName) (* TODO: maybe dedicated token type? *)
                (* !!! ignore unparsed *)
            | _ => ()
        end
            (* | OpUnparsedExpr of MixedStr.t (* not used *)
            | OpUnparsedDecl of MixedStr.t list not used *)
        

    (* fun updateUsefulTokensFromDeclarationParser(tokensInfo : token list ref)
        ((oper, _) : operator * MixedStr.t list) : unit = 
            updateUsefulTokensFromOperator tokensInfo oper (TokenInfo TkTpStructureKeyword) *)

    and updateUsefulTokensFromPJudgment(tokensInfo : token list ref)((p, ei) : OpAST.pJudgment * MixedStr.endinginfo) : unit =
        let val _ = 
            case p of 
            PComment _ => updateUsefulTokensFromEndingInfo tokensInfo ei TkTpComment
            | _ => updateUsefulTokensFromEndingInfo tokensInfo ei TkTpStructureKeyword
        val add = addTokenStr tokensInfo
        val upOpAST = updateUsefulTokensFromOpAST tokensInfo
        val upOper = updateUsefulTokensFromOperator tokensInfo
        val _ = case p of 
        PEmptyDecl => ()
        | PTypeMacro(tname, tbody, soi) => (add tname TkTpIdentifierBinder; upOpAST tbody; upOper soi TkTpStructureKeyword)
        | PTermTypeJudgment(ename, tbody, soi) => (add ename TkTpIdentifierBinder; upOpAST tbody; upOper soi TkTpStructureKeyword)
        | PTermMacro(ename, ebody, soi) =>  (add ename TkTpIdentifierBinder; upOpAST ebody; upOper soi TkTpStructureKeyword)
        | PTermDefinition(ename, ebody, soi) =>  (add ename TkTpIdentifierBinder; upOpAST ebody; upOper soi TkTpStructureKeyword)
        | POpDeclaration(opName, assoc, pred, (assocText, predText, soi)) => 
            (add opName TkTpIdentifierBinder; 
            add assocText TkTpLabel;
            add predText TkTpLabel)
        | PDirectExpr(ebody) => (upOpAST ebody)
        | PStructure(v, name, ebody, soi) => (add name TkTpIdentifierBinder; upOpAST ebody; upOper soi TkTpStructureKeyword)
        | POpenStructure(name, soi) => (add (reconstructOriginalFromOpAST name) TkTpIdentifierBinder;  upOper soi TkTpStructureKeyword)
        | PImportStructure(name, soi) => (add (reconstructOriginalFromOpAST name) TkTpIdentifierBinder;  upOper soi TkTpStructureKeyword)
        | PComment(ebody, soi) => (add (MixedStr.toUTF8String ebody) TkTpComment;  upOper soi TkTpComment)
        in ()
        end

    and updateUsefulTokensFromPreprocessingAST(tokensInfo : token list ref)(x : PreprocessingAST.t) : unit =
        (map (updateUsefulTokensFromPJudgment tokensInfo) x; ())


end
