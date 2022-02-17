structure CompilationTokens = struct 

    open CompilationStructure
    open Operators
    open OpAST
    open OpASTOps

    fun addTokenStr(tokensInfo : token list ref) (s: UTF8String.t) (tktype : tokenType) : unit = 
    (* (DebugPrint.p ("add tokens info called on " ^ PrettyPrint.show_utf8string s ^ "\n"); *)
        tokensInfo := (Token(s, TokenInfo tktype)) :: (!tokensInfo)
    (* ) *)
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
        (* val _ = DebugPrint.p ("updating from opast" ^ PrettyPrint.show_opast ast ^"\n\n") *)
        val add = addTokenStr tokensInfo
        in 
        case ast of 
            OpAST (oper as Operator(_, _, _, _, uid), lst) => 
                ( (* operator itself *)
                if uid = getUID (PreprocessingOperators.inlineCommentOp)
                then (
                    updateUsefulTokensFromOperator tokensInfo oper (TkTpComment);
                    add (reconstructOriginalFromOpAST (hd (tl lst))) TkTpComment;
                    updateUsefulTokensFromOpAST tokensInfo (hd lst)
                    )
                else
                    (if uid >= PreprocessingOperators.elabAppBound
                    then (* user defined ops *) 
                        updateUsefulTokensFromOperator tokensInfo oper (TkTpCustomOperatorName)
                    else 
                    if uid <= PreprocessingOperators.typeOpBound
                    then updateUsefulTokensFromOperator tokensInfo oper (TkTpTypeKeyword)
                    else updateUsefulTokensFromOperator tokensInfo oper (TkTpExprKeyword)
                    ; (* children *)
                    map (updateUsefulTokensFromOpAST tokensInfo)lst
                    ; ()
                    )
                )
            | UnknownOpName s => 
                add s TkTpIdentifierReference
                (* ((tokensInfo := Token (s, (TokenInfo TkTpIdentifierReference)) :: (!tokensInfo)); ()) *)
            | NewOpName s => 
                add s TkTpIdentifierBinder
                (* ((tokensInfo := Token (s, (TokenInfo TkTpIdentifierBinder)) :: (!tokensInfo)  ); ()) *)
            | OpStrLiteral  (s, qi) =>
                (if length s > 0 then add s TkTpStringLiteral else ();
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
        val upOpAST = if true then fn _ => () else updateUsefulTokensFromOpAST tokensInfo (* no need to double update *)
        val upOper = updateUsefulTokensFromOperator tokensInfo
        (* val _ = DebugPrint.p "updating from pjudgment\n" *)
        val _ = case p of 
        PEmptyDecl => ()
        | PTypeMacro(tname, tbody, soi) => (add tname TkTpIdentifierBinder; upOpAST tbody; upOper soi TkTpStructureKeyword)
        | PTermTypeJudgment(ename, tbody, soi) => (add ename TkTpIdentifierBinder; upOpAST tbody; upOper soi TkTpStructureKeyword)
        | PConstructorDecl(ename, tbody, soi) => (add ename TkTpIdentifierBinder; upOpAST tbody; upOper soi TkTpStructureKeyword)
        | PTermMacro(ename, ebody, soi) =>  (add ename TkTpIdentifierBinder; upOpAST ebody; upOper soi TkTpStructureKeyword)
        | PTermDefinition(ename, ebody, soi) =>  (add ename TkTpIdentifierBinder; upOpAST ebody; upOper soi TkTpStructureKeyword)
        | POpDeclaration(opName, assoc, pred, (assocText, predText, soi)) => 
            (upOper soi TkTpStructureKeyword;
            add opName TkTpIdentifierBinder; 
            add assocText TkTpLabel;
            add predText TkTpLabel)
        | PDirectExpr(ebody) => ((upOpAST ebody))
        | PStructure(v, name, ebody, soi) => (add name TkTpIdentifierBinder; upOpAST ebody; upOper soi TkTpStructureKeyword)
        | POpenStructure(name, soi) => (add (reconstructOriginalFromOpAST name) TkTpIdentifierBinder;  upOper soi TkTpStructureKeyword)
        | PImportStructure(name, path, soi) => (add (reconstructOriginalFromOpAST name) TkTpIdentifierBinder;  upOper soi TkTpStructureKeyword)
        | PReExportStructure(name, soi) => (add (reconstructOriginalFromOpAST name) TkTpIdentifierBinder; upOper soi TkTpStructureKeyword)
        | PComment(ebody, soi) => (add (MixedStr.toUTF8String ebody) TkTpComment;  upOper soi TkTpComment)
        in ()
        end

    and updateUsefulTokensFromPreprocessingAST(tokensInfo : token list ref)(x : PreprocessingAST.t) : unit =
        (map (updateUsefulTokensFromPJudgment tokensInfo) x; ())


end
