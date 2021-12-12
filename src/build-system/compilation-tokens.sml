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
end
