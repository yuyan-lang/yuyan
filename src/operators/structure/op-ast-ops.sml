structure OpASTOps =
struct
    open OpAST
    open Operators
    fun reconstructOriginalFromOpAST(opast : OpAST) : UTF8String.t = 
        case opast of
            OpAST(oper, args) => reconstructWithArgs oper (map reconstructOriginalFromOpAST args)
            | UnknownOpName s =>  s
            | NewOpName s =>  s
            | OpUnparsedExpr (m, q) => MixedStr.putQuoteAround  (MixedStr.toUTF8String m) q
            | OpUnparsedDecl (l, q) => MixedStr.putQuoteAround (MixedStr.showWithEndingsInfoList l MixedStr.toUTF8String) q
            | OpParsedQuotedExpr (e, q) =>  MixedStr.putQuoteAround  (reconstructOriginalFromOpAST e) q
            | OpParsedDecl (l, q) =>  MixedStr.putQuoteAround (MixedStr.showWithEndingsInfoList l reconstructOriginalFrompJudgment) q
            | OpStrLiteral (s, q) => MixedStr.putQuoteAround  (s) q
    and reconstructOriginalFrompJudgment (p : pJudgment) : UTF8String.t = 
    case p of 
    PEmptyDecl => []
  | PTypeMacro(tname, tbody, soi) => reconstructWithArgs soi [tname, reconstructOriginalFromOpAST tbody]
  | PTermTypeJudgment(ename, tbody, soi) => reconstructWithArgs soi [ename, reconstructOriginalFromOpAST tbody]
  | PTermMacro(ename, ebody, soi) => reconstructWithArgs soi [ename, reconstructOriginalFromOpAST ebody]
  | PTermDefinition(ename, ebody, soi) => reconstructWithArgs soi [ename, reconstructOriginalFromOpAST ebody]
  | POpDeclaration(opName, assoc, pred, (assocText, predText, soi)) => reconstructWithArgs soi [opName, assocText, predText]
  | PDirectExpr(ebody) => reconstructOriginalFromOpAST ebody
  | PStructure(v, name, ebody, soi) => reconstructWithArgs soi [name, reconstructOriginalFromOpAST ebody]
  | POpenStructure(name, soi) => reconstructWithArgs soi [reconstructOriginalFromOpAST name]
  | PImportStructure(name, path, soi) => reconstructWithArgs soi [reconstructOriginalFromOpAST name]
  | PReExportStructure(name, soi) => reconstructWithArgs soi [reconstructOriginalFromOpAST name]
  | PComment(ebody, soi) => reconstructWithArgs soi [MixedStr.toUTF8String ebody]

end
