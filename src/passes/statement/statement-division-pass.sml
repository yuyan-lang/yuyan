structure StatementDivisionPass =
struct

    val statementOps = [GroupOps.sepOp, GroupOps.endOp]
    val registry = OperatorRegistry.build statementOps

    open StatementAST
    open Operators
    fun toStatementAST (opast : OpAST) : StatementAST.t = 
        case opast of
            UnknownOpName s => StatementNode(opast,Leaf)
            | NewOpName s => StatementNode(opast,Leaf)
            | OpAST(oper, l) => if oper = GroupOps.endOp
            then (case l of [arg] => toStatementAST arg | _ => raise Fail "sdp13")
            else if oper = GroupOps.sepOp
                 then case l of (h :: t :: []) => StatementNode(h,(toStatementAST t))
                        | _ => raise Fail "sdp16"
                 else  raise Fail "sdp17"

    structure Parser = MixFixParser(structure Options = struct
        val enableBracketedExpression = false
    end)

    fun parseStatementAST (source : UTF8String.t) : StatementAST.t = 
        let val opast =  Parser.parseMixfixExpression statementOps source
        in toStatementAST opast
        end


end