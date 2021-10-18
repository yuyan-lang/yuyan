structure StatementDivisionPass =
struct

    val statementOps = [GroupOps.sepOp, GroupOps.endOp]
    val registry = OperatorRegistry.make statementOps

    open StatementAST
    open Operators
    (* fun toStatementAST (opast : OpAST) : StatementAST.t = 
        case opast of
            UnknownOpName s => StatementNode(opast,Leaf)
            | NewOpName s => StatementNode(opast,Leaf)
            | OpAST(oper, l) => if oper = GroupOps.endOp
            then (case l of [arg] => toStatementAST arg | _ => raise Fail "sdp13")
            else if oper = GroupOps.sepOp
                 then case l of (h :: t :: []) => StatementNode(h,(toStatementAST t))
                        | _ => raise Fail "sdp16"
                 else  raise Fail "sdp17" *)

    structure Parser = MixFixParser(structure Options = struct
        val enableBracketedExpression = false
    end)

    fun parseStatementAST (source : UTF8String.t) : StatementAST.t = 
    case source of 
        [] => Leaf
        | [s] => if s = SpecialChars.period then Leaf else StatementNode([s], Leaf)
        | (x :: xs) => if x = SpecialChars.period 
                then parseStatementAST xs 
                else let val (stmt, next) =  DeclarationParser.parseBinding [SpecialChars.period] source 
                     in StatementNode (stmt, parseStatementAST next)
                    end


end