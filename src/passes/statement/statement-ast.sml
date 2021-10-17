structure StatementAST =
struct
    datatype statementAST = Leaf
                            | StatementNode of Operators.OpAST * statementAST
    type t = statementAST
end