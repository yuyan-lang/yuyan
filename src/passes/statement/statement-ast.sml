structure StatementAST =
struct
    datatype statementAST = Leaf
                            | StatementNode of UTF8String.t * statementAST
    type t = statementAST
end