structure PreprocessingAST = struct



    datatype preprocessAST = EmptySig
                       | TypeMacro of UTF8String.t * Type
                       | TypeJudgment of UTF8String.t * Type
                       | TermDefinition of UTF8String.t * Expr
                       | OpDeclaration of UTF8String.t * Operators.associativity * int

    type t = preprocessAST
                    

end
