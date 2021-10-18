structure PreprocessingAST = struct



    datatype preprocessAST = EmptySig
                       | TypeMacro of UTF8String.t * UTF8String.t
                       | TypeJudgment of UTF8String.t * UTF8String.t
                       | TermDefinition of UTF8String.t * UTF8String.t
                       | OpDeclaration of UTF8String.t * Operators.associativity * int

    type t = preprocessAST
                    

end
