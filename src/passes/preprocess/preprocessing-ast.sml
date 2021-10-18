structure PreprocessingAST = struct



    datatype pJudgment = PEmpty
                       | PTypeMacro of UTF8String.t * UTF8String.t
                       | PTermTypeJudgment of UTF8String.t * UTF8String.t
                       | PTermMacro of UTF8String.t * UTF8String.t
                       | PTermDefinition of UTF8String.t * UTF8String.t
                       | POpDeclaration of UTF8String.t * Operators.associativity * int
                       | PDirectExpr of UTF8String.t
    type preprocessAST = pJudgment list

    type t = preprocessAST
                    

end
