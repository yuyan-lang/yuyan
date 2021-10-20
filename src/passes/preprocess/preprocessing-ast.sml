structure PreprocessingAST = struct



    datatype pJudgment = PEmpty
                       | PTypeMacro of UTF8String.t * MixedStr.t
                       | PTermTypeJudgment of UTF8String.t * MixedStr.t
                       | PTermMacro of UTF8String.t * MixedStr.t
                       | PTermDefinition of UTF8String.t * MixedStr.t
                       | POpDeclaration of UTF8String.t * Operators.associativity * int
                       | PDirectExpr of MixedStr.t
                       | PComment of MixedStr.t
    type preprocessAST = pJudgment list

    type t = preprocessAST
                    

end
