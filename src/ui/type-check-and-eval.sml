structure TypeCheckAndEval =
struct
    fun typeCheckAndEval (input : string) =
        StatementDivisionPass.parseStatementAST (UTF8String.fromString input)
    
end