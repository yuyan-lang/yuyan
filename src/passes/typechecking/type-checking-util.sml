
structure TypeCheckingUtil = struct
    open TypeCheckingAST
    open TypeCheckingASTOps
    open StaticErrorStructure
    infix 5 >>=

    fun assertTypeEquiv (ctx : context) (expr: RExpr) (synthesized : CType) (checked : CType) : unit witherrsoption =
        typeEquiv expr ctx []  synthesized checked  >>= (fn tpequiv => if tpequiv
            then Success() 
            else TypeCheckingErrors.typeMismatch expr (synthesized) checked ctx
        )

end