
structure TypeCheckingUtil = struct
    open TypeCheckingAST
    open TypeCheckingASTOps
    open StaticErrorStructure
    infix 5 >>=

    fun assertTypeEquiv (ctx : context) (expr: RExpr) (synthesized : CType) (checked : CType) : unit witherrsoption =
    TypeCheckingErrors.genericError expr ctx ("Assert Type Equiv called on " ^ PrettyPrint.show_typecheckingCExpr synthesized ^ " and "  ^
        PrettyPrint.show_typecheckingCExpr checked  ^ ", replace with type unify" )

end