
structure TypeCheckingUtil = struct
    open TypeCheckingAST
    open TypeCheckingASTOps
    open StaticErrorStructure
    infix 5 >>=

    fun assertTypeEquiv (ctx : context) (expr: RExpr) (synthesized : CType) (checked : CType) : unit witherrsoption =
    TypeCheckingErrors.genericError expr ctx ("Assert Type Equiv called on " ^ PrettyPrint.show_typecheckingCExpr synthesized ^ " and "  ^
        PrettyPrint.show_typecheckingCExpr checked  ^ ", replace with type unify" )
        
        fun foldMapCtx (ctx : context) 
                   (f : ('a * context) -> ('b * context) witherrsoption) 
                   (l : 'a list) : ('b list * context) witherrsoption = 
        case l of  
        [] => Success([], ctx)
        | (x :: xs) => f (x, ctx) >>= (fn (y, ctx) => foldMapCtx ctx f xs >>= (fn (ys, ctx) => Success(y::ys, ctx)))


end