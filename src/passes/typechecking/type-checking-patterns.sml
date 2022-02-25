
structure TypeCheckingPatterns = struct 

    open TypeCheckingAST
    open TypeCheckingASTOps
    open TypeCheckingContext
    open StaticErrorStructure

    fun toHeadSpineForm (ctx : context) (pat : RExpr) : RExpr * RExpr list = 
        case pat of 
            RApp(e1, e2, soi) => case toHeadSpineForm ctx e1 of 
                (hd, tl') => (hd, tl'@[e2])
            | _ => (pat, [])

    (* returns a new context with the bindings added from well defined pattern  *)
    fun checkPattern(ctx : context) ( pat : RExpr ) : context witherrsoption = 
    let val (head,spine) = toHeadSpineForm ctx pat
    in
        (* look up the type of the header *)
        case head of 
            RVar(name) => 
    end

end