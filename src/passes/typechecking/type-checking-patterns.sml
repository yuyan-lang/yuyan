
structure TypeCheckingPatterns = struct 

    open TypeCheckingAST
    open TypeCheckingASTOps
    open TypeCheckingContext
    open StaticErrorStructure

    structure Errors = TypeCheckingErrors
    fun toHeadSpineForm (ctx : context) (pat : RExpr) : RExpr * RExpr list = 
        case pat of 
            RApp(e1, e2, soi) => case toHeadSpineForm ctx e1 of 
                (hd, tl') => (hd, tl'@[e2])
            | _ => (pat, [])

    (* returns a new context with the bindings added from well defined pattern  *)
    fun checkPattern(ctx : context) ( pat : RExpr ) (analysisType : RType) : (CPattern * context) witherrsoption = 
    let val (head,spine) = toHeadSpineForm ctx pat
        fun countSpineTypeArgs (tp : RType) = 
            case tp of 
                RFunc(t1, t2, soi) => 1 + countSpineTypeArgs t2
                | RPiType(t1, _, t2, soi) => 1 + countSpineTypeArgs t2
                | _ => 0
            
        fun checkSpineAgainstType (accCVar : CExpr list) (ctx :  context) (tp : CType) (restSpine : RExpr list) : (CExpr list * context) witherrsoption = 
            case (tp, restSpine) of 
                (CFunc(t1, t2 ), (RVar(hd), tls)) => 
                checkSpineAgainstType (accCVar@[CVar(hd, CVTBinder)]) 
                    (addToCtxA (TermTypeJ (hd, t1, JTLocalBinder, NONE) ctx))
                    t2 tls
                | (CFunc(t1, t2 ), (uns, tls)) => Errors.unsupportedPatternType uns ctx
                | (CPiType(t1, evop, t2 ), (RVar(hd), tls)) => 
                checkSpineAgainstType (accCVar@[CVar(hd, CVTBinder)])
                                      (addToCtxA (TermTypeJ (hd, t1, JTLocalBinder, NONE) ctx))
                                      (case evop of SOME(x) => substTypeInCType (RVar(hd)) x t2 | NONE => t2 )
                                      tls
                | (CPiType(t1, evop, t2 ), (uns, tls)) => Errors.unsupportedPatternType uns ctx
                | _ => assertTypeEquiv ctx pat tp analysisType >> (Success(accCVar, ctx))
    in
        (* look up the type of the header *)
        case head of 
            RVar(name) => 
                case findCtx ctx name of
                    NONE => (* head not found in the context, check if spine empty *)
                        if length spine = 0 
                        then Success(addToCtxA (TermTypeJ(name, analysisType, JTLocalBinder, NONE)) ctx)
                        else Errors.unboundTermConstructor head ctx
                    | SOME(cname, tp, jtp) =>
                        (case jtp of 
                            JTConstructor => 
                            if length spine <> countSpineTypeArgs tp
                            then Errors.patternArgumentCountMismatch pat ctx (countSpineTypeArgs tp) (length spine)
                            else checkSpineAgainstType [] tp spine
                            | _ => Errors.expectedTermConstructor head ctx
                        )
            | _ => Errors.unsupportedPatternType head ctx
    end

end