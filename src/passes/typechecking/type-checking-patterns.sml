
structure TypeCheckingPatterns = struct 

    open TypeCheckingAST
    open TypeCheckingASTOps
    open TypeCheckingContext
    open TypeCheckingUtil
    open TypeCheckingUnify
    open StaticErrorStructure
    infix 5 >>
    infix 5 >>=

    structure Errors = TypeCheckingErrors
    fun toHeadSpineForm (ctx : context) (pat : RExpr) : RExpr * RExpr list = 
        case pat of 
            RApp(e1, e2, soi) => (case toHeadSpineForm ctx e1 of 
                (hd, tl') => (hd, tl'@[e2]))
            | _ => (pat, [])

    (* returns a new context with the bindings added from well defined pattern  *)
    fun checkPattern(ctx : context) ( pat : RExpr ) (analysisType : CType) : (CPattern * context) witherrsoption = 
    let val (head,spine) = toHeadSpineForm ctx pat

        fun checkSpineAgainstType (accCVar : CPattern list) (ctx :  context) (tp : CType) (restSpine : RExpr list) : (CPattern list * context) witherrsoption = 
            case (tp, restSpine) of 
                (* (CFunc(t1, t2 ), (RVar([hd]):: tls)) => 
                checkSpineAgainstType (accCVar@[CPatVar(hd)]) 
                    (addToCtxA (TermTypeJ ([hd], t1, JTLocalBinder, NONE)) ctx)
                    t2 tls
                | (CFunc(t1, t2 ), (uns::tls)) => Errors.unsupportedPatternType uns ctx *)
                 (CPiType(t1, evop, t2 ), (RVar([hd]):: tls)) => 
                checkSpineAgainstType (accCVar@[CPatVar(hd)])
                                      (addToCtxA (TermTypeJ ([hd], t1, JTLocalBinder, NONE)) ctx)
                                      (case evop of SOME(x) => substTypeInCExpr (CVar([hd], CVTBinder)) ([x]) t2 | NONE => t2 )
                                      tls
                | (CPiType(t1, evop, t2 ),(uns:: tls)) => Errors.unsupportedPatternType uns ctx
                | _ => tryTypeUnify ctx pat tp analysisType >>= (fn (ctx) => ((Success(accCVar, ctx)); raise Fail "TODO: unify pattern matching"))
    in
        (* look up the type of the header *)
        case head of 
            RVar(name) => 
                (case findCtx ctx name of
                    NONE => (* head not found in the context, check if spine empty *)
                        if length spine = 0  andalso length name = 0 (* variable must be simple name *)
                        then Success(CPatVar(hd name), addToCtxA (TermTypeJ(name, analysisType, JTLocalBinder, NONE)) ctx)
                        else Errors.unboundTermConstructor head ctx
                    | SOME(cname, tp, jtp) =>
                        (case jtp of 
                            JTConstructor cinfo => 
                            if length spine <> countSpineTypeArgs tp
                            then Errors.patternArgumentCountMismatch pat ctx (countSpineTypeArgs tp) (length spine)
                            else checkSpineAgainstType [] ctx tp spine >>= 
                                    (fn (checkedSpine, newCtx) => Success(CPatHeadSpine((cname,cinfo) , checkedSpine), newCtx))
                            | _ => Errors.expectedTermConstructor head ctx
                        ))
            | _ => Errors.unsupportedPatternType head ctx
    end

end