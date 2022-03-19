
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
    fun toHeadSpineForm (ctx : context) (pat : RExpr) : RExpr * (plicity * RExpr) list = 
        case pat of 
            RApp(e1, e2, p, soi) => (case toHeadSpineForm ctx e1 of 
                (hd, tl') => (hd, tl'@[(p, e2)]))
            | _ => (pat, [])

    (* returns a new context with the bindings added from well defined pattern  *)
    (* all implicit arguments will be inserted into the pattern with a dummy name *)
    fun checkPattern(ctx : context) ( pat : RExpr ) (analysisType : CType) : (CPattern * context) witherrsoption = 
    let val (head,spine) = toHeadSpineForm ctx pat

        (* insert metavars for all pi constructs, and unify the 
        result with the analysis type *)
        fun getConstructorIndicesConstraints (ctx : context) (ctype : CType) : (StructureName.t option list * context) witherrsoption = 
            let 
                fun rest ctx t2 = getConstructorIndicesConstraints ctx t2
            in
            case ctype of 
                CPiType(t1, evop, t2, pt) => 
                    (case evop of 
                        NONE => rest ctx t2 >>= (fn (l, ctx) => Success((NONE::l), ctx))
                        | SOME(name) => 
                            let val metaVarName = StructureName.metaVarName()
                            in  
                                rest (addToCtxA (TermTypeJ(metaVarName, t1, JTMetaVarPendingResolve (reconstructFromRExpr pat), NONE)) ctx) 
                                    (substTypeInCExpr (CMetaVar(metaVarName)) [name] t2)
                                >>= (fn (l, ctx) => Success(((SOME metaVarName) :: l), ctx))
                            end
                    )
                | _ => tryTypeUnify ctx pat ctype analysisType >>= (fn ctx => Success([], ctx))
            end


        fun checkSpineAgainstType (accCVar : CPattern list) (ctx :  context) (ctype : CType)
         (indices : StructureName.t option list) (restSpine : (plicity * RExpr) list)
          : (CPattern list * context) witherrsoption = 
            case (ctype,indices) of 
                (* (CFunc(t1, t2 ), (RVar([hd]):: tls)) => 
                checkSpineAgainstType (accCVar@[CPatVar(hd)]) 
                    (addToCtxA (TermTypeJ ([hd], t1, JTLocalBinder, NONE)) ctx)
                    t2 tls
                | (CFunc(t1, t2 ), (uns::tls)) => Errors.unsupportedPatternType uns ctx *)
                 (CPiType(t1, evop, t2, pt ), (idxh::idxt)) => 
                 let 
                    fun continueWithName hd tls = checkSpineAgainstType (accCVar@[CPatVar(hd)])
                                            (addToCtxA (TermTypeJ ([hd], t1, (
                                                case idxh of 
                                                    SOME(y) => JTLocalBinderWithDef (y)
                                                    |  NONE => JTLocalBinder
                                            ), NONE)) ctx)
                                            (case (evop, idxh) of 
                                                (SOME(x), SOME(name)) => substTypeInCExpr (CVar([hd], CVTBinderDefinition name)) ([x]) t2 
                                                | (NONE, NONE) => t2 
                                                | _ => raise Fail "tcpat64: evop and idx should agree")
                                            idxt
                                            tls
                    fun continueWithRExpr hdexpr tls =  
                    case hdexpr of 
                    RVar ([hd]) => continueWithName hd tls
                    | _ => Errors.unsupportedPatternType hdexpr ctx
                    
                 (* insert implicit arguments if possible *)
                 in
                    (case pt of 
                    Implicit => 
                        (case restSpine of 
                            ((Implicit, hd):: tls) => continueWithRExpr hd tls  (* continue *)
                            | _ => (* insert implicit *)
                            continueWithName (StructureName.binderName()) restSpine
                        )
                    | Explicit => 
                        (case restSpine of 
                            ((Implicit, hd):: tls) => Errors.genericError hd ctx "期待普通参数，不期待隐式参数(Expected explicit args, unexpected implicit args)"
                            | [] => Errors.genericError pat ctx "参数不足"
                            | ((Explicit, hd):: tls) => continueWithRExpr hd tls
                        )
                    )
                 end
                   
                | (_, []) => tryTypeUnify ctx pat ctype analysisType >>= (fn (ctx) => ((Success(accCVar, ctx))))
                | (_, (h::t)) =>  raise Fail "the indices count does not match up"
        
        fun retrieveCInfo (jtp : judgmentType) : cconstructorinfo witherrsoption= 
                (case jtp of 
                            JTConstructor cinfo => Success(cinfo)
                            | JTDefinition cexpr => (
                                let fun traceVarDef(cexpr : CExpr) = 
                                    case cexpr of 
                                        CVar(x, CVTDefinition cexpr) => traceVarDef cexpr
                                        | CVar(x, CVTConstructor(_, cinfo)) => Success(cinfo)
                                        | _ => Errors.expectedTermConstructor head ctx
                                in 
                                    traceVarDef cexpr
                                end
                            )
                            | _ => Errors.expectedTermConstructor head ctx
                        )
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
                        (* retrieve the constructor info *)
                        retrieveCInfo jtp >>= (fn cinfo =>
                            (* if length spine <> countSpineTypeArgs tp
                            then Errors.patternArgumentCountMismatch pat ctx (countSpineTypeArgs tp) (length spine) *)
                            getConstructorIndicesConstraints  ctx tp >>= (fn (indices, ctx) => 
                                checkSpineAgainstType [] ctx tp indices spine >>= 
                                        (fn (checkedSpine, newCtx) => Success(CPatHeadSpine((cname,cinfo) , checkedSpine), newCtx))
                                )
                            )
                        )
            | _ => Errors.unsupportedPatternType head ctx
    end

end