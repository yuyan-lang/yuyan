(* Implemented from various sources including 
- Robert Harper, Practical Foundations for Programming Languages, 2016
- Ulf Norell, Towards a Practical Programming Language based on Dependent Type Theory, 2007
*)
structure TypeCheckingPass = struct
open TypeCheckingAST
open TypeCheckingASTOps
open TypeCheckingContext
open TypeCheckingUtil
open TypeCheckingPatterns
open TypeCheckingUnify
open StaticErrorStructure
infix 5 >>= 
infix 5 >> 
infix 5 <|>
infix 6 =/=
infix 5 <?>

    (* val DEBUG = true *)
    val DEBUG = false


    (*  !!! we assume the context is well formed in the sense that 
    all term type judgments have no free type variables !!! *) 
    (* so before anything is added to context, must perform substitution first ! *)
    (* the context is not a telescope !!! *)
    (* This also applies to type definitions! They must be expanded as well! *)
    (* ^^^ THis is not true !!! *)
    (* this is called the closed-world assumption and in practice, this 
    helps to reduce bugs during type checking *)
    (* also assume no name clash  *)
           
   
                    (* curStructure, curVisibility and mapping *)

    fun getMapping (c: context ):mapping list = 
        case c of  (Context(cSname, cVis, m)) => m



    fun nextContextOfOpenStructure  (curSName : StructureName.t) (curVis : bool) (bindings : mapping list) 
    (openName : StructureName.t)=

     Context(curSName, curVis, 
            (* extract all bindings from bindings in order and put them into the current context *)
                    List.mapPartial (fn x => 
                    case x of TermTypeJ(name, t, jtp,  u) => 
                    (case StructureName.checkRefersToScope name openName curSName of
                        SOME(nameStripped) => SOME(TermTypeJ(curSName@nameStripped, t, jtp, 
                            (case u of SOME x => SOME x | NONE => SOME (name, jtp))))
                        | NONE => NONE)
                    (* | TermDefJ(name, t, u) =>
                    (case StructureName.checkRefersToScope name openName curSName of
                        SOME(nameStripped) => SOME(TermDefJ(curSName@nameStripped, t, u))
                        | NONE => NONE) *)
                    ) bindings @ bindings
                )

    fun reExportDecls  (ctx as Context(curSName ,curVis, bindings): context)
    (reexportName : StructureName.t) : CSignature witherrsoption =

            (* extract all bindings from bindings in order and put them into the current context *)
        let val decls = 
        List.mapPartial (fn x => 
            case x of TermTypeJ(name, t, jtype,  u) => 
            (case StructureName.checkRefersToScope name reexportName curSName of
                SOME(nameStripped) => SOME(CTermDefinition(curSName@nameStripped,  
                                            (case u of SOME (x, jtp) => CVar(x, (case jtp of JTDefinition d => CVTDefinition d 
                                                                                            | _ => CVTBinder))  (* TODO: BIG ISSUE: REExport of type constructors *)
                                            | NONE => CVar (name, (case jtype of JTDefinition d => CVTDefinition d 
                                                                                            | _ => CVTBinder))), t))
                                            (* TODO: export of constructors *)
                | NONE => NONE)
            (* | TypeDef(name, t, u) =>
            (case StructureName.checkRefersToScope name reexportName curSName of
                SOME(nameStripped) => SOME(CTypeMacro(curSName@nameStripped, t))
                | NONE => NONE) *)
            ) bindings 
        in if length decls > 0
        then Success(List.rev decls) (* context order are reverse of reexport order *)
        else genSingletonError (StructureName.toString reexportName) "????????????????????????????????????" (showctxSome ctx)
        end

        

    (* fun applyContextTo (ctx : context) (subst : CType -> StructureName.t -> 'a -> 'a) (t : 'a) : 'a = 
    (
        (* print ("apply ctx to gen called"  ^ Int.toString(case ctx of (Context(_, _, l)) => length l)^ "\n") ; *)
        case ctx of Context(curName, curVis, mapl) =>
        (case mapl of
            [] => t
            | TermDefJ(n1, t1, u)::cs => (
                let val stepOne = (subst t1 n1 t)
                    val stepTwo = (subst t1 (StructureName.stripPrefixOnAgreedParts curName n1) stepOne)
                    val rest = applyContextTo (Context(curName, curVis, cs)) subst stepTwo
                    in rest end
                )
                (* print "HHHH"; *)
            (* the current subsituting name is a prefix! we need also to perform local subsitution *)
            (* always eagerly perform prefix-stripped substitutions *)
            | TermTypeJ(_) :: cs => applyContextTo (Context(curName, curVis, cs)) subst t)
            (* to get the semantics correct, context need to be applied in reverse order *)
            (* no reverse function is called because context is in reverse order *)
    )
    fun applyContextToType (ctx : context) (t : CType) : CType = 
    (
        (* print "apply ctx to type called\n"; *)
        applyContextTo ctx (fn t => fn  l => fn t1 => 
        (let 
        val _ = 
        if DEBUG then  print (" apply context subsituting "^ PrettyPrint.show_typecheckingCType t  
        ^ " for " ^ StructureName.toStringPlain l ^ " in " ^ PrettyPrint.show_typecheckingCType t1 ^  "\n") else ()
            val res = substTypeInCExpr t l t1
            val _ =if DEBUG then print (" res is " ^ PrettyPrint.show_typecheckingCType res ^ "\n") else ()
            in res end
            )) t
    ) *)
    (* fun applyContextToExpr (ctx : context) (e : RExpr) : RExpr = 
        applyContextTo ctx (fn t => fn  l => fn e1 => 
        (let 
        val _ = 
        if DEBUG then  print (" apply context subsituting "^ PrettyPrint.show_typecheckingCType t  
        ^ " for " ^ StructureName.toStringPlain l ^ " in " ^ PrettyPrint.show_typecheckingRExpr e1 ^  "\n") else ()
            (* val res = substTypeInCExpr t l e1 *)
            val res = raise Fail "undefined154"
            val _ =if DEBUG then print (" res is " ^ PrettyPrint.show_typecheckingRExpr res ^ "\n") else ()
            in res end
            )) e
    fun applyContextToSignature (ctx : context) (s : CSignature) : CSignature = 
        applyContextTo ctx (substituteTypeInCSignature) s *)


    (* index begins with zero *)
    fun lookupLabel ( ctx : (Label * 'a) list) (l : Label) : (int * 'a) witherrsoption = 
        let fun go i ctx = 
        case ctx of 
            [] =>  genSingletonError l ("??????`" ^ UTF8String.toString l ^ "`?????????") NONE
            (* raise TypeCheckingFailure ("label " ^ UTF8String.toString l ^ " not found in prod type") *)
            | (n1, t1)::cs => if UTF8String.semanticEqual n1 l then Success (i, t1) else go (i+1) cs
        in go 0 ctx
        end

      fun lookupLabel3 ( ctx : (Label * EVar *RType) list) (l : Label) : RType witherrsoption = 
        case ctx of 
            [] => genSingletonError l ("??????`" ^ UTF8String.toString l ^ "`?????????") NONE
            (* raise TypeCheckingFailure ("label " ^ UTF8String.toString l ^ " not found in sum type") *)
            | (n1, _, t1)::cs => if UTF8String.semanticEqual n1 l then Success t1 else lookupLabel3 cs l



    fun typeEquivList (ctx : context) (e : RExpr) (a : CType list) : CType witherrsoption =
        case a of
            [] => raise Fail ("INternal error: empty sum")
            | [t] => Success t
            | (x::y :: xs) => tryTypeUnify ctx e x y  >>= (fn (ctx) =>  typeEquivList ctx e (x :: xs)
            (* else genSingletonError (reconstructFromRExpr e) "???????????????"  (SOME 
                ("???????????????" ^  (PrettyPrint.show_typecheckingCType x) 
                ^ "\n???????????????" ^  (PrettyPrint.show_typecheckingCType x) *)
            )
            (* raise TypeCheckingFailure ("Type unify failed") *)
    
    structure Errors = TypeCheckingErrors
    
    fun withLocalBinder (ctx : context) (name : UTF8String.t) (tp : CType)
        (kont : context -> ('a * context) witherrsoption) : ('a * context) witherrsoption = 
         kont (addToCtxA (TermTypeJ([name], tp, JTLocalBinder, NONE)) ctx) >>= (fn (res, ctx) => 
            Success(res, modifyCtxDeleteBinding ctx [name])
        )
        
    
     fun addNewMetaVar (ctx : context)  (tp : CType)
     (errReporting : UTF8String.t) : (CExpr * context) witherrsoption =
     let 
                            (* val  allBindings = 
                            List.filter (fn (TermTypeJ(name, tp, jtp, _)) => 
                            (case jtp of 
                                JTPending => false
                                | _ => true
                            )) (getMapping ctx) *)
                            val metavarname = StructureName.metaVarName()
                            (* val resultingTerm = foldl (fn (TermTypeJ(name, tp, jtp, _), acc) => 
                                 CApp(acc, CVar(name, judgmentTypeToCVarType name (* TODO : why do we need canonical names? *) 
                                                jtp), CTypeAnnNotAvailable) (* Do we really need it ? *)
                                ) (CMetaVar metavarname) allBindings *)
                            (* val metaType = foldr (fn (TermTypeJ(name, tp, _, _), acc) => 
                                let val tempName = UTF8String.fromString ("??????????????????" ^ Int.toString (UID.next()) ^ "??????")
                                in 
                                    CPiType(tp, SOME(tempName), substTypeInCExpr (CVar([tempName], CVTBinder)) name acc)
                                end
                            ) (tt) allBindings *)
                            val newCtx = addToCtxA (TermTypeJ(metavarname, tp, JTMetaVarPendingResolve errReporting , NONE)) ctx
                            in
                                Success(CMetaVar(metavarname), newCtx)
                            end



    fun configureAndTypeCheckSignature
    (topLevelStructureName : StructureName.t)
    (
        getTypeCheckedAST:  (FileResourceURI.t * StructureName.t) -> TypeCheckingAST.CSignature witherrsoption
    )
    :  RSignature -> CSignature witherrsoption =
    let
            fun checkConstructorType(nameOfCons : UTF8String.t) ( ctx : context) (t : RType) : ((CType * cconstructorinfo) * context) witherrsoption = 
            let val typeInfoWeo : (CType * context) witherrsoption = 
                case t of 
                    RPiType(t1, evop, t2, p, soi) => 
                    checkExprOptionIsType ctx t1 ((case evop of SOME x => RVar([x]) | NONE => t)) >>= (fn (ct1, ctx) => 
                            (case evop of NONE => (fn k => k ctx)
                                | SOME(ev) => withLocalBinder ctx ev ct1
                            ) (fn ctx => 
                                        checkType ctx
                                            t2 
                                            (CUniverse) 
                            >>= (fn (ct2, ctx) => 
                                Success(CPiType(ct1, evop, ct2, p), ctx)
                            ))
                        )
                   
                    | _ => checkType ctx t (CUniverse) >>= (fn (t, ctx) => Success(t, ctx))

                fun countOccurrencesOfElementConstructor(foundTypeConstructorName: StructureName.t) = 
                List.length (List.filter (fn (TermTypeJ(name, tp, jt, originalName)) => 
                    case jt of 
                        JTConstructor(CConsInfoElementConstructor(tcname, _)) => StructureName.semanticEqual tcname foundTypeConstructorName
                        | _ => false (* TODO: Fix the case of open *)
                )    (getMapping ctx))
                 

                fun checkScopeAndIndexAgainstFoundTypeConstructor
                (errReporting : RExpr)
                (foundTypeConstructorName : StructureName.t) : cconstructorinfo witherrsoption =
                    if StructureName.semanticEqual (getCurSName ctx)  (StructureName.getDeclaringScope foundTypeConstructorName)
                    then Success(CConsInfoElementConstructor(foundTypeConstructorName, 
                            countOccurrencesOfElementConstructor(foundTypeConstructorName) + 1))
                    else Errors.elementConstructorScopeError  errReporting ctx



                (* only trace one level deep*)
                fun traceVarOnly(errReporting : RExpr) (cexpr : CExpr) =  case cexpr of
                    CUniverse => Success(CConsInfoTypeConstructor)
                    | CVar(v, vinfo) => (case vinfo of 
                        CVTConstructor (name, CConsInfoTypeConstructor) =>  
                            (checkScopeAndIndexAgainstFoundTypeConstructor errReporting name)
                        | CVTDefinition (v') => traceVarOnly errReporting v'
                        | _ => Errors.notATypeConstructor errReporting ctx
                    )
                    | _ => Errors.notATypeConstructor errReporting ctx

                fun analyzeVariable(v : StructureName.t) = 
                let val errReporting = RVar(v)
                in
                        lookupCtx ctx v  >>= (fn lookedUpJ => 
                                        case  lookedUpJ of
                                            (cname, tp, jinfo) => (case jinfo
                                            of JTConstructor (CConsInfoTypeConstructor) => 
                            (checkScopeAndIndexAgainstFoundTypeConstructor errReporting cname)
                                                | JTDefinition (v') => (traceVarOnly (RVar(v)) v')
                                                | _ => Errors.notATypeConstructor (RVar(v)) ctx
                                            )
                        )
                end

                fun getConsInfo (isCanonical : bool) (t : RType) = 
                if isCanonical
                then
                (case t of 
                    (* RFunc(t1, t2, soi) => getConsInfo true t2 *)
                     RPiType(t1, b, t2,p, soi) => getConsInfo true t2
                    | _ => getConsInfo false t)
                else
                (case t of 
                    RApp(t1, t2,p, soi) => getConsInfo false t1
                    | RVar(s) => analyzeVariable(s)
                    | RUniverse(s) => Success(CConsInfoTypeConstructor)
                    | _ => Errors.notATypeConstructor t ctx
                    )
                val consInfo = getConsInfo true t
            in 
            typeInfoWeo  >>= (fn (tpInfo, ctx) => 
                consInfo >>= (fn consInfo => 
                    Success((tpInfo, consInfo), ctx)
                )
            )
            end
            
            and checkExprIsType (ctx : context) (e : RType) : (CType * context) witherrsoption = 
                checkType ctx e CUniverse 
            and checkSpineIsType (ctx : context) (e : (UTF8String.t * RType) list ) : ((UTF8String.t  * CType) list * context) witherrsoption = 
                case e of 
                    [] => Success([], ctx)
                    | ((l,t) :: es) => 
                        checkExprIsType ctx t >>= (fn (ce, ctx) => 
                            withLocalBinder ctx l ce (fn ctx => 
                                checkSpineIsType ctx es >>= (fn (cel, ctx) => 
                                    Success((l,ce)::cel, ctx)
                                )
                            )
                        )

            and checkExprOptionIsType (ctx : context) (e : RType option) (errReporting : RType) : (CType * context) witherrsoption = 
                (case e of 
                        SOME t1 => checkType ctx t1 (CUniverse) 
                        | NONE => addNewMetaVar ctx CUniverse (reconstructFromRExpr errReporting) >>= (fn (metavar, ctx) => 
                                Success((metavar, ctx))
                            )
                        )
            

            (* synthesizeType and instantiating implicit arguments *)
            and synthesizeType (ctx : context)(e : RExpr) : ((CExpr * CType) * context) witherrsoption =
            let fun insertMetaVarRec (res as ((syne, synt), ctx) : ((CExpr * CType) * context)) = 
                    weakHeadNormalizeType e ctx synt >>= (fn nsynt => 
                    case nsynt of 
                        CPiType(t1, evop, t2, Implicit)=>
                            addNewMetaVar ctx t1 (reconstructFromRExpr e) >>= (fn (metaVarE, ctx) => 
                                insertMetaVarRec((CApp(syne, metaVarE, CTypeAnn nsynt), 
                                    (case evop of NONE => t2 | SOME tv => substTypeInCExpr metaVarE [tv] t2)), ctx)
                            )
                        | _ => Success(res)
                    )
            in
            synthesizeTypeNoInstMeta ctx e >>= (fn (res as ((syne, synt), ctx)) => 
                insertMetaVarRec ((syne, synt), ctx)
            )
            end


            (* synthesizeType without instantiating implicit arguments *)
            and synthesizeTypeNoInstMeta (ctx : context)(e : RExpr) : ((CExpr * CType) * context) witherrsoption =
            (
                let val _ = if DEBUG then print ("synthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e ^ "\n") else ()
                val originalExpr = e
                val res = (case e of
                    RVar v => lookupCtx ctx v >>= (fn (canonicalName, tp, jtp) =>
                    case jtp of 
                        JTPending => Errors.genericError e ctx "??????????????????"
                        | _ => Success((CVar(canonicalName, judgmentTypeToCVarType canonicalName jtp), tp), ctx)
                    )
                    | RUnitExpr(soi) => Success ((CUnitExpr, CUnitType), ctx)
                    | RProj(e, l, soi) => synthesizeType ctx e >>= (fn ((ce, tt), ctx) =>  
                                    ( weakHeadNormalizeType e ctx tt) >>= (fn ntt => case 
                                    ( ntt) of 
                            (CProd ls) => 
                                (lookupLabel ls l) >>= (fn (idx, _) => 
                                    let fun go curIdx acc = 
                                            (case acc of 
                                                [] => raise Fail "tcp338 lookup should not return an invalid index"
                                                | ((l1, t1)::tl) => 
                                                    if idx = curIdx
                                                    then t1
                                                    else 
                                                        (case substTypeInCExpr (CProj(ce, l1, curIdx, CTypeAnnNotAvailable)) [l1] (CProd tl)
                                                            of CProd tl' => go (curIdx+1) (tl')
                                                            | _ => raise Fail "tcp344: subst should also return prod")
                                            )
                                        val targetType = go 0 ls
                                    in 
                                        Success((CProj(ce, l, idx, CTypeAnnNotAvailable), targetType), ctx)
                                    end
                                )

                            | _ => Errors.attemptToProjectNonProd e (tt) ctx
                    ))
                    | RLazyProj(e, l, soi) => synthesizeType ctx e >>= (fn ((ce, tt), ctx) =>  
                    weakHeadNormalizeType e ctx tt >>= (fn ntt => case ntt of 
                            ( CLazyProd ls) => fmap (fn (idx,x) => ((CLazyProj(ce, l, CTypeAnn(CLazyProd ls)),x), ctx)) (lookupLabel ls l)
                            | _ => Errors.attemptToProjectNonLazyProd e (tt) ctx
                    ))
                    | RIfThenElse(e, tcase, fcase, soi)=> checkType ctx e (CBuiltinType BIBool) >>= (fn (ce, ctx) => 
                        (synthesizeType ctx tcase >>= (fn ((ctcase, rttp), ctx) => 
                                checkType ctx fcase (rttp) >>= (fn (cfcase, ctx) => 
                                    Success((CIfThenElse(ce, ctcase, cfcase), rttp), ctx)
                                )
                            ) 
                        ) <|> (fn () => (* alternative: either branch may synthesize *)
                                    (synthesizeType ctx fcase >>= (fn ((cfcase, rttp), ctx) => 
                                                checkType ctx tcase (rttp) >>= (fn (ctcase, ctx) => 
                                                    Success((CIfThenElse(ce, ctcase, cfcase), rttp), ctx)
                                                )
                                            ) 
                                        )
                        )
                    )
                    | RCase(e,cases, soi) => (synthesizeType ctx e) >>= (fn ((ce, t), ctx) => 
                        weakHeadNormalizeType e ctx t >>= (fn caseObjectTypeNormalized => 
                            let 
                                val checkedPatternsAndCases = (foldMapCtx ctx (fn ((pat, e), ctx) => 
                                    checkPattern ctx pat caseObjectTypeNormalized >>= (fn (cpat, newCtx) => 
                                        synthesizeType newCtx e >>= (fn ((synE, synT), ctx) => 
                                            Success((cpat, (synE, synT)), ctx)
                                        )
                                    ))
                                cases)
                            in checkedPatternsAndCases >>= (fn (l, ctx) => 
                                typeEquivList ctx originalExpr (map (fn (pat, (synE, synT)) => synT) l) >>= (fn returnType => 
                                    Success ((CCase ((CTypeAnn(caseObjectTypeNormalized), ce), 
                                        (map (fn (pat, (synE, synT)) => (pat, synE)) l)
                                    , CTypeAnn(returnType)), returnType), ctx)
                                )
                            )
                            end
                            )
                        )
                    | RLamWithType (t, ev, e, soi) => 
                    checkExprIsType ctx t >>= (fn (absTp, ctx) => 
                        withLocalBinder ctx ev absTp (fn ctx => 
                            synthesizeType ctx e >>= (fn ((bodyExpr, returnType), ctx) =>
                                let val funType = if List.exists (fn x => StructureName.semanticEqual [ev] x) (freeTCVar returnType)
                                    then CPiType(absTp, SOME ev, returnType, Explicit)
                                    else CPiType(absTp, NONE, returnType, Explicit)
                                in 
                                Success((CLam(ev, bodyExpr, CTypeAnn(funType)), funType), ctx)
                                end
                            )
                        )
                    )
                    | RApp (e1, e2, pe, soi) => 
                        (case pe of Explicit => synthesizeType ctx e1 
                                    | Implicit => synthesizeTypeNoInstMeta ctx e1)
                        >>= (fn ((ce1, synt), ctx) => 
                        weakHeadNormalizeType e1 ctx synt >>= (fn nsynt => 
                            case synt 
                                of (CPiType (t1, evop, t2, pt)) => 
                                    if pe = pt 
                                    then 
                                    ( 
                                            checkType ctx e2 (t1) >>= (fn (checkedArg, ctx) => 
                                        (
                                                Success ((CApp(ce1, checkedArg, CTypeAnn(synt)), (
                                        case evop of 
                                        NONE => t2
                                        | SOME ev => substTypeInCExpr checkedArg ([ev]) t2
                                        )), ctx)
                                            )
                                        )
                                    )
                                    else Errors.genericError e ctx ("??????/???????????????????????????"
                                        ^ " \n synthesized (e1) = " ^ PrettyPrint.show_typecheckingCExpr synt
                                        ^ " \n original (e1 e2) = " ^ PrettyPrint.show_typecheckingRType originalExpr) 
                                    (* raise Fail ("tcp359: plicity of (possible) pi type should match requested plicity"
                                        ^ " \n synthesized (e1) = " ^ PrettyPrint.show_typecheckingCExpr synt
                                        ^ " \n original (e1 e2) = " ^ PrettyPrint.show_typecheckingRType originalExpr) *)

                                | _ => Errors.attemptToApplyNonFunction e (synt) ctx
                            )
                        )
                    (* | RTAbs (tv, e2, soi) =>   synthesizeType 
                                (addToCtxA (TermTypeJ([tv], CUniverse, JTLocalBinder, NONE)) ctx )
                        e2 >>= (fn ((ce2, bodyType), ctx) => 
                    Success ((CTAbs(tv, ce2, CTypeAnn(CForall (tv, bodyType))), CForall (tv, bodyType)), ctx) ) *)
                    | RTApp (e2, t, soi) => synthesizeType ctx e2 >>= (fn ((ce2, st), ctx) => 
                                weakHeadNormalizeType e2 ctx st >>= (fn nst =>
                        case nst of
                            CForall (tv, tb) => 
                                checkExprIsType ctx t >>= (fn (nt, ctx) => 
                                    (* important need to normalized before subst *)
                                    (weakHeadNormalizeType t ctx nt >>= (fn nt => 
                                        Success((CTApp(ce2, nt, CTypeAnn(CForall(tv, tb))), (substTypeInCExpr nt [tv] (tb))), ctx)
                                ))
                            )
                            | _ => Errors.attemptToApplyNonUniversal e (st) ctx
                             )
                        )
                    (* | ROpen (e1, (tv, ev, e2), soi) => synthesizeType ctx e1 >>= (fn ((ce1, synt), ctx) => 
                    weakHeadNormalizeType e1 ctx synt >>= (fn nsynt => case nsynt  of
                                ( CExists (tv', tb)) => 
                        synthesizeType (addToCtxA (TermTypeJ([ev], 
                        substTypeInCExpr (CVar([tv], CVTBinder)) [tv'] (tb), JTLocalBinder, NONE)) ctx) e2 >>= (fn ((ce2, synthesizedType), ctx) =>
                        if List.exists (fn t => t = [tv]) (freeTCVar (synthesizedType))
                            then Errors.openTypeCannotExitScope e synthesizedType ctx
                            else Success((COpen((CTypeAnn(CExists(tv', tb)), ce1), (tv, ev, ce2), CTypeAnn(synthesizedType)), synthesizedType), ctx)
                        )
                            | _ => Errors.attemptToOpenNonExistentialTypes e ( synt) ctx)
                    ) *)
                    | RUnfold (e2, soi) => synthesizeType ctx e2 >>= (fn ((ce2, synt), ctx) => 
                    weakHeadNormalizeType e2 ctx synt >>= (fn nsynt => case nsynt  of
                        ( CRho (tv, tb)) => Success ((CUnfold(ce2, CTypeAnn(CRho(tv, tb))),  (substTypeInCExpr (CRho (tv, tb)) [tv] (tb))), ctx)
                        | _ => Errors.attemptToUnfoldNonRecursiveTypes e ( synt) ctx
                        ))
                    | RStringLiteral(l, soi) => Success((CStringLiteral l, CBuiltinType(BIString)), ctx)
                    | RIntConstant(i, soi) => Success((CIntConstant i, CBuiltinType(BIInt)), ctx)
                    | RRealConstant (r, soi) => Success((CRealConstant  r, CBuiltinType(BIReal)), ctx)
                    | RBoolConstant (b, soi) => Success((CBoolConstant b, CBuiltinType(BIBool)), ctx)
                    

                    | RLetIn(decls, e, soi) => (case ctx of 
                        Context(curName, curVis, bindings) => 
                        typeCheckSignature (Context(curName@StructureName.localName(), curVis, bindings)) decls [] >>= 
                        (fn (csig, Context(localName, _, newBindings) ) =>
                                synthesizeType (Context(localName,curVis, newBindings)) e >>= (fn ((ce, synthesizedType), Context(localName, curVis, bindings)) =>
                                        Success ((CLetIn(csig, ce, CTypeAnn(synthesizedType)), synthesizedType), Context(curName,curVis, bindings)) (* restore name when exiting *)
                                )
                        )
                    )
                    | RBuiltinFunc(f, s) => Success((CBuiltinFunc(f), (BuiltinFunctions.typeOf f)), ctx)
                    | RSeqComp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn ((ce1, t1), ctx) => 
                        synthesizeType ctx e2 >>= (fn ((ce2, t2), ctx) => 
                            Success((CSeqComp(ce1, ce2, CTypeAnn(t1), CTypeAnn(t2)), t2), ctx)
                        ))
                    (* types *)
                    | RUnitType(s) => Success((CUnitType, CUniverse), ctx)
                    | RNullType(s) => Success((CNullType, CUniverse), ctx)
                    | RBuiltinType(f, s) => Success((CBuiltinType(f), CUniverse), ctx)
                    | RUniverse(s) => Success((CUniverse, CUniverse), ctx) (* TODO: maybe universe levels? *)
                    | RPiType(t1, evoption, t2,p,  soi) => 
                    checkExprOptionIsType ctx t1 (case evoption of SOME x => RVar([x]) | NONE => e
                    ) >>= (fn (ct1, ctx) => 
                            let val kf = fn ctx => 
                            synthesizeType (ctx) t2 >>= (fn ((ct2, synT), ctx) => 
                                    tryTypeUnify ctx t2 synT CUniverse >>= 
                                        (fn ctx => Success((CPiType(ct1, evoption, ct2, p), CUniverse), ctx))
                            )
                            in 
                                case evoption of  NONE => Success(ctx) >>= kf 
                                | SOME(n) => withLocalBinder ctx n ct1 kf
                            end
                        )
                    | RSigmaType(t1, evoption, t2, soi) =>
                        checkType ctx t1 CUniverse >>= (fn (ct1, ctx) => 
                            (case evoption of  NONE => (fn f => f ctx) | SOME(n) => withLocalBinder ctx n ct1)
                            (fn ctx => 
                                synthesizeType (ctx) t2 >>= (fn ((ct2, synT), ctx) => 
                                        tryTypeUnify ctx t2 synT CUniverse >>= 
                                            (fn ctx => Success((CSigmaType(ct1, evoption, ct2), CUniverse), ctx))
                                    )
                                )
                            )
                    | RProd(ltsl, sepl) => 
                        checkSpineIsType ctx (map (fn (l, t, soi) => (l, t)) ltsl)
                         >>= (fn (l, ctx) => Success((CProd l, CUniverse), ctx))
                    | RLazyProd  (ltsl, sepl) => 
                         (foldMapCtx ctx (fn ((l, t, soi), ctx) => 
                            checkType ctx t CUniverse  >>= (fn (ct, ctx) => 
                            Success ((l, ct), ctx)
                            )
                        ) ltsl) >>= (fn (l, ctx) => Success((CLazyProd l, CUniverse), ctx))
                    | RSum(ltsl, sepl) => 
                         (foldMapCtx ctx (fn ((l, t, soi), ctx) => 
                            checkType ctx t CUniverse  >>= (fn (ct, ctx) => 
                            Success ((l, ct), ctx)
                            )
                        ) ltsl) >>= (fn (l, ctx) => Success((CSum l, CUniverse), ctx))
                    (* | RFunc(t1, t2, soi) => checkType ctx t1 CUniverse >>= (fn ct1 => 
                        checkType ctx t2 CUniverse >>= (fn ct2 => 
                            Success(CFunc(ct1, ct2), CUniverse)
                        )
                    ) *)
                    | RTypeInst(t1, t2, soi) => checkType ctx t1 CUniverse >>= (fn (ct1, ctx) => 
                        checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                            Success((CTypeInst(ct1, ct2), CUniverse), ctx)
                        )
                    )
                    | RForall(tv, t2, soi) => 
                        withLocalBinder ctx tv CUniverse (fn ctx => 
                        checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                Success((CForall(tv, ct2), CUniverse), ctx)
                            )
                        )
                    | RExists(tv, t2, soi) => 
                        withLocalBinder ctx tv CUniverse (fn ctx => 
                        checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                Success((CExists(tv, ct2), CUniverse), ctx)
                            )
                        )
                    | RRho(tv, t2, soi) => 
                        withLocalBinder ctx tv CUniverse (fn ctx => 
                        checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                Success((CRho(tv, ct2), CUniverse), ctx)
                            )
                        )
                    (* | Fix (ev, e)=> Fix (ev, substTypeInExpr tS x e) *)
                    | _ => Errors.expressionDoesNotSupportTypeSynthesis e ctx
                )

                val _ = if DEBUG then print ( "synthesize got result " ^
                PrettyPrint.show_static_error res (fn res => PrettyPrint.show_typecheckingCType (#2 (#1 res)))^
                " whensynthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e ^ "\n") else ()
                val res' = Errors.enrichErrWhenSynthesizing e res
                in res'
                end )
                (* handle TypeCheckingFailure s => 
                    raise TypeCheckingFailure (s ^ "\n when synthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e 
                    ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx) *)

            and checkType (ctx : context) (e : RExpr) (ttUnnorm: CType) (* tt target type *) : (CExpr * context) witherrsoption =
                     weakHeadNormalizeType e ctx ttUnnorm >>= (fn ttNorm =>
                case ttNorm of 
                    CPiType(t1, tvop, t2, Implicit) => (
                                case e of RLam(ev, eb, Implicit, soi) => checkTypeNoInsertLam ctx e ttNorm
                                | _ => ( (* insert lambda here *)
                                let 
                                in
                                    (case tvop of 
                                            NONE => 
                                                checkType ctx e t2 >>= (fn (ce, ctx) => 
                                                Success(CLam(StructureName.binderName(), ce, CTypeAnn ttNorm), ctx))
                                            | SOME tv => withLocalBinder ctx tv t1
                                            (fn ctx => 
                                                checkType ctx e t2 >>= (fn (ce, ctx) => 
                                                Success(CLam(tv, ce, CTypeAnn ttNorm), ctx))
                                            )
                                    )
                                end
                            )
                     )
                    | _ => checkTypeNoInsertLam ctx e ttNorm
                )

            and checkTypeNoInsertLam (ctx : context) (e : RExpr) (ttUnnorm: CType) (* tt target type *) : (CExpr * context) witherrsoption =
                     weakHeadNormalizeType e ctx ttUnnorm >>= (fn ttNorm =>
                (let 
                    val tt = ttNorm
                    val _ = if DEBUG then  print(  "DEBUG: checking the expr " ^ PrettyPrint.show_typecheckingRExpr e ^ 
                        " against type " ^ PrettyPrint.show_typecheckingCType tt ^
                        (* " in context " ^ PrettyPrint.show_typecheckingpassctx ctx ^ *)
                        "\n") else ()
                    val originalExpr = e
                    val res = (
                        case e of
                        RVar v => 
                            (synthesizeType ctx e) >>= (fn ((synthExpr, synthType), ctx) =>
                            tryTypeUnify ctx e (synthType) tt  >>= (fn ctx => Success (synthExpr, ctx))
                            )
                    
                        | RUnitExpr(soi) => tryTypeUnify ctx e CUnitType  tt >>= (fn ctx => Success(CUnitExpr, ctx))
                        | RTuple (l, soi) => (case tt of 
                            CProd ls => if List.length l <> List.length ls
                                        then Errors.prodTupleLengthMismatch e (tt) ctx
                                        else 
                                        let fun go (l,ls) ctx = case (l, ls) of 
                                                ([],[]) => (
                                                        (* DebugPrint.p "GO : DONE"; *)
                                                    Success([], ctx)
                                                )
                                                | ((elem::es), ((label,tp) :: tpl)) => 
                                                (
                                                    (* DebugPrint.p ("GOING..." ^ Int.toString (length l) ^ " ; " ^ Int.toString (length ls)); *)

                                                    checkType ctx elem tp >>= (fn (ce, ctx) => 
                                                        (
                                                            (* DebugPrint.p ("checked..." ); *)
                                                            go (es, substTypeInSpine ce [label] tpl) ctx >>= (fn (ces, ctx) => 
                                                                Success((ce::ces), ctx)
                                                            )
                                                        )
                                                    )
                                                )
                                                | _ => raise Fail "tcp629: should be the same length"
                                        in go (l, ls) ctx >>= (fn (ce, ctx) => Success(CTuple(ce, CTypeAnnNotAvailable), ctx))
                                        end
                            | _ => Errors.expectedProdType e (tt) ctx
                            )
                        | RLazyTuple (l, soi) => (case tt of 
                            CLazyProd ls => if List.length l <> List.length ls
                                        then Errors.lazyProdTupleLengthMismatch e (tt) ctx
                                        else foldMapCtx ctx (fn ((elem, (label,tp)), ctx) => 
                                            checkType ctx elem tp
                                        ) (ListPair.zipEq (l, ls)) >>= (fn (checkedElems, ctx) => 
                                        Success(CLazyTuple ( checkedElems, CTypeAnn((CLazyProd ls))), ctx))
                            | _ => Errors.expectedLazyProdType e (tt) ctx
                            )
                        | RProj(e, l, soi) =>
                        synthesizeType ctx (RProj(e, l, soi)) >>= (fn ((cproj, synt), ctx) => case (cproj, synt) of
                            (CProj(ce, l, idx, prodType), synthType) => tryTypeUnify ctx originalExpr synthType tt >>= (fn ctx => 
                            Success(CProj(ce, l, idx, prodType), ctx))
                            | _ => raise Fail "tcp229")
                        | RLazyProj(e, l, soi) =>
                        synthesizeType ctx (RLazyProj(e, l, soi)) >>= (fn ((cproj, synt), ctx) => case (cproj, synt) of
                            (CLazyProj(ce, l, lazyProdType), synthType) => tryTypeUnify ctx originalExpr synthType tt >>= (fn ctx => 
                            Success(CLazyProj(ce, l, lazyProdType), ctx))
                            | _ => raise Fail "tcp229")

                        | RInj (l, e, soi) => (case tt of
                            CSum ls => (lookupLabel ls l) >>= (fn (idx,lookedupType) => 
                                    checkType ctx e lookedupType >>= (fn (checkedExpr, ctx) => 
                                        Success(CInj(l, checkedExpr, CTypeAnn((CSum ls))), ctx)
                                    ))
                            | _ => Errors.expectedSumType originalExpr (tt) ctx
                        )
                        | RIfThenElse(e, tcase, fcase, soi) => (checkType ctx e (CBuiltinType(BIBool))  >>= (fn (ce, ctx) => 
                            checkType ctx tcase tt >>= (fn (ctcase, ctx) => 
                                checkType ctx fcase tt >>= (fn (cfcase, ctx) => 
                                    Success(CIfThenElse(ce, ctcase, cfcase), ctx)
                                )
                            )
                        ))
                        | RCase(e,cases, soi) => (synthesizeType ctx e) >>= (fn ((ce, synt), ctx) => 
                            weakHeadNormalizeType e ctx synt >>= (fn caseTpNormalized => 
                                (foldMapCtx ctx (fn ((pat, e), ctx) => 
                                    checkPattern ctx pat caseTpNormalized >>= (fn (cpat, newCtx) => 
                                            checkType newCtx e tt >>= (fn (checkedCase, ctx) => 
                                                Success((cpat, checkedCase), ctx)
                                            )
                                        )
                                    ) cases
                                    ) >>= (fn (checkedCases, ctx)
                                        => Success(CCase((CTypeAnn(caseTpNormalized), ce), checkedCases , CTypeAnn((tt))), ctx))
                            ))
                                (* | _ => Errors.attemptToCaseNonSum originalExpr (#2 synt) ctx) *)
                        | RLam(ev, eb,pe, soi) => 
                        weakHeadNormalizeType originalExpr ctx tt >>= (fn ntt => 
                            (case tt of
                                CPiType(t1, tevop, t2, pt) => 
                                    if pt <> pe 
                                    then raise Fail "tcp638: should have inserted implict lams"
                                    else
                                    withLocalBinder ctx ev t1 (fn ctx => 
                                        checkType 
                                            ctx
                                            eb 
                                            (case tevop of NONE => t2 | SOME tev => if UTF8String.semanticEqual tev ev then t2 else 
                                                substTypeInCExpr (CVar([ev], CVTBinder)) ([tev]) t2
                                            )
                                        >>= (fn (checkedExpr, ctx) => Success(CLam(ev, checkedExpr, CTypeAnn(tt)), ctx))
                                    )
                                | _ => Errors.expectedFunctionType e (tt) ctx
                                )
                        )
                        | RLamWithType (t, ev, eb, soi) => 
                        weakHeadNormalizeType originalExpr ctx tt >>= (fn ntt => 
                        (case ntt of
                            CPiType(t1, tevop, t2, Explicit) => (
                                checkExprIsType ctx t >>= (fn (t', ctx) => 
                                    tryTypeUnify ctx e t' t1 >>=
                                    (fn ctx => 
                                        withLocalBinder ctx ev t1 (fn ctx => 
                                            checkType 
                                                ctx
                                                eb 
                                                (case tevop of NONE => t2 | SOME tev => if UTF8String.semanticEqual tev ev then t2 else 
                                                    substTypeInCExpr (CVar([ev], CVTBinder)) ([tev]) t2
                                                )
                                            >>= (fn (checkedBody, ctx) => 
                                                Success(CLam(ev, checkedBody , CTypeAnn(tt)), ctx))
                                        )
                                    )
                                )
                                )
                            | _ => Errors.expectedFunctionType e  (tt) ctx
                            )
                        )
                        | RSeqComp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn ((ce1, t1), ctx) => 
                            checkType ctx e2 tt >>= (fn (ce2, ctx) => 
                                Success(CSeqComp(ce1, ce2, CTypeAnn(t1), CTypeAnn(tt)), ctx)
                            ))
                        | RApp (e1, e2, pe, soi) => 
                            (case pe of Explicit => 
                            synthesizeType ctx e1 
                            | Implicit => synthesizeTypeNoInstMeta ctx e1)
                            >>= (fn ((ce1, synt), ctx) => 
                            weakHeadNormalizeType e1 ctx synt >>= (fn nsynt => 
                                case synt 
                                    of (CPiType (t1, evop, t2, pt)) => 
                                        if pt = pe then
                                        ( 
                                                checkType ctx e2 (t1) >>= (fn (checkedArg, ctx) => 
                                        tryTypeUnify ctx e (
                                            case evop of 
                                            NONE => t2
                                            | SOME ev => substTypeInCExpr checkedArg ([ev]) t2
                                            ) tt >>= (fn ctx => 
                                                    Success (CApp(ce1, checkedArg, CTypeAnn(synt)), ctx)
                                                )
                                            )
                                        ) else 
                                        raise Fail "tcp638: plicity of (possible) pi type should match requested plicity"
                                    | _ => Errors.attemptToApplyNonFunction e (synt) ctx
                                )
                            )
                        (* | RTAbs (tv, e2, soi) => (case tt of
                            CForall (tv', tb) => 
                                    checkType 
                                    (addToCtxA (TermTypeJ([tv], CUniverse, JTLocalBinder, NONE)) ctx )
                                    e2 (substTypeInCExpr (CVar([tv], CVTBinder)) [tv'] tb) >>= (fn (ce2, ctx) => 
                                                Success(CTAbs (tv, ce2, CTypeAnn(tt)), ctx)
                                    )
                            | _ => Errors.expectedUniversalType e (tt) ctx
                        ) *)
                        | RTApp (e2, t, soi) => synthesizeType ctx e2  >>= (fn ((ce2, synt), ctx) => 
                        weakHeadNormalizeType e2 ctx synt >>= (fn synt => 
                            case synt of
                                (CForall (tv, tb)) => (
                                    checkExprIsType ctx t >>= (fn (ctapp, ctx) => 
                                        (* need to normalize type! important! *)
                                        (weakHeadNormalizeType t ctx ctapp >>= (fn nt => 
                                            tryTypeUnify ctx e (tt) (substTypeInCExpr ctapp [tv] ( tb))
                                        )) >>= (fn ctx => Success(CTApp(ce2, ctapp, CTypeAnn(CForall(tv, tb))), ctx)))
                                    )
                                | _ => Errors.attemptToApplyNonUniversal e (synt) ctx
                                )
                            )
                        | RPack (e1, e2, soi) => (case tt of
                            CSigmaType (t1, tvop, t2) => 
                                checkType ctx e1 t1 >>= (fn (ce1, ctx) =>
                                    checkType ctx e2 
                                    (case tvop of NONE => t2 | SOME tv => 
                                                    substTypeInCExpr ce1 ([tv]) t2
                                                )
                                    >>= (fn (ce2, ctx) => Success(CPack(ce1, ce2, CTypeAnn(tt)), ctx))
                                    )
                            | _ => Errors.expectedExistentialType e (tt) ctx
                        )
                        (* | ROpen (e1, (tv, ev, e2), soi) => synthesizeType ctx e1 >>= (fn ((ce1, synt), ctx) => 
                        weakHeadNormalizeType e1 ctx synt >>= (fn nsynt => case nsynt of
                            (CExists (tv', tb)) => 
                            checkType (addToCtxA (TermTypeJ([ev], substTypeInCExpr (CVar([tv], CVTBinder)) [tv'] ( tb), JTLocalBinder, NONE)) ctx) e2 tt
                            >>= (fn (ce2, ctx) => 
                            Success(COpen((CTypeAnn(CExists (tv', tb)), ce1), (tv, ev, ce2), CTypeAnn(tt)), ctx)
                            )
                            | _ => Errors.attemptToOpenNonExistentialTypes e (synt) ctx
                        )
                        ) *)
                        | RFold (e2, soi) => (case tt
                            of 
                            CRho (tv ,tb) => 
                            checkType ctx e2 (substTypeInCExpr (CRho(tv, tb)) [tv] tb)
                            >>= (fn (ce2, ctx) => Success (CFold(ce2, CTypeAnn(tt)), ctx))
                            | _ => Errors.expectedRecursiveType e (tt) ctx
                                )
                        | RUnfold (e2,soi) => synthesizeType ctx e2  >>= (fn ((ce2, synt), ctx) => 
                        weakHeadNormalizeType e2 ctx synt >>= (fn nsynt => case nsynt of
                            ( CRho (tv, tb)) =>(
                                tryTypeUnify ctx e ((substTypeInCExpr (CRho (tv,  tb)) [tv] ( tb))) tt >>= (fn ctx =>
                                Success(CUnfold(ce2, CTypeAnn(CRho(tv,tb))), ctx)))
                            | _ => Errors.attemptToUnfoldNonRecursiveTypes e (synt) ctx
                            ))
                        | RFix (ev, e, soi)=> 
                            withLocalBinder ctx ev tt (fn ctx => 
                                checkType ctx e tt
                                                    >>= (fn (ce, ctx) => Success(CFix(ev,ce, CTypeAnn(tt)), ctx))
                            )
                        | RStringLiteral (s, soi) => (tryTypeUnify ctx e (CBuiltinType(BIString)) (tt) >>= (fn (ctx) => Success (CStringLiteral s, ctx)))
                        | RIntConstant (i, soi) => (tryTypeUnify ctx e (CBuiltinType(BIInt)) tt >>= (fn ctx => Success ( CIntConstant i, ctx)))
                        | RRealConstant (r, soi) => (tryTypeUnify ctx e (CBuiltinType(BIReal)) tt >>= (fn ctx => Success (CRealConstant r, ctx)))
                        | RBoolConstant (r, soi) => (tryTypeUnify ctx e (CBuiltinType(BIBool)) tt >>= (fn ctx => Success (CBoolConstant r, ctx)))
                        | RFfiCCall (e1, e2, soi) => (
                            case e1 of
                                RStringLiteral (cfuncName, soi) => 
                                    let fun elaborateArguments  (args : RExpr list ) : (CExpr * context) witherrsoption = 
                                        foldMapCtx ctx (fn (a, ctx) =>  (synthesizeType ctx a)) args >>= 
                                        (fn (al, ctx) => 
                                            Success(CFfiCCall(cfuncName, map (#1) al), ctx)
                                        )
                                    in
                                                (case e2 of 
                                                    RVar v => (Success ([RVar v])) >>= elaborateArguments
                                                    | RTuple (l, soi) => (collectAll (map (fn arg => case arg of 
                                                        RVar v => Success (RVar v)
                                                        | _ => Errors.ccallArgumentsMustBeImmediate arg ctx
                                                        (* raise TypeCheckingFailure "ccall arguments must be immediate values" *)
                                                        ) l)) >>= elaborateArguments
                                                    | RUnitExpr(soi) => elaborateArguments []
                                                    | e => raise Fail ("tcp439 : " ^ PrettyPrint.show_typecheckingRExpr e)
                                                )
                                    end
                                | _ => Errors.firstArgumentOfCCallMustBeStringLiteral e1 ctx
                                (* raise TypeCheckingFailure "First argument of the ccall must be string literal" *)

                        )
                        | RLetIn(decls, e, soi) => (case ctx of 
                            Context(curName, curVis, bindings) => 
                        typeCheckSignature (Context(curName@StructureName.localName(), curVis, bindings)) decls []
                        >>= (fn(csig, Context(localName, _, newBindings)) =>
                            (* assume the typeChecking is behaving properly, 
                            no conflicting things will be added to the signature *)
                            (* sub context will be determined by whether the signature is private or not ? *)
                                checkType (Context(localName,curVis, newBindings)) e tt >>= (fn (ce, Context(localName, curVis, bindings)) => 
                                            Success(CLetIn(csig, ce, CTypeAnn(tt)), Context(curName, curVis, bindings))
                                )
                            )
                        
                            )
                        | RBuiltinFunc(f, soi) => (tryTypeUnify ctx e ((BuiltinFunctions.typeOf f)) tt >>= (fn ctx => Success(CBuiltinFunc(f), ctx)))
                        (* types *)
                        | RUnitType(s) => tryTypeUnify ctx e CUniverse tt >>= (fn ctx => Success(CUnitType, ctx))
                        | RNullType(s) => tryTypeUnify ctx e CUniverse tt >>= (fn ctx => Success(CNullType , ctx))
                        | RBuiltinType(f, s) => tryTypeUnify ctx e CUniverse tt >>= (fn ctx => Success(CBuiltinType(f) , ctx))
                        | RUniverse(s) => tryTypeUnify ctx e CUniverse tt >>= (fn ctx => Success(CUniverse , ctx) (* TODO: maybe universe levels? *))
                        | RPiType(t1, evoption, t2,p, soi) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            checkExprOptionIsType ctx t1 ((case evoption of SOME x => RVar([x]) | NONE => e)) >>= (fn (ct1, ctx) => 
                                (case evoption of  NONE => (fn f => f ctx) | SOME(n) => withLocalBinder ctx n ct1)
                                (fn ctx =>
                                synthesizeType ctx t2 >>= (fn ((ct2, synT), ctx) => 
                                        tryTypeUnify ctx t2 synT CUniverse >>= (fn ctx => 
                                            (Success(CPiType(ct1, evoption, ct2,p), ctx)))
                                ))
                            ))
                        | RSigmaType(t1, evoption, t2, soi) =>
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            checkType ctx t1 CUniverse >>= (fn (ct1, ctx) => 
                                (case evoption of  NONE => (fn f => f ctx) | SOME(n) => withLocalBinder ctx n ct1)
                                (fn ctx => 
                                synthesizeType ctx t2 >>= (fn ((ct2, synT), ctx) => 
                                        tryTypeUnify ctx t2 synT CUniverse >>= 
                                            (fn ctx => Success(CSigmaType(ct1, evoption, ct2) , ctx))
                                    ))
                                ))
                        | RProd(ltsl, sepl) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            checkSpineIsType ctx (map (fn (l, t, soi) => (l, t)) ltsl)
                             >>= (fn (l, ctx) => Success(CProd l, ctx)))
                        | RLazyProd  (ltsl, sepl) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            (foldMapCtx ctx (fn ((l, t, soi), ctx) => 
                                checkType ctx t CUniverse  >>= (fn (ct, ctx) => 
                                Success ((l, ct), ctx)
                                )
                            ) ltsl) >>= (fn (l, ctx) => Success(CLazyProd l, ctx)))
                        | RSum(ltsl, sepl) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            (foldMapCtx ctx (fn ((l, t, soi), ctx) => 
                                checkType ctx t CUniverse  >>= (fn (ct, ctx) => 
                                Success ((l, ct), ctx)
                                )
                            ) ltsl) >>= (fn (l, ctx) => Success(CSum l, ctx)))
                        (* | RFunc(t1, t2, soi) => 
                            tryTypeUnify ctx e CUniverse tt >> (
                        checkType ctx t1 CUniverse >>= (fn ct1 => 
                            checkType ctx t2 CUniverse >>= (fn ct2 => 
                                Success(CFunc(ct1, ct2) )
                            )
                        )) *)
                        | RTypeInst(t1, t2, soi) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                                checkType ctx t1 CUniverse >>= (fn (ct1, ctx) => 
                                    checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                        Success(CTypeInst(ct1, ct2) , ctx)
                                    )
                                ))
                        | RForall(tv, t2, soi) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            withLocalBinder ctx tv CUniverse (fn ctx => 
                            checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                    Success(CForall(tv, ct2) , ctx)
                                )))
                        | RExists(tv, t2, soi) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            withLocalBinder ctx tv CUniverse (fn ctx => 
                            checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                    Success(CExists(tv, ct2) , ctx)
                                )))
                        | RRho(tv, t2, soi) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            withLocalBinder ctx tv CUniverse (fn ctx => 
                            checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                    Success(CRho(tv, ct2) , ctx)
                                )))
                        | RPairOfQuotes((ql, qr)) => 
                        addNewMetaVar ctx tt [ql, qr]
                        | _ => genSingletonError (reconstructFromRExpr e) ("check type failed on " ^ PrettyPrint.show_typecheckingRType e 
                        ^ " <= " ^ PrettyPrint.show_typecheckingCType tt) NONE
                    )
                    val res' = Errors.enrichErrWhenChecking e ttUnnorm res
                in res'
                end 
        ))
                    (* handle TypeCheckingFailure s =>
                    raise TypeCheckingFailure (s ^ "\n when checking the expr " ^ PrettyPrint.show_typecheckingRExpr e ^ 
                        " against type " ^ PrettyPrint.show_typecheckingType tt
                    ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx
                    ) *)
                    (* type check signature will return all bindings *)
            and typeCheckSignature(ctx : context) (s : RSignature) (acc : CSignature) : (CSignature * context) witherrsoption =

                (


                    if DEBUG then DebugPrint.p ("DEBUG952 " ^ PrettyPrint.show_typecheckingRSig s ^
                    " in context " ^ PrettyPrint.show_typecheckingpassctx ctx ^"\n") else (); 
                    (* DebugPrint.p ("TCSig r="  ^ Int.toString (length s) ^   " acc=" ^ Int.toString (length acc) ^
                    "\nDEBUG " ^ PrettyPrint.show_typecheckingRSig s ^
                    "\n"); *)
                    (* DebugPrint.p "TCSig..."; *)

                    case s of
                    [] => 
                resolveAllMetaVarsInCSig ctx acc >>= (fn acc => 
                (
                    (* DebugPrint.p "OK"; *)
                    Success(acc, ctx)
                    )
                )
                    (* normalize should not change the set of free variables *)
                (* | RTypeMacro (n, t)::ss => 
                let val freeTVars = freeTCVar (applyContextToType ctx (rTypeToCType t)) in if freeTVars <> [] then 
                    Errors.typeDeclContainsFreeVariables (StructureName.toString (hd freeTVars)) ctx
                    else 
                    weakHeadNormalizeType (applyContextToType ctx (rTypeToCType t)) >>= (fn normalizedType => 
                    (
                        (* DebugPrint.p (
                            StructureName.toStringPlain (getCurSName ctx)
                            ^" normlizedType is " ^ PrettyPrint.show_typecheckingType normalizedType ^ "\n")
                        ; *)
                    typeCheckSignature (addToCtxR (TypeDef([n], normalizedType, ())) ctx) ss 
                        (acc@[CTypeMacro((getCurSName ctx)@[n],  normalizedType)])
                    )
                        )
                    end *)
                | RTermTypeJudgment(n, t):: ss => 
                let 
                (* val freeTVars = freeTCVar  (rTypeToCType ctx t) *)
                (* (applyContextToType ctx (rTypeToCType ctx t))  *)
                in  (* do not check for free variables, as it will be catched in a later stage? *)
                (* if freeTVars <> [] 
                    then Errors.termTypeDeclContainsFreeVariables (StructureName.toString (hd freeTVars)) ctx
                    (* raise SignatureCheckingFailure ("TermType decl contains free var" ^ PrettyPrint.show_sttrlist (freeTVar (applyContextToType ctx t)) ^" in "^ PrettyPrint.show_typecheckingType (applyContextToType ctx t))  *)
                    else  *)
                    checkExprIsType ctx t >>= (fn (ct, ctx) =>
                        weakHeadNormalizeType t ctx ct
                        (* (applyContextToType ctx (rTypeToCType ctx t))  *)
                        >>= (fn normalizedType => 
                        typeCheckSignature (addToCtxR (TermTypeJ([n], normalizedType, JTPending, NONE)) ctx) ss (acc))
                    )
                end
                (* | RTermMacro(n, e) :: ss => 
                    synthesizeType ctx (e) >>= 
                    (fn (transformedExpr , synthesizedType)  =>
                        typeCheckSignature (addToCtxR (TermTypeJ([n], synthesizedType, NONE)) ctx) ss 
                            (acc@[CTermDefinition((getCurSName ctx)@[n], transformedExpr, synthesizedType)])
                    ) *)
                | RTermDefinition(n, e) :: ss => 
                let fun newDef() = 
                    (case e of 
                    RVar _ => synthesizeTypeNoInstMeta ctx e
                    | _ => synthesizeType ctx (e))
                     >>= (fn ((transformedExpr , synthesizedType), ctx)  =>
                                typeCheckSignature 
                                    (addToCtxR (TermTypeJ([n], synthesizedType, JTDefinition transformedExpr, NONE)) ctx) ss 
                                    (acc@[CTermDefinition((getCurSName ctx)@[n], transformedExpr, synthesizedType)])
                            ) 
                in
                (case findCtx ctx ((getCurSName ctx)@[n]) of  (* must find fully qualified name as we allow same name for substructures *)
                    NONE  =>  newDef()
                    (* optimize: if e is just an expr, do not instantiate implicit args *)
                    | SOME(cname, lookedUpType, lookedUpDef) => 
                        (case lookedUpDef of 
                         JTPending =>  
                            let val transformedExprOrFailure = checkType ctx (e) lookedUpType
                            in 
                            case transformedExprOrFailure of
                            Success(transformedExpr, ctx) => typeCheckSignature (modifyCtxAddDef ctx cname transformedExpr) ss 
                                                            (acc@[CTermDefinition((getCurSName ctx)@[n], transformedExpr, lookedUpType)])
                            | DErrors(l) => (case typeCheckSignature ctx ss (acc) of 
                                        Success _ => DErrors(l)
                                        | DErrors l2 => DErrors(l @l2)
                                        | _ => raise Fail "tcp458"
                                )
                            | _ => raise Fail "tcp457"
                            end
                        | _ => newDef() (* allow repeated definitions *)
                        (* Errors.redefinitionError n (StructureName.toStringPlain cname) ctx (List.last cname)  *)
                            )
                )
                end
                | RConstructorDecl(name, rtp) :: ss => 
                    checkConstructorType name ctx rtp >>= (fn ((checkedType, cconsinfo),ctx) => 
                    
                        typeCheckSignature (addToCtxR(TermTypeJ([name], checkedType, JTConstructor cconsinfo, NONE)) ctx) ss
                        (acc@[CConstructorDecl((getCurSName ctx)@[name], checkedType, cconsinfo)])
                    )
                | RStructure (vis, sName, decls) :: ss => 
                (case ctx of 
                Context(curName, curVis, bindings) => 
                    typeCheckSignature (Context(curName@[sName], vis, bindings)) decls [] >>=
                    (fn(checkedSig, Context(_, _, newBindings)) =>
                        (* assume the typeChecking is behaving properly, 
                        no conflicting things will be added to the signature *)
                        (* sub context will be determined by whether the signature is private or not ? *)
                    typeCheckSignature (Context(curName, curVis, newBindings)) ss (acc@checkedSig)
                    )
                    
                )
                | ROpenStructure openName :: ss =>
                (case ctx of 
                Context(curName, curVis, bindings) => 
                    let val nextContext = nextContextOfOpenStructure curName curVis bindings openName
                        (* assume the typeChecking is behaving properly, 
                        no conflicting things will be added to the signature *)
                        (* sub context will be determined by whether the signature is private or not ? *)
                    in typeCheckSignature nextContext ss (acc)
                    end
                )
                | RReExportStructure reExportName :: ss =>
                        ((reExportDecls ctx reExportName) <?> ( fn _ =>
                            typeCheckSignature ctx ss (acc) (* we collect remaining possible failures *)
                        )) >>= (fn newBindings => 
                                            typeCheckSignature ctx ss (acc@newBindings)
                        )                
                        (* note that the order of <?> and >>= is important as >>= won't ignore previous error 
                        reverse would have exponential wasted computation *)
                | RImportStructure(importName, path) :: ss => 
                    (getTypeCheckedAST (path, importName)
                    <?> (fn _ => Errors.importError (StructureName.toString importName)  ctx)
                    )
                     >>= (fn csig => 
                        typeCheckSignature 
                        (addToCtxAL (List.concat (List.mapPartial (fn x => case x of 
                            (* CTypeMacro(sname, t) => SOME(TypeDef(sname, t, ())) *)
                             CTermDefinition(sname, e, t) => SOME([TermTypeJ(sname, t, JTDefinition(e), NONE)
                                ])
                            | CDirectExpr _ => NONE
                            | CImport _ => NONE
                            | CConstructorDecl(sname, t, consinfo) => SOME([TermTypeJ(sname, t, JTConstructor consinfo, NONE)
                                ])
                            ) csig)) ctx)
                        ss (acc@[CImport(importName, path)])
                    )
                | RDirectExpr e :: ss=> 
                    let 
                    val synthedExprOrFailure = (synthesizeType ctx (e))
                    in case synthedExprOrFailure of 
                        Success((checkedExpr, synthesizedType), ctx) => typeCheckSignature ctx ss (acc@[CDirectExpr(checkedExpr, synthesizedType)])
                        | DErrors l => (case typeCheckSignature ctx ss (acc) of
                                        Success _ => DErrors l
                                        | DErrors l2 => DErrors (l @ l2)
                                        | _ => raise Fail "tcp 492"
                        )
                        | _ => raise Fail "tcp494"
                    end
                )
                (* handle SignatureCheckingFailure st =>
                raise TypeCheckingFailure (st ^ "\n when checking the signature " ^ PrettyPrint.show_typecheckingRSig s 
                    ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx
                    ) *)
    in 
        fn s => 
            (typeCheckSignature 
            (Context (topLevelStructureName, true, 
                    []))
            s []) >>= (fn (csig, ctx) => 
                (* resolveAllMetaVarsInCSig ctx csig >>= (fn csig =>  *)
                (
                    (* DebugPrint.p ("DEBUG1070: " ^ PrettyPrint.show_typecheckingCSig csig) ;  *)
                    Success(csig))
                (* ) *)
            )
                (* val _ = DebugPrint.p "Type checked top level\n"
                val _ = DebugPrint.p (PrettyPrint.show_typecheckingCSig res) *)
    end
end
