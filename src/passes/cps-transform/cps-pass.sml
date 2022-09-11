structure CPSPass =
struct

open TypeCheckingAST
open TypeCheckingASTOps
open CPSAst
open CPSHelper

exception CPSInternalError





    fun klookupLabel ( ctx : (Label * CType) list) (l : Label) : int = 
        case ctx of 
             (n1, t1)::cs => if UTF8String.semanticEqual n1 l then 0 else klookupLabel cs l+1
             | _ => raise Fail "cpspass25"
      fun klookupLabel3 ( ctx : (Label * EVar * CExpr ) list) (l : Label) : int = 
        case ctx of 
             (n1, _, t1)::cs => if UTF8String.semanticEqual n1 l then 0 else klookupLabel3 cs l+1
             | _ => raise Fail "cpspass29"

  

    fun registerFunctionNameMapping (i : cpsvar) (e : CExpr) (msg : string) : unit = 
        (
            (* DebugPrint.p ( "CPSNameMapping fid=" ^  PrettyPrint.show_cpsvar i ^ " msg=" ^ msg ^ " ==> " ^ PrettyPrint.show_typecheckingCExpr (e) ^ "\n"); *)
        ())

    fun resolveSNameInCtx (ctx : context) (sname : StructureName.t) (cc : cpsvar -> cpscomputation) = 
        (case ListSearchUtil.lookupSName ctx sname of 
                PlainVar v => cc v
                | GlobalVar v => cc v
                | SelfVar v => CPSAbsSingle(kcc' (fn arg => 
                        cc (CPSVarLocal arg)
                    ), NONE, kcc (fn kont => 
                    ( 
                        (* registerFunctionNameMapping kont e "selfApp"; *)
                        CPSApp(CPSValueVar v, (CPSValueVar v, CPSValueVar kont)) (* apply the recursive value to itself *)
                    )
            )))
    
    fun ctxWithNewOpenStructure (ctx' : context) (structureValue : cpsvalue) (csig : CDeclaration list) (cc : context -> cpscomputation ) = 
            let 
                fun go i ctx = 
                    if i = length csig 
                    then cc ctx
                    else 
                        let val curDec = List.nth(csig, i)
                        in (case curDec of 
                            (CPureDeclaration(dname, _) 
                            | CTermDefinition(dname, _, _)
                            | CConstructorDecl(dname, _, _)
                            ) => CPSProj(structureValue, i, kcc (fn projected => go (i+1) (([dname], PlainVar projected)::ctx)))
                            | (CDirectExpr(_ ) 
                            | CImport _ | COpenStructure _) => go (i+1) ctx
                        )
                        end
            in
            go 0 ctx'
            end

    and cpsTransformExpr   
        (ctx : context) (e : CExpr) (cc : cpsvar -> cpscomputation) (* cc is current continutaion *)
        : cpscomputation =
    (
        let 
        (* val _ = DebugPrint.p ("cpsTransformExpr on " ^ PrettyPrint.show_typecheckingCExpr e ^ " in context " ^ PrettyPrint.show_cpscontext ( ctx) ^ "\n"); *)
         val originalExpr = e
         val res = case e of
            CVar (sn, _) => resolveSNameInCtx ctx sn cc
            | CUnitExpr => CPSUnit (kcc cc)
            | CTuple (cs, u) => (
                    let fun go i values = 
                        if i = List.length cs
                        then CPSTuple(values, kcc cc)
                        else cpsTransformExpr ctx (List.nth(cs, i)) 
                                    (fn v => go (i+1) (values@[CPSValueVar v]))
                    in go 0 [] end
                )
                (* compile lazy tuple as if every term has a implicit abstraction  of zero arugment (continuation only) *)
            | CLazyTuple (cs, CTypeAnn(CLazyProd ls)) => (
                    let fun go i values = 
                        if i = List.length cs
                        then CPSTuple(values, kcc cc)
                        else CPSAbsSingle(kcc' (fn ret => 
                                cpsTransformExpr ctx (List.nth(cs, i)) (fn exprVal =>
                                        CPSAppSingle(CPSValueVar (CPSVarLocal ret), CPSValueVar exprVal)
                                    )
                                ), NONE, kcc (fn v => go (i+1) (values@[CPSValueVar v]))
                            )
                    in go 0 [] end
                )
            | CProj(e, idx, u ) => cpsTransformExpr ctx e (fn v 
                        => CPSProj(CPSValueVar v, idx,(kcc cc)))
            | CLazyProj(e, l, CTypeAnn(CLazyProd ls)) => cpsTransformExpr ctx e (fn v 
                        => CPSProj(CPSValueVar v, (klookupLabel ls l),
                        (kcc (fn projected => 
                            (CPSAbsSingle (kcc' (fn ret => 
                                cc (CPSVarLocal ret)
                            ), NONE, kcc (fn ccAbs => 
                                CPSAppSingle(CPSValueVar projected, CPSValueVar ccAbs)
                            )))
                        ))))
            | CIfThenElse(e, tcase, fcase) => cpsTransformExpr ctx e (fn v => 
                CPSAbsSingle (kcc' (fn ret => 
                    cc (CPSVarLocal ret)
                ), NONE, kcc (fn ccAbs => 
                let val _ = registerFunctionNameMapping ccAbs originalExpr "Cont of"
                val cc' = (fn v => CPSAppSingle(CPSValueVar ccAbs, CPSValueVar v))
                in
                    CPSIfThenElse(CPSValueVar v, cpsTransformExpr ctx tcase cc, 
                    cpsTransformExpr ctx fcase cc) 
                end
                ))
                    (* is this acutally efficient? I would imagine 
                    it leads to lots of wasted codes (by having two copies of cc, 
                    also in cases)  TOOO: investigate *)
            )
            | CInj (l, e, CTypeAnn(CSum ls )) => 
                cpsTransformExpr ctx e 
                    (fn v => CPSInj(l, klookupLabel ls l, CPSValueVar v, kcc cc)
            )
            | CCase((_, e),cases, resType) => 

                (cpsTransformExpr ctx e) (fn v => 
                        CPSCases (CPSValueVar v, (map (fn (pat, body) =>
                    let 
                        fun toCPSPattern (ctx : context)(pat : CPattern) : cpspattern * context  = 
                            case pat of 
                                CPatVar x => let 
                                    val localvar = CPSVarLocal (UID.next())
                                    in ((CPSPatVar (localvar)), ([x], PlainVar localvar)::ctx)
                                    end
                                | CPatHeadSpine((hdname, cinfo), spinepats) =>  
                                    (case cinfo of 
                                            CConsInfoElementConstructor(_, index) =>  
                                                (let val (bodyPats, ctx) = foldl (fn (pat, (accl, ctx)) =>  
                                                            let val (cpspat, ctx) = toCPSPattern ctx pat 
                                                            in (accl@[cpspat], ctx)
                                                        end) ([], ctx) spinepats
                                                in (CPSPatHeadSpine(index, bodyPats), ctx)
                                                end)
                                            | _ => raise Fail "ni107: unsupported patterns"                                  
                                    )
                        val (cpspattern, ctx)  = toCPSPattern ctx pat
                    in 
                        (cpspattern, cpsTransformExpr ctx body cc)
                    end
                    ) cases )))

                        (* case pat of 
                            CPatVar x => raise Fail "ni103: cps unsupported (yet) patterns"
                            | CPatHeadSpine((hdname, cinfo), spinepats) => 
                                (case cinfo of 
                                    CConsInfoElementConstructor(_, index) => 
                                    let 
                                        val names = map (fn spinepat => 
                                            case spinepat of 
                                                CPatVar x => (x, CPSVarLocal (UID.next()))
                                                | _ => raise Fail "ni111: cps unsupported patterns"
                                        ) spinepats
                                        in 
                                        (index, 
                                        map (fn x => (#2 x)) names,
                                        cpsTransformExpr (
                                            (map (fn (name, var) => ([name], PlainVar var)) names)@ctx
                                        ) body cc
                                        )
                                        end
                                    | _ => raise Fail "ni107: unsupported patterns"
                                    )
                                ) cases)) 
            ) *)
            | CLam(ev, eb, t) => 
                CPSAbs (kcc2' (fn arg => fn ret =>
                    cpsTransformExpr ((([ev], PlainVar (CPSVarLocal arg)))::ctx) eb 
                        (fn r => CPSAppSingle(CPSValueVar (CPSVarLocal ret),CPSValueVar r))
                ), NONE, kcc (fn f => 
                    ( registerFunctionNameMapping f originalExpr "Body of";
                        cc f
                    )
                ))
            | CApp (e1, e2, t) => 
                cpsTransformExpr ctx e1 (fn v1 => 
                 cpsTransformExpr ctx e2 (fn v2 => 
                    CPSAbsSingle(kcc' (fn arg => 
                        cc (CPSVarLocal arg)
                    ), NONE, kcc (fn kont => 
                        ( registerFunctionNameMapping kont originalExpr "Cont of ";
                            CPSApp(CPSValueVar v1, (CPSValueVar v2, CPSValueVar kont))
                        )
                     ))
                 ))
            | CSeqComp(e1, e2, _, _) =>
                cpsTransformExpr ctx e1 (fn v1 => 
                 cpsTransformExpr ctx e2 cc)
            | CTAbs (tv, e2, _) => 
            let  (* TODO: erase type args *)
            in CPSUnit(kcc (fn v => 
                cpsTransformExpr (([tv], PlainVar v)::ctx) e2 cc
            ))
            end
            | CTApp (e2, t, _) => cpsTransformExpr ctx e2 cc
            | CPack (t, e2, et) => cpsTransformExpr ctx e2 cc
            | COpen ((et, e1), (tv, ev, e2), rt) => 
                (cpsTransformExpr ctx e1) 
                    (fn v => 
                cpsTransformExpr (([ev],PlainVar v)::ctx) e2 cc
                    )
            | CFold (e2, t) => 
                cpsTransformExpr ctx e2 (fn v => CPSFold(CPSValueVar v, kcc cc))
            | CUnfold (e2, _) => cpsTransformExpr ctx e2 (fn v => CPSUnfold(CPSValueVar v , kcc cc))
            | CFix (ev, e, _)=>  
            let val fixedPointBoundId = CPSVarLocal (UID.next())
                val _ = registerFunctionNameMapping fixedPointBoundId originalExpr "fixed point computation func"
            in
              CPSAbs (kcc2' (fn self => fn ret =>
                    cpsTransformExpr (([ev], SelfVar (CPSVarLocal self)) :: ctx) e 
                     (fn r => CPSAppSingle(CPSValueVar (CPSVarLocal ret),CPSValueVar r))
                ), NONE, ( fixedPointBoundId , 
                    CPSAbsSingle(kcc' (fn arg => 
                        cc (CPSVarLocal arg)
                        ), NONE, kcc (fn kont => 
                            ( registerFunctionNameMapping kont originalExpr "Cont of fixedpoint";
                                CPSApp(CPSValueVar fixedPointBoundId, (CPSValueVar fixedPointBoundId, CPSValueVar kont))
                            )
                        ))
                    )
                )
                end
                (* Is this really the case ??? *)
            (* let
             fix F = ((\y. f (y y)) (\y. f (y y)))
             (this one causes infinite look)
                    val compiledF = KAbs (fn v => cpsTransformSig ((ev, v)::kctx) ((ev, tt)::ctx) e tt)
                    fun appVV (v1 : kvalue) (v2:kvalue) : kcomputation = KApp(KRet(v1), KRet(v2))
                    val lamydotfyy = KAbs(fn y => KApp(KRet(compiledF),(appVV y y)))
                in appVV lamydotfyy lamydotfyy
            end *)
            | CStringLiteral l => 
                CPSBuiltinValue(CPSBvString l, kcc cc)
            | CIntConstant i => 
                CPSBuiltinValue(CPSBvInt i, kcc cc)
            | CRealConstant r => 
                CPSBuiltinValue(CPSBvReal (NumberParser.toRealValue r), kcc cc)
            | CBoolConstant r => 
                CPSBuiltinValue(CPSBvBool r, kcc cc)
            | CFfiCCall (cFuncName, args) => 
                foldr (fn (arg, acc) => 
                    (fn (prevArgs : cpsvalue list) => 
                    cpsTransformExpr ctx (arg) (fn argv => 
                            acc (prevArgs@[CPSValueVar argv])
                        )
                    )
                ) (fn argvs => 
                    CPSFfiCCall (cFuncName, argvs, kcc cc)
                ) args []
            | CLetIn(csig, e,t) => 
             ( (cpsTransformSig ctx csig [] (fn (v) => 
             let
                (* val _ = DebugPrint.p ("CPS Let Partial Context:" ^ (PrettyPrint.show_cpscontext  newCtx) ^ "\n") *)
            in
                ctxWithNewOpenStructure ctx (CPSValueVar v) csig (fn newCtx => 
                    cpsTransformExpr newCtx e cc
                )
            end
            )))
            | CBuiltinFunc(f) =>  CPSBuiltin.cpsTransformBuiltinFunc f cc
            (* types *)
            (* TODO: erase them*)
            (* temporarily transform all types as unit *)
            | CProd l => CPSUnit (kcc cc)
            | CLazyProd l =>  CPSUnit (kcc cc)
            | CSum l =>  CPSUnit (kcc cc)
            (* | CFunc (t1,t2) => CPSUnit (kcc cc) *)
            | CTypeInst (t1,t2) => CPSUnit (kcc cc)
            | CForall (tv,t2) => CPSUnit (kcc cc)
            | CExists (tv,t2) => CPSUnit (kcc cc)
            | CRho (tv,t2) =>  CPSUnit (kcc cc)
            | CUnitType => CPSUnit (kcc cc)
            | CNullType => CPSUnit (kcc cc)
            | CBuiltinType(b) => CPSUnit (kcc cc)
            | CUniverse => CPSUnit (kcc cc)
            | CBlock(decls) => cpsTransformSig ctx decls [] cc
            | CBlockProj(e, lbl, idx) => cpsTransformExpr ctx e (fn v 
                        => CPSProj(CPSValueVar v, idx,(kcc cc)))
            | _ => raise Fail ("cpsp116: " ^ PrettyPrint.show_typecheckingCExpr originalExpr)

        (* val _ = print ("cpsTransformSig result is " ^ PrettyPrint.show_pkcomputation res ^ "cpsTransformSig on " ^ PrettyPrint.show_typecheckingExpr e ^ 
        " against type" ^ PrettyPrint.show_typecheckingType tt ^
        " in context " ^ PrettyPrint.show_typecheckingpassctx (eraseCtx ctx) ^ "\n"); *)
        in res end
        )

        handle ListSearchUtil.NotFoundSName sname => 
            (DebugPrint.p ("Internal error: " ^ StructureName.toStringPlain sname  ^ " ( " ^ 
            PrettyPrint.show_utf8strings sname ^ " ) not found \n" 
            ^ " in context " ^ PrettyPrint.show_cpscontext ctx);
            raise CPSInternalError)
        handle CPSInternalError =>
            (DebugPrint.p ("When transforming expression " ^ PrettyPrint.show_typecheckingCExpr e ^ " \n");
            raise CPSInternalError)

    and cpsTransformSig  (ctx : context) (s : CSignature)  (acc : CSignature)
    (cc :  cpsvar -> cpscomputation)
     :  cpscomputation  =
    let 
    in
            (* print ("eraseSigLazy DEBUG " ^ PrettyPrint.show_typecheckingSig s )
            ; *)
            case s of
            [] => (
                let fun go i values = 
                        if i = List.length acc
                        then CPSTuple(values, kcc cc)
                        else (
                            let
                                val curDec = List.nth(acc, i)
                            in (case curDec of 
                                (CPureDeclaration(dname, _) 
                                | CTermDefinition(dname, _, _)
                                | CConstructorDecl(dname, _, _)
                                ) => resolveSNameInCtx ctx [dname] (fn v => go (i+1) (values@[CPSValueVar v]))
                                | (CDirectExpr _ |
                                CImport _ | COpenStructure _) => CPSUnit(kcc (fn v => go (i+1)(values@[CPSValueVar v])))
                            )
                            end
                        )
                in go 0 [] end
                )
            (* (case kont of SOME f => f(ctx) | NONE => PKRet(PKUnit)) *)
            (* optimize if tail of the block is an expression, it is the value of the expression *)
        (* | [CDirectExpr (e, tp)]  => 
            (cpsTransformExpr ctx e 
                (fn resvar => cc (ctx, SOME resvar))) *)
             (* cpsTransformExpr ctx e (fn resvar => 
             cc (ctx, SOME resvar)) *)
        | (d as CTermDefinition(name, def, tp)):: ss =>  
            cpsTransformExpr ctx def 
            (fn resvar =>
                (cpsTransformSig (([name], PlainVar resvar)::ctx) ss (acc@[d]) cc)
            )

        | (d as CDirectExpr (e, tp)) :: ss => 
            cpsTransformExpr ctx e 
            (fn resvar =>  (cpsTransformSig (ctx) ss (acc@[d]) cc))
        | (d as CImport _) :: ss => 
            cpsTransformSig (ctx) ss (acc@[d]) cc
        | (d as COpenStructure(sname, csig)) :: ss => 
            (* structure are compiled as tuple, and open compiled as complete projection *)
            (* csig is the INTERFACE of the structure, not implementation *)
            (* TODO support signature accumulation via OPEN *)
            resolveSNameInCtx ctx sname (fn openModuleV => 
                ctxWithNewOpenStructure ctx (CPSValueVar openModuleV) csig (fn ctx => 
                    cpsTransformSig (ctx) ss (acc@[d]) cc (* next *)
                )
            )
        | (d as CConstructorDecl (name, ctp, CConsInfoTypeConstructor) ):: ss => 
            let val nargs = countSpineTypeArgs ctp
            in
                clams  nargs [] (fn (arglist, ret) => 
                    CPSUnit (kcc ret)
                ) 
                (fn (cloc) => 
                    cpsTransformSig (([name], PlainVar cloc) :: ctx) ss (acc@[d]) cc
                )
            end

        | (d as CConstructorDecl(name, tp, CConsInfoElementConstructor(_, index))) :: ss => 
        let val nargs = countSpineTypeArgs tp
        (* val _ = DebugPrint.p ("constructor index is " ^ Int.toString index ^ "\n") *)
        in
            clams nargs [] (fn (arglist, ret) => 
            CPSBuiltinValue(CPSBvInt index, kcc (fn indexVal =>
                CPSTuple(CPSValueVar indexVal :: (map CPSValueVar arglist), kcc ret)
                ))
            ) 
            (fn (cloc) => 
                cpsTransformSig (([name], PlainVar cloc) :: ctx) ss (acc@[d]) cc
            )
        end
        | (d as CPureDeclaration _) :: ss => raise Fail "cannot compile pure declaration"
    end
        (* handle CPSInternalError =>
            ((DebugPrint.p ("When transforming signature " ^  (if 
            length s > 0 then 
            (PrettyPrint.show_typecheckingCDecl (hd s) ^ " ... " ^ 
            PrettyPrint.show_typecheckingCDecl (List.last s) ^ "  " )
            else ""
            )
            ^ " \n"));
            raise CPSInternalError) *)


 fun cpsTransformSigTopLevel (initialCtx : context) (s : CSignature) (storeLoc : cpsvar)
     :  cpscomputation =
     (
         (* DebugPrint.p (PrettyPrint.show_typecheckingCSig s); *)

    let 
    (* val finalContext = ref []
    val finalResult = ref NONE  *)
    (* TODO: this is actually problematic, by case-3, the final return may have two values!, need 
    other mechanisms! *)
     val comp = cpsTransformSig initialCtx s [] (fn ( resvar) => 
     (* val _ = finalResult := resvar *)
     (* let val _ = finalContext := ctx
     in
        case resvar of  *)
        (* SOME resvar =>  *)
        (* TODO STORE IN PASSED IN GLOBAL LOCATION *)
        CPSStore(storeLoc, CPSValueVar resvar,  
        CPSDone (CPSValueVar resvar) (* CPSDonw should not necessarily return but is a silent does nothing *)
        )
        (* return unit if last expression is not a expr *)
                     (* | NONE => CPSUnit (kcc (fn resvar =>  CPSDone (CPSValueVar resvar))) *)
    (* end *)
    )
         (* val _ = DebugPrint.p ("CPS Final Context:" ^ (PrettyPrint.show_cpscontext  context) ^ "\n") *)
        (* val _ = DebugPrint.p ("CPS Final Computation:" ^ (PrettyPrint.show_cpscomputation comp) ^ "\n") *)
        (* TODO: STORE? *)
    in (comp)
    end
    (*  *)
     )






end
