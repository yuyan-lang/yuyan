structure CPSPass =
struct

open TypeCheckingAST
open CPSAst

exception CPSInternalError


    fun kcc (cc : cpsvar -> cpscomputation) : cpscontinuation = 
        let val v = CPSVarLocal (UID.next())
        in (v, cc v) end
    fun kcc' (cc : int -> cpscomputation) : (int * cpscomputation) = 
        let val v = (UID.next())
        in (v, cc v) end
    fun kcc2 (cc : cpsvar -> cpsvar -> cpscomputation) : cpsvar * cpsvar * cpscomputation = 
        let val v1 = CPSVarLocal (UID.next())
        val v2 = CPSVarLocal (UID.next())
        in (v1, v2, cc v1 v2) end
    fun kcc2' (cc : int -> int -> cpscomputation) : int * int * cpscomputation = 
        let val v1 = (UID.next())
        val v2 = (UID.next())
        in (v1, v2, cc v1 v2) end

    val getCurExnHandlerName = "yyGetCurrentExceptionHandler"
    val setCurExnHandlerName = "yySetCurrentExceptionHandler"
    fun cpsGetCurrentExnHanlder(cc : cpsvar -> cpscomputation) = 
            CPSFfiCCall (UTF8String.fromString getCurExnHandlerName, [], kcc cc) 
    fun cpsSetCurrentExnHandler (h : cpsvalue) (cc : unit -> cpscomputation) =
            CPSFfiCCall( UTF8String.fromString setCurExnHandlerName, [h], kcc (fn _ => cc ()))
    fun klookupLabel ( ctx : (Label * Type) list) (l : Label) : int = 
        case ctx of 
             (n1, t1)::cs => if UTF8String.semanticEqual n1 l then 0 else klookupLabel cs l+1
             | _ => raise Fail "cpspass25"
      fun klookupLabel3 ( ctx : (Label * EVar * CExpr ) list) (l : Label) : int = 
        case ctx of 
             (n1, _, t1)::cs => if UTF8String.semanticEqual n1 l then 0 else klookupLabel3 cs l+1
             | _ => raise Fail "cpspass29"

    fun cpsTransformBuiltinFunc( x : BuiltinFunc) (cc : cpsvar -> cpscomputation) : cpscomputation = 
        case x of 
             BFCallCC => 
                CPSAbs (kcc2' (fn arg  (* arg is a function ((b -> c) -> b) that are waiting for a current continuation (b -> c) *)
                => fn ret (* return is the continuation (the real cc) that expects a value of b 
                                (* obtained either from the return of the arg or from throw of the arg *)
                ! *)=>
                (* grab the current exception handler for restoring *)
                    cpsGetCurrentExnHanlder (fn handlerAtEntry => 
                        (* we first construct the throw function *)
                        CPSAbs(kcc2' (fn throwArg =>  (* this is the shortcut answer we're looking for *)
                            fn throwRet (* the continuation of the throw is ignored! *)
                            =>
                                (* reset the current handler when the inner computation throws *)
                                cpsSetCurrentExnHandler (CPSValueVar handlerAtEntry) (fn _ => 
                                    (* throwing to continuation argument always apply single, 
                                        see the codes for app and abs *)
                                    CPSAppSingle(CPSValueVar (CPSVarLocal ret), CPSValueVar (CPSVarLocal throwArg))
                                )
                            ), NONE, kcc (fn throwFunc => 
                                CPSApp(CPSValueVar (CPSVarLocal arg), 
                                        (CPSValueVar throwFunc, CPSValueVar (CPSVarLocal ret))
                                    )
                            )
                        )
                    )
                ), NONE, kcc cc) (* cc is the continuation that expects the value of callcc *)
            | BFNewDynClsfdValueWithString => 
                CPSAbs (kcc2' (fn argName  (* arg the name of classified  *)
                => fn retTup (* should pass the result of the computation to return *)
                =>
                    let val thisDynId = UID.next()
                    in
                            CPSAbs (kcc2' (fn argValue (* the value to store *)
                        => fn retClsfd (* the stored value *)
                        => CPSDynClsfdIn (
                            CPSValueVar (CPSVarLocal argName), 
                            thisDynId,
                            CPSValueVar (CPSVarLocal argValue), 
                            kcc (fn storedVar => 
                                    CPSAppSingle(CPSValueVar (CPSVarLocal retClsfd), CPSValueVar storedVar)
                                )
                            )
                        ), NONE, kcc (fn makeFun =>
                                    CPSAbs (kcc2' (fn valueMatchUnmatch => 
                                        fn matchReturn => 
                                        let 
                                            val vmumVal = CPSValueVar (CPSVarLocal valueMatchUnmatch)
                                        in
                                            CPSProj(vmumVal, 0, kcc (fn valueToMatch => 
                                                CPSProj(vmumVal, 1, kcc (fn matchFunc => 
                                                    CPSProj(vmumVal, 2, kcc (fn unmatchFunc => 
                                                        CPSDynClsfdMatch(CPSValueVar valueToMatch, 
                                                        (thisDynId, kcc (fn unwrappedValue => 
                                                                CPSApp(CPSValueVar (matchFunc), 
                                                                        (CPSValueVar (unwrappedValue), 
                                                                            CPSValueVar (CPSVarLocal matchReturn) (* the continuation is match return *)
                                                                        )
                                                                    )
                                                            )),
                                                        CPSUnit(kcc (fn unitValue => 
                                                            CPSApp(CPSValueVar (unmatchFunc), 
                                                                (CPSValueVar unitValue, CPSValueVar (CPSVarLocal matchReturn)) 
                                                                )
                                                        ))
                                                        )
                                                    ))
                                                ))
                                            ))
                                        end
                                    ), NONE, kcc (fn analyzeFunc => 
                                            CPSTuple ( [CPSValueVar makeFun, CPSValueVar analyzeFunc], kcc (fn tup => 
                                                CPSAppSingle(CPSValueVar (CPSVarLocal retTup), CPSValueVar tup))
                                            )
                                    )
                                )
                            )
                        )
                    end
                ), NONE, kcc cc) (* cc is the continuation that expects the value of this builtin function *)
            | BFRaise => 
                CPSAbs(kcc2' (fn exnVal => fn ret => 
                    cpsGetCurrentExnHanlder(fn curHandler => 
                        CPSAppSingle(CPSValueVar curHandler, CPSValueVar (CPSVarLocal exnVal))
                    )
                ), NONE, kcc cc)
                (* Handler should always be AbsSingle *)
            | BFHandle => 
                
                CPSAbs(kcc2' (fn tup => 
                fn retVal (* the return value of the entire computation *)=> 
                    (* retrieves the current exception handler *)
                    cpsGetCurrentExnHanlder (fn originalHandler => 
                        (* construct the function that resets the handler to original and return *)
                        CPSAbsSingle(kcc' (fn retValue => 
                                cpsSetCurrentExnHandler (CPSValueVar originalHandler) (fn _ => 
                                    CPSAppSingle(CPSValueVar (CPSVarLocal retVal), CPSValueVar (CPSVarLocal retValue))
                                )
                            ), NONE, kcc (fn resetHandlerAndReturn => 
                                (* retrieves the new handler from argument tuple *)
                                CPSProj(CPSValueVar (CPSVarLocal tup), 1, kcc (fn newHandler => 
                                    (* constructs the real handler that is to be executed in the original handler *)
                                    CPSAbsSingle(kcc' (fn exceptionVal => 
                                        (* it first resets the current exception handler *)
                                        cpsSetCurrentExnHandler (CPSValueVar originalHandler) (fn _ => 
                                            (* it applies the new handler to the exception value and the global return point *)
                                            CPSApp(CPSValueVar newHandler, (CPSValueVar (CPSVarLocal exceptionVal), CPSValueVar (CPSVarLocal retVal)))
                                        )
                                    ), NONE, kcc (fn realNewHanlder =>
                                        (* sets the current exception handler to the real new handler *)
                                        cpsSetCurrentExnHandler (CPSValueVar realNewHanlder) (fn _ => 
                                            (* gets the real expression *)
                                            CPSProj(CPSValueVar (CPSVarLocal tup), 0, kcc (fn tryFunc => 
                                                CPSUnit (kcc (fn unitVal => 
                                                    (* apply the function , with the current continuation to reset the handler*)
                                                    CPSApp(CPSValueVar tryFunc, (CPSValueVar unitVal, CPSValueVar (resetHandlerAndReturn)))
                                                ))
                                            ))
                                        )
                                    ))
                                ))

                        ))
                    )
                ), NONE, kcc cc)


    and cpsTransformExpr   
        (ctx : context) (e : CExpr) (cc : cpsvar -> cpscomputation) (* cc is current continutaion *)
        : cpscomputation =
    (
        let 
        (* val _ = DebugPrint.p ("cpsTransformExpr on " ^ PrettyPrint.show_typecheckingCExpr e ^ " in context " ^ PrettyPrint.show_cpscontext ( ctx) ^ "\n"); *)
         val res = case e of
            CExprVar sn => (case ListSearchUtil.lookupSName ctx sn of 
                PlainVar v => cc v
                | GlobalVar v => cc v
                | SelfVar v => CPSAbsSingle(kcc' (fn arg => 
                        cc (CPSVarLocal arg)
                    ), NONE, kcc (fn kont => 
                        CPSApp(CPSValueVar v, (CPSValueVar v, CPSValueVar kont)) (* apply the recursive value to itself *)
                     )))
            | CUnitExpr => CPSUnit (kcc cc)
            | CTuple (cs, Prod ls) => (
                    let fun go i values = 
                        if i = List.length cs
                        then CPSTuple(values, kcc cc)
                        else cpsTransformExpr ctx (List.nth(cs, i)) 
                                    (fn v => go (i+1) (values@[CPSValueVar v]))
                    in go 0 [] end
                )
            | CProj(e, l, Prod ls) => cpsTransformExpr ctx e (fn v 
                        => CPSProj(CPSValueVar v, (klookupLabel ls l),(kcc cc)))
            | CInj (l, e, Sum ls ) => 
                cpsTransformExpr ctx e 
                    (fn v => CPSInj(l, klookupLabel ls l, CPSValueVar v, kcc cc)
            )
            | CCase((Sum ls, e),cases, resType) => 
            (cpsTransformExpr ctx e) (fn v => 
                    CPSCases (CPSValueVar v, (List.tabulate(List.length ls, 
                    fn index => let val  (label,t) = List.nth(ls, index)
                                    val caseIndex = klookupLabel3 cases label
                                    val (_, ev, e) = List.nth(cases, caseIndex)
                                in kcc (fn boundId => cpsTransformExpr (
                                     ([ev], PlainVar boundId)::ctx) e cc
                                ) end)))
            )
            | CLam(ev, eb, Func(t1, t2)) => 
                CPSAbs (kcc2' (fn arg => fn ret =>
                    cpsTransformExpr ((([ev], PlainVar (CPSVarLocal arg)))::ctx) eb 
                        (fn r => CPSAppSingle(CPSValueVar (CPSVarLocal ret),CPSValueVar r))
                ), NONE, kcc cc)
            | CApp (e1, e2, t) => 
                cpsTransformExpr ctx e1 (fn v1 => 
                 cpsTransformExpr ctx e2 (fn v2 => 
                    CPSAbsSingle(kcc' (fn arg => 
                        cc (CPSVarLocal arg)
                    ), NONE, kcc (fn kont => 
                        CPSApp(CPSValueVar v1, (CPSValueVar v2, CPSValueVar kont))
                     ))
                 ))
            | CSeqComp(e1, e2, _, _) =>
                cpsTransformExpr ctx e1 (fn v1 => 
                 cpsTransformExpr ctx e2 cc)
            | CTAbs (tv, e2, _) => cpsTransformExpr ctx e2 cc
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
            in
              CPSAbs (kcc2' (fn self => fn ret =>
                    cpsTransformExpr (([ev], SelfVar (CPSVarLocal self)) :: ctx) e 
                     (fn r => CPSAppSingle(CPSValueVar (CPSVarLocal ret),CPSValueVar r))
                ), NONE, ( fixedPointBoundId , 
                    CPSAbsSingle(kcc' (fn arg => 
                        cc (CPSVarLocal arg)
                        ), NONE, kcc (fn kont => 
                        CPSApp(CPSValueVar fixedPointBoundId, (CPSValueVar fixedPointBoundId, CPSValueVar kont))
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
                CPSBuiltinValue(CPSBvReal r, kcc cc)
            | CFfiCCall (cFuncName, args) => 
                foldr (fn (arg, acc) => 
                    (fn (prevArgs : cpsvalue list) => 
                    cpsTransformExpr ctx (CExprVar arg) (fn argv => 
                            acc (prevArgs@[CPSValueVar argv])
                        )
                    )
                ) (fn argvs => 
                    CPSFfiCCall (cFuncName, argvs, kcc cc)
                ) args []
            | CLetIn(csig, e,t) => 
             ( (cpsTransformSig ctx csig false  (fn (newCtx, _) => 
             let
                (* val _ = DebugPrint.p ("CPS Let Partial Context:" ^ (PrettyPrint.show_cpscontext  newCtx) ^ "\n") *)
            in
                cpsTransformExpr newCtx e cc
            end
            )))
            | CBuiltinFunc(f) =>  cpsTransformBuiltinFunc f cc
            | _ => raise Fail "cpsp116"

        (* val _ = print ("cpsTransformSig result is " ^ PrettyPrint.show_pkcomputation res ^ "cpsTransformSig on " ^ PrettyPrint.show_typecheckingExpr e ^ 
        " against type" ^ PrettyPrint.show_typecheckingType tt ^
        " in context " ^ PrettyPrint.show_typecheckingpassctx (eraseCtx ctx) ^ "\n"); *)
        in res end
        )

        handle ListSearchUtil.NotFoundSName sname => 
            (DebugPrint.p ("Internal error: " ^ StructureName.toStringPlain sname  ^ " not found \n");
            raise CPSInternalError)
        handle CPSInternalError =>
            (DebugPrint.p ("When transforming expression " ^ PrettyPrint.show_typecheckingCExpr e ^ " \n");
            raise CPSInternalError)

    and cpsTransformSig  (ctx : context) (s : CSignature) (useGlobalVar:bool)
    (cc : context * cpsvar option  -> cpscomputation)
     :  cpscomputation  =

    let 
    in
            (* print ("eraseSigLazy DEBUG " ^ PrettyPrint.show_typecheckingSig s )
            ; *)
            case s of
            [] => (cc (ctx, NONE))
            (* (case kont of SOME f => f(ctx) | NONE => PKRet(PKUnit)) *)
            (* optimize if tail of the block is an expression, it is the value of the expression *)
        | [CDirectExpr (e, tp)]  => 
            (cpsTransformExpr ctx e 
                (fn resvar => cc (ctx, SOME resvar)))
             (* cpsTransformExpr ctx e (fn resvar => 
             cc (ctx, SOME resvar)) *)
        | CTypeMacro _ :: ss => (* ignore type macro during cps *)
            cpsTransformSig ctx ss useGlobalVar cc
        | CTermDefinition(name, def, tp):: ss =>  
            cpsTransformExpr ctx def 
            (fn resvar =>
            if useGlobalVar
            then
            (let val globalVar = CPSVarGlobal (UID.next())
            in 
            CPSStore(globalVar, CPSValueVar resvar,  (cpsTransformSig ((name, GlobalVar globalVar)::ctx) ss useGlobalVar cc))
            end)
            else (cpsTransformSig ((name, PlainVar resvar)::ctx) ss useGlobalVar cc)
            )

        | CDirectExpr (e, tp) :: ss => 
            cpsTransformExpr ctx e 
            (fn resvar =>  (cpsTransformSig (ctx) ss useGlobalVar cc))
        | CImport _ :: ss => 
            cpsTransformSig (ctx) ss useGlobalVar cc
    end


 fun cpsTransformSigTopLevel  (s : CSignature) 
     : context * cpsvar option * cpscomputation =
     (
         (* DebugPrint.p (PrettyPrint.show_typecheckingCSig s); *)

    let val finalContext = ref []
    val finalResult = ref NONE (* TODO: this is actually problematic, by case-3, the final return may have two values!, need 
    other mechanisms! *)
     val comp = cpsTransformSig [] s true (fn (ctx, resvar) => 
     let val _ = finalContext := ctx
     val _ = finalResult := resvar
     in
        case resvar of SOME resvar => CPSDone (CPSValueVar resvar)
        (* return unit if last expression is not a expr *)
                     | NONE => CPSUnit (kcc (fn resvar =>  CPSDone (CPSValueVar resvar)))
    end
    )
         (* val _ = DebugPrint.p ("CPS Final Context:" ^ (PrettyPrint.show_cpscontext  context) ^ "\n") *)
        (* val _ = DebugPrint.p ("CPS Final Computation:" ^ (PrettyPrint.show_cpscomputation comp) ^ "\n") *)
    in (!finalContext, !finalResult,  comp)
    end
    (*  *)
     )






end
