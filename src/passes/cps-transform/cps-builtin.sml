structure CPSBuiltin = struct
open CPSAst
open CPSHelper
    val getCurExnHandlerName = "yyGetCurrentExceptionHandler"
    val setCurExnHandlerName = "yySetCurrentExceptionHandler"
    fun cpsGetCurrentExnHanlder(cc : cpsvar -> cpscomputation) = 
            CPSFfiCCall (UTF8String.fromString getCurExnHandlerName, [], kcc cc) 
    fun cpsSetCurrentExnHandler (h : cpsvalue) (cc : unit -> cpscomputation) =
            CPSFfiCCall( UTF8String.fromString setCurExnHandlerName, [h], kcc (fn _ => cc ()))


    fun transformCallCC(cc : cpsvar -> cpscomputation) : cpscomputation = 
                clams1 
                (fn (tpignore, outerRet) => 
                    CPSAbs (kcc2' (fn arg  (* arg is a function ((b -> c) -> b) that are waiting for a current continuation (b -> c) *)
                    => fn ret (* return is the continuation (the real cc) that expects a value of b 
                                    (* obtained either from the return of the arg or from throw of the arg *)
                    ! *)=>
                    (* grab the current exception handler for restoring *)
                        cpsGetCurrentExnHanlder (fn handlerAtEntry => 
                            (* we first construct the throw function *)
                            clams2 
                            (fn ((tpIgnore, throwArg), (* this is the shortcut answer we're looking for *)
                                    throwRet) =>  (* the continuation of the throw is ignored! *)
                                            (* reset the current handler when the inner computation throws *)
                                            cpsSetCurrentExnHandler (CPSValueVar handlerAtEntry) (fn _ => 
                                                (* throwing to continuation argument always apply single, 
                                                    see the codes for app and abs *)
                                                CPSAppSingle(CPSValueVar (CPSVarLocal ret), CPSValueVar (throwArg))
                                            )
                                )  (fn throwFunc => 
                                    CPSApp(CPSValueVar (CPSVarLocal arg), 
                                            (CPSValueVar throwFunc, CPSValueVar (CPSVarLocal ret))
                                        )
                                )
                            )
                    ), NONE, kcc outerRet) (* cc is the continuation that expects the value of callcc *)
                ) cc

    fun transformNewDynClsfdValueWithString (cc : cpsvar -> cpscomputation ) : cpscomputation = 
        CPSAbs (kcc2' (fn typeNameIgnore  (* the value of the type  *) =>
            fn typeNameRet =>
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
                                    CPSAbs (kcc2' 
                                    (fn typeVar => 
                                    fn typeVarRet => 
                                        CPSAbs(kcc2' (fn valueMatchUnmatch => 
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
                                        ), NONE, kcc(fn analyzeFuncBody => 
                                            CPSAppSingle(CPSValueVar (CPSVarLocal typeVarRet), CPSValueVar analyzeFuncBody)
                                    ))), NONE, kcc (fn analyzeFunc => 
                                            CPSTuple ( [CPSValueVar makeFun, CPSValueVar analyzeFunc], kcc (fn tup => 
                                                CPSAppSingle(CPSValueVar (CPSVarLocal retTup), CPSValueVar tup))
                                            )
                                    )
                                )
                            )
                        )
                    end
                ), NONE, kcc (fn body => CPSAppSingle(CPSValueVar (CPSVarLocal typeNameRet), CPSValueVar body))))
            , NONE, kcc cc) (* cc is the continuation that expects the value of this builtin function *)


    fun transformRaise(cc : cpsvar -> cpscomputation) : cpscomputation = 
            clams2  (fn ((tpignore, exnVal), ret) => 
                    cpsGetCurrentExnHanlder(fn curHandler => 
                        CPSAppSingle(CPSValueVar curHandler, CPSValueVar (exnVal))
                    )
            ) (cc)
    fun transformHandle(cc : cpsvar -> cpscomputation) : cpscomputation = 
                (* Handler should always be AbsSingle *)
                clams1 (fn (tpignore, tpRet) => 
                    CPSAbs(kcc2' (fn tup => fn retVal(* the return value of the entire computation *) => 
                        (* retrieves the current exception handler *)
                        cpsGetCurrentExnHanlder (fn originalHandler => 
                            (* construct the function that resets the handler to original and return *)
                            CPSAbsSingle(kcc' 
                                (fn retValue => 
                                    cpsSetCurrentExnHandler (CPSValueVar originalHandler) (fn _ => 
                                        CPSAppSingle(CPSValueVar (CPSVarLocal retVal), CPSValueVar (CPSVarLocal retValue))
                                    )
                                ), NONE, kcc 
                                (fn resetHandlerAndReturn => 
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
                                    )
                                    )
                                )
                        )
                    )), NONE, kcc (fn innerFunc => tpRet innerFunc))
                ) cc


    fun transformPrimitiveBinaryOp
    (primitiveBinaryOp : cpsvalue * cpsvalue * cpscontinuation -> cpsprimitiveop)
    (cc : cpsvar -> cpscomputation) : cpscomputation = 
        CPSAbs(kcc2' (fn arg1 => fn retFunc => 
            CPSAbs(kcc2' (fn arg2 => fn retRes => 
                    CPSPrimitiveOp(primitiveBinaryOp(
                        CPSValueVar (CPSVarLocal arg1), CPSValueVar (CPSVarLocal arg2)
                    , kcc (fn res => 
                        CPSAppSingle(CPSValueVar (CPSVarLocal retRes), CPSValueVar res)
                    )))
            ), NONE, kcc (fn addFunc => 
                CPSAppSingle(CPSValueVar (CPSVarLocal retFunc), CPSValueVar addFunc)
            ))
        ), NONE, kcc cc)


    fun cpsTransformBuiltinFunc( x : BuiltinFunc) (cc : cpsvar -> cpscomputation) : cpscomputation = 
        case x of 
             BFCallCC => transformCallCC cc
            | BFNewDynClsfdValueWithString => transformNewDynClsfdValueWithString cc
            | BFRaise => transformRaise cc
            | BFHandle =>  transformHandle cc
            | BFIntSub => transformPrimitiveBinaryOp CPSPOpIntSub cc
            | BFIntEq => transformPrimitiveBinaryOp CPSPOpIntEq cc
            | BFIntGt => transformPrimitiveBinaryOp CPSPOpIntGt cc
            
end