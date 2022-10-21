structure LLVMConvert =
struct

open LLVMAst
open CPSAst
open CPSAstOps

fun zipCons (e1, e2) (a, b) = (e1@a, e2 @ b)

fun ::: (x, y) = zipCons x y

fun withNewLoc (f : int -> 'a) : 'a = f (UID.next())

infix 4 :::

fun llvmLocToValue (x : llvmlocation) : llvmvalue = case x of 
    LLVMLocationLocal i => LLVMLocalVar i
    | LLVMLocationGlobal i => LLVMGlobalVar i

fun genLLVM 
    (ctx as (freeVAddr, freeVL) : int  (* the address of the tuple of free vars*)
        * int list (* the list of free vars that is contained in the address *) 
        )
    (cpscomp : cpscomputation ) 
        : llvmdeclaration list * llvmstatement list  = 
let val recur = genLLVM ctx

    (* transform access will transform the access to 
    the record value if the record value is itself bound *)
    fun transformAccess (v : cpsvalue) : llvmstatement list * llvmlocation = 
    case v of 
        CPSValueVar(CPSVarGlobal v) =>
                        let val newName = UID.next()
                    in 
                        ([LLVMLoadGlobal(newName, v)],(LLVMLocationLocal newName))
                    end
        | CPSValueVar(CPSVarLocal v) => 
            (case ListSearchUtil.indexOf freeVL v of
                SOME idx =>
                let val newName = UID.next()
                    in 
                        ([LLVMArrayAccess(LLVMLocationLocal newName, (LLVMLocationLocal freeVAddr), idx)], (LLVMLocationLocal newName))
                    end
                | NONE => ([], (LLVMLocationLocal v)) (* bound locally, just use the name *)
            )
    fun vaccess (v : cpsvalue) (f : llvmlocation -> llvmstatement list) : llvmstatement list = 
        let val (decls, v') = transformAccess v
        in decls @(f v') end
    fun vaccess' (v : cpsvalue) (f : llvmlocation -> llvmdeclaration list * llvmstatement list) : llvmdeclaration list * llvmstatement list = 
        let val (decls, v') = transformAccess v
        in ([], decls) ::: (f v') end

    fun vaccessL (cpsvallist : cpsvalue list)  (f : llvmlocation list -> llvmstatement list) : llvmstatement list = 
        let 
            fun go acc remaining = case remaining of [] => f acc
                        | (v :: vs) => vaccess v (fn v' => go (acc@[v']) vs)
        in go [] cpsvallist
        end

(* TODO : Review use of this function ! 
why is there both vaccess and cpsVarToLLVMLoc?
The reason is that this is guaranteed to be a local variable that 
is bound by the continuation
And this is usually used for WRITING , while vaccess is used for reading
Another difference is that transformAccess transforms access of cpsvalue, while
this transforms access of cpsvar
*)
    fun cpsVarToLLVMLoc (x : cpsvar) : llvmlocation = 
        case x of
            CPSVarLocal i => LLVMLocationLocal i 
            | CPSVarGlobal i => LLVMLocationGlobal i
    
    fun registerFunctionNameToAddrMapping (fname : int) (addr : llvmlocation) = 
    (
        (* DebugPrint.p ("LLVM Loc Closure Mapping f" ^ Int.toString fname ^ " = " ^ (case addr of 
            LLVMLocationLocal i => ("l" ^ Int.toString i)
            | LLVMLocationGlobal i => ("g" ^ Int.toString i)
        ) ^ "\n"); *)
        ())

    fun compileFunctionClosure(funLoc : llvmlocation ) (args : int list) 
            (fvs : int list) (body : cpscomputation)
        (kont : cpscomputation) :  llvmdeclaration list * llvmstatement list  =
            let 
                val compiledFunctionName = UID.next()
                val _ = registerFunctionNameToAddrMapping compiledFunctionName funLoc
                val compiledFreeVarsAddr = UID.next()
                val freeVarsInBody = fvs
                val (decls, compiledBody) = genLLVM (
                (* important : first argument is always function name (guaranteed to be fresh (we don't have fix fun) *)
                (compiledFreeVarsAddr, compiledFunctionName::freeVarsInBody)
            ) body
            in
            ( (* declare top level function *)
                [LLVMFunction(compiledFunctionName, compiledFreeVarsAddr::args, compiledBody)]@decls, 
            (* represent the function as a closure *)
            (* then values for free variables *)
                vaccessL (map (fn x => CPSValueVar(CPSVarLocal x)) freeVarsInBody) (fn freeVarValues' => 
                [LLVMStoreArray(LLVMArrayTypeFunctionClosure, funLoc, 
                [LLVMFunctionName(compiledFunctionName, length args + 1)]@(map llvmLocToValue freeVarValues'))]
                )
            ) ::: recur kont
            end 
        
    fun compileFunctionCall(fClosureAddr: cpsvalue) (argumentNames : cpsvalue list) : llvmdeclaration list * llvmstatement list
        =
        let 
        val functionReg = UID.next()
        in
        ([], 
        vaccess fClosureAddr (fn realClosureAddr => 
            vaccessL argumentNames (fn realArgumentNames => 
                [LLVMArrayAccess(LLVMLocationLocal functionReg, realClosureAddr, 0),
                LLVMCall(LLVMLocationLocal functionReg, realClosureAddr::realArgumentNames)]
            )
        ) 
        )
        end


        fun testAndCompileCPSPattern (subject : llvmlocation) 
             (cpspattern : cpspattern) 
             (trueBlockName : int)
             (falseBlockName : int)
             (* location that stores whether we should proceed *)
              : llvmdeclaration list * llvmstatement list
              = 
        let
                        fun handleArgList (l : cpspattern list) (curIdx : int) 
                        (shouldShiftIndexWhenAccessingSubject : bool) (* curIdx is the current index of next pattern *)
                            : llvmdeclaration list * llvmstatement list= 
                                let
                                    val resultBoolLocation = LLVMLocationLocal (UID.next())
                                in
                                case l of 
                                [] => ([], [LLVMUnconditionalJump(trueBlockName)])
                                | (pat) :: rest => 
                                    let val nextBlockName = UID.next()
                                        val subsubjectLoc = UID.next()
                                        val (thisDecls, thisComps) = testAndCompileCPSPattern (LLVMLocationLocal subsubjectLoc) pat nextBlockName falseBlockName
                                        val (nextDecls, nextComps) = handleArgList rest (curIdx + 1) shouldShiftIndexWhenAccessingSubject
                                    in 
                                        (thisDecls @ nextDecls, [LLVMArrayAccess(LLVMLocationLocal subsubjectLoc, subject, curIdx+
                                            (if shouldShiftIndexWhenAccessingSubject then 1 else 0)
                                        )] @(thisComps)
                                        @[LLVMComment ("handleArgList at " ^ Int.toString curIdx)]
                                        @[LLVMBlock(nextBlockName, nextComps)])
                                    end
                                end
        in
              (* the reason for failure is that cc is invoked multiple times, 
              and thus multiple identical code pieces have been generated. 
              The solution is to fix this problem by generating only once *)
            case cpspattern of 
                CPSPatVar cpsvar => 
                let val resultBoolLocation = LLVMLocationLocal (UID.next())
                in
                    ([], [LLVMStoreLocal (cpsVarToLLVMLoc cpsvar, subject), 
                    LLVMUnconditionalJump(trueBlockName)
                    (* LLVMStoreBool(resultBoolLocation, true) *)
                    ])
                end
                | CPSPatBuiltin(v) => 
                    let val constantStoreLoc = LLVMLocationLocal (UID.next())
                    val cmpDest = LLVMLocationLocal (UID.next())
                    val subjectConvLoc = LLVMLocationLocal (UID.next())
                    in 
                    (case v of 
                    CPSBvInt i => ([], [ 
                            LLVMPrimitiveOp(LLVMPOpValueToInt(subjectConvLoc, llvmLocToValue subject)),
                            LLVMPrimitiveOp(LLVMPOpCmpEqInt(cmpDest, llvmLocToValue subjectConvLoc, LLVMIntConst i ))
                    ])
                    | CPSBvBool b => ([], [ 
                            LLVMPrimitiveOp(LLVMPOpValueToBool(subjectConvLoc, llvmLocToValue subject)),
                            LLVMPrimitiveOp(LLVMPOpCmpEqBool(cmpDest, llvmLocToValue subjectConvLoc, LLVMIntConst (if b then 1 else 0) ))
                    ])
                    | CPSBvString s => 
                    let val strLoc = UID.next() 
                    in
                        ([LLVMStringConstant(strLoc, s)
                        ], [ 
                            (* LLVMComment("Storing String " ^ UTF8String.toString s 
                            ^ PrettyPrint.show_source_range (UTF8String.getSourceRange s "llvmc183")), *)
                            LLVMStoreString(constantStoreLoc, (strLoc, s)), 
                            LLVMPrimitiveOp(LLVMPOpCmpEqString(cmpDest, llvmLocToValue subject, llvmLocToValue (constantStoreLoc) ))
                        ])
                    end
                    | _ => raise Fail "llvmconv159"
                    ):::([], 
                        [
                            LLVMConditionalJumpBinary(cmpDest,
                                [LLVMUnconditionalJump(trueBlockName) ],
                                [LLVMUnconditionalJump(falseBlockName) ]
                            )
                        ]
                    )
                    end
                | CPSPatTuple(arglist) => 
                let 
                in
                    handleArgList arglist 0 false
                end
                | CPSPatHeadSpine(cid, arglist) => 
                (
                    let 
                        val indexLoc = UID.next()
                        val indexValLoc = UID.next()
                        val cmpBoolLoc = UID.next()
                        (* val argResultBool = List.tabulate(length arglist, fn _ => (UID.next()))
                        val reductionBoolLocs = List.tabulate(length arglist+1, fn _ => (UID.next())) *)
                        (* first in reduction always true, result in last *)
                        val (trueDecls, trueComps) = handleArgList arglist 0 true
                    in
                            (trueDecls, [
                            LLVMArrayAccess(LLVMLocationLocal indexLoc, subject, 0) (* store the index *), 
                            LLVMPrimitiveOp(LLVMPOpValueToInt(LLVMLocationLocal indexValLoc, LLVMLocalVar indexLoc)),
                            LLVMPrimitiveOp(LLVMPOpCmpEqInt(LLVMLocationLocal cmpBoolLoc, LLVMLocalVar indexValLoc, LLVMIntConst cid)), 
                            LLVMComment "(pattern matching) jumping based on the whether constructor id matches the pattern",
                            LLVMConditionalJumpBinary(LLVMLocationLocal cmpBoolLoc, 
                                (* get each argument and collect the result *)
                                (
                                    trueComps
                                )
                                ,
                                (let
                                    (* val resultBoolLocation = LLVMLocationLocal (UID.next()) *)
                                in 
                                    (* [LLVMStoreBool(resultBoolLocation, false)]@(cc resultBoolLocation) exit, not equal *)
                                    [LLVMUnconditionalJump(falseBlockName)]
                                end)
                            )
                            ])
                    end
                )
        end

    fun compilePrimitiveOp(cpspop : cpsprimitiveop) : llvmdeclaration list * llvmstatement list = 
        let fun vaccessInt(v : cpsvalue) (accessed : llvmlocation -> llvmstatement list)  : llvmstatement list = 
            let val newLoc = LLVMLocationLocal (UID.next())
            in vaccess v (fn l => LLVMPrimitiveOp(LLVMPOpValueToInt(newLoc, llvmLocToValue l)) :: accessed newLoc)
            end
            val tempLoc1 = LLVMLocationLocal (UID.next())
            val tempLoc2 = LLVMLocationLocal (UID.next())
            val tempLoc3 = LLVMLocationLocal (UID.next())
            val tempLoc4 = LLVMLocationLocal (UID.next())
        in
        case cpspop of
            CPSPOpIntEq (i1, i2, (i, k)) => ([], vaccessInt i1 (fn ai1 => 
                vaccessInt i2 (fn ai2 => 
                    [LLVMPrimitiveOp(LLVMPOpCmpEqInt(tempLoc1, llvmLocToValue ai1, llvmLocToValue ai2)),
                    LLVMPrimitiveOp(LLVMPOpBoolToValue(cpsVarToLLVMLoc i, llvmLocToValue tempLoc1))]
                )
            )) ::: recur k
            | CPSPOpIntGt (i1, i2, (i, k)) => ([], vaccessInt i1 (fn ai1 => 
                vaccessInt i2 (fn ai2 => 
                    [LLVMPrimitiveOp(LLVMPOpCmpGtInt(tempLoc1, llvmLocToValue ai1, llvmLocToValue ai2)),
                    LLVMPrimitiveOp(LLVMPOpBoolToValue(cpsVarToLLVMLoc i, llvmLocToValue tempLoc1))]
                )
            )) ::: recur k
            | CPSPOpIntSub (i1, i2, (i, k)) => ([], vaccessInt i1 (fn ai1 => 
                vaccessInt i2 (fn ai2 => 
                    [LLVMPrimitiveOp(LLVMPOpIntSub(tempLoc1, llvmLocToValue ai1, llvmLocToValue ai2)),
                    LLVMPrimitiveOp(LLVMPOpIntToValue(cpsVarToLLVMLoc i, llvmLocToValue tempLoc1))]
                )
            )) ::: recur k
        end
in

        case cpscomp of
            CPSUnit((k, comp)) => ([], [LLVMStoreUnit (cpsVarToLLVMLoc k)]) ::: recur comp
            | CPSTuple(l, (t, k)) => ([], vaccessL l (fn l' => [LLVMStoreArray(LLVMArrayTypeProd, (cpsVarToLLVMLoc t), map llvmLocToValue l')])) ::: recur k
            | CPSProj(v, i, (t, k)) => ([], vaccess v (fn v' => [LLVMArrayAccess((cpsVarToLLVMLoc t),v',i)])) ::: recur k
            | CPSInj(label, index, value, (v, k)) => 
                let val labelLoc = UID.next()
                in ([LLVMStringConstant(labelLoc, label)],  (* TODO FIX BUG*)
                vaccess value (fn value' => [LLVMStoreArray(LLVMArrayTypeSum,(cpsVarToLLVMLoc v),[LLVMIntConst index, LLVMStringName (labelLoc, label), llvmLocToValue value'])])) ::: recur k
                end

            | CPSIfThenElse(v, tcase, fcase) => 
            let val (declt, tcomps) = recur tcase
                val (declf, fcomps) = recur fcase
                val i1loc = LLVMLocationLocal (UID.next())
            in
                (declt @ declf, vaccess v (fn v' => [
                    LLVMPrimitiveOp(LLVMPOpValueToBool(i1loc, llvmLocToValue v')),
                    LLVMComment "CPS Converting If Then Else",
                    LLVMConditionalJumpBinary(i1loc, tcomps, fcomps)]))
            end
            | CPSSimpleCases(v, vkl) => 
                let val indexLoc = UID.next()
                val recurResult = map (fn (index, arglist, k) => 
                ([], vaccess v (fn accessedv => 
                List.tabulate(length arglist, fn i => 
                    LLVMArrayAccess((cpsVarToLLVMLoc (List.nth(arglist, i))),accessedv,i+1) (* first location stores in the index of the constructor
                    and the rest stores the arguments *)
                )))::: recur k) vkl
                val recurComps = ListPair.mapEq (fn ((idx, arglist, k), x) => (idx, #2 x)) (vkl, recurResult)
                val recurDecls = List.concat (map (fn x => #1 x) recurResult)
                in (recurDecls, 
                    vaccess v (fn v' => [LLVMArrayAccess(LLVMLocationLocal indexLoc,v',0)])
                    @ [LLVMConditionalJump(indexLoc,recurComps)]
                    )
                end
            | CPSCases(v, cases) => 
                    let
                        (* val decls = ref [] *)
                        (* ds the next case entry for all cases *)
                        (* contains all cases entries for all cases except the first, including the last spurious one *)
                        val nextBlockNames = List.tabulate (length cases, fn _ => UID.next()) 
                        fun compileCases i : (llvmdeclaration list * llvmstatement list) = 
                        if i = length cases 
                        then([],
                            [LLVMComment "CPSCases258: run out of cases"
                            ]@
                                (vaccess v (fn v' => [
                                    LLVMComment "run out of patterns, throw exception",
                                    LLVMRaiseException(LLVMExceptionMatch v')])
                                )
                            )
                            
                        else (let val (pat, body) = List.nth(cases, i)
                                (* val currentBlockName = List.nth(blockNames, i) *)
                                val nextBlockName = List.nth(nextBlockNames, i)
                                val blockBodyName = UID.next()
                                in
                                    ([], [LLVMComment ("CPSCases269, in case at index " ^ Int.toString i)]):::
                                        ( vaccess' v (fn v' => testAndCompileCPSPattern v' pat 
                                        blockBodyName nextBlockName))
                                        :::
                                    (
                                            let 
                                                val (bodyDecls, bodyComps) = recur body
                                                (* val _ = decls := (!decls) @(bodyDecls)  TODO: fix the hack *)
                                                val (nextDecls, nextComps) = compileCases (i+1)
                                            in
                                                    (* LLVMComment "finished testing pattern, branching on whether test succeeded",
                                                    LLVMPrimitiveOp(LLVMPOpValueToBool(LLVMLocationLocal i1rboolLoc, LLVMLocalVar rboolLoc)), 
                                                    LLVMConditionalJumpBinary(LLVMLocationLocal i1rboolLoc,  *)
                                                (bodyDecls@nextDecls, [ LLVMBlock(blockBodyName, bodyComps)]@
                                                 [LLVMBlock(nextBlockName, nextComps)]) (* next comps will have a block name *)
                                            end)
                                        end
                                        (* | _ => raise Fail "llvm256" TODO: conditional jump should take i64* *)
                                        )
                    in
                    (compileCases 0)
                    end
            | CPSFold(v, (t, k)) => ([], vaccess v (fn v' => [LLVMStoreArray(LLVMArrayTypeFold, (cpsVarToLLVMLoc t),[llvmLocToValue v'])])) ::: recur k
            | CPSUnfold(v, (t, k)) => ([], vaccess v (fn v' => [LLVMArrayAccess((cpsVarToLLVMLoc t),v',0)])) ::: recur k
            | CPSAbs((i,ak, c), SOME fvs, (t,k)) => 
                compileFunctionClosure (cpsVarToLLVMLoc t) ([i, ak]) fvs c k
            | CPSAbs((i,ak, c), NONE, (t,k)) => 
                raise Fail "you forgot to perform closure conversion"
            | CPSApp(a, (b, c)) => compileFunctionCall a [b,c]
            | CPSAppSingle (a,b)=> compileFunctionCall a [b]
            | CPSAbsSingle((i, c), SOME fvs, (t,k)) => 
                compileFunctionClosure (cpsVarToLLVMLoc t) [i] fvs c k
            | CPSAbsSingle((i, c), NONE, (t,k)) => 
                raise Fail "you forgot to perform closure conversion"
            | CPSDone (CPSValueVar i) (* signals return *) => ([], [LLVMReturn (cpsVarToLLVMLoc i)])
            | CPSFfiCCall (fname, args, (t, k)) =>
                ([
                    LLVMFfiFunction(fname, length args)
                ],
                vaccessL args (fn args' => [LLVMFfiCCall((cpsVarToLLVMLoc t), fname, map llvmLocToValue args')])) ::: recur k
            | CPSBuiltinValue(CPSBvString s, (t,k)) => 
            let val stringName = UID.next()
            in (
                [LLVMStringConstant(stringName, s)], [
                    (* LLVMStoreArray(LLVMArrayTypeString, (cpsVarToLLVMLoc t), [LLVMStringName (stringName, s)]), *)
                    LLVMStoreString((cpsVarToLLVMLoc t), ((stringName, s)))
                ](* TODO: I think this is erroneous as k will assume t to be a local variable, but it is actually a string constant! *)
            ) ::: recur k
            end
            | CPSBuiltinValue(CPSBvInt i, (t,k)) => 
            let 
            (* val name = UID.next() *)
            in (
                [
                    (* LLVMIntConstant(name, i) *)
                ], [
                    LLVMStoreInt((cpsVarToLLVMLoc t), i)
                ]
            ) ::: recur k
            end
            | CPSBuiltinValue(CPSBvReal r, (t,k)) => 
            let
            in ( [ ], [ LLVMStoreReal((cpsVarToLLVMLoc t), r) ]
            ) ::: recur k
            end
            | CPSBuiltinValue(CPSBvBool b, (t,k)) => 
            let
            in ( [ ], [ LLVMStoreBool((cpsVarToLLVMLoc t), b) ]
            ) ::: recur k
            end
            | CPSStore(CPSVarGlobal g, src, cc) => ([LLVMGlobalVariableDecl g], vaccess src (fn i => [LLVMStoreGlobal(g, llvmLocToValue i)])) ::: recur cc
            | CPSStore(_) => raise Fail "CPSStore must be storing to a global location"
            | CPSDynClsfdIn(s,id, v, (t, k)) => (
                [], 
                vaccess s (fn accessedStrName => 
                    vaccess v (fn accesssedValue => 
                        [LLVMStoreArray(LLVMArrayTypeDynClsfd, 
                            cpsVarToLLVMLoc t,
                                [ LLVMIntConst id,
                                    llvmLocToValue accessedStrName, 
                                    llvmLocToValue accesssedValue
                                ]
                            )
                        ]
                    )
                )
            ) ::: recur k
            | CPSDynClsfdMatch(v, (id, (a, c1)), c2) => 
                let 
                    val idStorageLoc = (UID.next())
                    val idDirectIntLoc = (UID.next())
                    val idEqStorageLoc = LLVMLocationLocal (UID.next())
                    val (c1decls, c1comp) = ([], vaccess v (fn llvmLocOfArr => 
                    [LLVMArrayAccess (cpsVarToLLVMLoc a, llvmLocOfArr, 2)]
                    )) ::: recur c1
                    val (c2decls, c2comp) = recur c2
                in 
                ((c1decls @ c2decls ), 
                vaccess v (fn llvmLocOfArr => 
                [LLVMArrayAccess(LLVMLocationLocal idStorageLoc, llvmLocOfArr, 0),
                (* This is a temporary hack: since all values are represented as int*, to get 
                a direct int for comparison, just convert *)
                LLVMPrimitiveOp(LLVMPOpValueToInt(LLVMLocationLocal idDirectIntLoc,  LLVMLocalVar idStorageLoc)),
                LLVMPrimitiveOp(LLVMPOpCmpEqInt(idEqStorageLoc, LLVMIntConst id, LLVMLocalVar idDirectIntLoc)),
                LLVMConditionalJumpBinary(idEqStorageLoc, 
                    c1comp, c2comp
                )
                ]
                )
                )
                end
            | CPSPrimitiveOp(cpspop) => compilePrimitiveOp cpspop

            (* | CPSSequence(l) => ([], [LLVMComment "sequence start"]) ::: (foldr (op:::) ([], [LLVMComment "sequence end"]) (map recur l)) *)
            (* | _ => raise Fail "not impl llvmconv 155" *)
end

fun removeGlobalVarDuplicates (s : llvmdeclaration list) : llvmdeclaration list = 
    case s of 
        [] => []
        | ((x as LLVMGlobalVariableDecl(i1)) :: xs) => x :: removeGlobalVarDuplicates (List.filter (fn y => 
                case y of 
                    LLVMGlobalVariableDecl(i2) => if i1 = i2 then false else true
                    | _ => true
            ) xs)
        | (y :: xs) => y :: removeGlobalVarDuplicates xs

fun removeFfiDuplicate(s : llvmdeclaration list) : llvmdeclaration list = 
    case s of 
        [] => []
        | ((x as LLVMFfiFunction(name, nargs)) :: xs) => x :: removeFfiDuplicate (List.filter (fn y => 
                case y of 
                    LLVMFfiFunction(name2, _) => if UTF8String.semanticEqual name name2 then false else true
                    | _ => true
            ) xs)
        | (y :: xs) => y :: removeFfiDuplicate xs

fun genLLVMSignatureTopLevel (cpscomp : cpscomputation ) :(
    llvmsignature) = 
    let val entryFuncName =  UID.next()
        val (decls, entryBody) = genLLVM (entryFuncName, []) cpscomp
        val removedDuplicateFfiDeclarations = removeFfiDuplicate decls
        val removedDuplicateGlobalVar = removeGlobalVarDuplicates removedDuplicateFfiDeclarations
    in (entryFuncName, [LLVMFunction(entryFuncName, [], entryBody)]@ removedDuplicateGlobalVar)
    end

end
