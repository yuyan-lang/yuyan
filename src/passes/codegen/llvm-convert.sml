structure LLVMConvert =
struct

open LLVMAst
open CPSAst
open CPSAstOps

fun zipCons (e1, e2) (a, b) = (e1@a, e2 @ b)

fun ::: (x, y) = zipCons x y

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
    fun transformAccess (CPSValueVar v) (f : llvmlocation -> llvmstatement list) : llvmstatement list = 
    case v of 
        CPSVarGlobal v => f (LLVMLocationGlobal v)
        | CPSVarLocal v => 
            (case ListSearchUtil.indexOf freeVL v of
                SOME idx =>
                let val newName = UID.next()
                    in 
                        [LLVMArrayAccess(LLVMLocationLocal newName, (LLVMLocationLocal freeVAddr), idx)]@(f (LLVMLocationLocal newName))
                    end
                | NONE => f (LLVMLocationLocal v) (* bound locally, just use the name *)
            )
    val vaccess = transformAccess

    fun vaccessL (cpsvallist : cpsvalue list)  (f : llvmlocation list -> llvmstatement list) : llvmstatement list = 
        let 
            fun go acc remaining = case remaining of [] => f acc
                        | (v :: vs) => vaccess v (fn v' => go (acc@[v']) vs)
        in go [] cpsvallist
        end

    fun cpsVarToLLVMLoc (x : cpsvar) : llvmlocation = 
        case x of
            CPSVarLocal i => LLVMLocationLocal i 
            | CPSVarGlobal i => LLVMLocationGlobal i

    fun compileFunctionClosure(funLoc : llvmlocation ) (args : int list) 
            (fvs : int list) (body : cpscomputation)
        (kont : cpscomputation) :  llvmdeclaration list * llvmstatement list  =
            let 
                val compiledFunctionName = UID.next()
                val compiledFreeVarsAddr = UID.next()
                (* val freeVarsInBody = IntSet.toList (remove (freeVars body) args) *)
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
            | CPSCases(v, vkl) => 
                let val indexLoc = UID.next()
                val recurResult = map (fn (v', k) => 
                ([], vaccess v (fn accessedv => [LLVMArrayAccess((cpsVarToLLVMLoc v'),accessedv,2)])):::
                recur k) vkl
                val recurComps = map (fn x => #2 x) recurResult
                val recurDecls = List.concat (map (fn x => #1 x) recurResult)
                in (recurDecls, 
                    vaccess v (fn v' => [LLVMArrayAccess(LLVMLocationLocal indexLoc,v',0)])
                    @ [LLVMConditionalJump(indexLoc,recurComps)]
                    )
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
                    LLVMStoreArray(LLVMArrayTypeString, (cpsVarToLLVMLoc t), [LLVMStringName (stringName, s)])
                ](* TODO: I think this is erroneous as k will assume t to be a local variable, but it is actually a string constant! *)
            ) ::: recur k
            end
            | CPSBuiltinValue(CPSBvInt i, (t,k)) => 
            let val name = UID.next()
            in (
                [LLVMIntConstant(name, i)], [
                    LLVMStoreArray(LLVMArrayTypeInt, (cpsVarToLLVMLoc t), [LLVMIntName name])
                ]
            ) ::: recur k
            end
            | CPSBuiltinValue(CPSBvReal r, (t,k)) => 
            let val name = UID.next()
            in (
                [LLVMRealConstant(name, r)], [
                    LLVMStoreArray(LLVMArrayTypeReal, (cpsVarToLLVMLoc t), [LLVMRealName name])
                ]
            ) ::: recur k
            end
            | CPSStore(dst, src) => ([], vaccess src (fn i => [LLVMStore(cpsVarToLLVMLoc dst, llvmLocToValue i)]))
            | CPSSequence(l) => ([], [LLVMComment "sequence start"]) ::: (foldr (op:::) ([], [LLVMComment "sequence end"]) (map recur l))
            | _ => raise Fail "not impl llvmconv 155"
end

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
    in (entryFuncName, [LLVMFunction(entryFuncName, [], entryBody)]@removedDuplicateFfiDeclarations)
    end

end
