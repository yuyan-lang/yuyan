structure LLVMConvert =
struct

open LLVMAst
open CPSAst
open CPSAstOps

fun zipCons (e1, e2) (a, b) = (e1@a, e2 @ b)

fun ::: (x, y) = zipCons x y

infix 4 :::

fun genLLVM 
    (ctx as (freeVAddr, freeVL) : int  (* the address of the tuple of free vars*)
        * int list (* the list of free vars that is contained in the address *) 
        )
    (cpscomp : cpscomputation ) 
        : llvmdeclaration list * llvmstatement list  = 
let val recur = genLLVM ctx

(* transform access will transform the access to 
the record value if the record value is itself bound *)
fun transformAccess (CPSVar v) (f : int -> llvmstatement list) : llvmstatement list = 
case ListSearchUtil.indexOf freeVL v of
    SOME idx =>
    let val newName = UID.next()
        in 
            [LLVMArrayAccess(newName, freeVAddr, idx)]@(f newName)
        end
    | NONE => f v (* bound locally, just use the name *)
val vaccess = transformAccess

fun vaccessL (cpsvallist : cpsvalue list)  (f : int list -> llvmstatement list) : llvmstatement list = 
    let 
        fun go acc remaining = case remaining of [] => f acc
                    | (v :: vs) => vaccess v (fn v' => go (acc@[v']) vs)
    in go [] cpsvallist
    end

fun compileFunctionClosure(funLoc : int ) (args : int list) 
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
            vaccessL (map CPSVar freeVarsInBody) (fn freeVarValues' => 
            [LLVMStoreArray(LLVMArrayTypeFunctionClosure, funLoc, 
            [LLVMFunctionVar(compiledFunctionName, length args + 1)]@(map LLVMLocalVar freeVarValues'))]
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
            [LLVMArrayAccess(functionReg, realClosureAddr, 0),
            LLVMCall(functionReg, realClosureAddr::realArgumentNames)]
        )
    ) 
    )
    end
in

    case cpscomp of
        CPSUnit((k, comp)) => ([], [LLVMStoreUnit k]) ::: recur comp
        | CPSTuple(l, (t, k)) => ([], vaccessL l (fn l' => [LLVMStoreArray(LLVMArrayTypeProd, t, map LLVMLocalVar l')])) ::: recur k
        | CPSProj(v, i, (t, k)) => ([], vaccess v (fn v' => [LLVMArrayAccess(t,v',i)])) ::: recur k
        | CPSInj(label, index, value, (v, k)) => 
            let val labelLoc = UID.next()
            in ([LLVMStringConstant(labelLoc, label)],  (* TODO FIX BUG*)
            vaccess value (fn value' => [LLVMStoreArray(LLVMArrayTypeSum,v,[LLVMIntConst index, LLVMStringVar (labelLoc, label), LLVMLocalVar value'])])) ::: recur k
            end
        | CPSCases(v, vkl) => 
            let val indexLoc = UID.next()
            val recurResult = map (fn (v', k) => 
            ([], vaccess v (fn accessedv => [LLVMArrayAccess(v',accessedv,2)])):::
            recur k) vkl
            val recurComps = map (fn x => #2 x) recurResult
            val recurDecls = List.concat (map (fn x => #1 x) recurResult)
            in (recurDecls, 
                vaccess v (fn v' => [LLVMArrayAccess(indexLoc,v',0)])
                @ [LLVMConditionalJump(indexLoc,recurComps)]
                )
            end
        | CPSFold(v, (t, k)) => ([], vaccess v (fn v' => [LLVMStoreArray(LLVMArrayTypeFold, t,[LLVMLocalVar v'])])) ::: recur k
        | CPSUnfold(v, (t, k)) => ([], vaccess v (fn v' => [LLVMArrayAccess(t,v',0)])) ::: recur k
        | CPSAbs((i,ak, c), SOME fvs, (t,k)) => 
            compileFunctionClosure t [i, ak] fvs c k
        | CPSAbs((i,ak, c), NONE, (t,k)) => 
            raise Fail "you forgot to perform closure conversion"
        | CPSApp(a, (b, c)) => compileFunctionCall a [b,c]
        | CPSAppSingle (a,b)=> compileFunctionCall a [b]
        | CPSAbsSingle((i, c), SOME fvs, (t,k)) => 
            compileFunctionClosure t [i] fvs c k
        | CPSAbsSingle((i, c), NONE, (t,k)) => 
            raise Fail "you forgot to perform closure conversion"
        | CPSDone (CPSVar i) (* signals return *) => ([], [LLVMReturn i])
        | CPSBuiltinValue(CPSBvString s, (t,k)) => (
            [LLVMStringConstant(t, s)], [](* TODO: I think this is erroneous as k will assume t to be a local variable, but it is actually a string constant! *)
        ) ::: recur k
        | _ => raise Fail "not implemented yet"
end

fun genLLVMSignatureTopLevel (cpscomp : cpscomputation ) :(
    llvmsignature) = 
    let val entryFuncName =  UID.next()
        val (decls, entryBody) = genLLVM (entryFuncName, []) cpscomp
    in (entryFuncName, [LLVMFunction(entryFuncName, [], entryBody)]@decls)
    end

end
