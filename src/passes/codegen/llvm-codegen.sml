structure LLVMCodegen = struct




fun toLocalVar (i : int) = "%v" ^ Int.toString i
fun toStringName (i : int) = "@s" ^ Int.toString i
fun toBlockNameJump (i : int) = "%b" ^ Int.toString i
fun toFunctionName (i : int) = "@f" ^ Int.toString i
fun toBlockNameLabel (i : int) = "b" ^ Int.toString i

fun storeIntToLocalVar (localVar : int)(intValue : int)  : string list= 
let 
in
    [  toLocalVar localVar ^ " = call i64* @allocateArray(i64 1)", 
       "store i64 "^ Int.toString intValue ^", i64* " ^ toLocalVar localVar
    ]
end

fun storeIntArrayToLocalVar (localVar : int)(intValues : int list) (isFuncClosure : bool)  : string list= 
let 
    val num = length intValues
in
    [  toLocalVar localVar ^ " = call i64* @allocateArray(i64 " ^ Int.toString num ^")"
    ]
    @(List.concat (List.tabulate (num, fn index => 
    let val tempVar = UID.next()
    in 
    if index <> 0 orelse isFuncClosure <> true (* store regularly if we're not storing the function itself *)
    then
    [
        toLocalVar tempVar ^ " = getelementptr i64, i64* "^ toLocalVar localVar ^ ", i64 "^ Int.toString index,
       "store i64 "^ Int.toString (List.nth(intValues, index)) ^", i64* " ^ toLocalVar tempVar
    ]
    else
    [
        toLocalVar tempVar ^ " = getelementptr i64, i64* "^ toLocalVar localVar ^ ", i64 "^ Int.toString index,
       "store i64 "^ toFunctionName (List.nth(intValues, index)) ^", i64* " ^ toLocalVar tempVar
    ]
    end
    )))
end


fun derefArrayFrom (localVar : int )(arrptr : int)(index : int)  : string list= 
let val tempVar = UID.next()
val beforeTypeCast = UID.next()
in 
[
    toLocalVar tempVar ^ " = getelementptr i64, i64* "^ toLocalVar arrptr ^ ", i64 "^ Int.toString index,
    toLocalVar beforeTypeCast ^ " = load i64, i64* " ^ toLocalVar tempVar,
    toLocalVar localVar ^ " = inttoptr i64 " ^ toLocalVar beforeTypeCast ^ " to i64*"
    (* casting everything to be a pointer to avoid typing conflict (I don't know whether is is sensible *)
]
end

open LLVMAst
fun genLLVMStatement (s : llvmstatement) : string list = 
    case s of   
        LLVMStoreUnit(v) => storeIntToLocalVar v 0
        | LLVMStoreArray(v, arr) => storeIntArrayToLocalVar v arr false
        | LLVMStoreFunctionClosure(v, arr) => storeIntArrayToLocalVar v arr true
        | LLVMArrayAccess(v, arrptr, idx) => derefArrayFrom v arrptr idx
        | LLVMConditionalJump(v, blocks) => 
            let 
            val num = length blocks
            val blockNames = List.tabulate (num, fn _ => UID.next()) 
            val comparisonNames = List.tabulate ((num+1), fn _ => UID.next())   (* the extra is to signal the end *)
            val realV = UID.next()
            in
            List.concat(
                [
                [toLocalVar realV ^ " = ptrtoint i64* " ^ toLocalVar v ^ " to i64"],
                    (* since the first subsequent instruction is a label, we need a ternimation instruction *)
                ["br label "^ (toBlockNameJump (hd comparisonNames))],
                List.concat(
                    List.tabulate(num, fn i => let
                        val comparisonResultName = UID.next()
                        val currentBlockName = List.nth(blockNames, i)
                        val currentComparisonName = List.nth(comparisonNames, i)
                        val nextComparisonName = List.nth(comparisonNames, i+1)
                        in
                        [
                            toBlockNameLabel currentComparisonName ^ ":",
                            toLocalVar comparisonResultName ^ " = icmp eq i64 " ^ Int.toString i ^ ", " ^ toLocalVar realV,
                            "br i1 "^ toLocalVar comparisonResultName ^  
                            ", label " ^ toBlockNameJump currentBlockName ^ ", label " ^ toBlockNameJump nextComparisonName
                        ]
                        end
                    ))
                , 
                [
                    toBlockNameLabel (List.nth(comparisonNames, num))  ^ ":",
                    toLocalVar (UID.next()) ^ " = call i64 @internalError()",
                    "br label " ^ toBlockNameJump (List.nth(comparisonNames, num))  
                    (* jump to self (no other things we can do) ,
                     assume internalError kills the process *)
                ]
                , 
                     List.concat(
                    List.tabulate(num, fn i => let
                        val currentBlockName = List.nth(blockNames, i)
                        val currentBlock= List.nth(blocks, i)
                        in
                        [
                            toBlockNameLabel currentBlockName ^ ":"
                        ]@(List.concat (map genLLVMStatement currentBlock))
                        end
                    ))
                ]
            )
            end
        | LLVMCall(fname, args) => 
        let val castedFname = UID.next()
            val discard = UID.next()
            val ftype = "i64 (" ^ String.concatWith ", " (map (fn _ => "i64*") args) ^ ")"
        in
        [
            toLocalVar castedFname ^ " = bitcast i64* " ^ toLocalVar fname  ^ " to " ^ ftype ^ "*",
            toLocalVar discard ^ " = call i64 " ^ toLocalVar castedFname ^ 
                "(" ^  String.concatWith ", " (map (fn arg => "i64* " ^ toLocalVar arg) args) ^ ")",
            (* assumed to terminate after call *)
            "ret i64 " ^ toLocalVar discard
        ]
        end
        | LLVMReturn => ["ret i64 0"]
            


fun genLLVMDelcaration (d : llvmdeclaration ) : string list =
    case d of LLVMFunction (fname, args, body) => 
        ["define i64 "^ toFunctionName fname ^ "(" 
            ^ String.concatWith ", " (map (fn arg => "i64* "^ toLocalVar arg ) args)
            ^ ") {"
            ]@(List.concat (map genLLVMStatement body))@
            ["}"]
    | LLVMStringConstant(sname, s) => 
        let 
        val rawChars = String.explode (UTF8String.toString s)
        val ordinals = map (Char.ord) rawChars @[0]
        in 
        [toStringName sname ^ " = constant [" ^ Int.toString (length  ordinals) ^ " x i8] ["
            ^ String.concatWith ", " (map (fn i => "i8 "^ Int.toString i) ordinals) ^ "]"]
        end
fun genLLVMSignature (s : llvmsignature)  : string list= List.concat (map genLLVMDelcaration s)


fun genLLVMSignatureWithMainFunction ((entryFunc,s) : int * llvmsignature)  : string list = 
    let val genSig = genLLVMSignature s
    val tempVar = UID.next()
    in 
        [ (* generate main function *)
        "define i64 @main() {",
        toLocalVar tempVar ^ " =  call i64 " ^ toFunctionName entryFunc ^ "()",
        "ret i64 0",
        "}",
        (* declare runtime functions *)
        "declare i64* @allocateArray(i64)",
        "declare i64 @internalError()"
        ]@genSig
end


end
