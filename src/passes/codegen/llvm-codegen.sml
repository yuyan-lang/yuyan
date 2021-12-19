structure LLVMCodegen = struct




fun toLocalVar (i : int) = "%" ^ Int.toString i
fun toBlockNameJump (i : int) = "%" ^ Int.toString i
fun toFunctionName (i : int) = "@" ^ Int.toString i
fun toBlockNameLabel (i : int) = "" ^ Int.toString i

fun storeIntToLocalVar (localVar : int)(intValue : int)  : string list= 
let 
in
    [  toLocalVar localVar ^ " = call i32* @allocateArray(i32 1)", 
       "store i32 "^ Int.toString intValue ^", i32* " ^ toLocalVar localVar
    ]
end

fun storeIntArrayToLocalVar (localVar : int)(intValues : int list) (isFuncClosure : bool)  : string list= 
let 
    val num = length intValues
in
    [  toLocalVar localVar ^ " = call i32* @allocateArray(i32 " ^ Int.toString num ^")"
    ]
    @(List.concat (List.tabulate (num, fn index => 
    let val tempVar = UID.next()
    in 
    if index <> 0 orelse isFuncClosure <> true (* store regularly if we're not storing the function itself *)
    then
    [
        toLocalVar tempVar ^ " = getelementptr i32, i32* "^ toLocalVar localVar ^ ", i32 "^ Int.toString index,
       "store i32 "^ Int.toString (List.nth(intValues, index)) ^", i32* " ^ toLocalVar tempVar
    ]
    else
    [
        toLocalVar tempVar ^ " = getelementptr i32, i32* "^ toLocalVar localVar ^ ", i32 "^ Int.toString index,
       "store i32 "^ toFunctionName (List.nth(intValues, index)) ^", i32* " ^ toLocalVar tempVar
    ]
    end
    )))
end


fun derefArrayFrom (localVar : int )(arrptr : int)(index : int)  : string list= 
let val tempVar = UID.next()
in 
[
    toLocalVar tempVar ^ " = getelementptr i32, i32* "^ toLocalVar localVar ^ ", i32 "^ Int.toString index,
    toLocalVar localVar ^ "= load i32 , i32* " ^ toLocalVar tempVar
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
            in
            List.concat(
                [
                List.concat(
                    List.tabulate(num, fn i => let
                        val comparisonResultName = UID.next()
                        val currentBlockName = List.nth(blockNames, i)
                        val currentComparisonName = List.nth(comparisonNames, i)
                        val nextComparisonName = List.nth(comparisonNames, i+1)
                        in
                        [
                            toBlockNameLabel currentComparisonName ^ ":",
                            toLocalVar comparisonResultName ^ " = icmp eq i32 " ^ Int.toString i ^ " " ^ toLocalVar v,
                            "br i1 "^ toLocalVar comparisonResultName ^  
                            " " ^ toBlockNameJump currentBlockName ^ " " ^ toBlockNameJump nextComparisonName
                        ]
                        end
                    ))
                , 
                [
                    toBlockNameLabel (List.nth(comparisonNames, num))  ^ ":",
                    toLocalVar (UID.next()) ^ " = call i32 @internalError()"
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
        let val discard = UID.next()
        in
        [
            toLocalVar discard ^ " = call i32 " ^ toLocalVar fname ^ 
                "(" ^  String.concatWith ", " (map (fn arg => "i32 " ^ toLocalVar arg) args) ^ ")"
        ]
        end
        | LLVMReturn => ["!DONE (I Don't know how this is actually implemented"]
            


fun genLLVMDelcaration (d : llvmdeclaration ) : string list =
    case d of LLVMFunction (fname, args, body) => 
        ["define i32 "^ toFunctionName fname ^ "(" 
            ^ String.concatWith ", " (map (fn arg => "i32* "^ toLocalVar arg ) args)
            ^ ") {"
            ]@(List.concat (map genLLVMStatement body))@
            ["}"]
    | LLVMStringConstant(sname, s) => 
        let 
        val rawChars = String.explode (UTF8String.toString s)
        val ordinals = map (Char.ord) rawChars @[0]
        in 
        [toLocalVar sname ^ " = constant [ " ^ Int.toString (length  ordinals) ^ " x i8 ] <"
            ^ String.concatWith ", " (map (fn i => "i8 "^ Int.toString i) ordinals) ^ ">"]
        end
fun genLLVMSignature (s : llvmsignature)  : string list= List.concat (map genLLVMDelcaration s)


fun genLLVMSignatureWithMainFunction ((entryFunc,s) : int * llvmsignature)  : string list = 
    let val genSig = genLLVMSignature s
    val tempVar = UID.next()
    in 
        ["define i32 @main() {",
        toLocalVar tempVar ^ " =  call i32 " ^ toFunctionName entryFunc ^ "()",
        "ret i32 0",
        "}"]@genSig
end


end
