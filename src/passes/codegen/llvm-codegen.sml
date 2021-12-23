structure LLVMCodegen = struct



open LLVMAst

fun toLocalVar (i : int) = "%v" ^ Int.toString i
fun toStringName (i : int) = "@s" ^ Int.toString i
fun toBlockNameJump (i : int) = "%b" ^ Int.toString i
fun toFunctionName (i : int) = "@f" ^ Int.toString i
fun toBlockNameLabel (i : int) = "b" ^ Int.toString i

fun toLLVMValue (v : llvmvalue) = case v of
    LLVMLocalVar i => toLocalVar i
                | LLVMStringVar (i, _)=> toStringName i
                | LLVMFunctionVar(i, _) => toFunctionName i
                | LLVMIntConst i => Int.toString i
fun toLLVMValueType (v : llvmvalue) = case v of
    LLVMIntConst i => "i64"
    | _ => "i64"

fun getIntRepresentationOfLLVMArrayType (t : llvmarraytype) = case t of
        LLVMArrayTypeFunctionClosure => 1
        | LLVMArrayTypeFold =>2
        | LLVMArrayTypeProd => 3
        | LLVMArrayTypeSum => 4
        | LLVMArrayTypeUnit => 5

(* f takes the name of the thing (may be int) *)
fun convertValueToIntForStorage ( v : llvmvalue) (f : string -> string list) : string list = 
let 
    val tempVarName = UID.next()
    in
    case v of 
          LLVMLocalVar i =>  (* the local var is guaranteed to be a pointer *)
          [toLocalVar tempVarName ^ " = ptrtoint i64* " ^ toLocalVar i ^ " to i64 "] @ (f (toLocalVar tempVarName))
        | LLVMStringVar (i, s)=> 
          [toLocalVar tempVarName ^ " = ptrtoint [" ^ Int.toString (length (UTF8String.getBytes s) + 1) ^" x i8]* " ^ toStringName i ^ " to i64 " ] @ (f (toLocalVar tempVarName))
        | LLVMFunctionVar(i, argLength) => 
          [toLocalVar tempVarName ^ " = ptrtoint i64 ("^ String.concatWith ", " (List.tabulate (argLength, fn _ => "i64*")) ^ ")* " 
          ^ toFunctionName i ^ " to i64 " ] @ (f (toLocalVar tempVarName))
        | LLVMIntConst i => f (Int.toString i)
    end

fun storeIntToLocalVar (localVar : int)(intValue : int)  : string list= 
let 
in
    [  toLocalVar localVar ^ " = call i64* @allocateArray(i64 1)", 
       "store i64 "^ Int.toString intValue ^", i64* " ^ toLocalVar localVar
    ]
end

(* fun intToBitString (a : int) (length : int) : char list = 
    if length = 0 
    then []
    else  (intToBitString (a div 2) (length -1))@[
        if a mod 2 = 0 then #"0"  else #"1"
    ] *)



fun storeArrayToLocalVar (arrType : llvmarraytype) (localVar : int)(values : llvmvalue list)  : string list= 
let 
    val num = length values
    val headerPointerVar = UID.next() 
    val headerPointerVarArr = UID.next() 
    (* naive attempt of storing compile time information for 
    use during runtime ,
    The header will be the first 64 bits of the allocated memory, 
    which consists of (Highest significant bits first):
    - 2 bits of useless information (to prevent bugs due to the sign bit)
    - 5 bits of typing infomration (indicate which type this belongs to)
    - 10 bits of the length (L) of the allocation block (which doesn't include the 
    header block itself) [This means that we can't store array of size greater than 1024]
    - the remaining L bits are to indicate which of the remaining blocks are pointers 
    to another allocated structure 1, 1 indicates true and 0 indicates false.
    if L > 64-17, then the words after entire block are used to store this information until we run out of the blocks 
        (so that the length of the header block is always 1)
    *)
     (* compute the header value *)
     val _ = if num > 1023 then raise Fail "not supported yet llvmcg 81" else ()
    val headerLength = (15 + num) div 62 + (if (15 + num) mod 62 = 0 then 0 else 1) (* only use the last 62 bits per block *)
     val headerInfo = let
    open IntInf
    val firstFiveBits : IntInf.int = IntInf.fromInt (getIntRepresentationOfLLVMArrayType arrType)
    val lengthOfList : IntInf.int = IntInf.fromInt num
    val markingBits : IntInf.int list = map (fn (x ) => 
            case x of 
             LLVMIntConst _ => 0
             | _ =>  1
        )  values
    fun markingBitsToInt (bits : IntInf.int list) : IntInf.int = foldl (fn (x, acc) => 
            acc * 2 + x
        ) 0 bits
    val paddingBitLength : IntInf.int= fromInt headerLength * fromInt 62 - fromInt num - 15
    fun getNumbers (remainingMarking : int list) (remainingHeaderLength : int) : string list = 
        if remainingHeaderLength = 1
        then (if fromInt (length remainingMarking) + paddingBitLength = 62 
            then 
        [toString ( 
         (markingBitsToInt remainingMarking * pow(fromInt 2 , toInt paddingBitLength))
        )]
        else if fromInt 15 + fromInt (length remainingMarking) + paddingBitLength = 62
        then
        [toString ( 
            firstFiveBits * pow(fromInt 2, toInt (64-7))
        +  lengthOfList * pow( fromInt 2, toInt (64 -17))
        + markingBitsToInt remainingMarking * pow(fromInt 2 , toInt paddingBitLength)
        )]
        else raise Fail ("Not Possible llvmcg 109, got remainingMarking length " ^ Int.toString (length remainingMarking) ^ " padding bit length " ^ Int.toString (toInt paddingBitLength)))
        else (if remainingHeaderLength = fromInt headerLength 
              then (* we're in the first block, and remaining header length <> 1 *)
                [toString ( 
                    firstFiveBits * pow(fromInt 2, toInt (64-7))
                +  lengthOfList * pow( fromInt 2, toInt (64 -17))
                + markingBitsToInt (List.take(remainingMarking, toInt (62 - 15)))
                )]@(getNumbers (List.drop(remainingMarking, toInt (62-15))) (remainingHeaderLength -1))
            else(* we're not in the first block, and remaining header length <> 1 *)
                [toString ( 
                 markingBitsToInt (List.take(remainingMarking, 62))
                )]@(getNumbers (List.drop(remainingMarking, 62)) (remainingHeaderLength -1))
            )
    val result = getNumbers markingBits (fromInt headerLength)
    in 
    DebugPrint.p ("first five bits " ^ toString firstFiveBits ^ " length " ^ toString lengthOfList ^ " remainingMarking " ^ toString
    (markingBitsToInt markingBits) ^ " result " ^ String.concatWith "," result ^" \n");
    result
    end
    val headerArrType = "["  ^ Int.toString headerLength ^" x i64]"
in
    (* perform the header computation directly *)
    [
        (* toLocalVar localVar ^ " = call i64* @allocateArray(i64 " ^ Int.toString (num + headerLength) ^")"
          (* get the first block address and store*)
        , toLocalVar headerPointerVar ^ " = getelementptr i64, i64* "^ toLocalVar localVar ^ ", i64 0"
        , toLocalVar headerPointerVarArr ^ " = bitcast i64* " ^ toLocalVar headerPointerVar ^ " to " ^ headerArrType ^ "*"
        , "store " ^ headerArrType ^ " [" ^ String.concatWith ", " 
            (map(fn i => "i64 "^ i) headerInfo) ^ "], " ^ headerArrType ^ "* "^ toLocalVar headerPointerVarArr *)

        toLocalVar localVar ^ " = call i64* @allocateArray(i64 " ^ Int.toString (num + 1) ^")"
          (* get the first block address and store*)
        , toLocalVar headerPointerVar ^ " = getelementptr i64, i64* "^ toLocalVar localVar ^ ", i64 0"
        , "store i64 " ^ hd headerInfo ^ ", i64* "^ toLocalVar headerPointerVar
    ]
    @(List.concat (List.tabulate (headerLength - 1 , fn index => 
    let val tempVar = UID.next()
    in 
    [
        toLocalVar tempVar ^ " = getelementptr i64, i64* "^ toLocalVar localVar ^ ", i64 "^ Int.toString (index+num+1)
    ]@(
       ["store i64 " ^ List.nth(headerInfo, index+1) ^", i64* " ^ toLocalVar tempVar])
    end
    )))
    @(List.concat (List.tabulate (num, fn index => 
    let val tempVar = UID.next()
    in 
    [
        toLocalVar tempVar ^ " = getelementptr i64, i64* "^ toLocalVar localVar ^ ", i64 "^ Int.toString (index+1)
    ]@(
        convertValueToIntForStorage(List.nth(values, index)) (fn name => 
       ["store i64 "
       ^ name ^", i64* " ^ toLocalVar tempVar]
        )
    )
        
    end
    )))
end


fun derefArrayFrom (localVar : int )(arrptr : int)(index : int)  : string list= 
let val tempVar = UID.next()
val beforeTypeCast = UID.next()
(* val headerLengthPointer = UID.next()
val headerLengthName = UID.next()
val hIntermediate1 = UID.next()
val hIntermediate2 = UID.next()
val realIndexName = UID.next() *)
in 
[
    (* toLocalVar headerLengthPointer ^ " = getelementptr i64, i64* "^ toLocalVar arrptr ^ ", i64 "^ Int.toString 0 (* first header block*),
    toLocalVar headerLengthName ^ " = load i64, i64* " ^ toLocalVar headerLengthPointer,
    toLocalVar hIntermediate1 ^ " = lshiftr i64 " ^ toLocalVar headerLengthName ^ ", " ^ Int.toString(62 - 15),
    toLocalVar hIntermediate2 ^ " = and i64 " ^ toLocalVar hIntermediate1 ^ ", 1023", *)
    toLocalVar tempVar ^ " = getelementptr i64, i64* "^ toLocalVar arrptr ^ ", i64 "^ Int.toString (index+1) (* skip header block*),
    toLocalVar beforeTypeCast ^ " = load i64, i64* " ^ toLocalVar tempVar,
    toLocalVar localVar ^ " = inttoptr i64 " ^ toLocalVar beforeTypeCast ^ " to i64*"
    (* casting everything to be a pointer to avoid typing conflict (I don't know whether is is sensible *)
]
end

fun genLLVMStatement (s : llvmstatement) : string list = 
    case s of   
        LLVMStoreUnit(v) => storeArrayToLocalVar LLVMArrayTypeUnit v []
        | LLVMStoreArray(arrtype, v, arr) => storeArrayToLocalVar arrtype v arr
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
            toLocalVar discard ^ " = musttail call i64 " ^ toLocalVar castedFname ^ 
                "(" ^  String.concatWith ", " (map (fn arg => "i64* " ^ toLocalVar arg) args) ^ ")",
            (* assumed to terminate after call *)
            "ret i64 " ^ toLocalVar discard
        ]
        end
        | LLVMReturn i => let
        val tempName = (UID.next())
        in
        [ toLocalVar tempName ^ " = call i64 @informResult(i64* " ^ toLocalVar i^")",
          "ret i64 " ^ toLocalVar tempName
        ]
        end
            


fun genLLVMDelcaration (d : llvmdeclaration ) : string list =
    case d of LLVMFunction (fname, args, body) => 
        ["define i64 "^ toFunctionName fname ^ "(" 
            ^ String.concatWith ", " (map (fn arg => "i64* "^ toLocalVar arg ) args)
            ^ ") {"
            ]@(List.concat (map genLLVMStatement body))@
            ["}"]
    | LLVMStringConstant(sname, s) => 
        let 
        val rawChars = UTF8String.getBytes s
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
        "define i64 @entryMain() {",
        toLocalVar tempVar ^ " =  call i64 " ^ toFunctionName entryFunc ^ "()",
        "ret i64 "^ toLocalVar tempVar,
        "}",
        (* declare runtime functions *)
        "declare i64* @allocateArray(i64)",
        "declare i64 @internalError()",
        "declare i64 @informResult(i64*)"
        ]@genSig
end


end
