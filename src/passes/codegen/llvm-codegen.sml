structure LLVMCodegen = struct



open LLVMAst

fun toLocalVar (i : int) = "%v" ^ Int.toString i
fun toGlobalVar (i : int) = "@v" ^ Int.toString i
fun toLLVMLoc (i : llvmlocation) = case i of 
    LLVMLocationLocal i => toLocalVar i
    | LLVMLocationGlobal i => toGlobalVar i
fun toStringName (i : int) = "@s" ^ Int.toString i
fun toIntName (i : int) = "@i" ^ Int.toString i
fun toRealName (i : int) = "@r" ^ Int.toString i
fun toBlockNameJump (i : int) = "%b" ^ Int.toString i
fun toFunctionName (i : int) = "@f" ^ Int.toString i
fun toBlockNameLabel (i : int) = "b" ^ Int.toString i

fun toLLVMValue (v : llvmvalue) = case v of
    LLVMLocalVar i => toLocalVar i
                | LLVMGlobalVar i => toGlobalVar i
                | LLVMStringName (i, _)=> toStringName i
                | LLVMFunctionName(i, _) => toFunctionName i
                | LLVMIntConst i => Int.toString i
                (* | LLVMIntName i => toIntName i
                | LLVMRealName i => toRealName i *)
fun toLLVMValueType (v : llvmvalue) = case v of
    LLVMIntConst i => "i64"
    | _ => "i64"

fun getIntRepresentationOfLLVMArrayType (t : llvmarraytype) = case t of
        LLVMArrayTypeFunctionClosure => 1
        | LLVMArrayTypeFold =>2
        | LLVMArrayTypeProd => 3
        | LLVMArrayTypeSum => 4
        | LLVMArrayTypeUnit => 5
        | LLVMArrayTypeString => 6
        | LLVMArrayTypeInt => 7
        | LLVMArrayTypeReal => 8
        | LLVMArrayTypeDynClsfd => 9

(* f takes the name of the thing (may be int) *)
fun convertValueToIntForStorage ( v : llvmvalue) (f : string -> string list) : string list = 
let 
    val tempVarName = UID.next()
    val tempVarName2 = UID.next()
    in
    case v of 
          LLVMLocalVar i =>  (* the local var is guaranteed to be a pointer *)
          [toLocalVar tempVarName ^ " = ptrtoint i64* " ^ toLocalVar i ^ " to i64 "] @ (f (toLocalVar tempVarName))
        | LLVMGlobalVar i =>  (* the local var is guaranteed to be a pointer *)
          [toLocalVar tempVarName ^ " = ptrtoint i64* " ^ toGlobalVar i ^ " to i64 "] @ (f (toLocalVar tempVarName))
        | LLVMStringName (i, s)=> 
          [toLocalVar tempVarName ^ " = ptrtoint [" ^ Int.toString (length (UTF8String.getBytes s) + 1) ^" x i8]* " ^ toStringName i ^ " to i64 " ] @ (f (toLocalVar tempVarName))
        | LLVMFunctionName(i, argLength) => 
          [toLocalVar tempVarName ^ " = ptrtoint i64 ("^ String.concatWith ", " (List.tabulate (argLength, fn _ => "i64*")) ^ ")* " 
          ^ toFunctionName i ^ " to i64 " ] @ (f (toLocalVar tempVarName))
        | LLVMIntConst i => f (Int.toString i)
        (* | LLVMIntName i => 
          [toLocalVar tempVarName ^ " = load i64, i64* " ^ 
          toIntName i] @ (f (toLocalVar tempVarName))
        | LLVMRealName i => 
          [toLocalVar tempVarName ^ " = load double, double* " ^ 
          toRealName i 
          , toLocalVar tempVarName2 ^ " = bitcast double " ^ toLocalVar tempVarName
          ^ " to i64 "
          ] @ (f (toLocalVar tempVarName2)) *)
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


fun storeIntToLLVMLoc  (llvmLoc : llvmlocation)(i : int )  : string list= 
    [toLLVMLoc llvmLoc ^ " = inttoptr i64 " ^ Int.toString i ^ " to i64*"]
fun storeRealToLLVMLoc  (llvmLoc : llvmlocation)(r : real)  : string list= 
let val name = UID.next()
in
    [toLocalVar name ^ " = bitcast double " ^ Real.fmt (StringCvt.FIX (SOME 60)) r ^ " to i64",
    toLLVMLoc llvmLoc ^ " = inttoptr i64 " ^ toLocalVar name ^ " to i64*"]
end
fun storeBoolToLLVMLoc  (llvmLoc : llvmlocation)(b : bool)  : string list= 
    [toLLVMLoc llvmLoc ^ " = inttoptr i1 " ^ (if b then "1" else "0") ^ " to i64*"]


fun storeArrayToLLVMLoc (arrType : llvmarraytype) (llvmLoc : llvmlocation)(values : llvmvalue list)  : string list= 
let 
    val num = length values
    val headerPointerVar = UID.next() 
    val headerPointerVarArr = UID.next() 
    (* naive attempt of storing compile time information for 
    use during runtime ,
    The header will be the first 64 bits of the allocated memory, 
    which consists of (Highest significant bits first):
    - 2 bits of useless information (to prevent bugs due to the sign bit)
    - 6 bits of typing infomration (indicate which type this belongs to)
    - 16 bits of the length (L) of the allocation block (which doesn't include the 
    header block itself) [This means that we can't store array of size greater than 1024]
    - the remaining L bits are to indicate which of the remaining blocks are pointers 
    to another allocated structure 1, 1 indicates true and 0 indicates false.
    if L > 62-22, then the words after entire block are used to store this information until we run out of the blocks 
        (so that the length of the header block is always 1)
    *)
     (* compute the header value *)
     val _ = if num > 65535 then raise Fail "not supported yet llvmcg 81" else ()
    val headerLength = (22 + num) div 62 + (if (22 + num) mod 62 = 0 then 0 else 1) (* only use the last 62 bits per block *)
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
    val paddingBitLength : IntInf.int= fromInt headerLength * fromInt 62 - fromInt num - 22
    fun getNumbers (remainingMarking : int list) (remainingHeaderLength : int) : string list = 
        if remainingHeaderLength = 1
        then (if fromInt (length remainingMarking) + paddingBitLength = 62 
            then 
        [toString ( 
         (markingBitsToInt remainingMarking * pow(fromInt 2 , toInt paddingBitLength))
        )]
        else if fromInt 22 + fromInt (length remainingMarking) + paddingBitLength = 62
        then
        [toString ( 
            firstFiveBits * pow(fromInt 2, toInt (62-6))
        +  lengthOfList * pow( fromInt 2, toInt (62 -22))
        + markingBitsToInt remainingMarking * pow(fromInt 2 , toInt paddingBitLength)
        )]
        else raise Fail ("Not Possible llvmcg 109, got remainingMarking length " ^ Int.toString (length remainingMarking) ^ " padding bit length " ^ Int.toString (toInt paddingBitLength)))
        else (if remainingHeaderLength = fromInt headerLength 
              then (* we're in the first block, and remaining header length <> 1 *)
                [toString ( 
                    firstFiveBits * pow(fromInt 2, toInt (62-6))
                +  lengthOfList * pow( fromInt 2, toInt (62 -22))
                + markingBitsToInt (List.take(remainingMarking, toInt (62 - 22)))
                )]@(getNumbers (List.drop(remainingMarking, toInt (62-22))) (remainingHeaderLength -1))
            else(* we're not in the first block, and remaining header length <> 1 *)
                [toString ( 
                 markingBitsToInt (List.take(remainingMarking, 62))
                )]@(getNumbers (List.drop(remainingMarking, 62)) (remainingHeaderLength -1))
            )
    val result = getNumbers markingBits (fromInt headerLength)
    in 
    (* DebugPrint.p ("first five bits " ^ toString firstFiveBits ^ " length " ^ toString lengthOfList ^ " remainingMarking " ^ toString
    (markingBitsToInt markingBits) ^ " result " ^ String.concatWith "," result ^" \n"); *)
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

        toLLVMLoc llvmLoc ^ " = call i64* @allocateArray(i64 " ^ Int.toString (num + 1) ^")"
          (* get the first block address and store*)
        , toLocalVar headerPointerVar ^ " = getelementptr i64, i64* "^ toLLVMLoc llvmLoc ^ ", i64 0"
        , "store i64 " ^ hd headerInfo ^ ", i64* "^ toLocalVar headerPointerVar
    ]
    @(List.concat (List.tabulate (headerLength - 1 , fn index => 
    let val tempVar = UID.next()
    in 
    [
        toLocalVar tempVar ^ " = getelementptr i64, i64* "^ toLLVMLoc llvmLoc ^ ", i64 "^ Int.toString (index+num+1)
    ]@(
       ["store i64 " ^ List.nth(headerInfo, index+1) ^", i64* " ^ toLocalVar tempVar])
    end
    )))
    @(List.concat (List.tabulate (num, fn index => 
    let val tempVar = UID.next()
    in 
    [
        toLocalVar tempVar ^ " = getelementptr i64, i64* "^ toLLVMLoc llvmLoc ^ ", i64 "^ Int.toString (index+1)
    ]@(
        convertValueToIntForStorage(List.nth(values, index)) (fn name => 
       ["store i64 "
       ^ name ^", i64* " ^ toLocalVar tempVar]
        )
    )
        
    end
    )))
end


fun derefArrayFrom (resultLoc : llvmlocation )(arrptr : llvmlocation)(index : int)  : string list= 
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
    toLocalVar hIntermediate1 ^ " = lshiftr i64 " ^ toLocalVar headerLengthName ^ ", " ^ Int.toString(62 - 22),
    toLocalVar hIntermediate2 ^ " = and i64 " ^ toLocalVar hIntermediate1 ^ ", 1023", *)
    toLocalVar tempVar ^ " = getelementptr i64, i64* "^ toLLVMLoc arrptr ^ ", i64 "^ Int.toString (index+1) (* skip header block*),
    toLocalVar beforeTypeCast ^ " = load i64, i64* " ^ toLocalVar tempVar,
    toLLVMLoc resultLoc ^ " = inttoptr i64 " ^ toLocalVar beforeTypeCast ^ " to i64*"
    (* casting everything to be a pointer to avoid typing conflict (I don't know whether is is sensible *)
]
end


fun genLLVMPrimitiveOp (p : llvmprimitiveop) : string list =
    case p of
        LLVMPOpCmpEqInt(r, i1, i2) => 
            [toLLVMLoc r ^ " = icmp eq i64 " ^ toLLVMValue i1 ^ ", " ^ toLLVMValue i2]
        | LLVMPOpIntSub(r, i1, i2) => 
            [toLLVMLoc r ^ " = sub i64 " ^ toLLVMValue i1 ^ ", " ^ toLLVMValue i2]
        | LLVMPOpValueToBool(r, i1) => 
            [toLLVMLoc r ^ " = ptrtoint i64* " ^ toLLVMValue i1 ^ " to i1"]
        | LLVMPOpBoolToValue(r, i1) => 
            [toLLVMLoc r ^ " = inttoptr i1 " ^ toLLVMValue i1 ^ " to i64*"]
        | LLVMPOpValueToInt(r, i1) => 
            [toLLVMLoc r ^ " = ptrtoint i64* " ^ toLLVMValue i1 ^ " to i64"]
        | LLVMPOpIntToValue(r, i1) => 
            [toLLVMLoc r ^ " = inttoptr i64 " ^ toLLVMValue i1 ^ " to i64*"]
        | LLVMPopBoolAnd(r, i1, i2) => 
            [toLLVMLoc r ^ " = and i1 " ^ toLLVMValue i1 ^ ", " ^ toLLVMValue i2]
        | LLVMPopBoolAndWithConversion(r, i1, i2) => 
        let val i1loc = UID.next()
            val i2loc = UID.next()
            val rloc = UID.next()
        in
            [ toLocalVar i1loc ^ " = ptrtoint i64* " ^ toLLVMValue i1 ^ " to i1", 
             toLocalVar i2loc ^ " = ptrtoint i64* " ^ toLLVMValue i2 ^ " to i1", 
            toLocalVar rloc ^ " = and i1 " ^ toLocalVar i1loc ^ ", " ^ toLocalVar i2loc, 
            toLLVMLoc r ^ " = inttoptr i1 " ^ toLocalVar rloc ^ " to i64* "
            ]
        end

fun genLLVMStatement (s : llvmstatement) : string list = 
    case s of   
        LLVMStoreUnit(v) => storeArrayToLLVMLoc LLVMArrayTypeUnit v []
        | LLVMStoreInt(v, i) => storeIntToLLVMLoc v i
        | LLVMStoreReal(v, r) => storeRealToLLVMLoc v r
        | LLVMStoreBool(v, b) => storeBoolToLLVMLoc v b
        | LLVMStoreArray(arrtype, v, arr) => storeArrayToLLVMLoc arrtype v arr
        | LLVMArrayAccess(v, arrptr, idx) => derefArrayFrom v arrptr idx
        | LLVMConditionalJump(v, blocks) => 
            let 
            val numblocks = length blocks
            val blockNames = List.tabulate (numblocks, fn _ => UID.next()) 
            val defaultBlockName = UID.next()
            val realV = UID.next()
            in
            List.concat(
                [
                [toLocalVar realV ^ " = ptrtoint i64* " ^ toLocalVar v ^ " to i64"],
                    (* since the first subsequent instruction is a label, we need a ternimation instruction *)
                ["switch i64 "
                ^ toLocalVar realV 
                ^ ", label " ^ toBlockNameJump defaultBlockName
                ^ " [ "
                ^ String.concatWith  " " (
                    List.tabulate(numblocks, fn i => let
                        val currentBlockName = List.nth(blockNames, i)
                        val (currentIndex, currentBlock) = List.nth(blocks, i)
                        in
                            (" i64 " ^ Int.toString currentIndex  ^
                            " ,  label " ^ toBlockNameJump currentBlockName
                            )
                        end
                    )
                )
                ^ " ] "
                ],
                [
                    toBlockNameLabel defaultBlockName  ^ ":",
                    toLocalVar (UID.next()) ^ " = call i64 @matchException("
                    ^ "i64 " ^ toLocalVar realV 
                    ^ ")",
                    "br label " ^ toBlockNameJump defaultBlockName  
                    (* jump to self (no other things we can do) ,
                     assume internalError kills the process, 
                     TODO: raise Match exception *)
                ]
                , 
                     List.concat(
                    List.tabulate(numblocks, fn i => let
                        val currentBlockName = List.nth(blockNames, i)
                        val (currentIndex, currentBlock) = List.nth(blocks, i)
                        in
                        [
                            toBlockNameLabel currentBlockName ^ ":"
                        ]@(List.concat (map genLLVMStatement currentBlock))
                        end
                    ))
                ]
            )
            end
        | LLVMConditionalJumpBinary(b, tb, fb) => 
        let val tLabelName = UID.next()
        val fLabelName = UID.next()
        in
            ["br i1 " ^ toLLVMLoc b ^ 
            ", label " ^ toBlockNameJump tLabelName  ^
            ", label " ^ toBlockNameJump fLabelName ]
            @[
            toBlockNameLabel tLabelName ^ ":"
            ]@ (List.concat (map genLLVMStatement tb))
            @[
            toBlockNameLabel fLabelName ^ ":"
            ]@ (List.concat (map genLLVMStatement fb))
        end
        | LLVMBlock(name, stmts) => 
            (
                [toBlockNameLabel name ^ ":"
                ]@
                (List.concat (map genLLVMStatement stmts))
            )
        | LLVMUnconditionalJump(name) => 
            (
                ["br label " ^ toBlockNameJump name]
            )
        | LLVMRaiseException(LLVMExceptionMatch v) => 
            let val defaultBlockName = UID.next()
            in
                [
                        "br label " ^ toBlockNameJump defaultBlockName, 
                        toBlockNameLabel defaultBlockName  ^ ":",
                        toLocalVar (UID.next()) ^ " = call i64 @matchException("
                        ^ "i64* " 
                        ^ toLLVMLoc v 
                        ^ ")",
                        "br label " ^ toBlockNameJump defaultBlockName  
                        (* jump to self (no other things we can do) ,
                        assume internalError kills the process, 
                        TODO: raise Match exception *)
                    ]
            end

        | LLVMCall(fname, args) => 
        let val castedFname = UID.next()
            val discard = UID.next()
            val ftype = "i64 (" ^ String.concatWith ", " (map (fn _ => "i64*") args) ^ ")"
        in
        [
            toLocalVar castedFname ^ " = bitcast i64* " ^ toLLVMLoc fname  ^ " to " ^ ftype ^ "*",
            toLocalVar discard ^ " = musttail call i64 " ^ toLocalVar castedFname ^ 
                "(" ^  String.concatWith ", " (map (fn arg => "i64* " ^ toLLVMLoc arg) args) ^ ")",
            (* assumed to terminate after call *)
            "ret i64 " ^ toLocalVar discard
        ]
        end
        | LLVMFfiCCall(resultLoc, fname, args) => 
        let 
        in
        [
            toLLVMLoc resultLoc ^ 
            " = call i64* @" ^ UTF8String.toString fname ^ 
                "(" ^  String.concatWith ", " (map (fn arg => "i64* " ^ toLLVMValue arg) args) ^ ")"
            (* do not terminate after call *)
        ]
        end
        | LLVMReturn i => let
        val tempName = (UID.next())
        in
        [ toLocalVar tempName ^ " = call i64 @informResult(i64* " ^ toLLVMLoc i^")",
          "ret i64 " ^ toLocalVar tempName
        ]
        end
        | LLVMLoadGlobal(dst, src) => let
            val tempName = (UID.next())
        in
        [
            (* toLocalVar tempName " = ptrtoint" bitcast"store i64 " ^ toLLVMValue src ^ ", i64* " ^ toLLVMLoc dst *)
            (* , *)
            toLocalVar tempName ^ " = load i64, i64* " ^ toGlobalVar src,
            toLocalVar dst ^ " = inttoptr i64 " ^ toLocalVar tempName ^ " to i64*"
            (* "ret i64 0" *)
        ]
        end
        | LLVMStoreGlobal(dst, src) => let
        in
        convertValueToIntForStorage src (fn name => 
        [
            (* toLocalVar tempName " = ptrtoint" bitcast"store i64 " ^ toLLVMValue src ^ ", i64* " ^ toLLVMLoc dst *)
            (* , *)
            "store i64 " ^ name ^ ", i64* " ^ toGlobalVar dst
            (* "ret i64 0" *)
        ]
        )
        end
        | LLVMStoreLocal(dst, src) => let 
                val temp = UID.next()
            in 
            [
                toLocalVar temp ^ " = ptrtoint i64* " ^ toLLVMLoc src ^ " to i64",
                toLLVMLoc dst ^ " = inttoptr i64 " ^ toLocalVar temp ^ " to i64*"
            ]
            end
        | LLVMComment s => 
        [
            ";" ^ s
        ]
        | LLVMPrimitiveOp(pop) => genLLVMPrimitiveOp pop


fun genLLVMDelcaration (d : llvmdeclaration ) : string list =
    case d of 
    LLVMFunction (fname, args, body) => 
        ["define i64 "^ toFunctionName fname ^ "(" 
            ^ String.concatWith ", " (map (fn arg => "i64* "^ toLocalVar arg ) args)
            ^ ") alwaysinline {"
            ]@(List.concat (map genLLVMStatement body))@
            ["}"]
    (* | LLVMIntConstant(name, i) => 
        let 
        in 
        [toIntName name ^ " = constant i64 "  ^ Int.toString i]
        end
    | LLVMRealConstant(name, i) => 
        let 
        in 
        [toRealName name ^ " = constant double "  ^ Real.fmt (StringCvt.FIX (SOME 60)) i]
        end *)
    | LLVMStringConstant(sname, s) => 
        let 
        val rawChars = UTF8String.getBytes s
        val ordinals = map (Char.ord) rawChars @[0]
        in 
        [toStringName sname ^ " = constant [" ^ Int.toString (length  ordinals) ^ " x i8] ["
            ^ String.concatWith ", " (map (fn i => "i8 "^ Int.toString i) ordinals) ^ "]"]
        end
    | LLVMFfiFunction(fname, numberOfArgs) => 
        let 
        in 
        ["declare i64* @" ^ UTF8String.toString fname 
            ^ "("
             ^ String.concatWith ", " (List.tabulate ((numberOfArgs),(fn i => "i64*")))
            ^ ")"]
        end
    | LLVMGlobalVariableDecl(i) => 
        let 
        in 
        [toGlobalVar i ^ " = global i64 0"]
        end


fun genLLVMSignatureWithMainFunction ((entryFunc,s) : llvmsignature)  : string list = 
    let val genSig = List.concat (map genLLVMDelcaration s)
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
        "declare i64 @matchException(i64*)",
        "declare i64 @informResult(i64*)"
        ]@genSig
end


end
