structure MixedStr =
struct

    open StaticErrorStructure
    type quoteinfo = UTF8Char.t * UTF8Char.t (* a pair of quotes, useful for reconstruction *)
    type endinginfo= UTF8Char.t option (* colon to mark the ending of the declaration *)
 (* should provide an adequate representation of brackets. No periods or brackets may be considered after this step *)
    datatype mixedchar = 
                   UnparsedExpression of mixedchar list * quoteinfo(* an unparsed expression has quotes on top level between quotes  *)
                  | UnparsedDeclaration of (mixedchar list * endinginfo) list  * quoteinfo(* an unparsed declaration has periods between quotes*)
                  | Name of UTF8String.t  * quoteinfo(* a name is the string between quotes that don't have periods or quotes *)
                  | Literal of UTF8String.t  * quoteinfo(* a literal is the string between double quotes *)
                  (* | ParsedExpression of Operators.OpAST (* a parsed expression *)
                  | ParsedDeclaration of TypeCheckingAST.Signature  *)
                  | SChar of UTF8Char.t (* top level characters , every thing else is quoted *)
    type mixedstr = mixedchar list
    type t = mixedstr

    val ~= = UTF8Char.~=
    infix 4 ~=

    val >>= = StaticErrorStructure.>>=
    infix 5 >>=
    infix 5 >>/=
    


    fun toUTF8StringChar(u : mixedchar) : UTF8String.t = 
    let 
        fun putQuoteAround (u : UTF8String.t)((ql, qr): quoteinfo) = 
            ql :: u @ [qr]
        (* fun singleQuoteAround (u : UTF8String.t) = 
            SpecialChars.leftSingleQuote :: u @ [SpecialChars.rightSingleQuote]
        fun doubleQuoteAround (u : UTF8String.t) = 
            SpecialChars.leftDoubleQuote :: u @ [SpecialChars.rightDoubleQuote] *)
    in
    case  u of 
    UnparsedExpression(s, q) => putQuoteAround (toUTF8String s) q
    | UnparsedDeclaration(l,q) => putQuoteAround (List.concat ( (map (fn (x, ed) => 
                            case ed of SOME ed => toUTF8String x @[ed]
                            | NONE => toUTF8String x) l))) q
    | Name (t, q) => putQuoteAround t q
    | Literal (t, q) => putQuoteAround t q
    (* | ParsedExpression e  => UTF8String.fromString "PARSED EXPR"
    | ParsedDeclaration d => UTF8String.fromString "PARSED SIG" *)
    | SChar t => [t]
    end

    and toUTF8String(u : mixedstr ) : UTF8String.t = List.concat (map toUTF8StringChar u)
    fun toString(u : mixedstr) : string = UTF8String.toString (toUTF8String u)

    fun  unmatchedParenthesisError(startChar : UTF8Char.t ) (scannedSoFar : mixedstr ) : 'a witherrsoption = 
        genSingletonError(startChar:: toUTF8String scannedSoFar) "未关闭的左括号" NONE
    fun  unmatchedStringLiteralError(startChar : UTF8Char.t ) (scannedSoFar : UTF8String.t ) : 'a witherrsoption = 
        genSingletonError((startChar::scannedSoFar)) "未关闭的左字符串引号" NONE

    exception StringNotPlain of mixedstr
    fun toPlainUTF8Char (u : mixedchar) : UTF8Char.t = 
        case u of 
            SChar s => s
            |_ => raise StringNotPlain [u]

(* only name and plain schar's are plain *)
    fun toPlainUTF8String (u : mixedstr) : UTF8String.t = 
        case u of 
            [] => []
            | [Name (s, t)] => s
            | _ => map toPlainUTF8Char u


            
    fun isChar (c : mixedchar) (ck : UTF8Char.t) : bool=
        case c of 
            SChar c' =>  c' ~= ck
            | _ => false
    fun getChar (c : mixedchar)  : UTF8Char.t=
        case c of 
            SChar c' =>  c'
            | _ => raise Fail "ms63"

    fun isPlainChar (c : mixedchar) : bool=
        case c of 
            SChar _ => true
            | _ => false
    fun isPlainStr (c : mixedstr) : bool=
        case c of 
            [] => true
            | (SChar _ :: xs) => isPlainStr xs
            | _ => false

    fun isPrefix(s1 : UTF8String.t) (s2 : mixedstr) : bool = 
    case (s1, s2) of
        ([], _) => true
        | ((x :: xs), (SChar y :: ys)) => if  x ~= y then isPrefix xs ys
                                          else false
        | _ => false


    fun containsCharTopLevel (ck : mixedstr) (c : UTF8Char.t) : bool =
        case ck of 
            [] => false
            | (SChar s::xs) => if  s ~= c  then true else containsCharTopLevel xs c
            | (_::xs) => containsCharTopLevel xs c
    fun containsAllCharsTopLevel (s : mixedstr ) (test : UTF8String.t) : bool
        = foldr (fn (x, acc) => (
            (* print ("acc is " ^ Bool.toString acc ^ " x is " ^ UTF8Char.toString (x) ^ " text is " ^ UTF8String.toString test ^ "\n"); *)
        acc andalso containsCharTopLevel s x)) true test

    fun stripTail (s : mixedstr) = List.take (s, List.length(s) -1)

    fun separateBy (sep : UTF8Char.t) (s : mixedstr)  (sofar : mixedstr) : (mixedstr * endinginfo) list = 
        case s of
            [] => (case sofar of [] => [] | _ => [(sofar, NONE)])
            | (x :: xs) => if isChar x sep 
                            then (sofar, SOME (getChar x)) :: separateBy sep xs []
                            else separateBy sep xs (sofar@[x])
    
    exception InternalFailure of mixedstr
    fun unSChar (s : mixedstr ) : UTF8String.t =
        case s of [] => []
        | (SChar x :: xs) => x :: unSChar xs
        | _ => (raise InternalFailure s)


    fun shouldSkip (s : UTF8Char.t) = List.exists (fn c => c ~= s) 
        [ SpecialChars.tab, SpecialChars.newline, SpecialChars.space]

    fun processDeclaration (p : mixedstr) : (mixedstr * endinginfo) list = 
    if length p = 0 then [] else
    let 
    (* val _ = (print ("processDeclaration "^ toString p ^"\n")) *)
        val res = (separateBy SpecialChars.period (p
            (* if isChar (List.last p) SpecialChars.period
            then stripTail p else p *)
        ) [])
        (* val _ = print ("result is of length " ^ Int.toString(length res)) *)
        in res end

    fun processSingleQuoted( p : mixedstr)(q as (ql, qr) : quoteinfo) : mixedchar witherrsoption = 
        if length p = 0 then genSingletonError ([ql, qr]) "名称不可为空" NONE
        else
        Success(
            if containsCharTopLevel p SpecialChars.period
            then (* process as declaration *)
                UnparsedDeclaration ((processDeclaration p), q)
            else if isPlainStr p
                then  (if containsCharTopLevel p SpecialChars.leftAngledBracket
                        orelse containsCharTopLevel p SpecialChars.rightAngledBracket
                        then UnparsedExpression(p , q)
                        else (* name *) Name ((unSChar p), q))
                else (* expression *) UnparsedExpression(p, q)
        )
            

    (* string escape two endDoubleQuote to escape double quote, else no escape *)
    fun scanLiteral(startChar : UTF8Char.t)(remaining : UTF8String.t)(sofar : UTF8String.t) : 
        (UTF8Char.t * UTF8String.t * UTF8String.t) witherrsoption
     = case (
         (* print (UTF8String.toString remaining^"\n"); *)
      remaining) of
        [] => unmatchedStringLiteralError startChar sofar
        | [x] => if x ~= SpecialChars.rightDoubleQuote
                 then Success(x, sofar, [])
                 else unmatchedStringLiteralError startChar (sofar @[x])
        | (x::y::xs) => if  x ~= SpecialChars.rightDoubleQuote 
                        andalso  y ~= SpecialChars.rightDoubleQuote
                 then scanLiteral startChar xs (sofar @[x]) (*escape*)
                 else if  x ~= SpecialChars.rightDoubleQuote
                      then Success(x, sofar, y::xs)
                      else scanLiteral startChar (y::xs) (sofar @[x])

    fun scanSingleQuote(startChar : UTF8Char.t) ( remaining : UTF8String.t) (sofar : mixedstr) 
            (* right quote char, inside, rest *) (* <<-- callback is *)
         : (UTF8Char.t  * mixedstr * UTF8String.t) witherrsoption = case remaining of
        [] => unmatchedParenthesisError startChar sofar
        | (x :: xs) => if  x ~= SpecialChars.rightSingleQuote orelse
                            x ~= SpecialChars.rightParenthesis
                        then Success(x, sofar, xs)
                        else
                        if  x ~= SpecialChars.leftSingleQuote orelse
                            x ~= SpecialChars.leftParenthesis
                        then scanSingleQuote startChar xs []  >>= (fn (rq, inQuote, rest) =>
                                processSingleQuoted inQuote (x, rq) >>= (fn inSingleQuoteChar => 
                                    scanSingleQuote startChar rest (sofar@[inSingleQuoteChar])  
                                ) >>/= ( (* continue scanning in case of failure *)
                                    scanSingleQuote startChar rest (sofar)  
                                )
                            )
                        else
                        if  x ~= SpecialChars.leftDoubleQuote
                        then scanLiteral x xs []  >>= (fn (rq, inQuote, rest) => 
                                scanSingleQuote startChar rest (sofar@[Literal(inQuote, (x, rq))])  
                             )
                        else
                        if shouldSkip x then scanSingleQuote startChar xs (sofar)
                        else scanSingleQuote startChar xs (sofar@[SChar x]) 
        

    fun scanTopLevel( remaining : UTF8String.t)
         : mixedstr witherrsoption = case remaining of
        [] => Success([])
        | (x :: xs) => if  x ~= SpecialChars.leftSingleQuote orelse 
                            x ~= SpecialChars.leftParenthesis
                        then scanSingleQuote x xs []  >>= (fn (rq, inQuote, rest) => 
                                scanTopLevel rest >>= (fn cs  => 
                                        processSingleQuoted inQuote (x, rq) >>= (fn inSingleQuoteChar => 
                                            Success( inSingleQuoteChar :: cs)
                                        )
                                )
                            )
                        else
                        if  x ~= SpecialChars.leftDoubleQuote
                        then  scanLiteral x xs []  >>= 
                            (fn (rq, inQuote, rest) => 
                                scanTopLevel rest >>= (fn cs => 
                                    Success (Literal(inQuote, (x, rq)) :: cs)
                                )
                            )
                        else 
                        if shouldSkip x then scanTopLevel xs
                        else scanTopLevel xs >>= (fn y => Success (SChar x :: y))

    fun make(u : UTF8String.t) : mixedstr witherrsoption = scanTopLevel u

    fun makeDecl(u : UTF8String.t) : (mixedstr * endinginfo) list witherrsoption =  fmap processDeclaration (make u)
    
      

end