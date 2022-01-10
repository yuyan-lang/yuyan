structure MixedStr =
struct

    open StaticErrorStructure
 (* should provide an adequate representation of brackets. No periods or brackets may be considered after this step *)
    datatype mixedchar = 
                   UnparsedExpression of mixedchar list (* an unparsed expression has quotes on top level between quotes  *)
                  | UnparsedDeclaration of mixedchar list list (* an unparsed declaration has periods between quotes*)
                  | Name of UTF8String.t (* a name is the string between quotes that don't have periods or quotes *)
                  | Literal of UTF8String.t (* a literal is the string between double quotes *)
                  (* | ParsedExpression of Operators.OpAST (* a parsed expression *)
                  | ParsedDeclaration of TypeCheckingAST.Signature  *)
                  | SChar of UTF8Char.t (* top level characters , every thing else is quoted *)
    type mixedstr = mixedchar list
    type t = mixedstr

    val ~= = UTF8Char.~=
    infix 4 ~=

    val >>= = StaticErrorStructure.>>=
    infix 5 >>=

    exception UnmatchedParenthesis
    exception UnmatchedStringLiteral

    fun toUTF8StringChar(u : mixedchar) : UTF8String.t = 
    let 
        fun singleQuoteAround (u : UTF8String.t) = 
            SpecialChars.leftSingleQuote :: u @ [SpecialChars.rightSingleQuote]
        fun doubleQuoteAround (u : UTF8String.t) = 
            SpecialChars.leftDoubleQuote :: u @ [SpecialChars.rightDoubleQuote]
    in
    case  u of 
    UnparsedExpression s => singleQuoteAround (toUTF8String s)
    | UnparsedDeclaration l => singleQuoteAround (List.concat ( (map (fn x => toUTF8String x @[SpecialChars.period]) l)))
    | Name t => singleQuoteAround t
    | Literal t => doubleQuoteAround t
    (* | ParsedExpression e  => UTF8String.fromString "PARSED EXPR"
    | ParsedDeclaration d => UTF8String.fromString "PARSED SIG" *)
    | SChar t => [t]
    end

    and toUTF8String(u : mixedstr ) : UTF8String.t = List.concat (map toUTF8StringChar u)
    fun toString(u : mixedstr) : string = UTF8String.toString (toUTF8String u)

    exception StringNotPlain of mixedstr
    fun toPlainUTF8Char (u : mixedchar) : UTF8Char.t = 
        case u of 
            SChar s => s
            |_ => raise StringNotPlain [u]

(* only name and plain schar's are plain *)
    fun toPlainUTF8String (u : mixedstr) : UTF8String.t = 
        case u of 
            [] => []
            | [Name s] => s
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

    fun separateBy (sep : UTF8Char.t) (s : mixedstr)  (sofar : mixedstr) : mixedstr list = 
        case s of
            [] => (case sofar of [] => [] | _ => [sofar])
            | (x :: xs) => if isChar x sep 
                            then sofar :: separateBy sep xs []
                            else separateBy sep xs (sofar@[x])
    
    exception InternalFailure of mixedstr
    fun unSChar (s : mixedstr ) : UTF8String.t =
        case s of [] => []
        | (SChar x :: xs) => x :: unSChar xs
        | _ => (raise InternalFailure s)


    fun shouldSkip (s : UTF8Char.t) = List.exists (fn c => c ~= s) 
        [ SpecialChars.tab, SpecialChars.newline, SpecialChars.space]

    fun processDeclaration (p : mixedstr) : mixedstr list = 
    let 
    (* val _ = (print ("processDeclaration "^ toString p ^"\n")) *)
        val res = (separateBy SpecialChars.period (
            if isChar (List.last p) SpecialChars.period
            then stripTail p else p
        ) [])
        (* val _ = print ("result is of length " ^ Int.toString(length res)) *)
        in res end

    fun processSingleQuoted( p : mixedstr) : mixedchar = 
        if containsCharTopLevel p SpecialChars.period
        then (* process as declaration *)
             UnparsedDeclaration (processDeclaration p)
        else if isPlainStr p
            then  (if containsCharTopLevel p SpecialChars.leftAngledBracket
                    orelse containsCharTopLevel p SpecialChars.rightAngledBracket
                    then UnparsedExpression p 
                    else (* name *) Name (unSChar p))
            else (* expression *) UnparsedExpression p 
            

    (* string escape two endDoubleQuote to escape double quote, else no escape *)
    fun scanLiteral(remaining : UTF8String.t)(sofar : UTF8String.t) : (UTF8String.t * UTF8String.t) witherrsoption
     = case (
         (* print (UTF8String.toString remaining^"\n"); *)
      remaining) of
        [] => raise UnmatchedStringLiteral
        | [x] => if x ~= SpecialChars.rightDoubleQuote
                 then Success(sofar, [])
                 else raise UnmatchedStringLiteral
        | (x::y::xs) => if  x ~= SpecialChars.rightDoubleQuote 
                        andalso  y ~= SpecialChars.rightDoubleQuote
                 then scanLiteral xs (sofar @[x]) (*escape*)
                 else if  x ~= SpecialChars.rightDoubleQuote
                      then Success(sofar, y::xs)
                      else scanLiteral (y::xs) (sofar @[x])

    fun scanSingleQuote( remaining : UTF8String.t) (sofar : mixedstr) 
         : (mixedstr * UTF8String.t) witherrsoption = case remaining of
        [] => raise UnmatchedParenthesis
        | (x :: xs) => if  x ~= SpecialChars.rightSingleQuote orelse
                            x ~= SpecialChars.rightParenthesis
                        then Success(sofar, xs)
                        else
                        if  x ~= SpecialChars.leftSingleQuote orelse
                            x ~= SpecialChars.leftParenthesis
                        then scanSingleQuote xs []  >>= (fn (inQuote, rest) =>
                                scanSingleQuote rest (sofar@[processSingleQuoted inQuote])  
                            )
                        else
                        if  x ~= SpecialChars.leftDoubleQuote
                        then scanLiteral xs []  >>= (fn (inQuote, rest) => 
                                scanSingleQuote rest (sofar@[Literal inQuote])  
                             )
                        else
                        if shouldSkip x then scanSingleQuote xs (sofar)
                        else scanSingleQuote xs (sofar@[SChar x]) 
        

    fun scanTopLevel( remaining : UTF8String.t)
         : mixedstr witherrsoption = case remaining of
        [] => Success([])
        | (x :: xs) => if  x ~= SpecialChars.leftSingleQuote orelse 
                            x ~= SpecialChars.leftParenthesis
                        then scanSingleQuote xs []  >>= (fn (inQuote, rest) => 
                                scanTopLevel rest >>= (fn cs  => 
                                    Success(processSingleQuoted inQuote :: cs)
                                )
                            )
                        else
                        if  x ~= SpecialChars.leftDoubleQuote
                        then  scanLiteral xs []  >>= 
                            (fn (inQuote, rest) => 
                                scanTopLevel rest >>= (fn cs => 
                                    Success (Literal inQuote :: cs)
                                )
                            )
                        else 
                        if shouldSkip x then scanTopLevel xs
                        else scanTopLevel xs >>= (fn y => Success (SChar x :: y))

    fun make(u : UTF8String.t) : mixedstr witherrsoption = scanTopLevel u

    fun makeDecl(u : UTF8String.t) : mixedstr list witherrsoption =  fmap processDeclaration (make u)
    
      
end