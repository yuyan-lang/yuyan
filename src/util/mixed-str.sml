structure MixedStr =
struct
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

    exception UnmatchedParenthesis
    exception UnmatchedStringLiteral

    fun isChar (c : mixedchar) (ck : UTF8Char.t) : bool=
        case c of 
            SChar c' => c' = ck
            | _ => false

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
        | ((x :: xs), (SChar y :: ys)) => if x = y then isPrefix xs ys
                                          else false
        | _ => false


    fun containsCharTopLevel (ck : mixedstr) (c : UTF8Char.t) : bool =
        case ck of 
            [] => false
            | (SChar s::xs) => if s = c  then true else containsCharTopLevel xs c
            | (_::xs) => containsCharTopLevel xs c
    fun containsAllCharsTopLevel (s : mixedstr ) (test : UTF8String.t) : bool
        = foldr (fn (x, acc) => acc andalso containsCharTopLevel s x) true test

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

    fun processDeclaration (p : mixedstr) : mixedstr list = 
            (separateBy SpecialChars.period (
                if isChar (List.last p) SpecialChars.period
                then stripTail p else p
            ) [])

    fun processSingleQuoted( p : mixedstr) : mixedchar = 
        if containsCharTopLevel p SpecialChars.period
        then (* process as declaration *)
             UnparsedDeclaration (processDeclaration p)
        else if isPlainStr p
            then (* name *) Name (unSChar p)
            else (* expression *) UnparsedExpression p 
            

    (* string escape two endDoubleQuote to escape double quote, else no escape *)
    fun scanLiteral(remaining : UTF8String.t)(sofar : UTF8String.t) : UTF8String.t * UTF8String.t
     = case remaining of
        [] => raise UnmatchedStringLiteral
        | [x] => if x = SpecialChars.rightDoubleQuote
                 then (sofar, [])
                 else raise UnmatchedStringLiteral
        | (x::y::xs) => if x = SpecialChars.rightDoubleQuote andalso y = SpecialChars.rightDoubleQuote
                 then scanLiteral xs (sofar @[x]) (*escape*)
                 else if x = SpecialChars.rightDoubleQuote
                      then (sofar@[x], y::xs)
                      else scanLiteral (y::xs) (sofar @[x])

    fun scanSingleQuote( remaining : UTF8String.t) (sofar : mixedstr) 
         : mixedstr * UTF8String.t = case remaining of
        [] => raise UnmatchedParenthesis
        | (x :: xs) => if x = SpecialChars.rightSingleQuote
                        then (sofar, xs)
                        else
                        if x = SpecialChars.leftSingleQuote
                        then let val (inQuote, rest) = scanSingleQuote xs [] 
                             in scanSingleQuote rest (sofar@[processSingleQuoted inQuote])  end
                        else
                        if x = SpecialChars.leftDoubleQuote
                        then let val (inQuote, rest) = scanLiteral xs [] 
                             in scanSingleQuote rest (sofar@[Literal inQuote])  end
                        else
                        scanSingleQuote xs (sofar@[SChar x]) 
        

    fun scanTopLevel( remaining : UTF8String.t)
         : mixedstr  = case remaining of
        [] => []
        | (x :: xs) => if x = SpecialChars.leftSingleQuote
                        then let val (inQuote, rest) = scanSingleQuote xs [] 
                             in processSingleQuoted inQuote :: scanTopLevel rest
                             end
                        else
                        if x = SpecialChars.leftDoubleQuote
                        then let val (inQuote, rest) = scanLiteral xs [] 
                             in Literal inQuote :: scanTopLevel rest
                             end
                        else 
                        SChar x :: scanTopLevel xs

    fun make(u : UTF8String.t) : mixedstr = scanTopLevel u

    fun makeDecl(u : UTF8String.t) : mixedstr list = processDeclaration (make u)
    
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
      
end