
structure Operators =
struct
     datatype associativity = LeftAssoc | RightAssoc | NoneAssoc
    datatype fixity  = Prefix | Infix | Postfix | Closed
    datatype operator = Operator of int * fixity * associativity * string list 
    (* int is the precedence,  string list are the named parts *)
    (* Closed must have NoneAssoc fixity 
    Prefix : either none or Right
    Postfix : either none or left *)

  val underscoreChar = "〇"
  val underscoreCharUTF8 = hd (UTF8.explode("〇"))

  fun getPrecedence (Operator(p, _, _, _)) = p
    
    type allOperators = operator list

    datatype OpAST = OpAST of (operator * OpAST list )

    fun stripHead (s : string) = 
        String.extract(s, String.size(underscoreChar), NONE)
    fun stripTail (s : string) = 
        String.substring(s, 0, String.size(s) - String.size(underscoreChar))

    fun toNameComponents (s : string) : string  list = 
    let val chars = UTF8.explode(s)
        val (res, pending) = foldr (fn (schar,(res, pending)) => 
            if schar = underscoreCharUTF8 
            then (pending :: res, [])
            else (res, schar::pending)) ([], []) chars 
    in map UTF8.implode (pending :: res) end

    exception DoubleUnderscore

    fun parseOperator (name : string) (hasAssoc : bool) (isLeft : bool) (pred : int) : operator = 
        if String.isSubstring (underscoreChar ^ underscoreChar) name then raise DoubleUnderscore else 
        case (String.isPrefix underscoreChar name , String.isSuffix underscoreChar name) of
            (false, false) => Operator(pred, Closed, NoneAssoc, toNameComponents name)
            | (false, true) => Operator(pred, Prefix, 
                    if hasAssoc then RightAssoc else NoneAssoc, toNameComponents (stripTail name))
            | (true , false) => Operator(pred, Postfix, 
                    if hasAssoc then LeftAssoc else NoneAssoc, toNameComponents (stripHead name))
            | (true , true) => Operator(pred, Infix, 
                    if hasAssoc then (if isLeft then LeftAssoc else RightAssoc) else NoneAssoc, 
                    toNameComponents (stripHead (stripTail name)))
end