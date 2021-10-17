
structure Operators =
struct
     datatype associativity = LeftAssoc | RightAssoc | NoneAssoc
    datatype fixity  = Prefix | Infix | Postfix | Closed
    datatype opComponentType = OpCompExpr | OpCompBinding | OpCompString of string
    (* datatype opComponents = OpData of opComponentType list   *)
    (* although not enforced types, a string shall be in between any expr comp *)
    (* AND the first and last component must be OpCompString *)
    datatype operator = Operator of int * fixity * associativity * opComponentType list
    (* int is the precedence,  string list are the named parts *)
    (* Closed must have NoneAssoc fixity 
    Prefix : either none or Right
    Postfix : either none or left *)

  val underscoreChar = "〇"
  val bindingChar = "△"
  val underscoreCharUTF8 = hd (UTF8.explode("〇"))

  fun getPrecedence (Operator(p, _, _, _)) = p
    

    fun getAllOccuringNameChars (Operator(_, _, _, l)) : string list = 
        map (fn x => UTF8.implode [x]) (
            (List.concat (map (fn x => case x of OpCompString s => (UTF8.explode s) | _ => []) l))
            )
        
        
    type allOperators = operator list

    datatype OpAST = OpAST of (operator * OpAST list )
                    | UnknownOpName of string

    fun stripHead (s : string) = 
        String.extract(s, String.size(underscoreChar), NONE)
    fun stripTail (s : string) = 
        String.substring(s, 0, String.size(s) - String.size(underscoreChar))

    fun toNameComponents (s : string) (bindingIdxs : int list) : opComponentType list = 
    let val chars = UTF8.explode(s)
        val (res, pending) = foldr (fn (schar,(res, pending)) => 
            if schar = underscoreCharUTF8 
            then ((if List.exists (fn i => List.length(res) + 1 = i) bindingIdxs then OpCompBinding else OpCompExpr)
            :: OpCompString(UTF8.implode pending) :: res, [])
            else (res, schar::pending)) ([], []) chars 
    in (OpCompString(UTF8.implode pending) :: res) end

    exception DoubleUnderscore

    (* binding index is to count first string name as 0, the first hole as 1, and so on. Should always be odd *)
    fun parseOperator (name : string) (hasAssoc : bool) (isLeft : bool) (pred : int) (bindingIdxs : int list) : operator = 
        if String.isSubstring (underscoreChar ^ underscoreChar) name then raise DoubleUnderscore else 
        case (String.isPrefix underscoreChar name , String.isSuffix underscoreChar name) of
            (false, false) => Operator(pred, Closed, NoneAssoc, toNameComponents name bindingIdxs)
            | (false, true) => Operator(pred, Prefix, 
                    if hasAssoc then RightAssoc else NoneAssoc, toNameComponents (stripTail name) bindingIdxs) 
            | (true , false) => Operator(pred, Postfix, 
                    if hasAssoc then LeftAssoc else NoneAssoc, toNameComponents (stripHead name) bindingIdxs)
            | (true , true) => Operator(pred, Infix, 
                    if hasAssoc then (if isLeft then LeftAssoc else RightAssoc) else NoneAssoc, 
                    toNameComponents (stripHead (stripTail name)) bindingIdxs)
end