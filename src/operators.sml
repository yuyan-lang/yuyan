
structure Operators =
struct
     datatype associativity = LeftAssoc | RightAssoc | NoneAssoc
    datatype fixity  = Prefix | Infix | Postfix | Closed
    datatype opComponentType = OpCompExpr | OpCompBinding | OpCompString of UTF8String.t (* string is a list of chars *)
    (* datatype opComponents = OpData of opComponentType list   *)
    (* although not enforced types, a string shall be in between any expr comp *)
    (* AND the first and last component must be OpCompString *)
    datatype operator = Operator of int * fixity * associativity * opComponentType list
    (* int is the precedence,  string list are the named parts *)
    (* Closed must have NoneAssoc fixity 
    Prefix : either none or Right
    Postfix : either none or left *)

  val underscoreChar = UTF8Char.fromString "〇"
  val bindingChar = UTF8Char.fromString "囗"

  fun getPrecedence (Operator(p, _, _, _)) = p
    

    fun getAllOccuringNameChars (Operator(_, _, _, l)) : UTF8Char.t list = 
            List.concat (map (fn x => case x of OpCompString s => s | _ => []) l)
        
        
    type allOperators = operator list

    datatype OpAST = OpAST of (operator * OpAST list )
                    | UnknownOpName of UTF8String.t
                    | NewOpName of UTF8String.t

    fun stripHead (s : UTF8String.t) = tl s
    fun stripTail (s : UTF8String.t) = List.take (s, List.length(s) -1)

    fun toNameComponents (s : UTF8String.t) (bindingIdxs : int list) : opComponentType list = 
    let val (res, pending) = foldr (fn (schar,(res, pending)) => 
            if schar = underscoreChar
            then ((if List.exists (fn i => List.length(res) + 1 = i) bindingIdxs 
                   then OpCompBinding 
                   else OpCompExpr)
            :: OpCompString(pending) :: res, [])
            else (res, schar::pending)) ([], []) s 
    in (OpCompString(pending) :: res) end

    exception DoubleUnderscore


    (* binding index is to count first string name as 0, the first hole as 1, and so on. Should always be odd *)
    fun parseOperator (name : UTF8String.t) (hasAssoc : bool) (isLeft : bool) (pred : int) (bindingIdxs : int list) : operator = 
        if UTF8String.isSubstring ([underscoreChar, underscoreChar]) name then raise DoubleUnderscore else 
        case (hd name = underscoreChar , (List.last name) = underscoreChar) of
            (false, false) => Operator(pred, Closed, NoneAssoc, toNameComponents name bindingIdxs)
            | (false, true) => Operator(pred, Prefix, 
                    if hasAssoc then RightAssoc else NoneAssoc, toNameComponents (stripTail name) bindingIdxs) 
            | (true , false) => Operator(pred, Postfix, 
                    if hasAssoc then LeftAssoc else NoneAssoc, toNameComponents (stripHead name) bindingIdxs)
            | (true , true) => Operator(pred, Infix, 
                    if hasAssoc then (if isLeft then LeftAssoc else RightAssoc) else NoneAssoc, 
                    toNameComponents (stripHead (stripTail name)) bindingIdxs)

    fun parseOperatorStr name = parseOperator (UTF8String.fromString name)
end