
structure Operators =
struct
     datatype associativity = LeftAssoc | RightAssoc | NoneAssoc
    datatype fixity  = Prefix | Infix | Postfix | Closed
    datatype opComponentType = OpCompExpr | OpCompBinding | OpCompString of UTF8String.t (* string is a list of chars *)
    (* datatype opComponents = OpData of opComponentType list   *)
    (* although not enforced types, a string shall be in between any expr comp *)
    (* AND the first and last component must be OpCompString *)
    datatype operator = Operator of int * fixity * associativity * opComponentType list * int  (* first int is pred, final int is uid *)
    (* int is the precedence,  string list are the named parts *)
    (* Closed must have NoneAssoc fixity 
    Prefix : either none or Right
    Postfix : either none or left *)


  fun eqOpUid ((Operator(_, _, _,_, id1)) : operator) (Operator(_, _, _,_, id2) : operator) = id1 = id2

  fun ~=** (op1, op2) = eqOpUid op1 op2


  val underscoreChar = UTF8Char.fromString "〇" NONE
  val bindingChar = UTF8Char.fromString "囗" NONE

  fun getPrecedence (Operator(p, _, _, _, _)) = p
    fun getUID(Operator (_, _, _, _, uid)) = uid
    fun getOriginalName(Operator(p, fix, assoc, comps, uid))=
      let val baseName = List.concat (map (fn x => 
       case x of
        OpCompExpr =>  [underscoreChar]
        | OpCompBinding =>  [underscoreChar] (* Original name has no bindingChar *)
        | OpCompString s => s) comps)
      in case fix of
          Prefix =>  baseName @[underscoreChar] 
          | Infix => underscoreChar :: baseName @[ underscoreChar ]
          | Postfix =>underscoreChar :: baseName
          | Closed => baseName
        end


    fun getAllOccuringNameChars (Operator(_, _, _, l, _)) : UTF8Char.t list = 
            List.concat (map (fn x => case x of OpCompString s => s | _ => []) l)
        
        
    type allOperators = operator list

 
    fun stripHead (s : UTF8String.t) = tl s
  fun show_opcomptype (x : opComponentType) :string = let 
    in
      case x of
        OpCompExpr => UTF8Char.toString underscoreChar
        | OpCompBinding => UTF8Char.toString bindingChar
        | OpCompString s => UTF8String.toString s
    end
      fun show_opcomptypes (x : opComponentType list) :string = let 
    in
    "[" ^ String.concatWith ", " (map show_opcomptype x) ^ "]"
    end

    val ~= = UTF8Char.~=
    infix 4 ~=

    fun toNameComponents (s : UTF8String.t) (bindingIdxs : int list) : opComponentType list = 
    let 
    (* val _ = print ("doing " ^ UTF8String.toString s ^ "\n") *)
        val (res, pending) = foldl (fn (schar,(res, pending)) => 
            if schar ~= underscoreChar
            then (
                    (* print ("res = " ^ show_opcomptypes res ^ " pending is" ^ UTF8String.toString pending ^ "\n")
                    ; *)
                    (res @[OpCompString(pending),
                    (if List.exists (fn i => List.length(res) + 1 = i) bindingIdxs 
                   then OpCompBinding 
                   else OpCompExpr)
            ] , []))
            else (res, pending@[schar])) ([], []) s 
    in ( res@[OpCompString(pending)] ) end

    exception DoubleUnderscore



    (* binding index is to count first string name as 0, the first hole as 1, and so on. Should always be odd *)
    fun parseOperator (name : UTF8String.t) (hasAssoc : bool) (isLeft : bool) (pred : int) (bindingIdxs : int list) : operator = 
    let val nextUID = UID.next()
    in
        if UTF8String.isSubstring ([underscoreChar, underscoreChar]) name then raise DoubleUnderscore else 
        case (hd name ~= underscoreChar , (List.last name) ~= underscoreChar) of
            (false, false) => Operator(pred, Closed, NoneAssoc, toNameComponents name bindingIdxs, nextUID)
            | (false, true) => Operator(pred, Prefix, 
                    if hasAssoc then RightAssoc else NoneAssoc, toNameComponents (UTF8String.stripTail name) bindingIdxs, nextUID) 
            | (true , false) => Operator(pred, Postfix, 
                    if hasAssoc then LeftAssoc else NoneAssoc, toNameComponents (stripHead name) bindingIdxs, nextUID)
            | (true , true) => Operator(pred, Infix, 
                    if hasAssoc then (if isLeft then LeftAssoc else RightAssoc) else NoneAssoc, 
                    toNameComponents (stripHead (UTF8String.stripTail name)) bindingIdxs, nextUID)
        end

    fun parseOperatorStr name = parseOperator (UTF8String.fromString name)

    fun getStringComponents (Operator(_, _, _, comps, _))  : UTF8String.t list = 
      List.mapPartial (fn x => case x of OpCompString s => SOME s | _ => NONE) comps

    fun reconstructWithArgs(oper as Operator(_, fix, _, comps, _) : operator) (args : UTF8String.t list) = 
    let val allComps = case fix of
          Prefix =>  comps @[OpCompExpr] 
          | Infix => OpCompExpr :: comps @[ OpCompExpr ]
          | Postfix =>OpCompExpr :: comps
          | Closed => comps
    val _ = if length allComps <> length (getStringComponents oper) + length args  then 
          raise Fail "Operator has an incorrect number of arguments"
          else ()
        val res = foldl (fn (elem , (rargs, acc))=>
          case elem of 
            OpCompString s => (rargs, acc@[s])
            | _ => (case rargs of 
                (h:: trargs) => (trargs, acc@[h])
                | _ => raise Fail "impossilbe by length counting"
              )
        ) (args, []) allComps
      in
      List.concat (#2 res)
      end 


end