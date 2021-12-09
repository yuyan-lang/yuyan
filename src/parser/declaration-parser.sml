
structure DeclarationParser =
struct
    open ParseAST
    val DEBUG = false
    (* val DEBUG = true *)
    (* type parser = UTF8String.t -> (operator * UTF8String.t list) list  operator and all arguments *)
    (* declarations are top level constructs that ignore internal expression structure, 
    but they contain arbitrary unknown names and structured parenthesis *)

    val ~= = UTF8Char.~=
    infix 4 ~=

    (* returns the rest *)
    fun parseStr (s : UTF8String.t)  :MixedStr.t -> MixedStr.t option = fn exp =>
        (
            (* print ("parseStr "^ UTF8String.toString s ^ " on " ^ PrettyPrint.show_mixedstr exp ^"\n"   ); *)
                if UTF8String.size s = 0 then SOME(exp) else
                case exp of
                    ( MixedStr.SChar id :: exps)  => if hd s ~= id 
                                    then parseStr (tl s) exps
                                    else (
                                        (* print ("parseStr failed hd s is " ^ UTF8Char.toString (hd s) 
                                    ^ " at " ^ UTF8.toString (hd s) ^ " and id (hd of exp ) is " 
                                    ^ UTF8Char.toString (id) ^ " at " ^ UTF8.toString id ^" \n" ); *)
                                    NONE)
                    | _ =>  (
                        (* print "parseStr failed\n"; *)
                    NONE)
        )

    and projfirst2 ((a, b, c, d)) = (a, b)

     and parseUntil (until : UTF8String.t) : MixedStr.t -> MixedStr.t * MixedStr.t = fn exp =>
                let 
                    fun go (remaining : MixedStr.t) (pending : MixedStr.t) : (MixedStr.t * MixedStr.t) = 
                        (
                            (* print ("going on r = "^ UTF8String.toString remaining ^ " and pending = " ^ UTF8String.toString pending ^
                        " until = " ^ UTF8String.toString until ^ "\n" ); *)
                        if List.length remaining < List.length until 
                        then (*add all pending and remaining and return *)
                              ((pending @ remaining, [])) (* TODO: should this be empty as well? *)
                        else if MixedStr.isPrefix until remaining
                             then (pending, remaining)
                             else (case remaining of 
                                 (h::t) => (let 
                                    in go t (pending @ [h])
                                    end))
                        )
                in go exp [] 
                end
                


    fun parseDeclarationSingleOp(l :  opComponentType list) : MixedStr.t -> (MixedStr.t list) option
    = fn exp =>
        (
            (* print ("Parsing " ^ PrettyPrint.show_opcomptypes l ^ " on " ^ MixedStr.toString exp ^ "\n"); *)
        case l of
            [] => SOME([])
            | [OpCompExpr] => SOME([exp])
            | (OpCompExpr :: (OpCompString s) :: t) => 
                let val  (parsed, remaining) = parseUntil s exp
                in (case  parseDeclarationSingleOp (OpCompString s :: t) remaining
                    of SOME(args) => SOME(parsed::args)
                    | NONE => NONE)
                end
            | ((OpCompString s) :: t) => (case parseStr s exp of 
                SOME (remainingExp) => parseDeclarationSingleOp t remainingExp
                | NONE => NONE)
            | _ => raise Fail "dp84, unsupported component type"
        )


        



    fun getParseComponents (oper : operator): operator * opComponentType list = 
        case oper of 
            Operator(_, Prefix, NoneAssoc, l , _) => (oper,  l @ [OpCompExpr])
            | Operator(_, Postfix, NoneAssoc, l , _) => (oper, OpCompExpr :: l)
            | Operator(_, Infix, NoneAssoc, l , _) => (oper, OpCompExpr :: l @ [OpCompExpr])
            | Operator(_, Closed, NoneAssoc, l , _) => (oper,  l )
            | _ => raise Fail "Can only handle nonassoc ops: dp20"
    (* will be parsing in binding left most fashion *)
    (* declarations should not be associative *)
    fun parseDeclarations(ops : operator list) : MixedStr.t -> (operator * MixedStr.t list) list = fn exp =>
        (List.mapPartial (fn (oper, l) => case parseDeclarationSingleOp l exp of
        SOME(args) => SOME((oper, args)) | NONE => NONE)
         (map   getParseComponents ops))
        
    exception DeclNoParse of MixedStr.t
    exception DeclAmbiguousParse of (operator * MixedStr.t list) list
    fun parseDeclarationSingleOutput(ops : operator list) : MixedStr.t -> (operator * MixedStr.t list) = fn exp => 
        (if DEBUG then print ("DeclParser : Parsing " ^ MixedStr.toString exp ^ "\n") else ();
        case parseDeclarations ops exp of
            []  => raise DeclNoParse exp
            | [l] => l
            | ls => raise DeclAmbiguousParse ls
        )
        
end