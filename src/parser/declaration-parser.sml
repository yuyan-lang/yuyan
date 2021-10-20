
structure DeclarationParser =
struct
    open ParseAST
    val DEBUG = false
    (* type parser = UTF8String.t -> (operator * UTF8String.t list) list  operator and all arguments *)
    (* declarations are top level constructs that ignore internal expression structure, 
    but they contain arbitrary unknown names and structured parenthesis *)

    (* returns the rest *)
    fun parseStr (s : UTF8String.t)  :UTF8String.t -> UTF8String.t option = fn exp =>
                if UTF8String.size s = 0 then SOME(exp) else
                case exp of
                    ( id :: exps)  => if hd s = id 
                                    then parseStr (tl s) exps
                                    else NONE
                    | _ =>  NONE

    and projfirst2 ((a, b, c, d)) = (a, b)

     and parseBinding (until : UTF8String.t) : UTF8String.t -> UTF8String.t * UTF8String.t = fn exp =>
                let 
                    fun go (remaining : UTF8String.t) (pending : UTF8String.t) : (UTF8String.t * UTF8String.t) = 
                        (
                            (* print ("going on r = "^ UTF8String.toString remaining ^ " and pending = " ^ UTF8String.toString pending ^
                        " until = " ^ UTF8String.toString until ^ "\n" ); *)
                        if List.length remaining < List.length until 
                        then (*add all pending and remaining and return *)
                              ((pending @ remaining, []))
                        else if UTF8String.isPrefix until remaining
                             then (pending, remaining)
                             else (case remaining of 
                                 (h::t) => (let 
                                        val (deltaPending, nextRemaining) = (
                                            if h = SpecialChars.leftSingleQuote  
                                                    then projfirst2 (BracketScanner.scanUntilCorrespondingRightQuote 1 0 t)
                                                    else if h = SpecialChars.leftDoubleQuote
                                                    then projfirst2 (BracketScanner.scanUntilCorrespondingRightQuote 0 1 t)
                                                    else ([], t)
                                        )
                                    in go nextRemaining (pending @ [h]@deltaPending)
                                    end))
                        )
                in go exp [] 
                end
                


    fun parseDeclarationSingleOp(l :  opComponentType list) : UTF8String.t -> (UTF8String.t list) option
    = fn exp =>
        (
            (* print ("Parsing " ^ PrettyPrint.show_opcomptypes l ^ " on " ^ UTF8String.toString exp ^ "\n"); *)
        case l of
            [] => SOME([])
            | [OpCompExpr] => SOME([exp])
            | (OpCompExpr :: (OpCompString s) :: t) => 
                let val  (parsed, remaining) = parseBinding s exp
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
            | _ => raise Fail "Can only handle nonassoc ops: dp20"
    (* will be parsing in binding left most fashion *)
    (* declarations should not be associative *)
    fun parseDeclarations(ops : operator list) : UTF8String.t -> (operator * UTF8String.t list) list = fn exp =>
        (List.mapPartial (fn (oper, l) => case parseDeclarationSingleOp l exp of
        SOME(args) => SOME((oper, args)) | NONE => NONE)
         (map   getParseComponents ops))
        
    exception DeclNoParse of UTF8String.t
    exception DeclAmbiguousParse of (operator * UTF8String.t list) list
    fun parseDeclarationSingleOutput(ops : operator list) : UTF8String.t -> (operator * UTF8String.t list) = fn exp => 
        (if DEBUG then print ("DeclParser : Parsing " ^ UTF8String.toString exp ^ "\n") else ();
        case parseDeclarations ops exp of
            []  => raise DeclNoParse exp
            | [l] => l
            | ls => raise DeclAmbiguousParse ls
        )
        
end