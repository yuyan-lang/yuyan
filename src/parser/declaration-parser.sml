
structure DeclarationParser =
struct
    open ParseAST
    open StaticErrorStructure
    val DEBUG = false
    (* val DEBUG = true *)
    (* type parser = UTF8String.t -> (operator * UTF8String.t list) list  operator and all arguments *)
    (* declarations are top level constructs that ignore internal expression structure, 
    but they contain arbitrary unknown names and structured parenthesis *)

    val ~= = UTF8Char.~=
    infix 4 ~=

    (* returns (parsedString, rest) *)
    fun parseStrRec (predicate : UTF8String.t) (sofar : UTF8String.t)  :MixedStr.t -> (UTF8String.t * MixedStr.t) option = fn exp =>
        (
            (* print ("parseStr "^ UTF8String.toString s ^ " on " ^ PrettyPrint.show_mixedstr exp ^"\n"   ); *)
                if UTF8String.size predicate = 0 then SOME((sofar, exp)) else
                case exp of
                    ( MixedStr.SChar id :: exps)  => if hd predicate ~= id 
                                    then parseStrRec (tl predicate) (sofar@[id]) exps
                                    else (
                                        (* print ("parseStr failed hd s is " ^ UTF8Char.toString (hd s) 
                                    ^ " at " ^ UTF8.toString (hd s) ^ " and id (hd of exp ) is " 
                                    ^ UTF8Char.toString (id) ^ " at " ^ UTF8.toString id ^" \n" ); *)
                                    NONE)
                    | _ =>  (
                        (* print "parseStr failed\n"; *)
                    NONE)
        )
    fun parseStr (predicate : UTF8String.t)   :MixedStr.t -> (UTF8String.t * MixedStr.t) option = 
        parseStrRec predicate []

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
                            andalso  length pending > 0 (* require parsed thing to be nonempty*)
                             then (pending, remaining)
                             else (case remaining of 
                                 (h::t) => (let 
                                    in go t (pending @ [h])
                                    end)
                                | _ => raise Fail "dp52")
                        )
                in go exp [] 
                end
                


    fun parseDeclarationSingleOp(l :  opComponentType list) 
        : MixedStr.t -> 
            ({opComps: opComponentType list , args : MixedStr.t list}) option
    = fn exp =>
        (
            (* print ("Parsing " ^ PrettyPrint.show_opcomptypes l ^ " on " ^ MixedStr.toString exp ^ "\n"); *)
        case l of
            [] => (case exp of 
                [] => SOME({opComps=[], args=[]})
                | _ => NONE ) (* require input to be fully parsed *)
            | [OpCompExpr] => 
            (* forbid empty exp at the end tc-2.yuyan test*)
                if length exp = 0 then NONE else
                    SOME({opComps=[OpCompExpr], args=[exp]})
            | (OpCompExpr :: (OpCompString s) :: t) => 
                let val  (parsed, remaining) = parseUntil s exp
                in (case  parseDeclarationSingleOp (OpCompString s :: t) remaining
                    of SOME({opComps=opComps, args=args}) => SOME({opComps=(OpCompExpr :: opComps), args=parsed::args})
                    | NONE => NONE)
                end
            | ((OpCompString s) :: t) => (case parseStr s exp of 
                SOME ((parsed,remainingExp)) => (case  parseDeclarationSingleOp (t) remainingExp
                    of SOME({opComps=opComps, args=args}) => SOME({opComps=(OpCompString parsed)::opComps, args=args})
                    | NONE => NONE)
                | NONE => NONE)
            | _ => raise Fail "dp84, unsupported component type"
        )


        
        (* inverse of getParseComponents *)
    fun updateOperator (oper : operator) (opComps : opComponentType list) : operator = 
    (
        (* print ("updateOperator " ^ Int.toString (length opComps)^ " opComps : "^ PrettyPrint.show_opcomptypes opComps); *)
        case oper of 
            Operator(pred, Prefix, NoneAssoc, l , uid) => Operator(pred, Prefix, NoneAssoc, List.take(opComps, length opComps -1) , uid) 
            | Operator(pred, Postfix, NoneAssoc, l , uid) => Operator(pred, Postfix, NoneAssoc, tl opComps , uid) 
            | Operator(pred, Infix, NoneAssoc, l , uid) => Operator(pred, Infix, NoneAssoc, List.take(tl opComps, length opComps -2) , uid)
            | Operator(pred, Closed, NoneAssoc, l , uid) => Operator(pred, Closed, NoneAssoc, opComps , uid) 
            | _ => raise Fail "Can only handle nonassoc ops: dp20"
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
        SOME({opComps=opComps,args=args}) => SOME((updateOperator oper opComps, args)) | NONE => NONE)
         (map   getParseComponents ops))
        
    exception DeclNoParse of MixedStr.t
    exception DeclAmbiguousParse of MixedStr.t * (operator * MixedStr.t list) list
    open PreprocessingOperators
    fun parseDeclarationSingleOutput (exp :  MixedStr.t) : (operator * MixedStr.t list) = 
        (if DEBUG then print ("DeclParser : Parsing " ^ MixedStr.toString exp ^ "\n") else ();
        case parseDeclarations declOpsWithImportOpen exp of
            []  => 
            (
                case parseDeclarations declOpsNoImportOpen exp of
                    []  => 
                    (
                        raise DeclNoParse exp
                    )
                    | [l] => l
                    | ls => raise DeclAmbiguousParse(exp, ls)
            )
            | [l] => l
            | ls => raise DeclAmbiguousParse(exp, ls)
        )
        
end