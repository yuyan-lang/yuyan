
functor PrecedenceParser (P : sig 
        val allOps :Operators.allOperators 
        end) = struct 

        open Operators
        open RawAST
        open ParseAST


        val allOps = P.allOps
        fun removeDuplicateInSorted (l : int list) : int list = case l of
            (x :: y :: xs) => if x = y then removeDuplicateInSorted (x :: xs) else x :: removeDuplicateInSorted (y :: xs)
            | l => l
        val allPrecedences : int list= removeDuplicateInSorted (ListMergeSort.sort 
                    (fn (s, t) => s > t) (map (fn (Operator(i,_,  _, _)) => i) allOps))
       
        val y = (print ("ALL PRECEDENCES "^ String.concatWith "," (map Int.toString allPrecedences)); 2)


 (*https://stackoverflow.com/questions/17826034/sml-get-index-of-item-in-list*)
        fun index(item, xs) =
        let
            fun index'(m, nil) = NONE
            | index'(m, x::xr) = if x = item then SOME m else index'(m + 1, xr)
        in
            index'(0, xs)
        end

        fun nextPred (pred : int) : int option = 
            case index(pred,  allPrecedences)of
                SOME(idx) => (
                    print ("some next pred for " ^ Int.toString pred  ^ " idx is " ^ Int.toString idx ^ "\n");
                                if idx + 1 < List.length(allPrecedences) 
                              then SOME(List.nth(allPrecedences, (idx + 1))) 
                              else NONE)
                | NONE => (print ("no next pred for " ^ Int.toString pred ^ "\n");
                 raise Fail "not possible 27")

        fun up (pred : int) : operator list = 
            case nextPred pred of 
            SOME(np) => List.filter (fn (Operator(p', _, _, _)) => p' = np) allOps
            | NONE => (print ("no up for " ^ Int.toString (pred) ^ "\n"); [])


        type parser = RawAST list -> (ParseOpAST* (RawAST list)) list 

        fun combineAST (pr: ParseRule) : ParseOpAST list -> ParseOpAST = fn l => ParseOpAST (pr, l)

        fun parserResToList (l: (ParseOpAST* (RawAST list)) list  ): (ParseOpAST list* (RawAST list)) list 
        = map (fn (x, r) => ([x],r)) l
        fun listToParserResult (combine: ParseOpAST list -> ParseOpAST) (l: (ParseOpAST list* (RawAST list)) list  ): (ParseOpAST* (RawAST list)) list 
        = map (fn (x, r) => (combine x,r)) l

        and parseStr s oper = debug "parseStr" (parseStr_ s oper)
        and parseStr_ (s : string) (o' : ParseOpAST) : parser = fn exp =>
            if String.size s = 0 then [(o', exp)] else
            case exp of
                (RawID id :: exps)  => if String.isPrefix id s 
                                then parseStr (String.extract(s, String.size(id), NONE)) o' exps
                                else raise ParseFailure ("Cannot match " ^ s ^ " against " ^ id) (* strip off id from s*)
                | (RawList l :: exps) => raise ParseFailure ("Cannot match "^ s ^ " against a rawlist")
                | _ => raise ParseFailure ("Running out of input")

        and parseOpInternal (oper : operator) : parser = fn exp => 
        case oper of
                Operator (pred, fixity, assoc, lst)  
            => case lst of
                (hd :: tl) => 
                let 
                  val rawComponents = List.foldl (fn (name : string, acc) => seqL (seqL acc (parseExp())) 
                (parseStr name (ParseOpAST(OperatorNameComponent name, []))) )
                    (parserResToList (parseStr hd (ParseOpAST(OperatorNameComponent hd, [])) exp)) tl
                in listToParserResult (fn l => ParseOpAST (OperatorInternal oper,l)) rawComponents
                end
                | _ => raise Fail "59"


        and upP (oper : operator) : parser = 
            case oper of Operator (i, _, _, _) => 
                let val ops = up i in alternatives (map hat ops) end


        and hat oper = (debug "hat_" (hat_ oper))
        and hat_ (oper : operator) : parser = 
            let val opInt =parseOpInternal oper 
            in
            case oper of
                Operator (pred, fixity, assoc,  lst)  

                => case (fixity, assoc) of
                    (Closed, _) => opInt
                    | (Prefix, NoneAssoc ) => sequence (combineAST (PrefixNoneAssoc oper)) [opInt, upP oper]
                    | (Postfix, NoneAssoc ) => sequence (combineAST (PostfixNoneAssoc oper)) [upP oper, opInt]
                    | (Prefix, RightAssoc ) => sequence (combineAST (PrefixRightAssoc oper)) [many1 opInt, upP oper]
                    | (Postfix, LeftAssoc ) => sequence (combineAST (PostfixLeftAssoc oper)) [upP oper, many1 opInt]
                    | (Infix, NoneAssoc ) => sequence (combineAST (InfixNoneAssoc oper)) [upP oper, opInt, upP oper]
                    | (Infix, LeftAssoc ) => sequence (combineAST (InfixLeftAssoc oper)) [upP oper, 
                            many1 (sequence (combineAST (InfixLeftAssocLeftArrow oper)) [opInt, upP oper])]
                    | (Infix, RightAssoc ) => sequence (combineAST (InfixRightAssoc oper)) [
                            many1 (sequence (combineAST (InfixRightAssocRightArrow oper)) [upP oper , opInt]), upP oper]
                    | _ => raise Fail "Malformed Operator 92"
            end

        and show_rawast_list exp = String.concatWith ", " (map PrettyPrint.show_rawast exp)

        and debug (s : string) (p : parser) : parser = fn exp =>
            (print ("PARSER DEBUG: " ^ s ^ " exp is " ^ show_rawast_list exp ^ "\n"); 
            let val res = p exp
            in (print (s ^" Has " ^ Int.toString(List.length(res)) ^ " parses");
                print (String.concatWith "\n " (map (fn (past, r) => "AST " ^ show_parseopast past ^ " REST IS " ^ show_rawast_list r ) res ) ^ "\n");
                res
            )
            end
            )



        and alternatives (alt : parser list) : parser = fn exp =>
            List.concat (List.map (fn p => (try p) exp) alt)



        and seqL (pending : (ParseOpAST list * (RawAST list)) list) (p : parser) 
        : (ParseOpAST list * (RawAST list)) list =
            List.concat (List.map 
            (fn (asts, exp) => map (fn (ast, rest) => (asts@[ast], rest)) (p exp)) pending)


        and many1 p = debug "many1" (many1_ p)
        and many1_ (p : parser) : parser = fn exp => 
            let val base = parserResToList (p exp)
                fun f b = 
                let
                    val next = seqL b(try p)
                in if List.length(next) > 0
                    then (*try succeeded *) f (next)
                    else b
                end
            in listToParserResult (fn l => ParseOpAST(Many1, l)) (f base)
            end


        and sequence c p = debug "sequence" (sequence_ c p)
        and sequence_ (combine : ParseOpAST list -> ParseOpAST) (parserSeq : parser list) : parser = fn exp =>
            case parserSeq of 
                [] => raise Fail "Cannot have empty sequence"
                | (p1 :: ps) => List.map (fn (asts, r) => (combine asts, r)) 
                (List.foldl (fn (curParser, acc) => seqL acc curParser) 
    (parserResToList (p1 exp)) ps) (*map parsers over exp usign seq *)

        and try (p : parser) : parser = fn exp =>
            (debug "trying " p) exp
            handle ParseFailure s => []
            

        and parseExp (): parser = alternatives (map (debug "hat") (map hat allOps))


end
     