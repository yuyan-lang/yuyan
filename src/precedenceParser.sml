
functor Parser (P : sig 
        val allOps :MixFixParser.allOperators 
        end) = struct 

        open MixFixParser
        open RawAST

        val allOps = P.allOps
        val allPrecedences : int list= ListMergeSort.sort (fn (s, t) => s > t) (map (fn (Operator(i,_,  _, _)) => i) allOps)
        datatype ParseRule = OperatorNameComponent of string 
                            | OperatorInternal of operator
                            | PrefixNoneAssoc of operator
                            | PrefixRightAssoc of operator
                            | PostfixNoneAssoc of operator
                            | PostfixLeftAssoc of operator
                            | InfixNoneAssoc of operator
                            | InfixLeftAssoc of operator
                            | InfixLeftAssocLeftArrow of operator
                            | InfixRightAssoc of operator
                            | InfixRightAssocRightArrow of operator
                            | Many1 
        datatype ParseOpAST = ParseOpAST of ParseRule * ParseOpAST list
        exception ParseFailure of string

        fun elaborate (past : ParseOpAST) : OpAST = 
            case past of 
                ParseOpAST (r, l) => 
                case (r, l) of 
                    (OperatorNameComponent name, []) => raise Fail "undefined"

        fun nextPred (pred : int) : int option = 
            case List.find (fn x => x = pred) allPrecedences of
                SOME(idx) => if idx + 1 < List.length(allPrecedences) 
                              then SOME(List.nth(allPrecedences, (idx + 1))) 
                              else NONE
                | NONE => raise Fail "not possible 27"

        fun up (pred : int) : operator list = 
            case nextPred pred of 
            SOME(np) => List.filter (fn (Operator(p', _, _, _)) => p' = np) allOps
            | NONE => []


        type parser = RawAST list -> (ParseOpAST* (RawAST list)) list 

        fun combineAST (pr: ParseRule) : ParseOpAST list -> ParseOpAST = fn l => ParseOpAST (pr, l)

        fun parserResToList (l: (ParseOpAST* (RawAST list)) list  ): (ParseOpAST list* (RawAST list)) list 
        = map (fn (x, r) => ([x],r)) l
        fun listToParserResult (combine: ParseOpAST list -> ParseOpAST) (l: (ParseOpAST list* (RawAST list)) list  ): (ParseOpAST* (RawAST list)) list 
        = map (fn (x, r) => (combine x,r)) l

        fun parseStr (s : string) (o' : ParseOpAST) : parser = fn exp =>
            if String.size s = 0 then [(o', exp)] else
            case exp of
                (RawID id :: exps)  => if String.isPrefix id s 
                                then parseStr (String.extract(s, String.size(id), NONE)) o' exps
                                else raise ParseFailure ("Cannot match " ^ s ^ " against " ^ id) (* strip off id from s*)
                | (RawList l :: exps) => raise ParseFailure ("Cannot match "^ s ^ " against a rawlist")
                | _ => raise ParseFailure ("Running out of input")

        fun parseOpInternal (oper : operator) : parser = fn exp => 
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


        and upP (oper : operator) : parser = 
            case oper of Operator (i, _, _, _) => 
                let val ops = up i in alternatives (map hat ops) end


        and hat (oper : operator) : parser = 
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
                            sequence (combineAST (InfixLeftAssocLeftArrow oper)) [opInt, upP oper]]
                    | (Infix, RightAssoc ) => sequence (combineAST (InfixRightAssoc oper)) [
                            sequence (combineAST (InfixRightAssocRightArrow oper)) [upP oper , opInt], upP oper]
                    | _ => raise Fail "Malformed Operator 92"
            end





        and alternatives (alt : parser list) : parser = fn exp =>
            List.concat (List.map (fn p => (try p) exp) alt)



        and seqL (pending : (ParseOpAST list * (RawAST list)) list) (p : parser) 
        : (ParseOpAST list * (RawAST list)) list =
            List.concat (List.map 
            (fn (asts, exp) => map (fn (ast, rest) => (asts@[ast], rest)) (p exp)) pending)


        and many1 (p : parser) : parser = fn exp => 
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


        and sequence (combine : ParseOpAST list -> ParseOpAST) (parserSeq : parser list) : parser = fn exp =>
            case parserSeq of 
                [] => raise Fail "Cannot have empty sequence"
                | (p1 :: ps) => List.map (fn (asts, r) => (combine asts, r)) 
                (List.foldl (fn (curParser, acc) => seqL acc curParser) 
    (parserResToList (p1 exp)) ps) (*map parsers over exp usign seq *)

        and try (p : parser) : parser = fn exp =>
            p exp
            handle ParseFailure s => []
            

        and parseExp (): parser = alternatives (map hat allOps)


end
     