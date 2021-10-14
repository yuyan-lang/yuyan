
structure MixFixParser =
struct
    datatype associativity = LeftAssoc | RightAssoc | NoneAssoc
    datatype fixity  = Prefix | Infix of associativity | Postfix | Closedfix
    datatype operator = Operator of int * fixity * string list 
    (* int is the precedence,  string list are the named parts *)

    
    type allOperators = operator list

    datatype OpAST = OpAST of (operator * OpAST list )

    open RawAST


    functor Parser (P : sig 
        val allOps :allOperators 
        end) = struct 

        val allOps = P.allOps
        val allPrecedences : int list= ListMergeSort.sort (fn (s, t) => s > t) (map (fn (Operator(i, _, _)) => i) allOps)

        fun nextPred (pred : int) : int option = 
            case List.find (fn x => x = pred) allPrecedences of
                SOME(idx) => if idx + 1 < List.length(allPrecedences) 
                              then SOME(List.nth(allPrecedences, (idx + 1))) 
                              else NONE
                | NONE => raise Fail "not possible 27"

        fun up (pred : int) : operator list = 
            case nextPred pred of 
            SOME(np) => List.filter (fn (Operator(p', _, _)) => p' = np) allOps
            | NONE => []


        datatype ParseOpAST = ParseOpAST of (operator * OpAST list )
        exception ParseFailure of string
        type parser = RawAST -> (ParseOpAST, RawAST) list 

        fun parseStr

        fun parseOpInternal (oper : operator) : parser = fn exp => 
        case oper of
                Operator (pred, fixity, lst)  => 


        fun hat (oper : operator) : parser = fn exp =>
            case oper of




        

        (* parse the entire expression, needs to be full *)
        fun parseExp : parser = 
            List.concat (List.mapPartial (fn oper => let 
                val (sn, rest) = (hat oper exp)
                in if isRawASTEmpty rest then sn else NONE end) allOperators) 


    end
     
    
    fun parseMixfixExpression (ops : allOperators) (exp : RawAST.RawAST) : (OpAST option * RawAST) = 
        raise (Fail "Fail")

end