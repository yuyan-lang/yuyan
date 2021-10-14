
structure MixFixParser =
struct
    datatype associativity = LeftAssoc | RightAssoc | NoneAssoc
    datatype fixity  = Prefix | Infix | Postfix | Closed
    datatype operator = Operator of int * fixity * associativity * string list 
    (* int is the precedence,  string list are the named parts *)

    
    type allOperators = operator list

    datatype OpAST = OpAST of (operator * OpAST list )

    open RawAST


    
    fun parseMixfixExpression (ops : allOperators) (exp : RawAST.RawAST) : (OpAST option * RawAST) = 
        raise (Fail "Fail")

end