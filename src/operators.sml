
structure Operators =
struct
     datatype associativity = LeftAssoc | RightAssoc | NoneAssoc
    datatype fixity  = Prefix | Infix | Postfix | Closed
    datatype operator = Operator of int * fixity * associativity * string list 
    (* int is the precedence,  string list are the named parts *)

    
    type allOperators = operator list

    datatype OpAST = OpAST of (operator * OpAST list )
end