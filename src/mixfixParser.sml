
structure MixFixParser =
struct
    datatype associativity = LeftAssoc | RightAssoc | NoneAssoc
    datatype fixity  = Prefix | Infix | Postfix | Closed
    datatype operator = Operator of int * fixity * associativity * string list 
    (* int is the precedence,  string list are the named parts *)

    
    type allOperators = operator list

    datatype OpAST = OpAST of (operator * OpAST list )
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
    open RawAST
    val underscoreChar = "〇"

    fun opastAppendArg  (original :  OpAST )(arg : OpAST)  : OpAST = 
        case original of (OpAST (oper, l)) => OpAST(oper, l@[arg])
    fun opastPrependArg  (arg : OpAST) (original :  OpAST ) : OpAST = 
        case original of (OpAST (oper, l)) => OpAST(oper, arg :: l)
    
    fun parseMixfixExpression (ops : allOperators) (exp : RawAST.RawAST) : (OpAST option * RawAST) = 
        raise (Fail "Fail")

end