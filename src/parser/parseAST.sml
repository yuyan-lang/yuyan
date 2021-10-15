
structure ParseAST =
struct
open Operators
   datatype ParseRule = OperatorNameComponent of string * operator
                            | OperatorInternal of operator
                            | PrefixNoneAssoc of operator
                            | PrefixRightAssoc of int
                            | PostfixNoneAssoc of operator
                            | PostfixLeftAssoc of int
                            | InfixNoneAssoc of operator
                            | InfixLeftAssoc of int
                            | InfixLeftAssocLeftArrow of operator
                            | InfixRightAssoc of int
                            | InfixRightAssocRightArrow of operator
                            | Many1 
        datatype ParseOpAST = ParseOpAST of ParseRule * ParseOpAST list
        exception ParseFailure of string


    fun opastAppendArg  (original :  OpAST )(arg : OpAST)  : OpAST = 
        case original of (OpAST (oper, l)) => OpAST(oper, l@[arg])
    fun opastPrependArg  (arg : OpAST) (original :  OpAST ) : OpAST = 
        case original of (OpAST (oper, l)) => OpAST(oper, arg :: l)
    
end