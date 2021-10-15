
structure ParseAST =
struct
open Operators
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

            val underscoreChar = "〇"

    fun opastAppendArg  (original :  OpAST )(arg : OpAST)  : OpAST = 
        case original of (OpAST (oper, l)) => OpAST(oper, l@[arg])
    fun opastPrependArg  (arg : OpAST) (original :  OpAST ) : OpAST = 
        case original of (OpAST (oper, l)) => OpAST(oper, arg :: l)
    
end