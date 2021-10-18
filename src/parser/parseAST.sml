
structure ParseAST =
struct
open Operators
   datatype ParseRule = OperatorNameComponent of UTF8String.t * operator
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
                            | EOF
                            | ExpWithEOF
                            | UnknownId 
                            | UnknownIdComp of UTF8Char.t
                            | Binding of UTF8String.t
                            | QuotedName of UTF8String.t
                            | UnparsedDeclarationBlock of UTF8String.t (* a declaration block is a block with an immediate . inside of it, thus should not be parsed at this level *)
                            | PlaceHolder (* should not appear anywhere in final result *)
        datatype ParseOpAST = ParseOpAST of ParseRule * ParseOpAST list
        exception ParseFailure of string


    fun opastAppendArg  (original :  OpAST )(arg : OpAST)  : OpAST = 
        case original of (OpAST (oper, l)) => OpAST(oper, l@[arg])
    fun opastPrependArg  (arg : OpAST) (original :  OpAST ) : OpAST = 
        case original of (OpAST (oper, l)) => OpAST(oper, arg :: l)
    
end