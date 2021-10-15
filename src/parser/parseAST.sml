
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


fun show_parseopast x = case x of
    ParseOpAST (r, l) => show_parseRule r ^ "[" ^ (String.concatWith ", " (map show_parseopast l))  ^"]"

and show_parseRule x = case x of
    OperatorNameComponent f => "OperatorNameComponent" ^ f
    | OperatorInternal oper => "OperatorInternal " ^ PrettyPrint.show_op oper
    | PrefixNoneAssoc oper=> "PrefixNoneAssoc "^ PrettyPrint.show_op oper
    | PrefixRightAssoc oper=> "PrefixRightAssoc "^ PrettyPrint.show_op oper
    | PostfixNoneAssoc oper=> "PostfixNoneAssoc "^ PrettyPrint.show_op oper
    | PostfixLeftAssoc oper=> "PostfixLeftAssoc "^ PrettyPrint.show_op oper
    | InfixNoneAssoc oper=> "InfixNoneAssoc "^ PrettyPrint.show_op oper
    | InfixLeftAssoc oper=> "InfixLeftAssoc "^ PrettyPrint.show_op oper
    | InfixLeftAssocLeftArrow oper=> "InfixLeftAssocLeftArrow "^ PrettyPrint.show_op oper
    | InfixRightAssoc oper=> "InfixRightAssoc "^ PrettyPrint.show_op oper
    | InfixRightAssocRightArrow oper=> "InfixRightAssocRightArrow "^ PrettyPrint.show_op oper
    | Many1  => "Many1"

    fun opastAppendArg  (original :  OpAST )(arg : OpAST)  : OpAST = 
        case original of (OpAST (oper, l)) => OpAST(oper, l@[arg])
    fun opastPrependArg  (arg : OpAST) (original :  OpAST ) : OpAST = 
        case original of (OpAST (oper, l)) => OpAST(oper, arg :: l)
    
end