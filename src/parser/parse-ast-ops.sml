structure ParseASTOps = struct
    open ParseAST


    fun opastAppendArg  (original :  OpAST )(arg : OpAST)  : OpAST = 
        case original of (OpAST (oper, l)) => OpAST(oper, l@[arg])
                        | _ => raise Fail "past39"
    fun opastPrependArg  (arg : OpAST) (original :  OpAST ) : OpAST = 
        case original of (OpAST (oper, l)) => OpAST(oper, arg :: l)
                        | _ => raise Fail ("past42 : on " ^ PrettyPrint.show_opast arg ^ " and " ^ PrettyPrint.show_opast original)
 
end