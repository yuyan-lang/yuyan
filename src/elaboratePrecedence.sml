
structure ElaboratePrecedence =
struct
    open MixFixParser

        fun elaborate (past : ParseOpAST) : OpAST = 
            case past of 
                ParseOpAST (r, l) => 
                case (r, l) of 
                    (OperatorInternal oper, l) => (OpAST (oper, map elaborate (List.filter 
                        (* remove name components from operator internal and we're left with INTERNAL arguments *)
                    (fn x => case x of ParseOpAST(OperatorNameComponent _, _) => true | _ => false) l)))
                    | (PrefixNoneAssoc oper, [internal, arg]) => 
                            opastAppendArg (elaborate internal) (elaborate arg)
                    | (PostfixNoneAssoc oper, [arg, internal]) => 
                            opastPrependArg  (elaborate arg)(elaborate internal)
                    | (PrefixRightAssoc oper, [ParseOpAST(Many1, internals), arg]) => 
                        foldr (fn (internal, arg) => 
                            opastAppendArg (elaborate internal) arg
                        ) (elaborate arg) internals
                    | (PostfixLeftAssoc oper, [arg, ParseOpAST(Many1, internals)]) => 
                        foldl (fn (internal, arg) => 
                            opastPrependArg (elaborate internal) arg
                        ) (elaborate arg) internals
                    | (InfixNoneAssoc oper, [argL, internal, argR]) => 
                        opastPrependArg (opastAppendArg (elaborate internal) (elaborate argR)) (elaborate argL)
                    | (InfixLeftAssoc _, [argL, ParseOpAST(Many1, internalsArgR)]) => 
                        foldl (fn (internalArgR, argL) => 
                            case internalArgR of
                                ParseOpAST(InfixLeftAssocLeftArrow oper, [internal, argR]) =>
                            opastAppendArg (opastPrependArg (elaborate internal) argL) (elaborate argR)
                        ) (elaborate argL) internalsArgR
                    | (InfixRightAssoc _, [ParseOpAST(Many1, internalsArgL), argR]) => 
                        foldr (fn (internalArgL, argR) => 
                            case internalArgL of
                                ParseOpAST(InfixRightAssocRightArrow oper, [argL , internal]) =>
                            opastAppendArg (opastPrependArg (elaborate internal) (elaborate argL)) argR
                        ) (elaborate argR) internalsArgL



end