
structure ElaboratePrecedence =
struct
    open Operators
    open ParseAST

    exception ElaborationFail of ParseOpAST

        fun elaborate (past : ParseOpAST) : OpAST = 
            (
                (* print ("ELABORATING " ^ PrettyPrint.show_parseopast past ^ "\n"); *)
            case past of 
                ParseOpAST (r, l) => 
                case (r, l) of 
                    (OperatorInternal oper, l) => (OpAST (oper, map elaborate (List.filter 
                        (* remove name components from operator internal and we're left with INTERNAL arguments *)
                    (fn x => case x of ParseOpAST(OperatorNameComponent _, _) => false | _ => true) l)))
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
                        foldl (fn (internalArgR, argL) => (
                            (* print ("ARGL is " ^ PrettyPrint.show_opast argL ^ "\n"); *)
                            case internalArgR of
                                ParseOpAST(InfixLeftAssocLeftArrow oper, [internal, argR]) =>
                            opastAppendArg (opastPrependArg argL (elaborate internal)) (elaborate argR)
                            | _ => raise Fail "33")
                        ) (elaborate argL) internalsArgR
                    | (InfixRightAssoc _, [ParseOpAST(Many1, internalsArgL), argR]) => 
                        foldr (fn (internalArgL, argR) => 
                            case internalArgL of
                                ParseOpAST(InfixRightAssocRightArrow oper, [argL , internal]) =>
                            opastAppendArg (opastPrependArg (elaborate argL) (elaborate internal)) argR
                            | _ => raise Fail "42"
                        ) (elaborate argR) internalsArgL
                    | (ExpWithEOF, [opAST, ParseOpAST(EOF, [])])  => elaborate opAST
                    | (UnknownId, l)  => UnknownOpName ((map (fn ParseOpAST(UnknownIdComp (RawAST.RawID n), []) => n) l))
                    | (Binding l, [])  => NewOpName ((map (fn (RawAST.RawID n) => n) l))
                    | (OperatorNameComponent f, _)  => raise Fail (PrettyPrint.show_parseopast past)
                    | f => raise ElaborationFail (ParseOpAST f)

            )


end