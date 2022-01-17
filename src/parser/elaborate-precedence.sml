
structure ElaboratePrecedence =
struct
    open Operators
    open ParseAST

    exception ElaborationFail of ParseOpAST

    fun updateOperatorNameComponents (oper : Operators.operator) (parseOps : ParseOpAST list) : Operators.operator = 
    case oper of 
        Operator(pred, fixity, assoc, comps, uid) => 
        let 
        (* assert length are equal *)
        val _ = if length comps <> length parseOps then raise Fail "ep13" else ()
        val newComps = List.tabulate(length comps, fn i => 
        case List.nth(comps, i) of
            OpCompString _ => (
                case List.nth(parseOps, i) of 
                    ParseOpAST(OperatorNameComponent (name, _), _) => OpCompString name
                    | _ => raise Fail "ep20"
            )
            | comp => comp
        )
        in 
            Operator(pred, fixity, assoc, newComps, uid)
            end

        fun elaborate (past : ParseOpAST) : OpAST = 
            (
                (* print ("ELABORATING " ^ PrettyPrint.show_parseopast past ^ "\n"); *)
            case past of 
                ParseOpAST (r, l) => 
                case (r, l) of 
                    (OperatorInternal oper, l) => (OpAST (
                        updateOperatorNameComponents oper l, map elaborate (List.filter 
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
                        opastPrependArg (elaborate argL) (opastAppendArg (elaborate internal) (elaborate argR)) 
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
                    | (UnknownId, l)  => UnknownOpName ((map (fn ParseOpAST(UnknownIdComp  n, []) => n) l))
                    | (QuotedName s, l)  => UnknownOpName s
                    | (Binding l, [])  => NewOpName l
                    | (UnparsedExpr l, [])  => OpUnparsedExpr l
                    | (UnparsedDecl l, [])  => OpUnparsedDecl l
                    | (StringLiteral (l, qi), [])  => OpStrLiteral (l, qi)
                    | (OperatorNameComponent f, _)  => raise Fail (PrettyPrint.show_parseopast past)
                    | f => raise ElaborationFail (ParseOpAST f)

            )


end