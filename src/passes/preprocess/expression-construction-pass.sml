structure ExpressionConstructionPass =
struct
    open PreprocessingAST
    open TypeCheckingAST
    open Operators
    open PreprocessingOperators
    open OpAST
    
  structure PrecParser = MixFixParser
    exception ECPNoPossibleParse of string
    exception ElaborateFailure of string
    exception InternalErrorECP


    fun flattenRight (ast : OpAST.OpAST) (curOp : Operators.operator)  : OpAST list = 
        case ast of
        OpAST(oper, [l1,l2]) => if oper = curOp
                then l1 :: flattenRight l2 curOp
                else [ast]
        | _ => [ast]
    
    fun elaborateUnknownName (ast : OpAST) : UTF8String.t = 
        case ast of
        UnknownOpName(l1) => l1
        | _ => raise ElaborateFailure "Expect name here, (this is perhaps a bug in the design, but until we fix it, put a bracket around the name expecting expressions,  the parser may have incorrectly parsed that as an expression)"

    fun elaborateNewName (ast : OpAST) : UTF8String.t = 
        case ast of
        NewOpName(l1) => l1
        | _ => raise ElaborateFailure "Expect new name (perhaps internal)"
    
    fun elaborateLabeledType (ast : OpAST.t)  (addedOps : Operators.operator list): Label * Type = 
        case ast of
        OpAST(oper, [NewOpName(l1), l2]) => if 
            oper = labeledTypeCompOp 
            then (l1, elaborateOpASTtoType l2 addedOps)
            else raise ElaborateFailure "Expect labeledTypeComp as a child of prod/sum"
        | _ => raise ElaborateFailure "Expect labeledTypeComp as a child of prod/sum"

    and elaborateOpASTtoType 
         (ast : OpAST.OpAST) (addedOps : Operators.operator list) : TypeCheckingAST.Type = 
        (
            (* print (PrettyPrint.show_opast ast); *)
        case ast of
             UnknownOpName (s) => TypeVar s
            | OpUnparsedExpr x => (parseType x addedOps) 
            | OpAST(oper, []) => (
                if oper = unitTypeOp then UnitType
                else if oper = nullTypeOp then NullType
                else raise InternalErrorECP
                        )
            | OpAST(oper, [a1,a2]) => (
                if oper = prodTypeOp
                then (let val args = flattenRight ast oper
                    in Prod (map (fn x => elaborateLabeledType x addedOps) args )
                    end)
                else 
                if oper = sumTypeOp
                then (let val args = flattenRight ast oper
                    in Sum (map (fn x => elaborateLabeledType x addedOps) args)
                    end)
                else
                if oper = functionTypeOp
                then Func ((elaborateOpASTtoType a1 addedOps),(elaborateOpASTtoType a2 addedOps))
                else 
                if oper = universalTypeOp
                then Forall ((elaborateNewName a1),(elaborateOpASTtoType a2 addedOps))
                else 
                if oper = existentialTypeOp
                then Exists ((elaborateNewName a1),(elaborateOpASTtoType a2 addedOps))
                else 
                if oper = recursiveTypeOp
                then Rho ((elaborateNewName a1),(elaborateOpASTtoType a2 addedOps))
                else 
                raise ElaborateFailure "Expected a type constructor"
            )
            | _ => raise ElaborateFailure "Expected a type constructor"
        )
        handle ElaborateFailure s => 
        raise ElaborateFailure (s ^ "\n when elaborating "^ PrettyPrint.show_opast ast )
    
    and elaborateOpASTtoExpr  (ast : OpAST.t)(addedOps : Operators.operator list) : TypeCheckingAST.Expr = 
    let fun snd (x : OpAST list) : OpAST = (hd (tl x))
    in
        (case ast of
              UnknownOpName (s) => ExprVar s
            | OpUnparsedExpr x => (parseExpr x addedOps) 
            | OpStrLiteral l => StringLiteral l
            | OpAST(oper, l) => (
                if getUID oper >= elabAppBound 
                then (* elab app *)
                    foldl (fn (arg, acc) => App (acc,arg)) (ExprVar (getOriginalName oper)) (map (fn x => elaborateOpASTtoExpr x addedOps)  l)
                else
                if oper = unitExprOp
                then UnitExpr
                else
                if oper = projExprOp
                then Proj(elaborateOpASTtoExpr (hd l) addedOps, elaborateUnknownName (snd l))
                else 
                if oper = appExprOp
                then App(elaborateOpASTtoExpr (hd l) addedOps, elaborateOpASTtoExpr (snd l) addedOps)
                else 
                if oper = pairExprOp
                then Tuple(map (fn x => elaborateOpASTtoExpr x addedOps) (flattenRight ast pairExprOp))
                else 
                if oper = injExprOp
                then Inj( elaborateUnknownName (hd l), elaborateOpASTtoExpr (snd l) addedOps)
                else 
                if oper = foldExprOp
                then Fold( elaborateOpASTtoExpr (hd l) addedOps)
                else
                if oper = unfoldExprOp
                then Unfold( elaborateOpASTtoExpr (hd l) addedOps)
                else
                if oper = caseExprOp
                then let
                    val args = flattenRight (snd l) caseAlternativeOp
                    in Case (elaborateOpASTtoExpr (hd l) addedOps, (map (fn x => 
                    case x of
                        OpAST(oper, [lbl, evar, expr]) => 
                        if oper = caseClauseOp
                        then (elaborateUnknownName lbl, 
                        elaborateNewName evar, elaborateOpASTtoExpr expr addedOps)
                        else raise ElaborateFailure "Expected a case clause"
                        | _ => raise ElaborateFailure "Expected a case clause"
            ) args))
            end
                else 
                if oper = typeAppExprOp
                then TApp(elaborateOpASTtoExpr (hd l) addedOps, elaborateOpASTtoType (snd l) addedOps)
                else
                if oper = packExprOp
                then Pack(elaborateOpASTtoType (hd l) addedOps, elaborateOpASTtoExpr (snd l) addedOps)
                else
                if oper = unpackExprOp
                then Open(elaborateOpASTtoExpr (hd l) addedOps, (elaborateNewName (snd l), elaborateNewName (hd (tl (tl l))), 
                elaborateOpASTtoExpr (hd (tl (tl (tl (l))))) addedOps))
                else
                if oper = lambdaExprOp
                then Lam(elaborateNewName (hd l), elaborateOpASTtoExpr (snd l) addedOps)
                else
                if oper = lambdaExprWithTypeOp
                then LamWithType(elaborateOpASTtoType (hd l) addedOps, 
                elaborateNewName (snd l), elaborateOpASTtoExpr (hd (tl (tl l))) addedOps)
                else
                if oper = fixExprOp
                then Fix(elaborateNewName (hd l), 
                elaborateOpASTtoExpr (snd l) addedOps)
                else
                if oper = typeLambdaExprOp
                then TAbs(elaborateNewName (hd l), 
                elaborateOpASTtoExpr (snd l) addedOps) 
                else
                raise ElaborateFailure "Expected Expression constructs"
            )
                | _ => 
                raise ElaborateFailure "Expected Expression constructs"
        )
        handle ElaborateFailure s => 
        raise ElaborateFailure (s ^ "\n when elaborating "^ PrettyPrint.show_opast ast )
    end

            
    (* and recursivelyParseType declParse (opast : OpAST.t) : OpAST.t
    = case opast of
        UnknownOpName _ => opast
        | OpAST(oper, l) => OpAST(oper, map (recursivelyParseType declParse) l)
        | OpUnparsedDecl l => declParse
    and recursivelyParseExpr declParse (opast : OpAST.t) : OpAST.t
   *)


    and parseType (tbody : MixedStr.t)(addedOps : Operators.operator list) : TypeCheckingAST.Type = 
        elaborateOpASTtoType (PrecedenceParser.parseMixfixExpression allTypeOps tbody)  addedOps
        handle PrecedenceParser.NoPossibleParse s => 
            raise ECPNoPossibleParse ("Parsing failed at " ^  s ^ " No possible parse. Double check your grammar.")
    and parseExpr (ebody : MixedStr.t)(addedOps : Operators.operator list) : TypeCheckingAST.Expr
    = elaborateOpASTtoExpr (PrecedenceParser.parseMixfixExpression (allTypeAndExprOps@addedOps) ebody) addedOps 
        handle PrecedenceParser.NoPossibleParse s => 
            raise ECPNoPossibleParse ("Parsing failed at " ^  s ^ " No possible parse. Double check your grammar.")
    

    and constructOpAST  (ast : PreprocessingAST.t) (addedOps : Operators.operator list) 
        : TypeCheckingAST.Signature = 
        (
        (* print ("\rRemaining: "^ Int.toString(List.length ast) ^ " statements"); *)
        case ast of 
            [] => []
            | (x :: xs) => 
                let fun trailingNoOps() = constructOpAST xs addedOps
                fun trailingWithOps(addedOp:Operators.operator): TypeCheckingAST.Signature 
                    = constructOpAST xs (addedOps@[addedOp])
                in
                (case x of 
                    PTypeMacro(tname, tbody) => TypeMacro(tname, parseType tbody addedOps) :: trailingNoOps()
                    | PTermTypeJudgment(ename, tbody) => TermTypeJudgment(ename, parseType  tbody addedOps) :: trailingNoOps()
                    | PTermMacro(ename, ebody) => TermMacro(ename, parseExpr ebody addedOps) :: trailingNoOps()
                    | PTermDefinition(ename, ebody) => TermDefinition(ename, parseExpr ebody addedOps) :: trailingNoOps()
                    | POpDeclaration(opName, assoc, pred) => trailingWithOps(let 
                    val oper = Operators.parseOperator 
                            opName (assoc <> Operators.NoneAssoc) (assoc = Operators.LeftAssoc) pred []
                            in (
                                (* print (" PARSED OPER AS " ^ PrettyPrint.show_op oper);  *)
                                oper) end) 
                    | PDirectExpr(ebody) => DirectExpr(parseExpr ebody addedOps) :: trailingNoOps()
                    | PComment _ => trailingNoOps()
                )
                end
        )
                

    and constructTypeCheckingAST 
     ( ast : PreprocessingAST.t) : TypeCheckingAST.Signature = 
    let 
        (* val _ = print ("Total "^ Int.toString(List.length ast) ^ " statements\n"); *)
        val res =  constructOpAST ast []
        (* val _ = print ("Done "^ Int.toString(List.length ast) ^ " statements\n"); *)
    in 
        res end
    


    and parseJudgment (s : MixedStr.t) : pJudgment = 
    (let
       (* val _ = print ("Parsing judgment on" ^ UTF8String.toString s ^ "\n"); *)
       val tp  = MixedStr.toPlainUTF8String
       val res = 
        case DeclarationParser.parseDeclarationSingleOutput declOps s of
            (oper, [l1, l2]) => 
            if oper = typeMacroOp
            then PTypeMacro (tp l1, l2)
            else if oper = termTypeJudgmentOp
            then PTermTypeJudgment (tp l1, l2)
            else if oper = termMacroOp
            then PTermMacro (tp l1, l2)
            else if oper = termDefinitionOp
            then PTermDefinition (tp l1, l2)
            else  raise Fail "pp34"
            | (oper, [l1, l2, l3]) =>  
                if oper = opDeclarationOp
                then POpDeclaration (tp l1, parseAssoc (tp l2), parsePrecedence (tp l3))
                else raise Fail "pp85"
            | (oper, [l1]) =>  
                if oper = commentOp
                then PComment (l1)
                else raise Fail "pp95"
            | _ => raise Fail "pp26: malformed output : not two args or three args"
        (* val _ = print ("returning " ^ PrettyPrint.show_preprocessaastJ res) *)
        in res end
        handle DeclarationParser.DeclNoParse (expr) => PDirectExpr expr 
    )

    and preprocessAST (s : MixedStr.t list) : PreprocessingAST.t = 
    (
        (* print (PrettyPrint.show_statementast s); *)
    (* case s of  *)
         (* [MixedStr.UnparsedDeclaration l]  =>  *)
         map parseJudgment s
        (* | _ => [PDirectExpr s] *)
    )
        
end
