structure ExpressionConstructionPass =
struct
    open PreprocessingAST
    open TypeCheckingAST
    open Operators
    
    (* Precedence hierachy: 
    
        On Types: (from lowest) Rho < Exists < Forall < Function < Sum < Prod < 0/1
        On Expressons : 
        (from highest)
            () >  Proj > App > Pair = Inj = fold >  unfold  > Case > TApp 
            >  pack > open > Lambda > Lambda/wtype > fix >  TLambda
    
    
    *)

    (* l : e *)
    val unitTypeOp  = Operators.parseOperatorStr "有" false false 42 []
    val nullTypeOp  = Operators.parseOperatorStr "无" false false 42 []
    val labeledTypeCompOp  = Operators.parseOperatorStr "夫〇表〇" false false 40 [1]
    val prodTypeOp  = Operators.parseOperatorStr "〇合〇" true false 38 []
    val sumTypeOp  = Operators.parseOperatorStr "〇亦〇" true false 36 []
    val functionTypeOp  = Operators.parseOperatorStr "化〇而〇" true false 35 []
    val universalTypeOp  = Operators.parseOperatorStr "承〇而〇" true false 34 [1]
    val existentialTypeOp  = Operators.parseOperatorStr "有〇则〇" true false 32 [1]
    val recursiveTypeOp  = Operators.parseOperatorStr "复〇为〇" true false 30 [1]

    val unitExprOp = Operators.parseOperatorStr "元" true false 72 []
    val projExprOp = Operators.parseOperatorStr "〇中〇" true false 70 []
    val appExprOp = Operators.parseOperatorStr "〇于〇" true true 69 []
    val pairExprOp = Operators.parseOperatorStr "〇与〇" true false 68 []
    val injExprOp = Operators.parseOperatorStr "〇临〇" false false 67 []
    val foldExprOp = Operators.parseOperatorStr "卷〇" true false 66 []
    val unfoldExprOp = Operators.parseOperatorStr "舒〇" true false 65 []
    val caseClauseOp = Operators.parseOperatorStr "曰〇则有〇而〇" true false 64 [3]
    val caseAlternativeOp = Operators.parseOperatorStr "〇或〇" true false 63 []
    val caseExprOp = Operators.parseOperatorStr "鉴〇而〇" true false 62 []
    val typeAppExprOp = Operators.parseOperatorStr "授〇以〇" true false 60 []
    val packExprOp = Operators.parseOperatorStr "入〇合〇" true false 56 []
    val unpackExprOp = Operators.parseOperatorStr "开〇则有〇者〇而〇" true false 54 [3,5]
    val lambdaExprOp = Operators.parseOperatorStr "会〇而〇" true false 52 [1]
    val lambdaExprWithTypeOp = Operators.parseOperatorStr "遇〇者〇而〇" true false 52 [3]
    val fixExprOp = Operators.parseOperatorStr "循〇以〇" true false 51 [1]
    val typeLambdaExprOp = Operators.parseOperatorStr "受〇而〇" true false 50 [1]

    val elabAppBound = UID.next() (* This is a hack since uid is monotonically increasing *)

    val allTypeOps = [
        unitTypeOp, nullTypeOp, labeledTypeCompOp, prodTypeOp, sumTypeOp, functionTypeOp,
        universalTypeOp, existentialTypeOp, recursiveTypeOp]
    val allTypeAndExprOps = allTypeOps @ [ unitExprOp,
        projExprOp, appExprOp, pairExprOp, injExprOp, foldExprOp, unfoldExprOp, caseClauseOp, 
        caseAlternativeOp, caseExprOp, typeAppExprOp, packExprOp, unpackExprOp, lambdaExprOp,
        lambdaExprWithTypeOp, fixExprOp, typeLambdaExprOp
    ]

    exception ElaborateFailure of string
    exception InternalErrorECP
    fun flattenRight (ast : Operators.OpAST) (curOp : Operators.operator)  : OpAST list = 
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
    
    fun elaborateLabeledType (ast : Operators.OpAST) : Label * Type = 
        case ast of
        OpAST(oper, [NewOpName(l1), l2]) => if 
            oper = labeledTypeCompOp 
            then (l1, elaborateOpASTtoType l2)
            else raise ElaborateFailure "Expect labeledTypeComp as a child of prod/sum"
        | _ => raise ElaborateFailure "Expect labeledTypeComp as a child of prod/sum"

    and elaborateOpASTtoType (ast : Operators.OpAST) : TypeCheckingAST.Type = 
        (
            (* print (PrettyPrint.show_opast ast); *)
        case ast of
             UnknownOpName (s) => TypeVar s
            | OpAST(oper, []) => (
                if oper = unitTypeOp then UnitType
                else if oper = nullTypeOp then NullType
                else raise InternalErrorECP
                        )
            | OpAST(oper, [a1,a2]) => (
                if oper = prodTypeOp
                then (let val args = flattenRight ast oper
                    in Prod (map elaborateLabeledType args)
                    end)
                else 
                if oper = sumTypeOp
                then (let val args = flattenRight ast oper
                    in Sum (map elaborateLabeledType args)
                    end)
                else
                if oper = functionTypeOp
                then Func ((elaborateOpASTtoType a1),(elaborateOpASTtoType a2))
                else 
                if oper = universalTypeOp
                then Forall ((elaborateNewName a1),(elaborateOpASTtoType a2))
                else 
                if oper = existentialTypeOp
                then Exists ((elaborateNewName a1),(elaborateOpASTtoType a2))
                else 
                if oper = recursiveTypeOp
                then Rho ((elaborateNewName a1),(elaborateOpASTtoType a2))
                else 
                raise ElaborateFailure "Expected a type constructor"
            )
            | _ => raise ElaborateFailure "Expected a type constructor"
        )
        handle ElaborateFailure s => 
        raise ElaborateFailure (s ^ "\n when elaborating "^ PrettyPrint.show_opast ast )
    
    fun elaborateOpASTtoExpr (ast : Operators.OpAST) : TypeCheckingAST.Expr = 
    let fun snd (x : OpAST list) : OpAST = (hd (tl x))
    in
        (case ast of
             UnknownOpName (s) => ExprVar s
            | OpAST(oper, l) => (
                if getUID oper >= elabAppBound 
                then (* elab app *)
                    foldl (fn (arg, acc) => App (acc,arg)) (ExprVar (getOriginalName oper)) (map elaborateOpASTtoExpr l)
                else
                if oper = unitExprOp
                then UnitExpr
                else
                if oper = projExprOp
                then Proj(elaborateOpASTtoExpr (hd l), elaborateUnknownName (snd l))
                else 
                if oper = appExprOp
                then App(elaborateOpASTtoExpr (hd l), elaborateOpASTtoExpr (snd l))
                else 
                if oper = pairExprOp
                then Tuple(map elaborateOpASTtoExpr (flattenRight ast pairExprOp))
                else 
                if oper = injExprOp
                then Inj( elaborateUnknownName (hd l), elaborateOpASTtoExpr (snd l))
                else 
                if oper = foldExprOp
                then Fold( elaborateOpASTtoExpr (hd l))
                else
                if oper = unfoldExprOp
                then Unfold( elaborateOpASTtoExpr (hd l))
                else
                if oper = caseExprOp
                then let
                    val args = flattenRight (snd l) caseAlternativeOp
                    in Case (elaborateOpASTtoExpr (hd l), (map (fn x => 
                    case x of
                        OpAST(oper, [lbl, evar, expr]) => 
                        if oper = caseClauseOp
                        then (elaborateUnknownName lbl, 
                        elaborateNewName evar, elaborateOpASTtoExpr expr)
                        else raise ElaborateFailure "Expected a case clause"
                        | _ => raise ElaborateFailure "Expected a case clause"
            ) args))
            end
                else 
                if oper = typeAppExprOp
                then TApp(elaborateOpASTtoExpr (hd l), elaborateOpASTtoType (snd l))
                else
                if oper = packExprOp
                then Pack(elaborateOpASTtoType (hd l), elaborateOpASTtoExpr (snd l))
                else
                if oper = unpackExprOp
                then Open(elaborateOpASTtoExpr (hd l), (elaborateNewName (snd l), elaborateNewName (hd (tl (tl l))), 
                elaborateOpASTtoExpr (hd (tl (tl (tl (l)))))))
                else
                if oper = lambdaExprOp
                then Lam(elaborateNewName (hd l), elaborateOpASTtoExpr (snd l))
                else
                if oper = lambdaExprWithTypeOp
                then LamWithType(elaborateOpASTtoType (hd l), 
                elaborateNewName (snd l), elaborateOpASTtoExpr (hd (tl (tl l))))
                else
                if oper = fixExprOp
                then Fix(elaborateNewName (hd l), 
                elaborateOpASTtoExpr (snd l))
                else
                if oper = typeLambdaExprOp
                then TAbs(elaborateNewName (hd l), 
                elaborateOpASTtoExpr (snd l))
                else
                raise ElaborateFailure "Expected Expression constructs"
            )
                | _ => 
                raise ElaborateFailure "Expected Expression constructs"
        )
        handle ElaborateFailure s => 
        raise ElaborateFailure (s ^ "\n when elaborating "^ PrettyPrint.show_opast ast )
    end

            

    structure PrecParser = MixFixParser
    exception ECPNoPossibleParse of MixedStr.t

    fun parseType (tbody : MixedStr.t)(addedOps : Operators.operator list) : TypeCheckingAST.Type = 
        elaborateOpASTtoType (PrecParser.parseMixfixExpression allTypeOps tbody) 
        handle PrecParser.NoPossibleParse s => raise ECPNoPossibleParse s
    fun parseExpr (ebody : MixedStr.t)(addedOps : Operators.operator list) : TypeCheckingAST.Expr
    = elaborateOpASTtoExpr (PrecParser.parseMixfixExpression (allTypeAndExprOps@addedOps) ebody) 
        handle PrecParser.NoPossibleParse s => raise ECPNoPossibleParse s
    

    fun constructOpAST (ast : PreprocessingAST.t) (addedOps : Operators.operator list) 
        : TypeCheckingAST.Signature = 
        (
        print ("\rRemaining: "^ Int.toString(List.length ast) ^ " statements");
        case ast of 
            [] => []
            | (x :: xs) => 
                let fun trailingNoOps() = constructOpAST xs addedOps
                fun trailingWithOps(addedOp:Operators.operator): TypeCheckingAST.Signature 
                    = constructOpAST xs (addedOps@[addedOp])
                in
                (case x of 
                    PTypeMacro(tname, tbody) => TypeMacro(tname, parseType tbody addedOps) :: trailingNoOps()
                    | PTermTypeJudgment(ename, tbody) => TermTypeJudgment(ename, parseType tbody addedOps) :: trailingNoOps()
                    | PTermMacro(ename, ebody) => TermMacro(ename, parseExpr ebody addedOps) :: trailingNoOps()
                    | PTermDefinition(ename, ebody) => TermDefinition(ename, parseExpr ebody addedOps) :: trailingNoOps()
                    | POpDeclaration(opName, assoc, pred) => trailingWithOps(Operators.parseOperator 
                            opName (assoc = Operators.NoneAssoc) (assoc = Operators.LeftAssoc) pred [])
                    | PDirectExpr(ebody) => DirectExpr(parseExpr ebody addedOps) :: trailingNoOps()
                    | PComment _ => trailingNoOps()
                )
                end
        )
                

    fun constructTypeCheckingAST ( ast : PreprocessingAST.t) : TypeCheckingAST.Signature = 
    let 
        val _ = print ("Total "^ Int.toString(List.length ast) ^ " statements\n");
        val res =  constructOpAST ast []
        val _ = print ("Done "^ Int.toString(List.length ast) ^ " statements\n");
    in 
        res end
        
end
