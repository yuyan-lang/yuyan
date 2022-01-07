structure ExpressionConstructionPass =
struct
    open PreprocessingAST
    open TypeCheckingAST
    open Operators
    open PreprocessingOperators
    open OpAST
    
  structure PrecParser = MixFixParser
    exception ECPNoPossibleParse of string
    exception ECPAmbiguousParse of string
    exception ElaborateFailure of string
    exception InternalErrorECP

(* structureName will use the global naming convention *)
    type structureName = StructureName.t

    (* type is type of current module * all prior parsing module's type decl *)
    (* an open will forcibly bring in all previous type module to the current definition *)
    (* bool is visibility *)
    type contextType = structureName * bool * (structureName * bool * Operators.operator list) list
    
    val ~=** = Operators.~=**
    infix 4 ~=**

    fun lookupContextForOpers((curSName,curV,  ctx) : contextType) (sName : structureName) : Operators.operator list =
        case ctx of
            [] => raise ElaborateFailure ("Structure Name " ^ StructureName.toStringPlain sName ^ " not found in context")
            | ((s, v, opl):: ss) => if StructureName.semanticEqual s sName then opl else lookupContextForOpers (curSName, curV,ss) sName
    fun lookupCurrentContextForOpers(ctx as (curSName,curV, imports) : contextType)  : Operators.operator list
    = lookupContextForOpers ctx curSName

    fun insertIntoCurContextOp((curSName,curV, ctx) : contextType) (oper : Operators.operator) : contextType =
        (curSName, curV, ((curSName,curV, oper::lookupContextForOpers (curSName, curV, ctx) curSName)
        :: (List.filter (fn (cname, _, _) => cname <> curSName) ctx)))

    fun insertIntoCurContextOps((curSName,curV, ctx) : contextType) (opers : Operators.operator list) : contextType =
        (curSName, curV, ((curSName, curV, opers@lookupContextForOpers (curSName, curV, ctx) curSName)
        :: (List.filter (fn (cname, _, _) => cname <> curSName) ctx)))




    fun flattenRight (ast : OpAST.OpAST) (curOp : Operators.operator)  : OpAST list = 
        case ast of
        OpAST(oper, [l1,l2]) => if oper ~=** curOp
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

(* emit IntermediateParseOpAST through this function (optional) *)
(* the configure operation notifies the caller about infomrations useful 
for storing token information (i.e., parse tree).
If the caller is not interested in the information,
please provide trivial functions *)
    fun configureAndConstructTypeCheckingASTTopLevel
    (notifyParseOpAST : OpAST.t -> 'a) 
    (notifyDeclarationParserResult : (operator * MixedStr.t list) -> 'b) 
    (notifyDeclarationParsingResult : PreprocessingAST.t -> 'c) 
    : (MixedStr.t list) -> TypeCheckingAST.RSignature =
    let 
    fun elaborateLabeledType (ast : OpAST.t)  (ctx : contextType): Label * Type = 
        case ast of
        OpAST(oper, [NewOpName(l1), l2]) => if 
            oper ~=** labeledTypeCompOp 
            then (l1, elaborateOpASTtoType l2 ctx)
            else raise ElaborateFailure "Expect labeledTypeComp as a child of prod/sum"
        | _ => raise ElaborateFailure "Expect labeledTypeComp as a child of prod/sum"

    and elaborateOpASTtoType 
         (ast : OpAST.OpAST) (ctx : contextType) : TypeCheckingAST.Type = 
        (
            (* print (PrettyPrint.show_opast ast); *)
        case ast of
             UnknownOpName (s) => TypeVar [s]
            | OpUnparsedExpr x => (parseType x ctx) 
            | OpAST(oper, []) => (
                if oper ~=** unitTypeOp then UnitType
                else if oper ~=** nullTypeOp then NullType
                else if oper ~=** builtinTypeStringOp then BuiltinType(BIString)
                else raise InternalErrorECP
                        )
            | OpAST(oper, [a1,a2]) => (
                if oper ~=** prodTypeOp
                then (let val args = flattenRight ast oper
                    in Prod (map (fn x => elaborateLabeledType x ctx) args )
                    end)
                else 
                if oper ~=** sumTypeOp
                then (let val args = flattenRight ast oper
                    in Sum (map (fn x => elaborateLabeledType x ctx) args)
                    end)
                else
                if oper ~=** functionTypeOp
                then Func ((elaborateOpASTtoType a1 ctx),(elaborateOpASTtoType a2 ctx))
                else 
                if oper ~=** universalTypeOp
                then Forall ((elaborateNewName a1),(elaborateOpASTtoType a2 ctx))
                else 
                if oper ~=** existentialTypeOp
                then Exists ((elaborateNewName a1),(elaborateOpASTtoType a2 ctx))
                else 
                if oper ~=** recursiveTypeOp
                then Rho ((elaborateNewName a1),(elaborateOpASTtoType a2 ctx))
                else 
                if oper ~=** structureRefOp
                then TypeVar (map elaborateUnknownName (flattenRight ast structureRefOp))
                else
                raise ElaborateFailure "Expected a type constructor"
            )
            | _ => raise ElaborateFailure "Expected a type constructor"
        )
        handle ElaborateFailure s => 
        raise ElaborateFailure (s ^ "\n when elaborating type "^ PrettyPrint.show_opast ast )
    
    and elaborateOpASTtoExpr  (ast : OpAST.t)(ctx : contextType) : TypeCheckingAST.RExpr = 
    let fun snd (x : OpAST list) : OpAST = (hd (tl x))
    in
        (case ast of
              UnknownOpName (s) => if NumberParser.isNumber s then 
                 (case NumberParser.parseNumber s of 
                    NumberParser.NPInt i => RIntConstant i
                    | NumberParser.NPReal r => RRealConstant r
                 )
              else RExprVar [s]
            | OpUnparsedExpr x => (parseExpr x ctx) 
            | OpStrLiteral l => RStringLiteral l
            | OpAST(oper, l) => (
                if getUID oper >= elabAppBound 
                then (* elab app *)
                    foldl (fn (arg, acc) => RApp (acc,arg)) (RExprVar [getOriginalName oper]) (map (fn x => elaborateOpASTtoExpr x ctx)  l)
                else
                if oper ~=** structureRefOp
                then RExprVar (map elaborateUnknownName (flattenRight ast structureRefOp))
                else
                if oper ~=** unitExprOp
                then RUnitExpr
                else
                if oper ~=** projExprOp
                then RProj(elaborateOpASTtoExpr (hd l) ctx, elaborateUnknownName (snd l))
                else 
                if oper ~=** appExprOp
                then RApp(elaborateOpASTtoExpr (hd l) ctx, elaborateOpASTtoExpr (snd l) ctx)
                else 
                if oper ~=** pairExprOp
                then RTuple(map (fn x => elaborateOpASTtoExpr x ctx) (flattenRight ast pairExprOp))
                else 
                if oper ~=** injExprOp
                then RInj( elaborateUnknownName (hd l), elaborateOpASTtoExpr (snd l) ctx)
                else 
                if oper ~=** foldExprOp
                then RFold( elaborateOpASTtoExpr (hd l) ctx)
                else
                if oper ~=** unfoldExprOp
                then RUnfold( elaborateOpASTtoExpr (hd l) ctx)
                else
                if oper ~=** caseExprOp
                then let
                            val args = flattenRight (snd l) caseAlternativeOp
                            in RCase (elaborateOpASTtoExpr (hd l) ctx, (map (fn x => 
                            case x of
                                OpAST(oper, [lbl, evar, expr]) => 
                                if oper ~=** caseClauseOp
                                then (elaborateUnknownName lbl, 
                                elaborateNewName evar, elaborateOpASTtoExpr expr ctx)
                                else raise ElaborateFailure ("Expected a case clause 1" ^ " got " ^ PrettyPrint.show_opast x)
                                | _ => raise ElaborateFailure ("Expected a case clause 2" ^ " got " ^ PrettyPrint.show_opast x)
                    ) args))
                    end
                else 
                if oper ~=** typeAppExprOp
                then RTApp(elaborateOpASTtoExpr (hd l) ctx, elaborateOpASTtoType (snd l) ctx)
                else
                if oper ~=** packExprOp
                then RPack(elaborateOpASTtoType (hd l) ctx, elaborateOpASTtoExpr (snd l) ctx)
                else
                if oper ~=** unpackExprOp
                then ROpen(elaborateOpASTtoExpr (hd l) ctx, (elaborateNewName (snd l), elaborateNewName (hd (tl (tl l))), 
                elaborateOpASTtoExpr (hd (tl (tl (tl (l))))) ctx))
                else
                if oper ~=** ffiCCallOp
                then RFfiCCall(elaborateOpASTtoExpr (hd l) ctx, elaborateOpASTtoExpr (snd l) ctx)
                else
                if oper ~=** lambdaExprOp
                then RLam(elaborateNewName (hd l), elaborateOpASTtoExpr (snd l) ctx)
                else
                if oper ~=** lambdaExprWithTypeOp
                then RLamWithType(elaborateOpASTtoType (hd l) ctx, 
                elaborateNewName (snd l), elaborateOpASTtoExpr (hd (tl (tl l))) ctx)
                else
                if oper ~=** fixExprOp
                then RFix(elaborateNewName (hd l), 
                elaborateOpASTtoExpr (snd l) ctx)
                else
                if oper ~=** typeLambdaExprOp
                then RTAbs(elaborateNewName (hd l), 
                elaborateOpASTtoExpr (snd l) ctx) 
                else
                if oper ~=** letinOp
                then (
                    let val preprocessedTree = 
                        case (hd l) of
                        OpUnparsedDecl d => preprocessAST d
                        | _ => raise ElaborateFailure "Expect declaration block as first argument to let in"
                        val newOps = extractAllOperators preprocessedTree
                        val declTree = constructOpAST preprocessedTree ctx
                        val bodyExpr = elaborateOpASTtoExpr (snd l) (insertIntoCurContextOps ctx newOps)
                    in RLetIn(declTree, bodyExpr) end
                )
                else
                raise ElaborateFailure "Expected Expression constructs"
            )
                | _ => 
                raise ElaborateFailure "Expected Expression constructs"
        )
        handle ElaborateFailure s => 
        raise ElaborateFailure (s ^ "\n when elaborating expr "^ PrettyPrint.show_opast ast )
    end

            
    (* and recursivelyParseType declParse (opast : OpAST.t) : OpAST.t
    = case opast of
        UnknownOpName _ => opast
        | OpAST(oper, l) => OpAST(oper, map (recursivelyParseType declParse) l)
        | OpUnparsedDecl l => declParse
    and recursivelyParseExpr declParse (opast : OpAST.t) : OpAST.t
   *)


    and parseType (tbody : MixedStr.t)(ctx : contextType) : TypeCheckingAST.Type = 
    let val parseTree = (PrecedenceParser.parseMixfixExpression allTypeOps tbody)
        val _ = notifyParseOpAST parseTree
        in elaborateOpASTtoType  parseTree ctx end
        handle PrecedenceParser.NoPossibleParse s => 
            raise ECPNoPossibleParse ("Parsing failed at " ^  s ^ " No possible parse. Double check your grammar.")
        | PrecedenceParser.AmbiguousParse s => 
            raise ECPAmbiguousParse ("Parsing failed at " ^  s ^ " Ambiguous parse. Double check your grammar.")
    and parseExpr (ebody : MixedStr.t)(ctx : contextType) : TypeCheckingAST.RExpr
    =  let val parseTree = (PrecedenceParser.parseMixfixExpression 
                (allTypeAndExprOps@ lookupCurrentContextForOpers ctx) ebody)
           val _ = DebugPrint.p (PrettyPrint.show_opast parseTree ^ "\n\n")
           val _ = notifyParseOpAST parseTree
    in elaborateOpASTtoExpr parseTree ctx end
        handle PrecedenceParser.NoPossibleParse s => 
            raise ECPNoPossibleParse ("Parsing failed at " ^  s ^ " No possible parse. Double check your grammar.")
        | PrecedenceParser.AmbiguousParse s => 
            raise ECPAmbiguousParse ("Parsing failed at " ^  s ^ " Ambiguous parse. Double check your grammar.")
    
    and parsePOperator (POpDeclaration(opName, assoc, pred)) =let 
                    val oper = Operators.parseOperator 
                            opName (assoc <> Operators.NoneAssoc) (assoc = Operators.LeftAssoc) pred []
                            in (
                                (* print (" PARSED OPER AS " ^ PrettyPrint.show_op oper);  *)
                                oper) end
    and extractAllOperators (ast : PreprocessingAST.t) : Operators.operator list = 
        case ast of 
            [] => []
            | (x :: xs) => 
                (case x of 
                     POpDeclaration(opName, assoc, pred) => parsePOperator(x) :: extractAllOperators xs
                    | _ => extractAllOperators xs
                )

    and constructOpAST  (ast : PreprocessingAST.t) (ctx as (curSName, curV, addedOps) : contextType) 
    (* after constructing the ast, we need to get its list of operators *)
        : TypeCheckingAST.RSignature =
        (
        (* print ("\rRemaining: "^ Int.toString(List.length ast) ^ " statements"); *)
        (* print ("constructOpAST on "^ PrettyPrint.show_preprocessaast ast ^ " with curSName = " ^ 
        StructureName.toStringPlain curSName ^ " with addedOps = " ^ PrettyPrint.show_ecpops addedOps ^"\n\n"); *)
        case ast of 
            [] => []
            | (x :: xs) => 
                let fun trailingNoOps() = constructOpAST xs ctx
                fun trailingWithOps(addedOp:Operators.operator): TypeCheckingAST.RSignature 
                    = constructOpAST xs (insertIntoCurContextOp ctx addedOp)
                in
                (case x of 
                    PTypeMacro(tname, tbody) => RTypeMacro(tname, parseType tbody ctx) :: trailingNoOps()
                    | PTermTypeJudgment(ename, tbody) => RTermTypeJudgment(ename, parseType  tbody ctx) :: trailingNoOps()
                    | PTermMacro(ename, ebody) => RTermMacro(ename, parseExpr ebody ctx) :: trailingNoOps()
                    | PTermDefinition(ename, ebody) => RTermDefinition(ename, parseExpr ebody ctx) :: trailingNoOps()
                    | POpDeclaration(opName, assoc, pred) => trailingWithOps(parsePOperator(x)) 
                    | PDirectExpr(ebody) => RDirectExpr(parseExpr ebody ctx) :: trailingNoOps()
                    | PComment _ => trailingNoOps()
                    | PStructure(publicVisible, sname, decls) => 
                    let val preprocessedTree = preprocessAST decls
                        val newOps = extractAllOperators preprocessedTree
                        val declTree = constructOpAST preprocessedTree ctx
                    in RStructure(publicVisible,sname, declTree)::
                        constructOpAST xs (curSName, curV, ((curSName@[sname], publicVisible, newOps):: addedOps)) end
                    | POpenStructure(sname) =>  (* open will be as if there is a local declaration with 
                    the same name as the public members of the structure *)
                        ROpenStructure(sname) :: constructOpAST xs (insertIntoCurContextOps ctx (lookupContextForOpers ctx (curSName@sname)))
                )
                end
        )
        handle ElaborateFailure x => raise ElaborateFailure (x ^ 
            "\n when elaborating declaration " ^ PrettyPrint.show_preprocessaast ast)
                

    and constructTypeCheckingASTTopLevel
     ( ast : PreprocessingAST.t) : TypeCheckingAST.RSignature = 
    let 
        (* val _ = print ("Total "^ Int.toString(List.length ast) ^ " statements\n"); *)
        val res =  constructOpAST ast (StructureName.topLevelName, true, [
            (StructureName.topLevelName, true, [])
        ])
        (* val _ = print ("Done "^ Int.toString(List.length ast) ^ " statements\n"); *)
    in 
        res end
    


    and parseJudgment (s : MixedStr.t) : pJudgment = 
    (let
       (* val _ = print ("Parsing judgment on" ^ PrettyPrint.show_mixedstr s ^ "\n"); *)
       val tp  = MixedStr.toPlainUTF8String
       fun getDeclContent (x : MixedStr.t) = case x of
        [MixedStr.UnparsedDeclaration y] => y
        | _ => raise ElaborateFailure "expecting a single unparsed declaration"
       val declParseTree = DeclarationParser.parseDeclarationSingleOutput declOps s
       val _ = notifyDeclarationParserResult declParseTree
       val res = case declParseTree of
            (oper, [l1, l2]) => 
            if oper ~=** typeMacroOp
            then PTypeMacro (tp l1, l2)
            else if oper ~=** termTypeJudgmentOp
            then PTermTypeJudgment (tp l1, l2)
            else if oper ~=** termMacroOp
            then PTermMacro (tp l1, l2)
            else if oper ~=** termDefinitionOp
            then PTermDefinition (tp l1, l2)
            else if oper ~=** privateStructureOp
            then PStructure (false, tp l1, (getDeclContent l2))
            else if oper ~=** publicStructureOp
            then PStructure (true, tp l1, (getDeclContent l2))
            else  
            raise Fail "pp34"
            | (oper, [l1, l2, l3]) =>  
                if oper ~=** opDeclarationOp
                then POpDeclaration (tp l1, parseAssoc (tp l2), NumberParser.parseInteger (tp l3))
                else raise Fail "pp85"
            | (oper, [l1]) =>  
                if oper ~=** commentOp
                then PComment (l1)
                else 
                if oper ~=** openStructureOp
                then let 
                val parsedStructureRef = PrecedenceParser.parseMixfixExpression [structureRefOp] (l1)
                val _ = notifyParseOpAST parsedStructureRef
                val names = flattenRight parsedStructureRef structureRefOp
                        in POpenStructure (map elaborateUnknownName names)
                end 
                else
                raise Fail "pp95"
            | _ => raise Fail "pp26: malformed output : not two args or three args"
        (* val _ = print ("returning " ^ PrettyPrint.show_preprocessaastJ res) *)
        in res end
        handle DeclarationParser.DeclNoParse (expr) => PDirectExpr expr 
        handle ECPNoPossibleParse x => raise ECPNoPossibleParse (x ^ 
            "\n when parsing declaration " ^ MixedStr.toString s)
        handle ElaborateFailure x => raise ElaborateFailure (x ^ 
            "\n when parsing declaration " ^ MixedStr.toString s)
    )

    and preprocessAST (s : MixedStr.t list) : PreprocessingAST.t = 
    (
        (* print ("preprocessAST : " ^ Int.toString (length s) ^ " count : " ^PrettyPrint.show_mixedstrs s ^"\n"); *)
    (* case s of  *)
         (* [MixedStr.UnparsedDeclaration l]  =>  *)
         map parseJudgment s
        (* | _ => [PDirectExpr s] *)
    )

    in 
    fn s => constructTypeCheckingASTTopLevel (preprocessAST s)
    end
        
end
