structure ExpressionConstructionPass =
struct
    open PreprocessingAST
    open TypeCheckingAST
    open Operators
    open PreprocessingOperators
    open OpAST

    open StaticErrorStructure
    infix 5 >>=
    infix 6 =/=

  structure PrecParser = MixFixParser
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



    fun ::/ ((x,y), (xs,ys)) =(x :: xs, y :: ys)
    infix 5 ::/

    fun flattenRight (ast : OpAST.OpAST) (curOp : Operators.operator)  : OpAST list  * operator list= 
        case ast of
        OpAST(oper, [l1,l2]) => if oper ~=** curOp
                then (l1, oper) ::/ flattenRight l2 curOp
                else ([ast], [])
        | _ => ([ast], [])
    
    fun elaborateUnknownName (ast : OpAST) : UTF8String.t witherrsoption = 
        case ast of
        UnknownOpName(l1) => Success l1
        | _ => raise ElaborateFailure "Expect name here, (this is perhaps a bug in the design, but until we fix it, put a bracket around the name expecting expressions,  the parser may have incorrectly parsed that as an expression)"

    fun elaborateNewName (ast : OpAST) : UTF8String.t witherrsoption = 
        case ast of
        NewOpName(l1) => Success (l1)
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
    : (MixedStr.t list) -> TypeCheckingAST.RSignature witherrsoption =
    let 
    (* val _ = DebugPrint.p "Entering config" *)

    val noPossibleParseErrInfo = "不能够理解(parse)输入"
    fun ambiguousParse (x : ParseAST.ParseOpAST list) = "输入有多于一种理解方式：\n" ^
     String.concatWith "\n" (map (fn x => "可以这样理解：" ^ PrettyPrint.show_parseopast x ) x)


        (* handle PrecedenceParser.NoPossibleParse s =>  *)
            (* raise ECPNoPossibleParse ("Parsing failed at " ^  s ^ " No possible parse. Double check your grammar.") *)
        (* | PrecedenceParser.AmbiguousParse s =>  *)
            (* raise ECPAmbiguousParse ("Parsing failed at " ^  s ^ " Ambiguous parse. Double check your grammar.") *)
    
    and elaborateLabeledType (ast : OpAST.t)  (ctx : contextType): (Label * Type) witherrsoption = 
        case ast of
        OpAST(oper, [NewOpName(l1), l2]) => if 
            oper ~=** labeledTypeCompOp 
            then elaborateOpASTtoType l2 ctx >>= (fn l2' => Success (l1, l2'))
            else raise ElaborateFailure "Expect labeledTypeComp as a child of prod/sum"
        | _ => raise ElaborateFailure "Expect labeledTypeComp as a child of prod/sum"

    and elaborateOpASTtoType 
         (ast : OpAST.OpAST) (ctx : contextType) : TypeCheckingAST.Type witherrsoption = 
        (
            (* print (PrettyPrint.show_opast ast); *)
        case ast of
             UnknownOpName (s) => 
                if UTF8String.semanticEqual s (UTF8String.fromString "《《字符串》》") then Success(BuiltinType(BIString)) else
                if UTF8String.semanticEqual s (UTF8String.fromString "《《整数》》") then Success(BuiltinType(BIInt)) else
                if UTF8String.semanticEqual s (UTF8String.fromString "《《小数》》") then Success(BuiltinType(BIReal)) else
                Success(TypeVar [s])
            | OpUnparsedExpr x => (parseType x ctx) 
            | OpAST(oper, []) => (
                if oper ~=** unitTypeOp then Success(UnitType)
                else if oper ~=** nullTypeOp then Success(NullType)
                (* else if oper ~=** builtinTypeStringOp then BuiltinType(BIString) *)
                else raise InternalErrorECP
                        )
            | OpAST(oper, [a1,a2]) => (
                if oper ~=** prodTypeOp
                then (let val args = #1 (flattenRight ast oper)
                    in fmap Prod (collectAll (map (fn x => elaborateLabeledType x ctx) args ))
                    end)
                else 
                if oper ~=** sumTypeOp
                then (let val args = #1 (flattenRight ast oper)
                    in fmap Sum (collectAll (map (fn x => elaborateLabeledType x ctx) args))
                    end)
                else
                if oper ~=** functionTypeOp
                then fmap Func ((elaborateOpASTtoType a1 ctx) =/= (elaborateOpASTtoType a2 ctx))
                else 
                if oper ~=** typeInstantiationOp
                then fmap TypeInst ((elaborateOpASTtoType a1 ctx) =/= (elaborateOpASTtoType a2 ctx))
                else 
                if oper ~=** universalTypeOp
                then fmap Forall ((elaborateNewName a1) =/= (elaborateOpASTtoType a2 ctx))
                else 
                if oper ~=** existentialTypeOp
                then fmap Exists ((elaborateNewName a1) =/= (elaborateOpASTtoType a2 ctx))
                else 
                if oper ~=** recursiveTypeOp
                then fmap Rho ((elaborateNewName a1) =/= (elaborateOpASTtoType a2 ctx))
                else 
                if oper ~=** structureRefOp
                then fmap TypeVar (collectAll (map elaborateUnknownName (#1 (flattenRight ast structureRefOp))))
                else
                raise ElaborateFailure (
                    "Expected a type constructor 122, got " ^ PrettyPrint.show_op oper ^ " in " 
                    ^PrettyPrint.show_opast ast ^ "\n")
            )
            | _ => raise ElaborateFailure "Expected a type constructor 124"
        )
        handle ElaborateFailure s => 
        raise ElaborateFailure (s ^ "\n when elaborating type "^ PrettyPrint.show_opast ast )
    
    and elaborateOpASTtoExpr  (ast : OpAST.t)(ctx : contextType) : TypeCheckingAST.RExpr witherrsoption = 
    let fun snd (x : OpAST list) : OpAST = (hd (tl x))
    in
        (case ast of
              UnknownOpName (s) => if NumberParser.isNumber s then 
                 (case NumberParser.parseNumber s of 
                    NumberParser.NPInt i => Success (RIntConstant (i, s))
                    | NumberParser.NPReal r => Success (RRealConstant (r, s))
                 )
              else Success (RExprVar [s])
            | OpUnparsedExpr x => (parseExpr x ctx) 
            | OpStrLiteral (l, qi) => Success (RStringLiteral (l, qi))
            | OpAST(oper, l) => 
                let 
                    val operSuc = Success oper
                in
                    (
                    if getUID oper >= elabAppBound 
                    then (* elab app *)
                        collectAll (map (fn x => elaborateOpASTtoExpr x ctx)  l) >>=
                        (fn l => Success (foldl (fn (arg, acc) => RApp (acc , arg, oper)) (RExprVar [getOriginalName oper]) l))
                    else
                    if oper ~=** structureRefOp
                    then fmap RExprVar (collectAll (map elaborateUnknownName (#1 (flattenRight ast structureRefOp))))
                    else
                    if oper ~=** unitExprOp
                    then Success (RUnitExpr(oper))
                    else
                    if oper ~=** projExprOp
                    then fmap RProj (==/= ((elaborateOpASTtoExpr (hd l) ctx),  (elaborateUnknownName (snd l)), operSuc))
                    else 
                    if oper ~=** appExprOp
                    then fmap RApp(==/= (elaborateOpASTtoExpr (hd l) ctx , elaborateOpASTtoExpr (snd l) ctx, operSuc))
                    else 
                    if oper ~=** pairExprOp
                    then 
                        let val (es, ops) = (flattenRight ast pairExprOp) 
                        in fmap RTuple(collectAll (map (fn x => elaborateOpASTtoExpr x ctx) es) =/= Success ops)
                        end
                    else 
                    if oper ~=** injExprOp
                    then fmap RInj(==/=(elaborateUnknownName (hd l) , elaborateOpASTtoExpr (snd l) ctx, operSuc))
                    else 
                    if oper ~=** foldExprOp
                    then fmap RFold( elaborateOpASTtoExpr (hd l) ctx =/= operSuc)
                    else
                    if oper ~=** unfoldExprOp
                    then fmap RUnfold( elaborateOpASTtoExpr (hd l) ctx =/= operSuc)
                    else
                    if oper ~=** caseExprOp
                    then let
                                val (args, sepOps) = flattenRight (snd l) caseAlternativeOp
                                in 
                            elaborateOpASTtoExpr (hd l) ctx >>= (fn hdexpr => 
                            collectAll (map (fn x => case x of
                                                                OpAST(oper, [lbl, evar, expr]) => 
                                                                if oper ~=** caseClauseOp
                                                                then elaborateUnknownName lbl >>= (fn label => 
                                                                    elaborateNewName evar >>= (fn binding => 
                                                                elaborateOpASTtoExpr expr ctx >>= (fn body => 
                                                                    Success ((label, binding, body), oper)
                                                                )
                                                                    )
                                                                ) 
                                                                else raise ElaborateFailure ("Expected a case clause 1" ^ " got " ^ PrettyPrint.show_opast x)
                                                                | _ => raise ElaborateFailure ("Expected a case clause 2" ^ " got " ^ PrettyPrint.show_opast x)
                                                    ) args) >>= (fn l => 
                                        let val cases  = map (#1) l
                                            val casesOps = map (#2) l
                                        in Success (RCase (hdexpr, cases, (oper, sepOps, casesOps)))
                                        end
                                )
                            )
                        end
                    else 
                    if oper ~=** typeAppExprOp
                    then fmap RTApp(==/= (elaborateOpASTtoExpr (hd l) ctx , elaborateOpASTtoType (snd l) ctx, (operSuc =/= Success (OpAST.reconstructOriginalFromOpAST (snd l) ))))
                    else
                    if oper ~=** packExprOp
                    then fmap RPack(==/= (elaborateOpASTtoType (hd l) ctx, elaborateOpASTtoExpr (snd l) ctx, (Success (OpAST.reconstructOriginalFromOpAST (hd l))=/= operSuc)))
                    else
                    if oper ~=** unpackExprOp
                    then fmap ROpen(==/= (elaborateOpASTtoExpr (hd l) ctx,
                    (elaborateNewName (snd l) >>= (fn tvar => 
                        elaborateNewName (hd (tl (tl l))) >>= (fn evar => 
                            elaborateOpASTtoExpr (hd (tl (tl (tl (l))))) ctx  >>= (fn body => 
                                Success (tvar,evar, body)
                            )
                        )
                    )), operSuc))

                    else
                    if oper ~=** ffiCCallOp
                    then fmap RFfiCCall(==/= (elaborateOpASTtoExpr (hd l) ctx ,elaborateOpASTtoExpr (snd l) ctx, operSuc))
                    else
                    if oper ~=** lambdaExprOp
                    then fmap RLam(==/= (elaborateNewName (hd l), elaborateOpASTtoExpr (snd l) ctx, operSuc))
                    else
                    if oper ~=** lambdaExprWithTypeOp
                    then fmap RLamWithType (===/= (elaborateOpASTtoType (hd l) ctx, 
                    elaborateNewName (snd l), elaborateOpASTtoExpr (hd (tl (tl l))) ctx, operSuc))
                    else
                    if oper ~=** fixExprOp
                    then fmap RFix(==/= (elaborateNewName (hd l) ,
                    elaborateOpASTtoExpr (snd l) ctx, operSuc))
                    else
                    if oper ~=** typeLambdaExprOp
                    then fmap RTAbs(==/= (elaborateNewName (hd l), 
                    elaborateOpASTtoExpr (snd l) ctx, operSuc))
                    else
                    if oper ~=** letinOp
                    then (
                        let val preprocessedTree = 
                            case (hd l) of
                            OpUnparsedDecl(d, qi) => preprocessAST (map (#1) d)
                            | _ => raise ElaborateFailure "Expect declaration block as first argument to let in"
                        in 
                        preprocessedTree >>= (fn tree => 
                        let
                            val newOps = extractAllOperators tree
                            val declTree = constructOpAST tree ctx
                            val bodyExpr = elaborateOpASTtoExpr (snd l) (insertIntoCurContextOps ctx newOps)
                        in fmap RLetIn(==/=(declTree, bodyExpr,operSuc)) end
                        )
                        end
                    )
                    else
                    raise ElaborateFailure "Expected Expression constructs 224"
                )
                end
            | OpUnparsedDecl t =>  genSingletonError (OpAST.reconstructOriginalFromOpAST ast) "期待表达式，却遇到了声明块(expected expression, unexpected declaration block)" NONE
            | _ =>
                raise ElaborateFailure "Expected Expression constructs 227"
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
        : TypeCheckingAST.RSignature  witherrsoption =
        (
        (* print ("\rRemaining: "^ Int.toString(List.length ast) ^ " statements"); *)
        (* print ("constructOpAST on "^ PrettyPrint.show_preprocessaast ast ^ " with curSName = " ^ 
        StructureName.toStringPlain curSName ^ " with addedOps = " ^ PrettyPrint.show_ecpops addedOps ^"\n\n"); *)
        case ast of 
            [] => Success ([])
            | (x :: xs) => 
                let fun trailingNoOps() : 
                TypeCheckingAST.RSignature witherrsoption = constructOpAST xs ctx 
                fun ::: (x , y) = y >>= (fn y' => Success(x :: y'))
                infix 5 :::
                fun trailingWithOps(addedOp:Operators.operator): TypeCheckingAST.RSignature  witherrsoption
                    = constructOpAST xs (insertIntoCurContextOp ctx addedOp)
                in
                (case x of 
                    PTypeMacro(tname, tbody) => parseType tbody ctx  >>= 
                            (fn t  => RTypeMacro(tname, t) ::: trailingNoOps())
                    | PTermTypeJudgment(ename, tbody) => parseType  tbody ctx >>= (fn t => 
                            RTermTypeJudgment(ename, t) ::: trailingNoOps())
                    | PTermMacro(ename, ebody) => parseExpr ebody ctx >>= (fn e => 
                            RTermMacro(ename, e) ::: trailingNoOps())
                    | PTermDefinition(ename, ebody) => parseExpr ebody ctx >>= (fn eb => 
                        RTermDefinition(ename, eb) ::: trailingNoOps())
                    | POpDeclaration(opName, assoc, pred) => trailingWithOps(parsePOperator(x)) 
                    | PDirectExpr(ebody) => parseExpr ebody ctx >>= (fn eb => 
                    RDirectExpr(eb) ::: trailingNoOps())
                    | PComment _ => trailingNoOps()
                    | PStructure(publicVisible, sname, decls) => 
                        preprocessAST decls >>= (fn preprocessedTree => 
                                            let val newOps = extractAllOperators preprocessedTree
                                                val declTree = constructOpAST preprocessedTree ctx
                                            in declTree >>= (fn ds => RStructure(publicVisible,sname, ds):::
                                                constructOpAST xs (curSName, curV, ((curSName@[sname], publicVisible, newOps):: addedOps)) )
                                            end
                        )
                    | POpenStructure(sname) =>  (* open will be as if there is a local declaration with 
                    the same name as the public members of the structure *)
                        ROpenStructure(sname) ::: constructOpAST xs (insertIntoCurContextOps ctx (lookupContextForOpers ctx (curSName@sname)))
                    | PEmptyDecl => trailingNoOps()
                )
                end
        )
        handle ElaborateFailure x => raise ElaborateFailure (x ^ 
            "\n when elaborating declaration " ^ PrettyPrint.show_preprocessaast ast)
                

    and constructTypeCheckingASTTopLevel
     ( ast : PreprocessingAST.t) : TypeCheckingAST.RSignature witherrsoption = 
    let 
        (* val _ = print ("Total "^ Int.toString(List.length ast) ^ " statements\n"); *)
        val res =  constructOpAST ast (StructureName.topLevelName, true, [
            (StructureName.topLevelName, true, [])
        ])
        (* val _ = print ("Done "^ Int.toString(List.length ast) ^ " statements\n"); *)
    in 
        res end
    


    and parseJudgment (s : MixedStr.t) : pJudgment witherrsoption= 
    (let
       (* val _ = print ("Parsing judgment on" ^ PrettyPrint.show_mixedstr s ^ "\n"); *)
       val tp  = MixedStr.toPlainUTF8String
       fun getDeclContent (x : MixedStr.t) = case x of
        [MixedStr.UnparsedDeclaration(y, qi)] => map (fn (x, ei) => x) y
        | _ => raise ElaborateFailure "expecting a single unparsed declaration"
       val declParseTree = DeclarationParser.parseDeclarationSingleOutput declOps s
       val _ = notifyDeclarationParserResult declParseTree
       val res = case declParseTree of
            (oper, [l1, l2]) => 
            if oper ~=** typeMacroOp
            then Success(PTypeMacro (tp l1, l2))
            else if oper ~=** termTypeJudgmentOp
            then Success(PTermTypeJudgment (tp l1, l2))
            else if oper ~=** termMacroOp
            then Success(PTermMacro (tp l1, l2))
            else if oper ~=** termDefinitionOp
            then Success(PTermDefinition (tp l1, l2))
            else if oper ~=** privateStructureOp
            then Success(PStructure (false, tp l1, (getDeclContent l2)))
            else if oper ~=** publicStructureOp
            then Success(PStructure (true, tp l1, (getDeclContent l2)))
            else  
            raise Fail "pp34"
            | (oper, [l1, l2, l3]) =>  
                if oper ~=** opDeclarationOp
                then Success(POpDeclaration (tp l1, parseAssoc (tp l2), NumberParser.parseInteger (tp l3)))
                else raise Fail "pp85"
            | (oper, [l1]) =>  
                if oper ~=** commentOp
                then Success(PComment (l1))
                else 
                if oper ~=** openStructureOp
                then let 
                val parsedStructureRef = PrecedenceParser.parseMixfixExpression [structureRefOp] (l1)
                val _ = notifyParseOpAST parsedStructureRef
                val names = #1 (flattenRight parsedStructureRef structureRefOp)
                        in fmap POpenStructure (collectAll (map elaborateUnknownName names))
                end 
                else
                raise Fail "pp95"
            | _ => raise Fail "pp26: malformed output : not two args or three args"
        (* val _ = print ("returning " ^ PrettyPrint.show_preprocessaastJ res) *)
        in res end
        handle DeclarationParser.DeclNoParse (expr) => (
            if length expr = 0 then Success (PEmptyDecl)
            else Success(PDirectExpr expr)
        )
        (* handle ECPNoPossibleParse x => raise ECPNoPossibleParse (x ^ 
            "\n when parsing declaration " ^ MixedStr.toString s)
        handle ElaborateFailure x => raise ElaborateFailure (x ^ 
            "\n when parsing declaration " ^ MixedStr.toString s) *)
    )

    and parseType (tbody : MixedStr.t)(ctx : contextType) : TypeCheckingAST.Type witherrsoption = 
    let val parseTree = (PrecedenceParser.parseMixfixExpression allTypeOps tbody)
        val _ = notifyParseOpAST parseTree
        in (elaborateOpASTtoType parseTree ctx) end
        handle PrecedenceParser.NoPossibleParse s => 
        StaticErrorStructure.genSingletonErrorTuple (MixedStr.toUTF8String tbody)
            (PrecedenceParser.showParseExceptionInfo s (noPossibleParseErrInfo)) 
        | PrecedenceParser.AmbiguousParse (alts, s) => 
            StaticErrorStructure.genSingletonErrorTuple (MixedStr.toUTF8String tbody)
                (PrecedenceParser.showParseExceptionInfo s (ambiguousParse alts))

    and parseExpr (ebody : MixedStr.t)(ctx : contextType) : TypeCheckingAST.RExpr witherrsoption
    =  let val parseTree = (PrecedenceParser.parseMixfixExpression 
                (allTypeAndExprOps@ lookupCurrentContextForOpers ctx) ebody)
           (* val _ = DebugPrint.p (PrettyPrint.show_opast parseTree ^ "\n\n") *)
           val _ = notifyParseOpAST parseTree
    in (elaborateOpASTtoExpr parseTree ctx) end
        handle PrecedenceParser.NoPossibleParse s => 
        StaticErrorStructure.genSingletonErrorTuple (MixedStr.toUTF8String ebody)
            (PrecedenceParser.showParseExceptionInfo s (noPossibleParseErrInfo)) 
        | PrecedenceParser.AmbiguousParse (alts, s) => 
            StaticErrorStructure.genSingletonErrorTuple (MixedStr.toUTF8String ebody)
                (PrecedenceParser.showParseExceptionInfo s (ambiguousParse alts))

    and preprocessAST (s : MixedStr.t list) : PreprocessingAST.t witherrsoption = 
    (
        (* print ("preprocessAST : " ^ Int.toString (length s) ^ " count : " ^PrettyPrint.show_mixedstrs s ^"\n"); *)
    (* case s of  *)
         (* [MixedStr.UnparsedDeclaration l]  =>  *)
         collectAll (map parseJudgment s)
        (* | _ => [PDirectExpr s] *)
    )

    in 
    fn s => ((preprocessAST s) >>= constructTypeCheckingASTTopLevel )
    end
        
end
