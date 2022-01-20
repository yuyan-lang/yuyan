
structure PreprocessingPass = struct

    (* PTypeMacro of UTF8String.t * UTF8String.t
                       | PTypeJudgment of UTF8String.t * UTF8String.t
                       | PTermDefinition of UTF8String.t * UTF8String.t
                       | POpDeclaration *)
    open PreprocessingAST
    open Operators
    open PreprocessingOperators

    open PreprocessingAST
    open TypeCheckingAST
    open Operators
    open PreprocessingOperators
    open OpAST

    open StaticErrorStructure
    infix 5 >>=
    infix 6 =/=

  structure PrecParser = MixFixParser
    (* exception ElaborateFailure of string *)
    (* exception InternalErrorECP *)

(* structureName will use the global naming convention *)
    type structureName = StructureName.t

    (* type is type of current module * all prior parsing module's operator decl *)
    (* an open will forcibly bring in all previous type module to the current definition *)
    (* bool is visibility *)
    type contextType = structureName * bool * (structureName * bool * Operators.operator list) list
    
    val ~=** = Operators.~=**
    infix 4 ~=**

    fun lookupContextForOpers((curSName,curV,  ctx) : contextType) (sName : structureName) : Operators.operator list witherrsoption=
        case ctx of
            [] => ( genSingletonError (StructureName.toString sName) 
                 ("结构名未找到(Structure Name " ^ StructureName.toStringPlain sName ^ " not found in context)") NONE)
            | ((s, v, opl):: ss) => 
            (* if StructureName.semanticEqual s sName then opl else lookupContextForOpers (curSName, curV,ss) sName *)
            (case StructureName.checkRefersTo s sName curSName
             of 
                SOME cname => (
                    (* DebugPrint.p "lookup returning true ";  *)
                    Success(opl))
                | NONE => (
                    (* DebugPrint.p "lookup returning false ";  *)
                    lookupContextForOpers (curSName, curV,ss) sName)
                )
    fun lookupCurrentContextForOpers(ctx as (curSName,curV, imports) : contextType)  : Operators.operator list 
    = valOfSafe (lookupContextForOpers ctx curSName)
        (fn e => raise Fail ("current structure name must be present \n looking up " ^  StructureName.toStringPlain curSName
         ^ " imports are " ^ (PrettyPrint.show_ecpops imports)
        )
        )

    fun lookupAllActiveOpers (ctx as (curSName, curV, imports) : contextType) : Operators.operator list = 
     List.concat (List.mapPartial (fn (s, v, opl) => 
        if StructureName.isPrefix s curSName then SOME (opl) else NONE
    ) imports)


    fun insertIntoCurContextOp((curSName,curV, ctx) : contextType) (oper : Operators.operator) : contextType =
        (curSName, curV, ((curSName,curV, oper:: lookupCurrentContextForOpers (curSName, curV, ctx) )
        :: (List.filter (fn (cname, _, _) => cname <> curSName) ctx)))

    fun insertIntoCurContextOps((curSName,curV, ctx) : contextType) (opers : Operators.operator list) : contextType =
        (curSName, curV, ((curSName,curV, opers@ lookupCurrentContextForOpers (curSName, curV, ctx))
        :: (List.filter (fn (cname, _, _) => cname <> curSName) ctx)))

    fun newContextAfterOpeningStructure(openName : StructureName.t ) (ctx : contextType) : contextType witherrsoption =
        let 
        in 
            lookupContextForOpers ctx (openName) >>= (fn opl => 
                    Success(insertIntoCurContextOps ctx opl)
            )
        end



    fun ::/ ((x,y), (xs,ys)) =(x :: xs, y :: ys)
    infix 5 ::/



    val noPossibleParseErrInfo = "不能够理解(parse)输入"
    fun ambiguousParse (x : ParseAST.ParseOpAST list) = "输入有多于一种理解方式：\n" ^
     String.concatWith "\n" (map (fn x => "可以这样理解：" ^ PrettyPrint.show_parseopast x ) x)


    fun parsePOperator (POpDeclaration(opName, assoc, pred, soi)) =let 
                    val oper = Operators.parseOperator 
                            opName (assoc <> Operators.NoneAssoc) (assoc = Operators.LeftAssoc) pred []
                            in (
                                (* print (" PARSED OPER AS " ^ PrettyPrint.show_op oper);  *)
                                oper) end
    fun extractAllOperators (ast : PreprocessingAST.t) : Operators.operator list = 
        case ast of 
            [] => []
            | ((x, ei) :: xs) => 
                (case x of 
                     POpDeclaration(opName, assoc, pred, soi) => parsePOperator(x) :: extractAllOperators xs
                    | _ => extractAllOperators xs
                )


    fun configureAndConstructPreprocessingASTTopLevel
    (notifyOpAST : OpAST.t -> 'a) 
    (notifyPreprocessingAST : PreprocessingAST.t -> 'b) 
    : UTF8String.t -> PreprocessingAST.t witherrsoption  = 
    let
            (* removes all unparsed, correctness relies inductively on preprocessAST's 
            guarantee that return is free of unparsed *)
            fun recursivelyTraverseAndParseOpAST(s : OpAST) (ctx as (curSname, vis, info): contextType) : (OpAST * contextType) witherrsoption = 
                case s of 
                    OpUnparsedDecl (l, qi) => preprocessAST l ctx >>= (fn (past, newContext) =>  
                    (* TODO: Important: chain context through *)
                        Success (OpParsedDecl(past, qi), newContext)
                    )
                    | OpUnparsedExpr (e,qi) => parseTypeOrExpr e ctx >>= (fn (parsed) => Success(OpParsedQuotedExpr(parsed, qi), ctx))
                    | OpAST (oper, l) => (
                        if Operators.eqOpUid oper PreprocessingOperators.letinOp 
                        then (case l of 
                                [decls, expr] => recursivelyTraverseAndParseOpAST decls (curSname@(StructureName.localName()), vis, info)
                                    >>= (fn (decls, newContext as (_, _, newInfo)) => recursivelyTraverseAndParseOpAST expr (curSname, vis, newInfo)  
                                    >>= (fn (expr, newContext) => Success(OpAST(oper, [decls, expr]), newContext)))
                                | _ => raise Fail "pp101"
                            )
                        else mapM (fn x =>recursivelyTraverseAndParseOpAST x ctx) l >>= (fn l => Success(OpAST(oper, map (#1) l), ctx)) (* context info can be safely ignored in all other cases*)
                        )
                    | _ => Success(s, ctx)


            and parseJudgment (s : MixedStr.t)(ctx : contextType) : (pJudgment * contextType) witherrsoption= 
            (let
            (* val _ = print ("Parsing judgment on" ^ PrettyPrint.show_mixedstr s ^ "\n"); *)
            val tp  = MixedStr.toPlainUTF8String
            fun getDeclContent (x : MixedStr.t) : (OpAST * contextType) witherrsoption = case x of
                [MixedStr.UnparsedDeclaration(y, qi)] => (
                        preprocessAST y ctx >>=  (fn (preprocessedAST, newContext) => 
                            Success(OpParsedDecl(preprocessedAST, qi), newContext)
                        )
                    )
                | _ => genSingletonError (MixedStr.toUTF8String x) "期待单一的声明快(expecting a single unparsed declaration)" NONE
            val declParseTree = DeclarationParser.parseDeclarationSingleOutput declOps s
            (* val _ = notifyDeclarationParserResult declParseTree *)
            val res = case declParseTree of
                    (oper, [l1, l2]) => 
                    if oper ~=** typeMacroOp
                    then parseTypeOrExpr l2 ctx >>= (fn l2 =>  Success(PTypeMacro (tp l1, l2, oper), ctx))
                    else if oper ~=** termTypeJudgmentOp
                    then parseTypeOrExpr l2 ctx >>= (fn l2 =>  Success(PTermTypeJudgment (tp l1, l2, oper), ctx))
                    else if oper ~=** termMacroOp
                    then parseTypeOrExpr l2 ctx >>= (fn l2 =>  Success(PTermMacro (tp l1, l2, oper), ctx))
                    else if oper ~=** termDefinitionOp
                    then parseTypeOrExpr l2 ctx >>= (fn l2 =>  Success(PTermDefinition (tp l1, l2, oper), ctx))
                    else if oper ~=** privateStructureOp
                    then  (getDeclContent l2) >>= (fn (declOpAST, newContext) =>   
                    Success(PStructure (false, tp l1, declOpAST, oper), newContext))
                    else if oper ~=** publicStructureOp
                    then  (getDeclContent l2) >>= (fn (declOpAST, newContext) =>   
                    Success(PStructure (true, tp l1, declOpAST, oper), newContext))
                    else  
                    raise Fail "pp34"
                    | (oper, [l1, l2, l3]) =>  
                        if oper ~=** opDeclarationOp
                        then 
                        let val newOp = POpDeclaration (tp l1, parseAssoc (tp l2), NumberParser.parseInteger (tp l3), (tp l2, tp l3, oper))
                            val newContext = (insertIntoCurContextOp ctx (parsePOperator newOp))
                        in
                            Success(newOp, newContext)
                        end
                        else raise Fail "pp85"
                    | (oper, [l1]) =>  
                        let fun getStructureOpAST (s : MixedStr.t ) : OpAST = 
                            let 
                                val parsedStructureRef = PrecedenceParser.parseMixfixExpression [structureRefOp] (l1)
                            in 
                                parsedStructureRef
                            end
                        in
                            if oper ~=** commentOp
                            then Success(PComment (l1, oper), ctx)
                            else 
                            if oper ~=** openStructureOp
                            then ExpressionConstructionPass.getStructureName (getStructureOpAST l1) >>= (fn structureName => 
                                newContextAfterOpeningStructure structureName ctx >>= (fn newContext => 
                                    Success(POpenStructure (getStructureOpAST l1, oper), newContext)
                                )
                            )
                            else
                            if oper ~=** importStructureOp
                            then Success(PImportStructure (getStructureOpAST l1, oper), ctx)
                            else
                            raise Fail "pp95"
                        end
                    | _ => raise Fail "pp26: malformed output : not two args or three args"
                (* val _ = print ("returning " ^ PrettyPrint.show_preprocessaastJ res) *)
                in res end
                handle DeclarationParser.DeclNoParse (expr) => (
                    if length expr = 0 then Success (PEmptyDecl, ctx)
                    else (parseTypeOrExpr expr ctx) >>= (fn parsedExpr => Success(PDirectExpr parsedExpr, ctx))
                )
                (* handle ECPNoPossibleParse x => raise ECPNoPossibleParse (x ^ 
                    "\n when parsing declaration " ^ MixedStr.toString s)
                handle ElaborateFailure x => raise ElaborateFailure (x ^ 
                    "\n when parsing declaration " ^ MixedStr.toString s) *)
            )

            (* and parseType (tbody : MixedStr.t)(ctx : contextType) : OpAST witherrsoption = 
            let val parseTree = (PrecedenceParser.parseMixfixExpression allTypeOps tbody)
                (* val _ = notifyParseOpAST parseTree *)
                in (recursivelyTraverseAndParseOpAST parseTree ctx) end
                handle PrecedenceParser.NoPossibleParse s => 
                StaticErrorStructure.genSingletonErrorTuple (MixedStr.toUTF8String tbody)
                    (PrecedenceParser.showParseExceptionInfo s (noPossibleParseErrInfo)) 
                | PrecedenceParser.AmbiguousParse (alts, s) => 
                    StaticErrorStructure.genSingletonErrorTuple (MixedStr.toUTF8String tbody)
                        (PrecedenceParser.showParseExceptionInfo s (ambiguousParse alts)) *)

            (* the result will not contain unparsed anything *)
            and parseTypeOrExpr (ebody : MixedStr.t)(ctx : contextType) : OpAST witherrsoption
            =  let val parseTree = (PrecedenceParser.parseMixfixExpression 
                        (allTypeAndExprOps@ lookupAllActiveOpers ctx) ebody)
                (* val _ = DebugPrint.p (PrettyPrint.show_opast parseTree ^ "\n\n") *)
                val _ = notifyOpAST parseTree
            in recursivelyTraverseAndParseOpAST parseTree ctx >>= (
                fn (parsedOpAST, newContext) => Success(parsedOpAST) (* no need to add new context as constructs inside let is not accessible in any case *)
            ) end
                handle PrecedenceParser.NoPossibleParse s => 
                StaticErrorStructure.genSingletonErrorTuple (MixedStr.toUTF8String ebody)
                    (PrecedenceParser.showParseExceptionInfo s (noPossibleParseErrInfo)) 
                | PrecedenceParser.AmbiguousParse (alts, s) => 
                    StaticErrorStructure.genSingletonErrorTuple (MixedStr.toUTF8String ebody)
                        (PrecedenceParser.showParseExceptionInfo s (ambiguousParse alts))

        (* the resulting preprocessing ast will not contain unparsed anything *)
            and preprocessAST (s : (MixedStr.t * MixedStr.endinginfo) list)(ctx : contextType) : (PreprocessingAST.t * contextType) witherrsoption = 
            (

                let 
                        fun ::: ((x , y) : (OpAST.pJudgment * MixedStr.endinginfo) * (PreprocessingAST.t * contextType) witherrsoption) = 
                        y >>= (fn y' => Success(x :: (#1 y'), #2 y'))
                        infix 5 :::
                in
                (* print ("preprocessAST : " ^ Int.toString (length s) ^ " count : " ^PrettyPrint.show_mixedstrs s ^"\n"); *)
                case s of 
                [] => Success([], ctx)
                | ((x,ei) :: xs) => parseJudgment x ctx >>= (fn (parsed, newContext) =>
                    let val _  = notifyPreprocessingAST [(parsed, ei)]
                    in 
                        (parsed, ei) ::: preprocessAST xs newContext
                    end
                    )
                end
            )


            fun preprocessASTTopLevel(content : UTF8String.t) : PreprocessingAST.t witherrsoption = 
                MixedStr.makeDecl content >>= (fn s => 
                    preprocessAST s (((StructureName.topLevelName, true, [
                        (StructureName.topLevelName, true, [])
                    ]))) >>= (fn (r, newCtx) => Success(r))
                )
                
    in 
        fn s => preprocessASTTopLevel s
    end
                

end
