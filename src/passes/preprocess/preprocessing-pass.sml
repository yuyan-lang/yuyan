
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
    infix 5 <?>

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

     fun showctxPre x = SOME(case x of 
    (curSName, curVis, snamevisopl) =>  
    "当前结构名：" ^ StructureName.toStringPlain curSName ^
    "\n当前已定义的值及其类型：\n"  ^
            String.concatWith "；\n" (map (fn x => case x of
                (sname, vis, opl) => StructureName.toStringPlain sname  ^ "，其中有：【" ^ String.concatWith "，" (map PrettyPrint.show_op opl) ^ "】"
    ) snamevisopl) ^ "；\n"
          )

    fun structureNameNotFoundError sName ctx addmsg = ( genSingletonError (StructureName.toString sName) 
                 ("结构名未找到(Structure Name " ^ addmsg ^ StructureName.toStringPlain sName ^ " not found in context)") (showctxPre ctx))

    fun lookupContextForOpers((curSName,curV,  ctx) : contextType) (sName : structureName) : Operators.operator list witherrsoption=
        case ctx of
            [] => structureNameNotFoundError sName (curSName, curV, ctx) "(2)"
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
        (curSName, curV, ((curSName,curV, 
        (* do not insert repetitive operators into the context  (currently by name)
        TODO: do this in a principled manner 
        *)
        let 
        val curOps =  lookupCurrentContextForOpers (curSName, curV, ctx)
        in 
            oper :: List.filter (fn x =>  not (Operators.eqOpName x oper)) curOps (* TODO: THIS is a name hack, NEED FIXING! *)
        end
            )
        :: (List.filter (fn (cname, _, _) => cname <> curSName) ctx)))

    fun insertIntoCurContextOps(ctx  : contextType) (opers : Operators.operator list) : contextType =
    foldr (fn (oper, acc) => insertIntoCurContextOp acc oper ) ctx opers
        (* (curSName, curV, ((curSName,curV, opers@ lookupCurrentContextForOpers (curSName, curV, ctx))
        :: (List.filter (fn (cname, _, _) => cname <> curSName) ctx))) *)

    (* TODO: currently reexport only reexport the top level operators*)
    fun getReExportDecls(reExportName : StructureName.t ) (ctx as (curSName, v, snamevopl) : contextType) :  
            (Operators.operator list * (StructureName.t * Operators.operator list) list) =
       let 
            val allDirectReExports = (List.mapPartial (fn (s, v, opl) => case StructureName.checkRefersTo s reExportName curSName
                of SOME cname => SOME(opl)
                    | NONE => NONE ) snamevopl)
            val allDirectOperators = List.concat allDirectReExports
            val allIndirectStructures = List.mapPartial (fn (s, v, opl) => case StructureName.checkRefersToScope s reExportName curSName
                of SOME newStrippedName => SOME(curSName @ newStrippedName, opl)
                | NONE => NONE) snamevopl
        in 
         (allDirectOperators, allIndirectStructures)
        end

    fun newContextAfterOpeningStructure(openName : StructureName.t ) (ctx as (curSName, v, snamevopl) : contextType) : contextType witherrsoption =
        let 
            val allDirectOpens = (List.mapPartial (fn (s, v, opl) => case StructureName.checkRefersTo s openName curSName
                of SOME cname => SOME(opl)
                    | NONE => NONE ) snamevopl)
            val allDirectOperators = List.concat allDirectOpens
            val allIndirectStructures = List.mapPartial (fn (s, v, opl) => case StructureName.checkRefersToScope s openName curSName
                of SOME newStrippedName => SOME(curSName @ newStrippedName, v, opl)
                | NONE => NONE) snamevopl
            val (curSName, curV, newSNameOpL) = insertIntoCurContextOps ctx allDirectOperators
        in 
        if length allDirectOpens = 0 (* maybe it hits somecontext with no operators *)
        andalso length allDirectOperators = 0 
        andalso length allIndirectStructures = 0
        then structureNameNotFoundError openName ctx "(1)"
        else Success(curSName, curV, allIndirectStructures@newSNameOpL)
        end
 
    fun parsePOperator (opd) =
    case opd of 
            POpDeclaration(opName, assoc, pred, soi) => 
        let 
                    val oper = Operators.parseOperator 
                            opName (assoc <> Operators.NoneAssoc) (assoc = Operators.LeftAssoc) pred []
                            in (
                                (* DebugPrint.p ( "opName is " ^ UTF8String.toString opName ^
                                    " PARSED OPER AS " ^ PrettyPrint.show_op oper);  *)
                                oper) end
        | _ => raise Fail "pp104"
    
    fun extractAllOperators (curSName : StructureName.t) (vis : bool) (ast : PreprocessingAST.t) : (structureName * bool * Operators.operator list) list = 
        (* current scoped *)
        (curSName, vis, List.concat (List.mapPartial  (fn (x, ei) => 
                (case x of 
                     POpDeclaration(opName, assoc, pred, soi) => SOME([parsePOperator(x)])
                     | PReExportStructure (name, (opl, substructure), soi) => SOME(opl)
                    | _ => NONE
                )
        ) ast))::
        (* sub structures *)
        (List.concat(List.mapPartial (fn (x, ei) => case x of 
            PTermDefinition(structureName, OpParsedDecl(l, qi), soi) => 
                SOME (extractAllOperators (curSName@[structureName]) vis l)
                (* ^^^ THIS IS VERY STRANGE, TODO: FIX*)
            | PReExportStructure (name, (opl, substructure), soi) => SOME(map (fn (name, opl) => (name, true, opl)) substructure)
            | _ => NONE
        ) ast))





    fun ::/ ((x,y), (xs,ys)) =(x :: xs, y :: ys)
    infix 5 ::/



        
    fun configureAndConstructPreprocessingASTTopLevel
    (lookupImportPreprocessingAST : StructureName.t -> (PreprocessingAST.t * FileResourceURI.t) witherrsoption)
    (notifyOpAST : OpAST.t -> 'a) 
    (notifyPreprocessingAST : PreprocessingAST.t -> 'b) 
    (topLevelStructureName : StructureName.t)
    : UTF8String.t -> 
    PreprocessingAST.t witherrsoption  = 
    let
            fun newContextAfterImportingStructure(importName : StructureName.t ) (ctx : contextType) : (contextType * FileResourceURI.t) witherrsoption =
            let 
            in 
                lookupImportPreprocessingAST importName >>= (fn (tree, path) => 
                case ctx of 
                    (curSName, vis, imports) => 
                   Success ((curSName, vis, imports@extractAllOperators [List.last importName] true tree), path)
                    (* TODO: prevent repetitive imports *)
                )
            end
            fun removeCommentInMixedStr (s : MixedStr.t) : MixedStr.t = 
                List.mapPartial (fn c => (case c of 
                    MixedStr.Comment(content, soi) => (notifyPreprocessingAST [(PComment(content,soi), NONE)]; NONE)
                    | _ => SOME c
                )) s
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
                        if Operators.eqOpUid oper PreprocessingOperators.inlineCommentOp  (* do not parse the rhs of comment *)
                        then
                        recursivelyTraverseAndParseOpAST (hd l) ctx >>= (fn (hdl, _) => Success(OpAST(oper, [hdl, (hd (tl l))]), ctx)) (* context info can be safely ignored in all other cases*)
                        else
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


            and parseJudgment (s : MixedStr.t)(ctx : contextType) : (pJudgment list * contextType) witherrsoption= 
            (let
            (* val _ = print ("Parsing judgment on" ^ PrettyPrint.show_mixedstr s ^ "\n"); *)
            (* filter out top level comments using hacks *)
            val s = removeCommentInMixedStr s
            fun tp s = MixedStr.toPlainUTF8String s 
            fun getDeclContent (x : MixedStr.t) : (OpAST * contextType) witherrsoption = case x of
                [MixedStr.UnparsedDeclaration(y, qi)] => (
                        preprocessAST y ctx >>=  (fn (preprocessedAST, newContext) => 
                            Success(OpParsedDecl(preprocessedAST, qi), newContext)
                        )
                    )
                | _ => genSingletonError (MixedStr.toUTF8String x) "期待单一的声明快(expecting a single unparsed declaration)" NONE
            val declParseTree = DeclarationParser.parseDeclarationSingleOutput s
            (* val _ = notifyDeclarationParserResult declParseTree *)
            val res = case declParseTree of
                    (oper, [l1, l2]) => 
                    (* if oper ~=** typeMacroOp
                    then parseTypeOrExpr l2 ctx >>= (fn l2 =>  Success(PTypeMacro (tp l1, l2, oper), ctx)) *)
                    (* else  *)
                    if oper ~=** termTypeJudgmentOp
                    then parseTypeOrExpr l2 ctx >>= (fn l2 => tp l1 >>= (fn l1 =>  Success([PTermTypeJudgment (l1, l2, oper)], ctx)))
                    else if oper ~=** constructorDeclarationOp
                    then parseTypeOrExpr l2 ctx >>= (fn l2 =>  tp l1 >>= (fn l1 => Success([PConstructorDecl (l1, l2, oper)], ctx)))
                    (* else if oper ~=** termMacroOp
                    then parseTypeOrExpr l2 ctx >>= (fn l2 =>  Success(PTermMacro (tp l1, l2, oper), ctx)) *)
                    else if oper ~=** termDefinitionOp
                    then parseTypeOrExpr l2 ctx >>= (fn l2 =>  
                        tp l1 >>= (fn l1 => 
                        case l2  of
                        OpParsedDecl(ast, qi) => 
                                (
                                    case ctx of 
                                        (curSName, vis, imports) => 
                                                let 
                                                    fun goOps exploreSName ast
                                                     = (exploreSName, vis, List.concat (List.mapPartial  (fn (x, ei) => 
                                                                        (case x of 
                                                                            POpDeclaration(opName, assoc, pred, soi) => SOME([parsePOperator(x)])
                                                                            | PReExportStructure (name, (opl, substructure), soi) => SOME(opl)
                                                                            | _ => NONE
                                                                        )
                                                                ) ast)) 
                                                                (* TODO: NESTED STRUCTURE *)
                                                                ::
                                                                (* sub structures *)
                                                                (List.concat(List.mapPartial (fn (x, ei) => case x of 
                                                                    PStructure(vis, structureName, OpParsedDecl(l, qi), soi) => 
                                                                        SOME (goOps (exploreSName@[structureName]) l)
                                                                        (* ^^^ THIS IS VERY STRANGE, TODO: FIX*)
                                                                    | PReExportStructure (name, (opl, substructure), soi) => SOME(map (fn (name, opl) => (name, true, opl)) substructure)
                                                                    | _ => NONE
                                                                ) ast))
                                                val newOps = goOps [l1] ast
                                                val newctx = (curSName, vis, imports@newOps)
                                            in 
                                                Success([PTermDefinition (l1, l2, oper)], newctx)
                                            end
                                )
                        | _ => Success([PTermDefinition (l1, l2, oper)], ctx))
                        )
                    (* else if oper ~=** privateStructureOp
                    then  (getDeclContent l2) >>= (fn (declOpAST, newContext) =>   
                    tp l1 >>= (fn l1 => 
                    Success(PStructure (false, l1, declOpAST, oper), newContext)))
                    else if oper ~=** publicStructureOp
                    then  (getDeclContent l2) >>= (fn (declOpAST, newContext) =>   
                    tp l1 >>= (fn l1 => 
                    Success(PStructure (true, l1, declOpAST, oper), newContext))) *)
                    else  
                    raise Fail "pp34"
                    | (oper, [l1, l2, l3]) =>  
                        if oper ~=** opDeclarationOp
                        then 
                        tp l1 >>= (fn pl1 => 
                        tp l2 >>= (fn pl2 => 
                        tp l3 >>= (fn pl3 => 
                            parseAssoc (pl2) >>= (fn parsedAssoc => 
                            if not (NumberParser.isInteger (pl3))
                            then genSingletonError (pl3) "优先级必须是一个整数" NONE
                            else if UTF8String.isSubstring ([underscoreChar, underscoreChar]) pl1
                            then genSingletonError (pl1) "不可以出现连续的参数(〇〇)" NONE
                            else
                                let val newOp = POpDeclaration (pl1, parsedAssoc, NumberParser.parseInteger (pl3), (pl2, pl3, oper))
                                    val newContext = (insertIntoCurContextOp ctx (parsePOperator newOp))
                                in
                                    Success([newOp], newContext)
                                end
                            )
                        )
                        )
                        )
                        else raise Fail "pp85"
                    | (oper, [l1]) =>  
                        let fun getStructureOpAST (s : MixedStr.t ) : OpAST witherrsoption = 
                            let 
                                val parsedStructureRef = PrecedenceParser.parseMixfixExpression [structureRefOp] (l1)
                            in 
                                parsedStructureRef 
                            end
                        in
                            (* (* if oper ~=** commentOp
                            then Success(PComment (l1, oper), ctx) *)
                            else  *)
                            if oper ~=** openStructureOp
                            then 
                                getStructureOpAST l1 >>= (fn structureOpAST => 
                            ExpressionConstructionPass.getStructureName structureOpAST >>= (fn structureName => 
                                newContextAfterOpeningStructure structureName ctx >>= (fn newContext => 
                                        Success([POpenStructure (structureOpAST, oper)], newContext)
                                    )
                                )
                            )
                            else
                            if oper ~=** importStructureOp
                            then getStructureOpAST l1 >>= (fn structureOpAST => 
                            ExpressionConstructionPass.getStructureName structureOpAST >>= (fn structureName =>  
                            (newContextAfterImportingStructure structureName ctx 
                                <?> (fn _ => genSingletonError (StructureName.toString structureName) "导入模块时出错" NONE)
                            ) >>= (fn (newContext, path) =>
                                Success([PImportStructure (structureOpAST, path, oper)], newContext)
                                )
                            )
                            )
                            else
                            if oper ~=** importOpenStructureOp
                            then getStructureOpAST l1 >>= (fn structureOpAST => 
                            ExpressionConstructionPass.getStructureName structureOpAST >>= (fn structureName =>  
                            (newContextAfterImportingStructure structureName ctx 
                                <?> (fn _ => genSingletonError (StructureName.toString structureName) "导入模块时出错" NONE)
                            ) >>= (fn (ctx, path) =>
                                newContextAfterOpeningStructure [List.last structureName] ctx >>= (fn ctx => 
                                Success([PImportStructure (structureOpAST, path, oper), POpenStructure (UnknownOpName(List.last(structureName)), oper)], ctx)
                                )
                            )
                            ))
                            else
                            if oper ~=** reexportStructureOp
                            then 
                                getStructureOpAST l1 >>= (fn structureOpAST => 
                            ExpressionConstructionPass.getStructureName structureOpAST >>= (fn structureName => 
                                (* newContextAfterOpeningStructure structureName ctx >>= (fn newContext =>  *)
                                        Success([PReExportStructure (structureOpAST, getReExportDecls structureName ctx, oper)], ctx)
                                    (* ) *)
                                )
                            )
                            else
                            raise Fail "pp95"
                        end
                    | _ => raise Fail "pp26: malformed output : not two args or three args"
                (* val _ = print ("returning " ^ PrettyPrint.show_preprocessaastJ res) *)
                in res end
                handle DeclarationParser.DeclNoParse (expr) => (
                    if length expr = 0 then Success ([PEmptyDecl], ctx)
                    else (parseTypeOrExpr expr ctx) >>= (fn parsedExpr => Success([PDirectExpr parsedExpr], ctx))
                )
                | DeclarationParser.DeclAmbiguousParse (expr, parses) => (
                    genSingletonError (MixedStr.toUTF8String expr)
                     ("声明有多余一种理解方式(decl ambiguous parse)：") (SOME(String.concatWith "\n" (map (fn x => "可以这样理解：(possible parse :)" ^x)
            (map (fn (oper, args) => PrettyPrint.show_op oper ^ "参数(args): " ^ (String.concatWith "，" (map MixedStr.toString args))) parses)))))
                (* handle ECPNoPossibleParse x => raise ECPNoPossibleParse (x ^ 
                    "\n when parsing declaration " ^ MixedStr.toString s)
                handle ElaborateFailure x => raise ElaborateFailure (x ^ 
                    "\n when parsing declaration " ^ MixedStr.toString s) *)
            )

           
            (* the result will not contain unparsed anything *)
            and parseTypeOrExpr (ebody : MixedStr.t)(ctx : contextType) : OpAST witherrsoption
            =  
            let
                val ebody = removeCommentInMixedStr ebody
            in
                (PrecedenceParser.parseMixfixExpression 
                            (allTypeAndExprOps@ lookupAllActiveOpers ctx) ebody) >>= (fn parseTree => 
                            let
                    (* val _ = DebugPrint.p ("notifying " ^ PrettyPrint.show_opast parseTree ^ "\n\n") *)
                    val _ = notifyOpAST parseTree
                in recursivelyTraverseAndParseOpAST parseTree ctx >>= (
                    fn (parsedOpAST, newContext) => Success(parsedOpAST) (* no need to add new context as constructs inside let is not accessible in any case *)
                ) end
                            )
            end
              
        (* the resulting preprocessing ast will not contain unparsed anything *)
            and preprocessAST (s : (MixedStr.t * MixedStr.endinginfo) list)(ctx : contextType) : (PreprocessingAST.t * contextType) witherrsoption = 
            (

                let 
                        fun ::: ((x , y) : (OpAST.pJudgment * MixedStr.endinginfo) * (PreprocessingAST.t * contextType) witherrsoption) = 
                        y >>= (fn y' => Success(x :: (#1 y'), #2 y'))
                        infix 5 :::
                in
                (* DebugPrint.p ("preprocessAST : " ^ Int.toString (length s) ^ " count : " ^PrettyPrint.show_mixedstrs s ^"\n"); *)
                case s of 
                [] => Success([], ctx)
                | ((x,ei) :: xs) => parseJudgment x ctx >>= (fn (parsed, newContext) =>
                    let val parsedWithEi = (map (fn parsed => (parsed, ei)) parsed)
                        val _  = notifyPreprocessingAST parsedWithEi
                    in 
                        foldr (fn ((parsed, ei), acc) => (parsed, ei) ::: acc) (preprocessAST xs newContext) parsedWithEi
                    end
                    )
                end
            )


            fun preprocessASTTopLevel(content : UTF8String.t) : PreprocessingAST.t witherrsoption = 
                MixedStr.makeDecl content >>= (fn s => 
                    preprocessAST s (((topLevelStructureName, true, [
                        (topLevelStructureName, true, [])
                    ]))) >>= (fn (r, newCtx) => Success(r))
                )
                
    in 
        fn s => preprocessASTTopLevel s
    end
                

end
