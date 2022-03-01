
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

    fun structureNameNotFoundError sName ctx = ( genSingletonError (StructureName.toString sName) 
                 ("结构名未找到(Structure Name " ^ StructureName.toStringPlain sName ^ " not found in context)") (showctxPre ctx))

    fun lookupContextForOpers((curSName,curV,  ctx) : contextType) (sName : structureName) : Operators.operator list witherrsoption=
        case ctx of
            [] => structureNameNotFoundError sName (curSName, curV, ctx)
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
        then structureNameNotFoundError openName ctx
        else Success(curSName, curV, allIndirectStructures@newSNameOpL)
        end
    



    fun ::/ ((x,y), (xs,ys)) =(x :: xs, y :: ys)
    infix 5 ::/



 
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
        (curSName, vis, List.mapPartial  (fn (x, ei) => 
                (case x of 
                     POpDeclaration(opName, assoc, pred, soi) => SOME(parsePOperator(x))
                    | _ => NONE
                )
        ) ast)::
        (* sub structures *)
        (List.concat(List.mapPartial (fn (x, ei) => case x of 
            PStructure(vis, structureName, OpParsedDecl(l, qi), soi) => 
                SOME (extractAllOperators (curSName@[structureName]) vis l)
            | _ => NONE
        ) ast))


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
                   Success ((curSName, vis, imports@extractAllOperators importName true tree), path)
                    (* TODO: prevent repetitive imports *)
                )
            end
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
                    (* if oper ~=** typeMacroOp
                    then parseTypeOrExpr l2 ctx >>= (fn l2 =>  Success(PTypeMacro (tp l1, l2, oper), ctx)) *)
                    (* else  *)
                    if oper ~=** termTypeJudgmentOp
                    then parseTypeOrExpr l2 ctx >>= (fn l2 =>  Success(PTermTypeJudgment (tp l1, l2, oper), ctx))
                    else if oper ~=** constructorDeclarationOp
                    then parseTypeOrExpr l2 ctx >>= (fn l2 =>  Success(PConstructorDecl (tp l1, l2, oper), ctx))
                    (* else if oper ~=** termMacroOp
                    then parseTypeOrExpr l2 ctx >>= (fn l2 =>  Success(PTermMacro (tp l1, l2, oper), ctx)) *)
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
                        parseAssoc (tp l2) >>= (fn parsedAssoc => 
                        if NumberParser.isInteger (tp l3)
                        then
                            let val newOp = POpDeclaration (tp l1, parsedAssoc, NumberParser.parseInteger (tp l3), (tp l2, tp l3, oper))
                                val newContext = (insertIntoCurContextOp ctx (parsePOperator newOp))
                            in
                                Success(newOp, newContext)
                            end
                        else genSingletonError (tp l3) "优先级必须是一个整数" NONE
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
                            if oper ~=** commentOp
                            then Success(PComment (l1, oper), ctx)
                            else 
                            if oper ~=** openStructureOp
                            then 
                                getStructureOpAST l1 >>= (fn structureOpAST => 
                            ExpressionConstructionPass.getStructureName structureOpAST >>= (fn structureName => 
                                newContextAfterOpeningStructure structureName ctx >>= (fn newContext => 
                                        Success(POpenStructure (structureOpAST, oper), newContext)
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
                                Success(PImportStructure (structureOpAST, path, oper), newContext)
                                )
                            )
                            )
                            else
                            if oper ~=** reexportStructureOp
                            then 
                                getStructureOpAST l1 >>= (fn structureOpAST => 
                            (* ExpressionConstructionPass.getStructureName structureOpAST >>= (fn structureName =>  *)
                                (* newContextAfterOpeningStructure structureName ctx >>= (fn newContext =>  *)
                                        Success(PReExportStructure (structureOpAST, oper), ctx)
                                    (* ) *)
                                (* ) *)
                            )
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
            =  (PrecedenceParser.parseMixfixExpression 
                        (allTypeAndExprOps@ lookupAllActiveOpers ctx) ebody) >>= (fn parseTree => 
                        let
                (* val _ = DebugPrint.p ("notifying " ^ PrettyPrint.show_opast parseTree ^ "\n\n") *)
                val _ = notifyOpAST parseTree
            in recursivelyTraverseAndParseOpAST parseTree ctx >>= (
                fn (parsedOpAST, newContext) => Success(parsedOpAST) (* no need to add new context as constructs inside let is not accessible in any case *)
            ) end
                        )
              
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
                    let val _  = notifyPreprocessingAST [(parsed, ei)]
                    in 
                        (parsed, ei) ::: preprocessAST xs newContext
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
