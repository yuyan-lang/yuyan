structure ExpressionConstructionPass =
struct
    open PreprocessingAST
    open TypeCheckingAST
    open Operators
    open PreprocessingOperators
    open OpAST
    open OpASTOps

    open StaticErrorStructure
    infix 5 >>=
    infix 6 =/=

  structure PrecParser = MixFixParser
    (* exception ElaborateFailureInternal of string *)
    (* exception InternalErrorECP *)

(* structureName will use the global naming convention *)
    type structureName = StructureName.t

    
    val ~=** = Operators.~=**
    infix 4 ~=**


    
    (* todo: remove context as we perhaps do not need it during construction *)
    type contextType = unit

    fun ::/ ((x,y), (xs,ys)) =(x :: xs, y :: ys)
    infix 5 ::/

    (* returns list of N arguments with a list of N-1 intermediate operators *)
    fun flattenRight (ast : OpAST.OpAST) (curOp : Operators.operator)  : OpAST list  * operator list= 
        case ast of
        OpAST(oper, [l1,l2]) => if oper ~=** curOp
                then (l1, oper) ::/ flattenRight l2 curOp
                else ([ast], [])
        | _ => ([ast], [])

    fun elaborateUnknownName (ast : OpAST) : UTF8String.t witherrsoption = 
        case ast of
        UnknownOpName(l1) => Success l1
        | OpParsedQuotedExpr(e, qi) => elaborateUnknownName e
        | _ => genSingletonError (reconstructOriginalFromOpAST ast) ("内部错误：期待绑定名称(expected unknown name)。这可能是一个设计的局限性，直到我们更改设计之前，请在把名称用引号括起来。") NONE
        (* raise Fail "Expect name here, (this is perhaps a bug in the design, but until we fix it, put a bracket around the name expecting expressions,  the parser may have incorrectly parsed that as an expression)" *)

    fun getStructureName (s : OpAST ) : StructureName.t witherrsoption = 
                            let 
                                val names = #1 (flattenRight s structureRefOp)
                            in 
                                (collectAll (map elaborateUnknownName names))
                            end   

    fun elaborateNewName (ast : OpAST) : UTF8String.t witherrsoption = 
        case ast of
        NewOpName(l1) => Success (l1)
        | OpParsedQuotedExpr(e, qi) => elaborateNewName e
        | _ => raise Fail "Expect new name (perhaps internal)"

    fun elaborateSingleStructure(ast : OpAST)  = 
                        case ast of
                            OpParsedDecl(d) => Success(d)
                            | _ => 
                            genSingletonError (reconstructOriginalFromOpAST ast) "期待虑块的第一个参数是结构(Expect declaration block as first argument to let in)" NONE
                           (* raise ElaborateFailure "Expect declaration block as first argument to let in" *)

        (* handle PrecedenceParser.NoPossibleParse s =>  *)
            (* raise ECPNoPossibleParse ("Parsing failed at " ^  s ^ " No possible parse. Double check your grammar.") *)
        (* | PrecedenceParser.AmbiguousParse s =>  *)
            (* raise ECPAmbiguousParse ("Parsing failed at " ^  s ^ " Ambiguous parse. Double check your grammar.") *)
    
    and elaborateLabeledType (ast : OpAST.t)  (ctx : contextType): (Label * Type) witherrsoption = 
        case ast of
        OpAST(oper, [NewOpName(l1), l2]) => if 
            oper ~=** labeledTypeCompOp 
            then elaborateOpASTtoType l2 ctx >>= (fn l2' => Success (l1, l2'))
            else genSingletonError (reconstructOriginalFromOpAST ast) "期待`夫 表 `作为总和类型或者乘积类型的组成(expect labeledTypeComp as a child of prod/sum)" NONE 
        | _ => genSingletonError (reconstructOriginalFromOpAST ast) "期待`夫 表 `作为总和类型或者乘积类型的组成(expect labeledTypeComp as a child of prod/sum)" NONE 

    and elaborateOpASTtoType 
         (ast : OpAST.OpAST) (ctx : contextType) : TypeCheckingAST.Type witherrsoption = 
        (
            (* print (PrettyPrint.show_opast ast); *)
        case ast of
             UnknownOpName (s) => 
                if UTF8String.semanticEqual s (UTF8String.fromString "《《内建类型：字符串》》") then Success(BuiltinType(BIString)) else
                if UTF8String.semanticEqual s (UTF8String.fromString "《《内建类型：整数》》") then Success(BuiltinType(BIInt)) else
                if UTF8String.semanticEqual s (UTF8String.fromString "《《内建类型：小数》》") then Success(BuiltinType(BIReal)) else
                if UTF8String.semanticEqual s (UTF8String.fromString "《《内建类型：动态分类值》》") then Success(BuiltinType(BIDynClsfd)) else
                if UTF8String.semanticEqual s (UTF8String.fromString "《《内建类型：有》》") then Success(UnitType) else
                if UTF8String.semanticEqual s (UTF8String.fromString "《《内建类型：无》》") then Success(NullType) else
                if UTF8String.semanticEqual s (UTF8String.fromString "《《内建类型：新的外部类型》》") then Success(BuiltinType(BIForeignType(UID.next()))) else
                Success(TypeVar [s])
            | OpUnparsedExpr x => raise Fail "ecp74"
            | OpParsedQuotedExpr (e, qi) => elaborateOpASTtoType e ctx
            (* | OpAST(oper, []) => ( *)
                (* if oper ~=** unitTypeOp then Success(UnitType)
                else if oper ~=** nullTypeOp then Success(NullType) *)
                (* else if oper ~=** builtinTypeStringOp then BuiltinType(BIString) *)
                (* else  *)
                (* raise InternalErrorECP *)
                (* genSingletonError (reconstructOriginalFromOpAST ast) "期待类型表达式(expecting type expression)" NONE
                        ) *)
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
                if oper ~=** inlineCommentOp
                then elaborateOpASTtoType a1 ctx
                else 
                genSingletonError (reconstructOriginalFromOpAST ast) "期待类型表达式(expecting type expression)" NONE
                (* raise ElaborateFailure (
                    "Expected a type constructor 122, got " ^ PrettyPrint.show_op oper ^ " in " 
                    ^PrettyPrint.show_opast ast ^ "\n") *)
            )
            | OpUnparsedDecl t =>  genSingletonError (reconstructOriginalFromOpAST ast) "期待类型表达式，却遇到了声明块(expected type expression, unexpected declaration block)" NONE
            | _ => 
                genSingletonError (reconstructOriginalFromOpAST ast) "期待类型表达式(expecting type expression)" NONE
            (* raise ElaborateFailure "Expected a type constructor 124" *)
        )
        (* handle ElaborateFailure s => 
        raise ElaborateFailure (s ^ "\n when elaborating type "^ PrettyPrint.show_opast ast ) *)
    
    and elaborateOpASTtoExpr  (ast : OpAST.t)(ctx : contextType) : TypeCheckingAST.RExpr witherrsoption = 
    let fun snd (x : OpAST list) : OpAST = (hd (tl x))
    in
        (case ast of
              UnknownOpName (s) => if NumberParser.isNumber s then 
                 (case NumberParser.parseNumber s of 
                    NumberParser.NPInt i => Success (RIntConstant (i, s))
                    | NumberParser.NPReal r => Success (RRealConstant (r, s))
                 )
              else
              (case BuiltinFunctions.parseStr (UTF8String.toString s) of 
                SOME f => Success(RBuiltinFunc(f, s))
                | NONE => Success (RExprVar [s])
                )
            | OpUnparsedExpr x => raise Fail "ecp130"
            | OpParsedQuotedExpr (e, qi) => elaborateOpASTtoExpr e ctx
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
                    if oper ~=** inlineCommentOp
                    then elaborateOpASTtoExpr (hd l) ctx
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
                                                                else 
                                                                    genSingletonError (reconstructOriginalFromOpAST x) "期待一个分析分句(expected a case clause)" NONE
                                                                (* raise ElaborateFailure ("Expected a case clause 1" ^ " got " ^ PrettyPrint.show_opast x) *)
                                                                | _ =>
                                                                    genSingletonError (reconstructOriginalFromOpAST x) "期待一个分析分句(expected a case clause)" NONE
                                                                 (* raise ElaborateFailure ("Expected a case clause 2" ^ " got " ^ PrettyPrint.show_opast x) *)
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
                    then fmap RTApp(==/= (elaborateOpASTtoExpr (hd l) ctx , elaborateOpASTtoType (snd l) ctx, (operSuc =/= Success (reconstructOriginalFromOpAST (snd l) ))))
                    else
                    if oper ~=** packExprOp
                    then fmap RPack(==/= (elaborateOpASTtoType (hd l) ctx, elaborateOpASTtoExpr (snd l) ctx, (Success (reconstructOriginalFromOpAST (hd l))=/= operSuc)))
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
                    if oper ~=** sequentialCompositionOp
                    then fmap RSeqComp (==/= (elaborateOpASTtoExpr (hd l) ctx , elaborateOpASTtoExpr (snd l) ctx, operSuc))
                    else
                    if oper ~=** letinOp
                    then (
                        let 
                            val preprocessedTree = elaborateSingleStructure (hd l)
                        in 
                        preprocessedTree >>= (fn (tree, qi) => 
                        let
                            (* val newOps = extractAllOperators tree *)
                            val declTree = constructOpAST tree ctx
                            (* val bodyExpr = elaborateOpASTtoExpr (snd l) (insertIntoCurContextOps ctx newOps) *)
                            val bodyExpr = elaborateOpASTtoExpr (snd l) ctx
                        in fmap RLetIn(==/=(declTree, bodyExpr,operSuc)) end
                        )
                        end
                    )
                    else
                    genSingletonError (reconstructOriginalFromOpAST ast) "期待表达式结构(expected expression construct)" (SOME "你是否使用了类型表达式？请使用普通表达式。")
                    (* raise ElaborateFailure "Expected Expression constructs 224" *)
                )
                end
            | OpUnparsedDecl t =>  genSingletonError (reconstructOriginalFromOpAST ast) "期待表达式，却遇到了声明块(expected expression, unexpected declaration block)" NONE
            | _ =>
                    genSingletonError (reconstructOriginalFromOpAST ast) "期待表达式结构(expected expression construct)" NONE
                (* raise ElaborateFailure "Expected Expression constructs 227" *)
        )
        (* handle ElaborateFailure s => 
        raise ElaborateFailure (s ^ "\n when elaborating expr "^ PrettyPrint.show_opast ast ) *)
    end

            
     

    and constructOpAST  (ast : PreprocessingAST.t) (ctx  : contextType) 
    (* after constructing the ast, we need to get its list of operators *)
        : TypeCheckingAST.RSignature  witherrsoption =
        (
        (* print ("\rRemaining: "^ Int.toString(List.length ast) ^ " statements"); *)
        (* print ("constructOpAST on "^ PrettyPrint.show_preprocessaast ast ^ " with curSName = " ^ 
        StructureName.toStringPlain curSName ^ " with addedOps = " ^ PrettyPrint.show_ecpops addedOps ^"\n\n"); *)
        case ast of 
            [] => Success ([])
            | ((x, ei) :: xs) => 
                let fun trailingNoOps() : 
                TypeCheckingAST.RSignature witherrsoption = constructOpAST xs ctx 
                fun ::: (x , y) = y >>= (fn y' => Success(x :: y'))
                infix 5 :::
                fun trailingWithOps(): TypeCheckingAST.RSignature  witherrsoption
                    = constructOpAST xs ctx
                    (* = constructOpAST xs (insertIntoCurContextOp ctx addedOp) *)
                in
                (case x of 
                    PTypeMacro(tname, tbody, soi) => elaborateOpASTtoType tbody ctx  >>= 
                            (fn t  => RTypeMacro(tname, t) ::: trailingNoOps())
                    | PTermTypeJudgment(ename, tbody, soi) => elaborateOpASTtoType  tbody ctx >>= (fn t => 
                            RTermTypeJudgment(ename, t) ::: trailingNoOps())
                    | PTermMacro(ename, ebody, soi) => elaborateOpASTtoExpr ebody ctx >>= (fn e => 
                            RTermMacro(ename, e) ::: trailingNoOps())
                    | PTermDefinition(ename, ebody, soi) => elaborateOpASTtoExpr ebody ctx >>= (fn eb => 
                        RTermDefinition(ename, eb) ::: trailingNoOps())
                    | POpDeclaration(opName, assoc, pred, soi) => trailingWithOps() 
                    | PDirectExpr(ebody) => elaborateOpASTtoExpr ebody ctx >>= (fn eb => 
                    RDirectExpr(eb) ::: trailingNoOps())
                    | PComment _ => trailingNoOps()
                    | PStructure(publicVisible, sname, decls, soi) => 
                        (* preprocessAST decls >>= (fn preprocessedTree => 
                                            let val newOps = extractAllOperators preprocessedTree
                                                val declTree = constructOpAST preprocessedTree ctx *)
                                            (* in declTree >>= (fn ds =>  *)
                                            elaborateSingleStructure (decls) >>= (fn (decls, qi)  =>
                                            constructOpAST decls ctx >>= (fn ds => 
                                            RStructure(publicVisible,sname,  ds):::
                                                (* constructOpAST xs (curSName, curV, ((curSName@[sname], publicVisible, newOps):: addedOps)) ) *)
                                                constructOpAST xs ctx) )
                                            (* end *)
                        
                    | POpenStructure(sname, soi) =>  (* open will be as if there is a local declaration with 
                    the same name as the public members of the structure *)
                        (* ROpenStructure(sname) ::: constructOpAST xs (insertIntoCurContextOps ctx (lookupContextForOpers ctx (curSName@sname))) *)
                        let 
                        in
                                getStructureName(sname) >>= (fn sname => ROpenStructure(sname) ::: constructOpAST xs (ctx))
                        end
                    | PReExportStructure(sname, soi) =>  (* open will be as if there is a local declaration with 
                    the same name as the public members of the structure *)
                        (* ROpenStructure(sname) ::: constructOpAST xs (insertIntoCurContextOps ctx (lookupContextForOpers ctx (curSName@sname))) *)
                        let 
                        in
                                getStructureName(sname) >>= (fn sname => RReExportStructure(sname) ::: constructOpAST xs (ctx))
                        end
                    | PImportStructure(name,path,  soi) => 
                        let 
                        in
                                getStructureName(name) >>= (fn sname => RImportStructure(sname, path) ::: constructOpAST xs (ctx))
                        end
                    | PEmptyDecl => trailingNoOps()
                )
                end
        )
        (* handle ElaborateFailure x => raise ElaborateFailure (x ^ 
            "\n when elaborating declaration " ^ PrettyPrint.show_preprocessaast ast) *)
                

    and constructTypeCheckingASTTopLevel
     ( ast : PreprocessingAST.t) : TypeCheckingAST.RSignature witherrsoption = 
    let 
        (* val _ = print ("Total "^ Int.toString(List.length ast) ^ " statements\n"); *)
        val res =  constructOpAST ast  ()
        (* val _ = print ("Done "^ Int.toString(List.length ast) ^ " statements\n"); *)
    in 
        res end
    


        
end
