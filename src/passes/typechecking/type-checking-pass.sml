(* Implemented from various sources including 
- Robert Harper, Practical Foundations for Programming Languages, 2016
- Ulf Norell, Towards a Practical Programming Language based on Dependent Type Theory, 2007
*)
structure TypeCheckingPass = struct
open TypeCheckingAST
open TypeCheckingASTOps
open TypeCheckingContext
open TypeCheckingUtil
open TypeCheckingPatterns
open TypeCheckingUnify
open StaticErrorStructure
infix 5 >>= 
infix 5 >> 
infix 5 <|>
infix 6 =/=
infix 5 <?>

    (* val DEBUG = true *)
    val DEBUG = false


    fun getMapping (c: context ):mapping list = 
        case c of  (Context(cSname, cVis, m)) => m

    fun nextContextOfOpenStructure  (errReporting : UTF8String.t) (ctx : context) (resolvedSig : CSignature) (done : context -> ('a * context ) witherrsoption) 
    : ('a * context) witherrsoption =
            (* extract all bindings from bindings in order and put them into the current context *)
            case resolvedSig of 
            [] => done ctx
            | (dec :: tsig) => 
            let fun continue ctx = nextContextOfOpenStructure errReporting ctx tsig done in 
            (case dec of 
             CTermDefinition(name, e, t) => 
             withLocalGeneric ctx [name] t (JTDefinition(e)) continue
            | CConstructorDecl(name, t, consinfo) => 
                withLocalGeneric ctx [name] t (JTConstructor consinfo) continue
            | CDirectExpr _ => continue ctx
            | CImport _ => continue ctx
            | COpenStructure _ => continue ctx (* ofcourse open structure doesn't chain together *)
            | CPureDeclaration (name, tp) => (Errors.genericErrorStr errReporting ctx "opened structure cannot contain pure declaration")
                                )
                end


    (* fun nextContextOfOpenStructure  (curSName : StructureName.t) (curVis : bool) (bindings : mapping list) 
    (openName : StructureName.t)=

     Context(curSName, curVis, 
            (* extract all bindings from bindings in order and put them into the current context *)
                    List.mapPartial (fn x => 
                    case x of TermTypeJ(name, t, jtp,  u) => 
                    (case StructureName.checkRefersToScope name openName curSName of
                        SOME(nameStripped) => SOME(TermTypeJ(curSName@nameStripped, t, jtp, 
                            (case u of SOME x => SOME x | NONE => SOME (name, jtp))))
                        | NONE => NONE)
                    (* | TermDefJ(name, t, u) =>
                    (case StructureName.checkRefersToScope name openName curSName of
                        SOME(nameStripped) => SOME(TermDefJ(curSName@nameStripped, t, u))
                        | NONE => NONE) *)
                    ) bindings @ bindings
                ) *)


            (* extract all bindings from bindings in order and put them into the current context *)
        (* let val decls = 
        List.mapPartial (fn x => 
            case x of TermTypeJ(name, t, jtype,  u) => 
            (case StructureName.checkRefersToScope name reexportName curSName of
                SOME(nameStripped) => SOME(CTermDefinition(curSName@nameStripped,  
                                            (case u of SOME (x, jtp) => CVar(x, (case jtp of JTDefinition d => CVTDefinition d 
                                                                                            | _ => CVTBinder))  (* TODO: BIG ISSUE: REExport of type constructors *)
                                            | NONE => CVar (name, (case jtype of JTDefinition d => CVTDefinition d 
                                                                                            | _ => CVTBinder))), t))
                                            (* TODO: export of constructors *)
                | NONE => NONE)
            (* | TypeDef(name, t, u) =>
            (case StructureName.checkRefersToScope name reexportName curSName of
                SOME(nameStripped) => SOME(CTypeMacro(curSName@nameStripped, t))
                | NONE => NONE) *)
            ) bindings 
        in if length decls > 0
        then Success(List.rev decls) (* context order are reverse of reexport order *)
        else genSingletonError (StructureName.toString reexportName) "结构未包含任何可导出的值" (showctxSome ctx)
        end *)

        



    (* index begins with zero *)
    fun lookupLabel ( ctx : (Label * 'a) list) (l : Label) : (int * 'a) witherrsoption = 
        let fun go i ctx = 
        case ctx of 
            [] =>  genSingletonError l ("标签`" ^ UTF8String.toString l ^ "`未找到") NONE
            (* raise TypeCheckingFailure ("label " ^ UTF8String.toString l ^ " not found in prod type") *)
            | (n1, t1)::cs => if UTF8String.semanticEqual n1 l then Success (i, t1) else go (i+1) cs
        in go 0 ctx
        end

      fun lookupLabel3 ( ctx : (Label * EVar *RType) list) (l : Label) : RType witherrsoption = 
        case ctx of 
            [] => genSingletonError l ("标签`" ^ UTF8String.toString l ^ "`未找到") NONE
            (* raise TypeCheckingFailure ("label " ^ UTF8String.toString l ^ " not found in sum type") *)
            | (n1, _, t1)::cs => if UTF8String.semanticEqual n1 l then Success t1 else lookupLabel3 cs l

    (* find a declaration and an index *)
    (* the index currently includes directexpr, import, and open structure, so that 
    need to be consistent with cps *)
    fun findSigList (name : UTF8String.t)(decl : CDeclaration list) (debugMsg : string)  : (CDeclaration * int) option = 
    let fun go decl i = 
        case decl of 
            [] => NONE
            | ((d as CPureDeclaration(n, _)) :: dl) => if UTF8String.semanticEqual n name then SOME(d, i) else go dl (i+1)
            | ((d as CTermDefinition(n, _, _)) :: dl) => if UTF8String.semanticEqual n name then SOME(d, i) else go dl (i+1)
            | ((d as CConstructorDecl(n, _, _)) :: dl) => if UTF8String.semanticEqual n name then SOME(d, i) else go dl (i+1)
            | (CDirectExpr _ :: dl) => go dl (i+1)
            | (CImport _ :: dl) => go dl (i+1)
            | (COpenStructure _ :: dl) => go dl (i+1)
        val res = go decl 0 
        (* val _ = DebugPrint.p ("finding name " ^ UTF8String.toString name ^ " in " ^ 
        " signatrue " ^ PrettyPrint.show_typecheckingCSig decl
        ^ " returns " ^ 
        (case res of 
            SOME(_, idx) => Int.toString idx
            | NONE => "NONE")
            ^ " DEBUG: " ^ debugMsg ^ "\n\n") *)
    in res end
        

    (* modify signature according to modification funcation
    function arg: SOME indicates found with the object NONE indicates not FOUND
    function return : SOME indicates replacement(if found)/insertion (if not found) 
                        NONE indicates delection(if found)/do nothing (if not found)
     *)
    fun modifySigList (name : UTF8String.t) (modification : CDeclaration -> CDeclaration )
    (decl : CDeclaration list)  : CDeclaration list = 
    (* let fun go l acc =  *)
    let in
        case decl of 
            [] => raise Fail "modifySigList not found!"
            | ((d as 
            (CPureDeclaration(n, _) | 
            CTermDefinition(n, _, _) |
            CConstructorDecl(n, _, _)
            ) ):: dl) => if UTF8String.semanticEqual n name 
                then modification d :: dl
                else d :: modifySigList name modification dl
            | (h :: dl) => h :: modifySigList name modification dl
    end
    (* in
    end *)


    fun typeEquivList (ctx : context) (e : RExpr) (a : CType list) : CType witherrsoption =
        case a of
            [] => raise Fail ("INternal error: empty sum")
            | [t] => Success t
            | (x::y :: xs) => tryTypeUnify ctx e x y  >>= (fn (ctx) =>  typeEquivList ctx e (x :: xs)
            (* else genSingletonError (reconstructFromRExpr e) "类型不相等"  (SOME 
                ("第一类型：" ^  (PrettyPrint.show_typecheckingCType x) 
                ^ "\n第二类型：" ^  (PrettyPrint.show_typecheckingCType x) *)
            )
            (* raise TypeCheckingFailure ("Type unify failed") *)
    
    structure Errors = TypeCheckingErrors
    

    
     fun addNewMetaVar (ctx : context)  (tp : CType)
     (errReporting : UTF8String.t) : (CExpr * context) witherrsoption =
     let 
                            (* val  allBindings = 
                            List.filter (fn (TermTypeJ(name, tp, jtp, _)) => 
                            (case jtp of 
                                JTPending => false
                                 _ => true
                            )) (getMapping ctx) *)
                            val metavarname = StructureName.metaVarName()
                            (* val resultingTerm = foldl (fn (TermTypeJ(name, tp, jtp, _), acc) => 
                                 CApp(acc, CVar(name, judgmentTypeToCVarType name (* TODO : why do we need canonical names? *) 
                                                jtp), CTypeAnnNotAvailable) (* Do we really need it ? *)
                                ) (CMetaVar metavarname) allBindings *)
                            (* val metaType = foldr (fn (TermTypeJ(name, tp, _, _), acc) => 
                                let val tempName = UTF8String.fromString ("《《临时名称" ^ Int.toString (UID.next()) ^ "》》")
                                in 
                                    CPiType(tp, SOME(tempName), substTypeInCExpr (CVar([tempName], CVTBinder)) name acc)
                                end
                            ) (tt) allBindings *)
                            val newCtx = addToCtxA (TermTypeJ(metavarname, tp, JTMetaVarPendingResolve errReporting , NONE)) ctx
                            in
                                Success(CMetaVar(metavarname), newCtx)
                            end

    fun checkBlockIsSignature (errReporting : RExpr) (ctx : context) (block : CDeclaration list) : unit witherrsoption = 
        if List.exists (fn dec => 
            case dec of 
            CTermDefinition _ => true
            | CImport _ => true 
            | _ => false
            ) block
        then Errors.genericError errReporting ctx "结构体用作签名时不可以包含定义的表达式(3)/或者导入语句"
        else Success()

    fun checkBlockIsModule (ctx : context) (block : CDeclaration list) : unit witherrsoption = 
        let val notImplementedNames =  List.mapPartial (fn dec => 
            case dec of 
            CPureDeclaration(name, _) => SOME(name)
            | _ => NONE
            ) block

        in
        if length notImplementedNames <> 0
        then
            (let fun go acc l= 
                    case l of 
                    [] => acc
                    | (x :: xs) => go (acc <?> (fn _ => Errors.genericErrorStr x ctx "结构体用作签名时不可以包含定义的表达式(2)")) xs
            in
                    go (Errors.genericErrorStr (hd notImplementedNames) ctx "结构体用作签名时不可以包含定义的表达式(1)") 
                    (tl notImplementedNames)
            end
            )
        else Success()
        end


    fun getSingatureForModule   (block : CDeclaration list) : CDeclaration list = 
    List.mapPartial (fn dec => 
        case dec of 
        CTermDefinition(n, tm, tp) => SOME(CPureDeclaration (n, tp))
        | CPureDeclaration _ => raise Fail ("attempt to get signature for non-module " ^ PrettyPrint.show_typecheckingCSig block)
        | CConstructorDecl(n, tm, tp) => SOME(dec)
        | CImport _ => SOME(dec) (* TODO: REVIEW CHOICES *)
        | CDirectExpr _ => SOME(dec)
        | COpenStructure _ => SOME(dec) (* TODO: REVIEW CHOICES, SHOULD INCLUDE? *)
    ) block
    (* DO NOT Implement this, I prefer to generate coersions instead of implicit subtyping*)
    (* SO THIS IS ESSENTIALLY EQUALITY CHECKING ! (EXCEPT IMPORTS) *)
    fun checkSignatureAscription (errReporting : RExpr) (ctx : context) (block : CDeclaration list) (blocktype : CDeclaration list): context witherrsoption = 
    let fun sub' tS x s = 
            case (substituteTypeInCSignature tS x (s)) of
            s' => s'
            (* | _ => raise Fail "tcp238" *)
    in
    checkBlockIsModule ctx block >> checkBlockIsSignature errReporting ctx blocktype >> (
        case (block, blocktype) of
            ([], []) => Success(ctx)
            | (_, []) => Errors.genericError errReporting ctx "结构与类型不匹配"
            | ((CImport _ | CDirectExpr _ | COpenStructure _) :: blkTail, _) => 
                checkSignatureAscription errReporting ctx blkTail blocktype
            | ([], _) => Errors.genericError errReporting ctx "结构与类型不匹配"
            | (CTermDefinition(n, tm, tp) :: blkTail, 
            CPureDeclaration (n', tp') :: sigTail) => 
            if UTF8String.semanticEqual n n' 
            then tryTypeUnify ctx errReporting tp tp'  >>= (
                fn ctx => 
                checkSignatureAscription errReporting ctx blkTail (sub' tm [n'] sigTail)
            )
            else Errors.genericError errReporting ctx "结构与类型不匹配"
            | (CConstructorDecl(n,  tp, cconsinfo) :: blkTail, 
            CPureDeclaration (n', tp') :: sigTail) => 
            if UTF8String.semanticEqual n n' 
            then tryTypeUnify ctx errReporting  tp tp'  >>= (
                fn ctx => 
                (* TODO: Check this case *)
                checkSignatureAscription errReporting ctx blkTail (sub' (CVar([n], CVTConstructor([n], cconsinfo))) [n'] sigTail)
            )
            else Errors.genericError errReporting ctx "结构与类型不匹配"
            | (CConstructorDecl(n,  tp, cconsinfo) :: blkTail, 
            CConstructorDecl (n', tp', cconsinfo') :: sigTail) => 
            if UTF8String.semanticEqual n n' 
            then tryTypeUnify ctx errReporting tp tp'  >>= (
                fn ctx => 
                checkSignatureAscription errReporting ctx blkTail sigTail
            )
            else Errors.genericError errReporting ctx "结构与类型不匹配"
            | (CPureDeclaration _ :: _ , _) => raise Fail "pure decl in checked module"
            | (CTermDefinition _ :: _ , CConstructorDecl _ :: _) => Errors.genericError errReporting ctx "结构与类型不匹配"
            | ( _ , (CImport _ | CDirectExpr _ | COpenStructure _ | CTermDefinition _) :: _) => raise Fail "signature malformed"
    )
    end

        

    (* returned CExpr is CVar or CStructureProj *)
    fun resolveRVar (ctx : context) (rvarName : StructureName.t) : (CExpr * CType) witherrsoption = 
    let fun findCorrectType i (acctm, acctp) (* acc for accumulation *) = 
            if i > length rvarName then raise Fail "not possible tcp225"
            else
            if i = length rvarName
            then 
            Success(acctm, acctp)
            else
            let val curNameComp = List.nth(rvarName,i)
            (* val _ = DebugPrint.p ("IN CONTEXT " ^ 
                PrettyPrint.show_typecheckingpassctx ctx) *)
            in
                weakHeadNormalizeType (RVar(rvarName)) ctx acctp >>= (fn acctp => 
                    (case acctp of 
                        CBlock(csig) => 
                            (* TODO: dependent types should plug in concrete values for all previous 
                            indecies, check implementation of RProj (previous version) *)
                            (case findSigList curNameComp csig "302" of
                                SOME(
                                    ( CPureDeclaration(_, tp)
                                    | CConstructorDecl(_, tp, _)
                                    )
                                    , idx) =>
                                    let 
                                        val nextAccTm = CBlockProj(acctm, curNameComp, idx)
                                        (* val _ = DebugPrint.p (PrettyPrint.show_typecheckingCExpr tp) *)
                                        val nextAccTp = tp (*TODO: dependent case *)
                                        val nextI = i +1
                                    in findCorrectType nextI (nextAccTm, nextAccTp)
                                    end
                                | SOME _ => Errors.genericErrorStr curNameComp ctx "仅可投射签名？检查编译器中签名的合成"
                                | NONE => Errors.genericErrorStr curNameComp ctx ("结构中未找到该名称" ^ 
                                "\n所有名称：" ^ PrettyPrint.show_typecheckingCSig csig)
                            )
                        | _ => Errors.genericErrorStr curNameComp ctx "试图从非结构进行投影"
                            (* (StructureName.toString (List.take(rvarName, i-1))) *) (* this will not have source range *)
                    )
                )
            end

        (* i is the NUMBER of anme components (prefix) to search for a module *)
        fun lookupModuleName i = if i = length rvarName
        then
            lookupCtx ctx rvarName >>= (fn (canonicalName, tp, jtp) =>
            if not (StructureName.semanticEqual canonicalName rvarName) then raise Fail "canonical name different from rvar name(1)"
            else Success((CVar(canonicalName, judgmentTypeToCVarType canonicalName jtp), tp)))
        else (case findCtx ctx (List.take(rvarName, i)) of 
            NONE => lookupModuleName (i+1)
            | SOME(canonicalName, tp, jtp) => 
            if not (StructureName.semanticEqual canonicalName (List.take(rvarName, i)))
            then raise Fail "canonical name different from rvar name(2)"
            else findCorrectType (i) (CVar(canonicalName, judgmentTypeToCVarType canonicalName jtp), tp))
    in lookupModuleName 1
    end
    

    fun configureAndTypeCheckSignature
    (topLevelStructureName : StructureName.t)
    (
        getTypeCheckedAST:  (FileResourceURI.t * StructureName.t) -> TypeCheckingAST.CSignature witherrsoption
    )
    :  RSignature -> CSignature witherrsoption =
    let
            fun checkConstructorType(nameOfCons : UTF8String.t) ( ctx : context) (t : RType) : ((CType * cconstructorinfo) * context) witherrsoption = 
            let val typeInfoWeo : (CType * context) witherrsoption = 
                case t of 
                    RPiType(t1, evop, t2, p, soi) => 
                    checkExprOptionIsType ctx t1 ((case evop of SOME x => RVar([x]) | NONE => t)) >>= (fn (ct1, ctx) => 
                            (case evop of NONE => (fn k => k ctx)
                                | SOME(ev) => withLocalBinder ctx ev ct1
                            ) (fn ctx => 
                                        checkType ctx
                                            t2 
                                            (CUniverse) 
                            >>= (fn (ct2, ctx) => 
                                Success(CPiType(ct1, evop, ct2, p), ctx)
                            ))
                        )
                   
                    | _ => checkType ctx t (CUniverse) >>= (fn (t, ctx) => Success(t, ctx))

                fun countOccurrencesOfElementConstructor(foundTypeConstructorName: StructureName.t) = 
                List.length (List.filter (fn (TermTypeJ(name, tp, jt, originalName)) => 
                    case jt of 
                        JTConstructor(CConsInfoElementConstructor(tcname, _)) => StructureName.semanticEqual tcname foundTypeConstructorName
                        | _ => false (* TODO: Fix the case of open *)
                )    (getMapping ctx))
                 

                fun checkScopeAndIndexAgainstFoundTypeConstructor
                (errReporting : RExpr)
                (foundTypeConstructorName : StructureName.t) : cconstructorinfo witherrsoption =
                    (* if StructureName.semanticEqual (getCurSName ctx)  (StructureName.getDeclaringScope foundTypeConstructorName) *)
                    if length foundTypeConstructorName = 1 (* if in current scope, name should be singular *)
                    then Success(CConsInfoElementConstructor(foundTypeConstructorName, 
                            countOccurrencesOfElementConstructor(foundTypeConstructorName) + 1))
                    else Errors.elementConstructorScopeError  errReporting ctx  
                    ("\n当前结构名：" ^ (StructureName.toStringPlain (getCurSName ctx))
                    ^ "类型构造器名：" ^ (StructureName.toStringPlain ( foundTypeConstructorName))
                    )



                (* only trace one level deep*)
                fun traceVarOnly(errReporting : RExpr) (cexpr : CExpr) =  case cexpr of
                    CUniverse => Success(CConsInfoTypeConstructor)
                    | CVar(v, vinfo) => (case vinfo of 
                        CVTConstructor (name, CConsInfoTypeConstructor) =>  
                            (checkScopeAndIndexAgainstFoundTypeConstructor errReporting name)
                        | CVTDefinition (v') => traceVarOnly errReporting v'
                        | _ => Errors.notATypeConstructor errReporting ctx
                    )
                    | _ => Errors.notATypeConstructor errReporting ctx

                fun analyzeVariable(v : StructureName.t) = 
                let val errReporting = RVar(v)
                in
                        lookupCtx ctx v  >>= (fn lookedUpJ => 
                                        case  lookedUpJ of
                                            (cname, tp, jinfo) => (case jinfo
                                            of JTConstructor (CConsInfoTypeConstructor) => 
                            (checkScopeAndIndexAgainstFoundTypeConstructor errReporting cname)
                                                | JTDefinition (v') => (traceVarOnly (RVar(v)) v')
                                                | _ => Errors.notATypeConstructor (RVar(v)) ctx
                                            )
                        )
                end

                fun getConsInfo (isCanonical : bool) (t : RType) = 
                if isCanonical
                then
                (case t of 
                    (* RFunc(t1, t2, soi) => getConsInfo true t2 *)
                     RPiType(t1, b, t2,p, soi) => getConsInfo true t2
                    | _ => getConsInfo false t)
                else
                (case t of 
                    RApp(t1, t2,p, soi) => getConsInfo false t1
                    | RVar(s) => analyzeVariable(s)
                    | RUniverse(s) => Success(CConsInfoTypeConstructor)
                    | _ => Errors.notATypeConstructor t ctx
                    )
                val consInfo = getConsInfo true t
            in 
            typeInfoWeo  >>= (fn (tpInfo, ctx) => 
                consInfo >>= (fn consInfo => 
                    Success((tpInfo, consInfo), ctx)
                )
            )
            end
            and reExportDecls  (ctx : context)
            (reexportName : StructureName.t) : CSignature witherrsoption =
                lookupCtx ctx reexportName  >>= (fn (_, tp, jt) => 
                case jt of 
                    JTDefinition(block) => 
                        (weakHeadNormalizeType (RVar reexportName) ctx block >>= (fn x => 
                            case x of
                                CBlock(csig) =>
                                    checkBlockIsModule ctx csig >> (
                                        typeCheckSignature ctx
                                        (List.mapPartial (fn dec =>
                                            case dec of 
                                                (CTermDefinition (name, _, _) 
                                                | CConstructorDecl(name, _, _))
                                                => SOME(RTermDefinition (name, RVar(reexportName@[name])))
                                                | CImport(name, fp) => SOME(RImportStructure(name, fp))
                                                | _ => NONE
                                                (* | _ => raise Fail ("does not support reexport of " ^ PrettyPrint.show_typecheckingCDecl dec) *)
                                            ) csig) false [] >>= (fn (checkedsig, ctx') => 
                                        Success(checkedsig)
                                        )
                                    )
                                | _ => Errors.genericErrorStr (StructureName.toString reexportName) ctx ("名称不是模块，而是" ^ PrettyPrint.show_typecheckingCExpr tp 
                                ^ " 声明类型为 " ^  PrettyPrint.show_typecheckingjt jt 
                            )))
                    | _ => Errors.genericErrorStr (StructureName.toString reexportName) ctx ("名称不是模块，而是" ^ PrettyPrint.show_typecheckingCExpr tp 
                    ^ " 声明类型为 " ^  PrettyPrint.show_typecheckingjt jt )
                )
            
            and checkExprIsType (ctx : context) (e : RType) : (CType * context) witherrsoption = 
                checkType ctx e CUniverse 
            and checkSpineIsType (ctx : context) (e : (UTF8String.t * RType) list ) : ((UTF8String.t  * CType) list * context) witherrsoption = 
                case e of 
                    [] => Success([], ctx)
                    | ((l,t) :: es) => 
                        checkExprIsType ctx t >>= (fn (ce, ctx) => 
                            withLocalBinder ctx l ce (fn ctx => 
                                checkSpineIsType ctx es >>= (fn (cel, ctx) => 
                                    Success((l,ce)::cel, ctx)
                                )
                            )
                        )

            and checkExprOptionIsType (ctx : context) (e : RType option) (errReporting : RType) : (CType * context) witherrsoption = 
                (case e of 
                        SOME t1 => 
                        (* let val res =  *)
                        checkType ctx t1 (CUniverse) 
                        (* in Errors.enrichErrWhenChecking t1 CUniverse res *)
                        (* end *)
                        | NONE => addNewMetaVar ctx CUniverse (reconstructFromRExpr errReporting) >>= (fn (metavar, ctx) => 
                                Success((metavar, ctx))
                            )
                        )
            

            (* synthesizeType and instantiating implicit arguments *)
            and synthesizeType (ctx : context)(e : RExpr) : ((CExpr * CType) * context) witherrsoption =
            let fun insertMetaVarRec (res as ((syne, synt), ctx) : ((CExpr * CType) * context)) = 
                    weakHeadNormalizeType e ctx synt >>= (fn nsynt => 
                    case nsynt of 
                        CPiType(t1, evop, t2, Implicit)=>
                            addNewMetaVar ctx t1 (reconstructFromRExpr e) >>= (fn (metaVarE, ctx) => 
                                insertMetaVarRec((CApp(syne, metaVarE, CTypeAnn nsynt), 
                                    (case evop of NONE => t2 | SOME tv => substTypeInCExpr metaVarE [tv] t2)), ctx)
                            )
                        | _ => Success(res)
                    )
            in
            synthesizeTypeNoInstMeta ctx e >>= (fn (res as ((syne, synt), ctx)) => 
                insertMetaVarRec ((syne, synt), ctx)
            )
            end


            (* synthesizeType without instantiating implicit arguments *)
            and synthesizeTypeNoInstMeta (ctx : context)(e : RExpr) : ((CExpr * CType) * context) witherrsoption =
            (
                let val _ = if DEBUG then print ("synthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e ^ "\n") else ()
                val originalExpr = e
                val res = (case e of
                    RVar v => 
                    resolveRVar ctx v >>= (fn tmtp => Success (tmtp, ctx))
                    (* lookupCtx ctx v >>= (fn (canonicalName, tp, jtp) =>
                    case jtp of 
                    (* removed pending but instead have pure declaration, so obselete *)
                    (* ADDED BACK*)
                        (* JTPending => Errors.genericError e ctx "变量尚未定义" *)
                         _ => Success((CVar(canonicalName, judgmentTypeToCVarType canonicalName jtp), tp), ctx)
                    ) *)
                    | RUnitExpr(soi) => Success ((CUnitExpr, CUnitType), ctx)
                    | RProj(e, (idx, idxsoi), soi) => synthesizeType ctx e >>= (fn ((ce, tt), ctx) =>  
                                    ( weakHeadNormalizeType e ctx tt) >>= (fn ntt => case 
                                    ( ntt) of 
                            (CProd ls) => 
                                (
                                    let fun go curIdx acc = 
                                            (case acc of 
                                                [] => raise Fail "tcp338 lookup should not return an invalid index"
                                                | (( t1)::tl) => 
                                                    if idx = curIdx
                                                    then t1
                                                    else 
                                                            go (curIdx+1) (tl)
                                            )
                                        val targetType = go 0 ls
                                    in 
                                        Success((CProj(ce, idx, CTypeAnnNotAvailable), targetType), ctx)
                                    end
                                )

                            | _ => Errors.attemptToProjectNonProd e (tt) ctx
                    ))
                    (* LABELED VERSION *)
                    (* | RProj(e, l, soi) => synthesizeType ctx e >>= (fn ((ce, tt), ctx) =>  
                                    ( weakHeadNormalizeType e ctx tt) >>= (fn ntt => case 
                                    ( ntt) of 
                            (CProd ls) => 
                                (lookupLabel ls l) >>= (fn (idx, _) => 
                                    let fun go curIdx acc = 
                                            (case acc of 
                                                [] => raise Fail "tcp338 lookup should not return an invalid index"
                                                | ((l1, t1)::tl) => 
                                                    if idx = curIdx
                                                    then t1
                                                    else 
                                                        (case substTypeInCExpr (CProj(ce, l1, curIdx, CTypeAnnNotAvailable)) [l1] (CProd tl)
                                                            of CProd tl' => go (curIdx+1) (tl')
                                                            | _ => raise Fail "tcp344: subst should also return prod")
                                            )
                                        val targetType = go 0 ls
                                    in 
                                        Success((CProj(ce, l, idx, CTypeAnnNotAvailable), targetType), ctx)
                                    end
                                )

                            | _ => Errors.attemptToProjectNonProd e (tt) ctx
                    )) *)
                    | RLazyProj(e, l, soi) => synthesizeType ctx e >>= (fn ((ce, tt), ctx) =>  
                    weakHeadNormalizeType e ctx tt >>= (fn ntt => case ntt of 
                            ( CLazyProd ls) => fmap (fn (idx,x) => ((CLazyProj(ce, l, CTypeAnn(CLazyProd ls)),x), ctx)) (lookupLabel ls l)
                            | _ => Errors.attemptToProjectNonLazyProd e (tt) ctx
                    ))
                    | RIfThenElse(e, tcase, fcase, soi)=> checkType ctx e (CBuiltinType BIBool) >>= (fn (ce, ctx) => 
                        (synthesizeType ctx tcase >>= (fn ((ctcase, rttp), ctx) => 
                                checkType ctx fcase (rttp) >>= (fn (cfcase, ctx) => 
                                    Success((CIfThenElse(ce, ctcase, cfcase), rttp), ctx)
                                )
                            ) 
                        ) <|> (fn () => (* alternative: either branch may synthesize *)
                                    (synthesizeType ctx fcase >>= (fn ((cfcase, rttp), ctx) => 
                                                checkType ctx tcase (rttp) >>= (fn (ctcase, ctx) => 
                                                    Success((CIfThenElse(ce, ctcase, cfcase), rttp), ctx)
                                                )
                                            ) 
                                        )
                        )
                    )
                    | RCase(e,cases, soi) => (synthesizeType ctx e) >>= (fn ((ce, t), ctx) => 
                        weakHeadNormalizeType e ctx t >>= (fn caseObjectTypeNormalized => 
                            let 
                                val checkedPatternsAndCases = (foldMapCtx ctx (fn ((pat, e), ctx) => 
                                    checkPattern ctx pat caseObjectTypeNormalized NONE (fn (cpat, newCtx) => 
                                        synthesizeType newCtx e >>= (fn ((synE, synT), ctx) => 
                                            Success((cpat, (synE, synT)), ctx)
                                        )
                                    ))
                                cases)
                            in checkedPatternsAndCases >>= (fn (l, ctx) => 
                                typeEquivList ctx originalExpr (map (fn (pat, (synE, synT)) => synT) l) >>= (fn returnType => 
                                    Success ((CCase ((CTypeAnn(caseObjectTypeNormalized), ce), 
                                        (map (fn (pat, (synE, synT)) => (pat, synE)) l)
                                    , CTypeAnn(returnType)), returnType), ctx)
                                )
                            )
                            end
                            )
                        )
                    | RLamWithType (t, ev, e, soi) => 
                    checkExprIsType ctx t >>= (fn (absTp, ctx) => 
                        withLocalBinder ctx ev absTp (fn ctx => 
                            synthesizeType ctx e >>= (fn ((bodyExpr, returnType), ctx) =>
                                let val funType = if List.exists (fn x => StructureName.semanticEqual [ev] x) (freeTCVar returnType)
                                    then CPiType(absTp, SOME ev, returnType, Explicit)
                                    else CPiType(absTp, NONE, returnType, Explicit)
                                in 
                                Success((CLam(ev, bodyExpr, CTypeAnn(funType)), funType), ctx)
                                end
                            )
                        )
                    )
                    | RApp (e1, e2, pe, soi) => 
                        (case pe of Explicit => synthesizeType ctx e1 
                                    | Implicit => synthesizeTypeNoInstMeta ctx e1)
                        >>= (fn ((ce1, synt), ctx) => 
                        weakHeadNormalizeType e1 ctx synt >>= (fn nsynt => 
                            case synt 
                                of (CPiType (t1, evop, t2, pt)) => 
                                    if pe = pt 
                                    then 
                                    ( 
                                            checkType ctx e2 (t1) >>= (fn (checkedArg, ctx) => 
                                        (
                                                Success ((CApp(ce1, checkedArg, CTypeAnn(synt)), (
                                        case evop of 
                                        NONE => t2
                                        | SOME ev => substTypeInCExpr checkedArg ([ev]) t2
                                        )), ctx)
                                            )
                                        )
                                    )
                                    else Errors.genericError e ctx ("隐式/显示参数类型不匹配"
                                        ^ " \n synthesized (e1) = " ^ PrettyPrint.show_typecheckingCExpr synt
                                        ^ " \n original (e1 e2) = " ^ PrettyPrint.show_typecheckingRType originalExpr) 
                                    (* raise Fail ("tcp359: plicity of (possible) pi type should match requested plicity"
                                        ^ " \n synthesized (e1) = " ^ PrettyPrint.show_typecheckingCExpr synt
                                        ^ " \n original (e1 e2) = " ^ PrettyPrint.show_typecheckingRType originalExpr) *)

                                | _ => Errors.attemptToApplyNonFunction e (synt) ctx
                            )
                        )
                    (* | RTAbs (tv, e2, soi) =>   synthesizeType 
                                (addToCtxA (TermTypeJ([tv], CUniverse, JTLocalBinder, NONE)) ctx )
                        e2 >>= (fn ((ce2, bodyType), ctx) => 
                    Success ((CTAbs(tv, ce2, CTypeAnn(CForall (tv, bodyType))), CForall (tv, bodyType)), ctx) ) *)
                    (* | RTApp (e2, t, soi) => synthesizeType ctx e2 >>= (fn ((ce2, st), ctx) => 
                                weakHeadNormalizeType e2 ctx st >>= (fn nst =>
                        case nst of
                            CForall (tv, tb) => 
                                checkExprIsType ctx t >>= (fn (nt, ctx) => 
                                    (* important need to normalized before subst *)
                                    (weakHeadNormalizeType t ctx nt >>= (fn nt => 
                                        Success((CTApp(ce2, nt, CTypeAnn(CForall(tv, tb))), (substTypeInCExpr nt [tv] (tb))), ctx)
                                ))
                            )
                            | _ => Errors.attemptToApplyNonUniversal e (st) ctx
                             )
                        ) *)
                    (* | ROpen (e1, (tv, ev, e2), soi) => synthesizeType ctx e1 >>= (fn ((ce1, synt), ctx) => 
                    weakHeadNormalizeType e1 ctx synt >>= (fn nsynt => case nsynt  of
                                ( CExists (tv', tb)) => 
                        synthesizeType (addToCtxA (TermTypeJ([ev], 
                        substTypeInCExpr (CVar([tv], CVTBinder)) [tv'] (tb), JTLocalBinder, NONE)) ctx) e2 >>= (fn ((ce2, synthesizedType), ctx) =>
                        if List.exists (fn t => t = [tv]) (freeTCVar (synthesizedType))
                            then Errors.openTypeCannotExitScope e synthesizedType ctx
                            else Success((COpen((CTypeAnn(CExists(tv', tb)), ce1), (tv, ev, ce2), CTypeAnn(synthesizedType)), synthesizedType), ctx)
                        )
                            | _ => Errors.attemptToOpenNonExistentialTypes e ( synt) ctx)
                    ) *)
                    (* | RUnfold (e2, soi) => synthesizeType ctx e2 >>= (fn ((ce2, synt), ctx) => 
                    weakHeadNormalizeType e2 ctx synt >>= (fn nsynt => case nsynt  of
                        ( CRho (tv, tb)) => Success ((CUnfold(ce2, CTypeAnn(CRho(tv, tb))),  (substTypeInCExpr (CRho (tv, tb)) [tv] (tb))), ctx)
                        | _ => Errors.attemptToUnfoldNonRecursiveTypes e ( synt) ctx
                        )) *)
                    | RStringLiteral(l, soi) => Success((CStringLiteral l, CBuiltinType(BIString)), ctx)
                    | RIntConstant(i, soi) => Success((CIntConstant i, CBuiltinType(BIInt)), ctx)
                    | RRealConstant (r, soi) => Success((CRealConstant  r, CBuiltinType(BIReal)), ctx)
                    | RBoolConstant (b, soi) => Success((CBoolConstant b, CBuiltinType(BIBool)), ctx)
                    

                    | RLetIn(decls, e, soi) => (
                    typeCheckSignature ctx decls true []
                        >>= (fn(csig, ctx) =>
                        (* the context returned will be devoid of csig, so we add them via a fake local open *)
                            nextContextOfOpenStructure (reconstructFromRExpr e) ctx csig (fn ctx => 
                                    synthesizeType ctx e >>= (fn ((ce, syntt), ctx) => 
                                        Success((CLetIn(csig, ce, CTypeAnn(syntt)), syntt), ctx)
                                    )
                            )
                        )
                    )
                    | RBuiltinFunc(f, s) => Success((CBuiltinFunc(f), (BuiltinFunctions.typeOf f)), ctx)
                    | RSeqComp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn ((ce1, t1), ctx) => 
                        synthesizeType ctx e2 >>= (fn ((ce2, t2), ctx) => 
                            Success((CSeqComp(ce1, ce2, CTypeAnn(t1), CTypeAnn(t2)), t2), ctx)
                        ))
                    (* types *)
                    | RUnitType(s) => Success((CUnitType, CUniverse), ctx)
                    | RNullType(s) => Success((CNullType, CUniverse), ctx)
                    | RBuiltinType(f, s) => Success((CBuiltinType(f), CUniverse), ctx)
                    | RUniverse(s) => Success((CUniverse, CUniverse), ctx) (* TODO: maybe universe levels? *)
                    | RPiType(t1, evoption, t2,p,  soi) => 
                    checkExprOptionIsType ctx t1 (case evoption of SOME x => RVar([x]) | NONE => e
                    ) >>= (fn (ct1, ctx) => 
                            let val kf = fn ctx => 
                            checkType (ctx) t2 CUniverse >>= (fn ((ct2 ), ctx) => 
                                    (* tryTypeUnify ctx t2 synT CUniverse >>=  *)
                                        (* (fn ctx =>  *)
                                        Success((CPiType(ct1, evoption, ct2, p), CUniverse), ctx)
                            )
                            in 
                                case evoption of  NONE => Success(ctx) >>= kf 
                                | SOME(n) => withLocalBinder ctx n ct1 kf
                            end
                        )
                    | RSigmaType(t1, evoption, t2, soi) =>
                        checkType ctx t1 CUniverse >>= (fn (ct1, ctx) => 
                            (case evoption of  NONE => (fn f => f ctx) | SOME(n) => withLocalBinder ctx n ct1)
                            (fn ctx => 
                                synthesizeType (ctx) t2 >>= (fn ((ct2, synT), ctx) => 
                                        tryTypeUnify ctx t2 synT CUniverse >>= 
                                            (fn ctx => Success((CSigmaType(ct1, evoption, ct2), CUniverse), ctx))
                                    )
                                )
                            )
                    | RProd(ltsl, sepl) => 
                        (foldMapCtx ctx (fn ((t), ctx) => 
                             checkType ctx t CUniverse  >>= (fn (ct, ctx) => 
                             Success ((ct), ctx)
                             )
                         ) ltsl) >>= (fn (l, ctx) => Success((CProd l, CUniverse), ctx))
                        (* checkSpineIsType ctx (map (fn (l, t, soi) => (l, t)) ltsl)
                         >>= (fn (l, ctx) => Success((CProd l, CUniverse), ctx)) *)
                    | RLazyProd  (ltsl, sepl) => 
                         (foldMapCtx ctx (fn ((l, t, soi), ctx) => 
                            checkType ctx t CUniverse  >>= (fn (ct, ctx) => 
                            Success ((l, ct), ctx)
                            )
                        ) ltsl) >>= (fn (l, ctx) => Success((CLazyProd l, CUniverse), ctx))
                    | RSum(ltsl, sepl) => 
                         (foldMapCtx ctx (fn ((l, t, soi), ctx) => 
                            checkType ctx t CUniverse  >>= (fn (ct, ctx) => 
                            Success ((l, ct), ctx)
                            )
                        ) ltsl) >>= (fn (l, ctx) => Success((CSum l, CUniverse), ctx))
                    (* | RFunc(t1, t2, soi) => checkType ctx t1 CUniverse >>= (fn ct1 => 
                        checkType ctx t2 CUniverse >>= (fn ct2 => 
                            Success(CFunc(ct1, ct2), CUniverse)
                        )
                    ) *)
                    (* | RTypeInst(t1, t2, soi) => checkType ctx t1 CUniverse >>= (fn (ct1, ctx) => 
                        checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                            Success((CTypeInst(ct1, ct2), CUniverse), ctx)
                        )
                    ) *)
                    (* | RForall(tv, t2, soi) => 
                        withLocalBinder ctx tv CUniverse (fn ctx => 
                        checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                Success((CForall(tv, ct2), CUniverse), ctx)
                            )
                        )
                    | RExists(tv, t2, soi) => 
                        withLocalBinder ctx tv CUniverse (fn ctx => 
                        checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                Success((CExists(tv, ct2), CUniverse), ctx)
                            )
                        ) *)
                    (* | RRho(tv, t2, soi) => 
                        withLocalBinder ctx tv CUniverse (fn ctx => 
                        checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                Success((CRho(tv, ct2), CUniverse), ctx)
                            )
                        ) *)
                    | RBlock (decls, qi) => 
                        (
                            (* blocks are not necessarily modules *)
                            typeCheckSignature ctx decls false [] >>=
                            (fn(checkedSig, ctx) =>
                                (* assume the typeChecking is behaving properly, 
                                no conflicting things will be added to the signature *)
                                (* sub context will be determined by whether the signature is private or not ? *)
                            let 
                            (* val _ = DebugPrint.p ("[tc713] block " ^ PrettyPrint.show_typecheckingRExpr e 
                            ^ " ysnthesized as " ^ PrettyPrint.show_typecheckingCSig checkedSig) *)
                            in

                            Success((CBlock(checkedSig), CUniverse), ctx)(* the type of a block is just a block *)
                            end
                            )
                        )
                    (* | Fix (ev, e)=> Fix (ev, substTypeInExpr tS x e) *)
                    | _ => Errors.expressionDoesNotSupportTypeSynthesis e ctx
                )

                val _ = if DEBUG then print ( "synthesize got result " ^
                PrettyPrint.show_static_error res (fn res => PrettyPrint.show_typecheckingCType (#2 (#1 res)))^
                " whensynthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e ^ "\n") else ()
                val res' = Errors.enrichErrWhenSynthesizing e res
                in res'
                end )
                (* handle TypeCheckingFailure s => 
                    raise TypeCheckingFailure (s ^ "\n when synthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e 
                    ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx) *)

            and checkType (ctx : context) (e : RExpr) (ttUnnorm: CType) (* tt target type *) : (CExpr * context) witherrsoption =
                     weakHeadNormalizeType e ctx ttUnnorm >>= (fn ttNorm =>
                case ttNorm of 
                    CPiType(t1, tvop, t2, Implicit) => (
                                case e of RLam(ev, eb, Implicit, soi) => checkTypeNoInsertLam ctx e ttNorm
                                | _ => ( (* insert lambda here *)
                                let 
                                in
                                    (case tvop of 
                                            NONE => 
                                                checkType ctx e t2 >>= (fn (ce, ctx) => 
                                                Success(CLam(StructureName.binderName(), ce, CTypeAnn ttNorm), ctx))
                                            | SOME tv => withLocalBinder ctx tv t1
                                            (fn ctx => 
                                                checkType ctx e t2 >>= (fn (ce, ctx) => 
                                                Success(CLam(tv, ce, CTypeAnn ttNorm), ctx))
                                            )
                                    )
                                end
                            )
                     )
                    | _ => checkTypeNoInsertLam ctx e ttNorm
                )

            and checkTypeNoInsertLam (ctx : context) (e : RExpr) (ttUnnorm: CType) (* tt target type *) : (CExpr * context) witherrsoption =
                     weakHeadNormalizeType e ctx ttUnnorm >>= (fn ttNorm =>
                (let 
                    val tt = ttNorm
                    val _ = if DEBUG then  print(  "DEBUG: checking the expr " ^ PrettyPrint.show_typecheckingRExpr e ^ 
                        " against type " ^ PrettyPrint.show_typecheckingCType tt ^
                        (* " in context " ^ PrettyPrint.show_typecheckingpassctx ctx ^ *)
                        "\n") else ()
                    val originalExpr = e
                    val res = (
                        case e of
                        RVar v => 
                            (synthesizeType ctx e) >>= (fn ((synthExpr, synthType), ctx) =>
                            tryTypeUnify ctx e (synthType) tt  >>= (fn ctx => Success (synthExpr, ctx))
                            )
                    
                        | RUnitExpr(soi) => tryTypeUnify ctx e CUnitType  tt >>= (fn ctx => Success(CUnitExpr, ctx))
                        | RTuple (l, soi) => (case tt of 
                            CProd ls => if List.length l <> List.length ls
                                        then Errors.prodTupleLengthMismatch e (tt) ctx
                                        else 
                                        let fun go (l,ls) ctx = case (l, ls) of 
                                                ([],[]) => (
                                                        (* DebugPrint.p "GO : DONE"; *)
                                                    Success([], ctx)
                                                )
                                                | ((elem::es), ((tp) :: tpl)) => 
                                                (
                                                    (* DebugPrint.p ("GOING..." ^ Int.toString (length l) ^ " ; " ^ Int.toString (length ls)); *)

                                                    checkType ctx elem tp >>= (fn (ce, ctx) => 
                                                        (
                                                            (* DebugPrint.p ("checked..." ); *)
                                                            go (es, tpl) ctx >>= (fn (ces, ctx) => 
                                                                Success((ce::ces), ctx)
                                                            )
                                                        )
                                                    )
                                                )
                                                | _ => raise Fail "tcp629: should be the same length"
                                        in go (l, ls) ctx >>= (fn (ce, ctx) => Success(CTuple(ce, CTypeAnnNotAvailable), ctx))
                                        end
                            | CLabeledProd ls => if List.length l <> List.length ls
                                        then Errors.prodTupleLengthMismatch e (tt) ctx
                                        else 
                                        let fun go (l,ls) ctx = case (l, ls) of 
                                                ([],[]) => (
                                                        (* DebugPrint.p "GO : DONE"; *)
                                                    Success([], ctx)
                                                )
                                                | ((elem::es), ((label,tp) :: tpl)) => 
                                                (
                                                    (* DebugPrint.p ("GOING..." ^ Int.toString (length l) ^ " ; " ^ Int.toString (length ls)); *)

                                                    checkType ctx elem tp >>= (fn (ce, ctx) => 
                                                        (
                                                            (* DebugPrint.p ("checked..." ); *)
                                                            go (es, substTypeInSpine ce [label] tpl) ctx >>= (fn (ces, ctx) => 
                                                                Success((ce::ces), ctx)
                                                            )
                                                        )
                                                    )
                                                )
                                                | _ => raise Fail "tcp629: should be the same length"
                                        in go (l, ls) ctx >>= (fn (ce, ctx) => Success(CTuple(ce, CTypeAnnNotAvailable), ctx))
                                        end
                            | _ => Errors.expectedProdType e (tt) ctx
                            )
                        | RLazyTuple (l, soi) => (case tt of 
                            CLazyProd ls => if List.length l <> List.length ls
                                        then Errors.lazyProdTupleLengthMismatch e (tt) ctx
                                        else foldMapCtx ctx (fn ((elem, (label,tp)), ctx) => 
                                            checkType ctx elem tp
                                        ) (ListPair.zipEq (l, ls)) >>= (fn (checkedElems, ctx) => 
                                        Success(CLazyTuple ( checkedElems, CTypeAnn((CLazyProd ls))), ctx))
                            | _ => Errors.expectedLazyProdType e (tt) ctx
                            )
                        | RProj(e, (idx, idxsoi), soi) =>
                        synthesizeType ctx (RProj(e, (idx, idxsoi), soi)) >>= (fn ((cproj, synt), ctx) => case (cproj, synt) of
                            (CProj(ce, idx, prodType), synthType) => tryTypeUnify ctx originalExpr synthType tt >>= (fn ctx => 
                            Success(CProj(ce, idx, prodType), ctx))
                            | _ => raise Fail "tcp229")
                        | RLazyProj(e, l, soi) =>
                        synthesizeType ctx (RLazyProj(e, l, soi)) >>= (fn ((cproj, synt), ctx) => case (cproj, synt) of
                            (CLazyProj(ce, l, lazyProdType), synthType) => tryTypeUnify ctx originalExpr synthType tt >>= (fn ctx => 
                            Success(CLazyProj(ce, l, lazyProdType), ctx))
                            | _ => raise Fail "tcp229")

                        (* | RInj (l, e, soi) => (case tt of
                            CSum ls => (lookupLabel ls l) >>= (fn (idx,lookedupType) => 
                                    checkType ctx e lookedupType >>= (fn (checkedExpr, ctx) => 
                                        Success(CInj(l, checkedExpr, CTypeAnn((CSum ls))), ctx)
                                    ))
                            | _ => Errors.expectedSumType originalExpr (tt) ctx
                        ) *)
                        | RIfThenElse(e, tcase, fcase, soi) => (checkType ctx e (CBuiltinType(BIBool))  >>= (fn (ce, ctx) => 
                            checkType ctx tcase tt >>= (fn (ctcase, ctx) => 
                                checkType ctx fcase tt >>= (fn (cfcase, ctx) => 
                                    Success(CIfThenElse(ce, ctcase, cfcase), ctx)
                                )
                            )
                        ))
                        | RCase(e,cases, soi) => (synthesizeType ctx e) >>= (fn ((ce, synt), ctx) => 
                            weakHeadNormalizeType e ctx synt >>= (fn caseTpNormalized => 
                                (foldMapCtx ctx (fn ((pat, e), ctx) => 
                                    checkPattern ctx pat caseTpNormalized NONE (fn (cpat, newCtx) => 
                                            checkType newCtx e tt >>= (fn (checkedCase, ctx) => 
                                                Success((cpat, checkedCase), ctx)
                                            )
                                        )
                                    ) cases
                                    ) >>= (fn (checkedCases, ctx)
                                        => Success(CCase((CTypeAnn(caseTpNormalized), ce), checkedCases , CTypeAnn((tt))), ctx))
                            ))
                                (* | _ => Errors.attemptToCaseNonSum originalExpr (#2 synt) ctx) *)
                        | RLam(ev, eb,pe, soi) => 
                        weakHeadNormalizeType originalExpr ctx tt >>= (fn ntt => 
                            (case tt of
                                CPiType(t1, tevop, t2, pt) => 
                                    if pt <> pe 
                                    then raise Fail "tcp638: should have inserted implict lams"
                                    else
                                    withLocalBinder ctx ev t1 (fn ctx => 
                                        checkType 
                                            ctx
                                            eb 
                                            (case tevop of NONE => t2 | SOME tev => if UTF8String.semanticEqual tev ev then t2 else 
                                                substTypeInCExpr (CVar([ev], CVTBinder)) ([tev]) t2
                                            )
                                        >>= (fn (checkedExpr, ctx) => Success(CLam(ev, checkedExpr, CTypeAnn(tt)), ctx))
                                    )
                                | _ => Errors.expectedFunctionType e (tt) ctx
                                )
                        )
                        | RLamWithType (t, ev, eb, soi) => 
                        weakHeadNormalizeType originalExpr ctx tt >>= (fn ntt => 
                        (case ntt of
                            CPiType(t1, tevop, t2, Explicit) => (
                                checkExprIsType ctx t >>= (fn (t', ctx) => 
                                    tryTypeUnify ctx e t' t1 >>=
                                    (fn ctx => 
                                        withLocalBinder ctx ev t1 (fn ctx => 
                                            checkType 
                                                ctx
                                                eb 
                                                (case tevop of NONE => t2 | SOME tev => if UTF8String.semanticEqual tev ev then t2 else 
                                                    substTypeInCExpr (CVar([ev], CVTBinder)) ([tev]) t2
                                                )
                                            >>= (fn (checkedBody, ctx) => 
                                                Success(CLam(ev, checkedBody , CTypeAnn(tt)), ctx))
                                        )
                                    )
                                )
                                )
                            | _ => Errors.expectedFunctionType e  (tt) ctx
                            )
                        )
                        | RSeqComp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn ((ce1, t1), ctx) => 
                            checkType ctx e2 tt >>= (fn (ce2, ctx) => 
                                Success(CSeqComp(ce1, ce2, CTypeAnn(t1), CTypeAnn(tt)), ctx)
                            ))
                        | RApp (e1, e2, pe, soi) => 
                            (case pe of Explicit => 
                            synthesizeType ctx e1 
                            | Implicit => synthesizeTypeNoInstMeta ctx e1)
                            >>= (fn ((ce1, synt), ctx) => 
                            weakHeadNormalizeType e1 ctx synt >>= (fn nsynt => 
                                case synt 
                                    of (CPiType (t1, evop, t2, pt)) => 
                                        if pt = pe then
                                        ( 
                                                checkType ctx e2 (t1) >>= (fn (checkedArg, ctx) => 
                                        tryTypeUnify ctx e (
                                            case evop of 
                                            NONE => t2
                                            | SOME ev => substTypeInCExpr checkedArg ([ev]) t2
                                            ) tt >>= (fn ctx => 
                                                    Success (CApp(ce1, checkedArg, CTypeAnn(synt)), ctx)
                                                )
                                            )
                                        ) else 
                                        raise Fail "tcp638: plicity of (possible) pi type should match requested plicity"
                                    | _ => Errors.attemptToApplyNonFunction e (synt) ctx
                                )
                            )
                        (* | RTAbs (tv, e2, soi) => (case tt of
                            CForall (tv', tb) => 
                                    checkType 
                                    (addToCtxA (TermTypeJ([tv], CUniverse, JTLocalBinder, NONE)) ctx )
                                    e2 (substTypeInCExpr (CVar([tv], CVTBinder)) [tv'] tb) >>= (fn (ce2, ctx) => 
                                                Success(CTAbs (tv, ce2, CTypeAnn(tt)), ctx)
                                    )
                            | _ => Errors.expectedUniversalType e (tt) ctx
                        ) *)
                        (* | RTApp (e2, t, soi) => synthesizeType ctx e2  >>= (fn ((ce2, synt), ctx) => 
                        weakHeadNormalizeType e2 ctx synt >>= (fn synt => 
                            case synt of
                                (CForall (tv, tb)) => (
                                    checkExprIsType ctx t >>= (fn (ctapp, ctx) => 
                                        (* need to normalize type! important! *)
                                        (weakHeadNormalizeType t ctx ctapp >>= (fn nt => 
                                            tryTypeUnify ctx e (tt) (substTypeInCExpr ctapp [tv] ( tb))
                                        )) >>= (fn ctx => Success(CTApp(ce2, ctapp, CTypeAnn(CForall(tv, tb))), ctx)))
                                    )
                                | _ => Errors.attemptToApplyNonUniversal e (synt) ctx
                                )
                            ) *)
                        (* | RPack (e1, e2, soi) => (case tt of
                            CSigmaType (t1, tvop, t2) => 
                                checkType ctx e1 t1 >>= (fn (ce1, ctx) =>
                                    checkType ctx e2 
                                    (case tvop of NONE => t2 | SOME tv => 
                                                    substTypeInCExpr ce1 ([tv]) t2
                                                )
                                    >>= (fn (ce2, ctx) => Success(CPack(ce1, ce2, CTypeAnn(tt)), ctx))
                                    )
                            | _ => Errors.expectedExistentialType e (tt) ctx
                        ) *)
                        (* | ROpen (e1, (tv, ev, e2), soi) => synthesizeType ctx e1 >>= (fn ((ce1, synt), ctx) => 
                        weakHeadNormalizeType e1 ctx synt >>= (fn nsynt => case nsynt of
                            (CExists (tv', tb)) => 
                            checkType (addToCtxA (TermTypeJ([ev], substTypeInCExpr (CVar([tv], CVTBinder)) [tv'] ( tb), JTLocalBinder, NONE)) ctx) e2 tt
                            >>= (fn (ce2, ctx) => 
                            Success(COpen((CTypeAnn(CExists (tv', tb)), ce1), (tv, ev, ce2), CTypeAnn(tt)), ctx)
                            )
                            | _ => Errors.attemptToOpenNonExistentialTypes e (synt) ctx
                        )
                        ) *)
                        (* | RFold (e2, soi) => (case tt
                            of 
                            CRho (tv ,tb) => 
                            checkType ctx e2 (substTypeInCExpr (CRho(tv, tb)) [tv] tb)
                            >>= (fn (ce2, ctx) => Success (CFold(ce2, CTypeAnn(tt)), ctx))
                            | _ => Errors.expectedRecursiveType e (tt) ctx
                                ) *)
                        (* | RUnfold (e2,soi) => synthesizeType ctx e2  >>= (fn ((ce2, synt), ctx) => 
                        weakHeadNormalizeType e2 ctx synt >>= (fn nsynt => case nsynt of
                            ( CRho (tv, tb)) =>(
                                tryTypeUnify ctx e ((substTypeInCExpr (CRho (tv,  tb)) [tv] ( tb))) tt >>= (fn ctx =>
                                Success(CUnfold(ce2, CTypeAnn(CRho(tv,tb))), ctx)))
                            | _ => Errors.attemptToUnfoldNonRecursiveTypes e (synt) ctx
                            )) *)
                        | RFix (ev, e, soi)=> 
                            withLocalBinder ctx ev tt (fn ctx => 
                                checkType ctx e tt
                                                    >>= (fn (ce, ctx) => Success(CFix(ev,ce, CTypeAnn(tt)), ctx))
                            )
                        | RStringLiteral (s, soi) => (tryTypeUnify ctx e (CBuiltinType(BIString)) (tt) >>= (fn (ctx) => Success (CStringLiteral s, ctx)))
                        | RIntConstant (i, soi) => (tryTypeUnify ctx e (CBuiltinType(BIInt)) tt >>= (fn ctx => Success ( CIntConstant i, ctx)))
                        | RRealConstant (r, soi) => (tryTypeUnify ctx e (CBuiltinType(BIReal)) tt >>= (fn ctx => Success (CRealConstant r, ctx)))
                        | RBoolConstant (r, soi) => (tryTypeUnify ctx e (CBuiltinType(BIBool)) tt >>= (fn ctx => Success (CBoolConstant r, ctx)))
                        | RFfiCCall (e1, e2, soi) => (
                            case e1 of
                                RStringLiteral (cfuncName, soi) => 
                                    let fun elaborateArguments  (args : RExpr list ) : (CExpr * context) witherrsoption = 
                                        foldMapCtx ctx (fn (a, ctx) =>  (synthesizeType ctx a)) args >>= 
                                        (fn (al, ctx) => 
                                            Success(CFfiCCall(cfuncName, map (#1) al), ctx)
                                        )
                                    in
                                                (case e2 of 
                                                    RVar v => (Success ([RVar v])) >>= elaborateArguments
                                                    | RTuple (l, soi) => (collectAll (map (fn arg => case arg of 
                                                        RVar v => Success (RVar v)
                                                        | _ => Errors.ccallArgumentsMustBeImmediate arg ctx
                                                        (* raise TypeCheckingFailure "ccall arguments must be immediate values" *)
                                                        ) l)) >>= elaborateArguments
                                                    | RUnitExpr(soi) => elaborateArguments []
                                                    | e => raise Fail ("tcp439 : " ^ PrettyPrint.show_typecheckingRExpr e)
                                                )
                                    end
                                | _ => Errors.firstArgumentOfCCallMustBeStringLiteral e1 ctx
                                (* raise TypeCheckingFailure "First argument of the ccall must be string literal" *)

                        )
                        | RLetIn(decls, e, soi) => (
                        typeCheckSignature ctx decls true []
                            >>= (fn(csig, ctx) =>
                            (* the context returned will be devoid of csig, so we add them via a fake local open *)
                                nextContextOfOpenStructure (reconstructFromRExpr e) ctx csig (fn ctx => 
                                        checkType ctx e tt >>= (fn (ce, ctx) => 
                                            Success(CLetIn(csig, ce, CTypeAnn(tt)), ctx)
                                        )
                                )
                            )
                        )
                        | RBuiltinFunc(f, soi) => (tryTypeUnify ctx e ((BuiltinFunctions.typeOf f)) tt >>= (fn ctx => Success(CBuiltinFunc(f), ctx)))
                        (* types *)
                        | RUnitType(s) => tryTypeUnify ctx e CUniverse tt >>= (fn ctx => Success(CUnitType, ctx))
                        | RNullType(s) => tryTypeUnify ctx e CUniverse tt >>= (fn ctx => Success(CNullType , ctx))
                        | RBuiltinType(f, s) => tryTypeUnify ctx e CUniverse tt >>= (fn ctx => Success(CBuiltinType(f) , ctx))
                        | RUniverse(s) => tryTypeUnify ctx e CUniverse tt >>= (fn ctx => Success(CUniverse , ctx) (* TODO: maybe universe levels? *))
                        | RPiType(t1, evoption, t2,p, soi) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            checkExprOptionIsType ctx t1 ((case evoption of SOME x => RVar([x]) | NONE => e)) >>= (fn (ct1, ctx) => 
                                (case evoption of  NONE => (fn f => f ctx) | SOME(n) => withLocalBinder ctx n ct1)
                                (fn ctx =>
                                checkType ctx t2 CUniverse >>= (fn ((ct2), ctx) => 
                                        (* tryTypeUnify ctx t2 synT CUniverse >>= (fn ctx =>  *)
                                            (Success(CPiType(ct1, evoption, ct2,p), ctx))
                                            (* ) *)
                                ))
                            ))
                        | RSigmaType(t1, evoption, t2, soi) =>
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            checkType ctx t1 CUniverse >>= (fn (ct1, ctx) => 
                                (case evoption of  NONE => (fn f => f ctx) | SOME(n) => withLocalBinder ctx n ct1)
                                (fn ctx => 
                                synthesizeType ctx t2 >>= (fn ((ct2, synT), ctx) => 
                                        tryTypeUnify ctx t2 synT CUniverse >>= 
                                            (fn ctx => Success(CSigmaType(ct1, evoption, ct2) , ctx))
                                    ))
                                ))
                        | RProd(ltsl, sepl) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            (foldMapCtx ctx (fn ((t), ctx) => 
                                checkType ctx t CUniverse  >>= (fn (ct, ctx) => 
                                Success ((ct), ctx)
                                )) ltsl)
                             >>= (fn (l, ctx) => Success(CProd l, ctx)))
                        (* | RProd(ltsl, sepl) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            checkSpineIsType ctx (map (fn (l, t, soi) => (l, t)) ltsl)
                             >>= (fn (l, ctx) => Success(CProd l, ctx))) *)
                        | RLazyProd  (ltsl, sepl) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            (foldMapCtx ctx (fn ((l, t, soi), ctx) => 
                                checkType ctx t CUniverse  >>= (fn (ct, ctx) => 
                                Success ((l, ct), ctx)
                                )
                            ) ltsl) >>= (fn (l, ctx) => Success(CLazyProd l, ctx)))
                        | RSum(ltsl, sepl) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            (foldMapCtx ctx (fn ((l, t, soi), ctx) => 
                                checkType ctx t CUniverse  >>= (fn (ct, ctx) => 
                                Success ((l, ct), ctx)
                                )
                            ) ltsl) >>= (fn (l, ctx) => Success(CSum l, ctx)))
                        (* | RFunc(t1, t2, soi) => 
                            tryTypeUnify ctx e CUniverse tt >> (
                        checkType ctx t1 CUniverse >>= (fn ct1 => 
                            checkType ctx t2 CUniverse >>= (fn ct2 => 
                                Success(CFunc(ct1, ct2) )
                            )
                        )) *)
                        (* | RTypeInst(t1, t2, soi) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                                checkType ctx t1 CUniverse >>= (fn (ct1, ctx) => 
                                    checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                        Success(CTypeInst(ct1, ct2) , ctx)
                                    )
                                )) *)
                        (* | RForall(tv, t2, soi) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            withLocalBinder ctx tv CUniverse (fn ctx => 
                            checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                    Success(CForall(tv, ct2) , ctx)
                                ))) *)
                        (* | RExists(tv, t2, soi) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            withLocalBinder ctx tv CUniverse (fn ctx => 
                            checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                    Success(CExists(tv, ct2) , ctx)
                                ))) *)
                        (* | RRho(tv, t2, soi) => 
                            tryTypeUnify ctx e CUniverse tt >>= (fn ctx => 
                            withLocalBinder ctx tv CUniverse (fn ctx => 
                            checkType ctx t2 CUniverse >>= (fn (ct2, ctx) => 
                                    Success(CRho(tv, ct2) , ctx)
                                ))) *)
                        | RPairOfQuotes((ql, qr)) => 
                        addNewMetaVar ctx tt [ql, qr]
                        | RBlock(_) => 
                        (
                            synthesizeType ctx e >>= (fn ((ce, _), ctx) => 
                                case ce of 
                                CBlock(decl) => (case tt of 
                                        CUniverse => 
                                            (checkBlockIsSignature e ctx decl >> Success(ce, ctx))
                                        | CBlock(tt) => 
                                            (checkSignatureAscription e ctx decl tt >>= (fn ctx =>  Success(ce, ctx)))
                                        | _ => genSingletonError (reconstructFromRExpr e) "模块（结构）仅能拥有模块类型或者元类型" NONE
                                    )
                                | _ => raise Fail "tcp1027: synth block should always return a block"
                            )
                        )
                        | _ => genSingletonError (reconstructFromRExpr e) ("check type failed on " ^ PrettyPrint.show_typecheckingRType e 
                        ^ " <= " ^ PrettyPrint.show_typecheckingCType tt) NONE
                    )
                    val res' = Errors.enrichErrWhenChecking e ttUnnorm res
                in res'
                end 
        ))
                    (* handle TypeCheckingFailure s =>
                    raise TypeCheckingFailure (s ^ "\n when checking the expr " ^ PrettyPrint.show_typecheckingRExpr e ^ 
                        " against type " ^ PrettyPrint.show_typecheckingType tt
                    ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx
                    ) *)
                    (* type check signature will return all bindings *)
                    (* setting isModule to true will check that all declarations are defined.
                    should only be set for a compilation unit *)
            and typeCheckSignature(ctx : context) (s : RSignature) (isModule : bool) (acc : CSignature) : (CSignature * context) witherrsoption =

                (


                    if DEBUG then DebugPrint.p ("DEBUG952 " ^ PrettyPrint.show_typecheckingRSig s ^
                    " in context " ^ PrettyPrint.show_typecheckingpassctx ctx ^"\n") else (); 
                    (* if true then DebugPrint.p ("DEBUG1166 " ^ PrettyPrint.show_typecheckingCSig acc 
                    ) else ();  *)
                    (* DebugPrint.p ("TCSig r="  ^ Int.toString (length s) ^   " acc=" ^ Int.toString (length acc) ^
                    "\nDEBUG " ^ PrettyPrint.show_typecheckingRSig s ^
                    "\n"); *)
                    (* DebugPrint.p "TCSig..."; *)

                    case s of
                    [] => 
                resolveAllMetaVarsInCSig ctx acc >>= (fn acc => 
                    (
                        if isModule 
                        then checkBlockIsModule ctx acc >> Success(acc, ctx)
                        else Success(acc, ctx)
                    )
                )
                | RTermTypeJudgment(n, t):: ss => 
                let 
                (* val freeTVars = freeTCVar  (rTypeToCType ctx t) *)
                (* (applyContextToType ctx (rTypeToCType ctx t))  *)
                in  (* do not check for free variables, as it will be catched in a later stage? *)
                    checkExprIsType ctx t >>= (fn (ct, ctx) =>
                        weakHeadNormalizeType t ctx ct
                        >>= (fn normalizedType => 
                        withLocalGeneric ctx [n] normalizedType (JTPending) (fn ctx => 
                            typeCheckSignature ( ctx) ss isModule (acc@[CPureDeclaration(n, normalizedType)]))
                        )
                    )
                end
                | RTermDefinition(n, e) :: ss => 
                let fun newDef() = 
                    (case e of 
                    RVar _ => synthesizeTypeNoInstMeta ctx e
                    | _ => synthesizeType ctx (e))
                     >>= (fn ((transformedExpr , synthesizedType), ctx)  =>
                            withLocalGeneric ctx [n] synthesizedType (JTDefinition transformedExpr) (fn ctx => 
                                typeCheckSignature 
                                    ctx ss  isModule
                                    (acc@[CTermDefinition(n, transformedExpr, synthesizedType)])
                            )
                        ) 
                in
                (case findSigList n acc "1285" of   
                    NONE  =>  newDef()
                    | SOME(CPureDeclaration(_, lookedUpType), _) => 
                        (
                         (* case lookedUpDef of 
                         JTPending =>   *)
                            let val transformedExprOrFailure = checkType ctx (e) lookedUpType
                            in 
                            case transformedExprOrFailure of
                            Success(transformedExpr, ctx) => 
                                withLocalGeneric ctx [n] lookedUpType (JTDefinition transformedExpr) (fn ctx => 
                                    let val acc = (modifySigList n (fn _ => CTermDefinition(n, transformedExpr, lookedUpType)) acc)
                                    in
                                        typeCheckSignature ctx ss  isModule acc
                                    end
                                )
                            | DErrors(l) => (case typeCheckSignature ctx ss isModule (acc) of 
                                        Success _ => DErrors(l)
                                        | DErrors l2 => DErrors(l @l2)
                                        | _ => raise Fail "tcp458"
                                )
                            | _ => raise Fail "tcp457"
                            end
                        (* | _ => newDef() allow repeated definitions *)
                        (* Errors.redefinitionError n (StructureName.toStringPlain cname) ctx (List.last cname)  *)
                            )
                    | SOME(_) => (Errors.redefinitionError n (UTF8String.toString n) ctx n)
                )

                end
                | RConstructorDecl(name, rtp) :: ss => 
                    checkConstructorType name ctx rtp >>= (fn ((checkedType, cconsinfo),ctx) => 
                        withLocalGeneric ctx [name] checkedType (JTConstructor cconsinfo) (fn ctx => 
                            typeCheckSignature ctx ss isModule (acc@[CConstructorDecl(name, checkedType, cconsinfo)])
                        )
                    )
                (* | RStructure (vis, sName, decls) :: ss => 
                (case ctx of 
                Context(curName, curVis, bindings) => 
                    typeCheckSignature (Context(curName@[sName], vis, bindings)) decls [] >>=
                    (fn(checkedSig, Context(_, _, newBindings)) =>
                        (* assume the typeChecking is behaving properly, 
                        no conflicting things will be added to the signature *)
                        (* sub context will be determined by whether the signature is private or not ? *)
                    typeCheckSignature (Context(curName, curVis, newBindings)) ss (acc@checkedSig)
                    )
                    
                ) *)
                | ROpenStructure openName :: ss =>
                lookupCtx ctx openName  >>= (fn (_, tp, jt) => 
                case jt of 
                    JTDefinition(block) => 
                        (weakHeadNormalizeType (RVar openName) ctx block >>= (fn x => 
                            case x of
                                CBlock(csig) =>
                                    nextContextOfOpenStructure (StructureName.toString openName) ctx csig (fn nextContext => 
                                        typeCheckSignature nextContext ss isModule (acc @ [COpenStructure(openName, csig)])
                                    )
                                | _ => Errors.genericErrorStr (StructureName.toString openName) ctx ("名称不是模块，而是" ^ PrettyPrint.show_typecheckingCExpr tp 
                                ^ " 声明类型为 " ^  PrettyPrint.show_typecheckingjt jt )
                        ))
                    | _ => Errors.genericErrorStr (StructureName.toString openName) ctx ("名称不是模块，而是" ^ PrettyPrint.show_typecheckingCExpr tp 
                    ^ " 声明类型为 " ^  PrettyPrint.show_typecheckingjt jt )
                )
                (* (case ctx of 
                Context(curName, curVis, bindings) => 
                    let val nextContext = nextContextOfOpenStructure curName curVis bindings openName
                        (* assume the typeChecking is behaving properly, 
                        no conflicting things will be added to the signature *)
                        (* sub context will be determined by whether the signature is private or not ? *)
                    in typeCheckSignature nextContext ss (acc)
                    end
                ) *)
                | RReExportStructure reExportName :: ss =>
                        ((reExportDecls ctx reExportName) <?> ( fn _ =>
                            typeCheckSignature ctx ss isModule (acc) (* we collect remaining possible failures *)
                        )) >>= (fn newBindings => 
                                            typeCheckSignature ctx ss isModule (acc@newBindings)
                        )                
                        (* note that the order of <?> and >>= is important as >>= won't ignore previous error 
                        reverse would have exponential wasted computation *)
                | RImportStructure(importName, path) :: ss => 
                    (getTypeCheckedAST (path, importName)
                    <?> (fn _ => Errors.importError (StructureName.toString importName)  ctx)
                    )
                     >>= (fn csig => 
                     (* assume what we get is a type checked module, not its signature 
                     TODO: it makes sense for module to store both its signature and implementation 
                     as result of type checking *)
                        (withLocalGeneric ctx importName 
                        (CBlock (getSingatureForModule csig)) 
                        (JTDefinition (CBlock csig)) (
                            fn ctx => 
                        typeCheckSignature ctx ss isModule (acc@[CImport(importName, path)])
                        ))
                        (* (addToCtxAL (List.concat (List.mapPartial (fn x => case x of 
                            (* CTypeMacro(sname, t) => SOME(TypeDef(sname, t, ())) *)
                             CTermDefinition(sname, e, t) => SOME([TermTypeJ(importName@[sname], t, JTDefinition(e), NONE)
                                ])
                            | CDirectExpr _ => NONE
                            | CImport _ => NONE
                            | CConstructorDecl(sname, t, consinfo) => SOME([TermTypeJ(importName@[sname], t, JTConstructor consinfo, NONE)
                                ])
                            | CPureDeclaration (name, tp) => SOME([TermTypeJ(importName@[name], tp, JTPending (* TODO: do import *), NONE)
                                ])
                            ) csig)) ctx) *)
                    )
                | RDirectExpr e :: ss=> 
                    let 
                    val synthedExprOrFailure = (synthesizeType ctx (e))
                    in case synthedExprOrFailure of 
                        Success((checkedExpr, synthesizedType), ctx) => typeCheckSignature ctx ss isModule (acc@[CDirectExpr(checkedExpr, synthesizedType)])
                        | DErrors l => (case typeCheckSignature ctx ss isModule (acc) of
                                        Success _ => DErrors l
                                        | DErrors l2 => DErrors (l @ l2)
                                        | _ => raise Fail "tcp 492"
                        )
                        | _ => raise Fail "tcp494"
                    end
                )
                (* handle SignatureCheckingFailure st =>
                raise TypeCheckingFailure (st ^ "\n when checking the signature " ^ PrettyPrint.show_typecheckingRSig s 
                    ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx
                    ) *)
    in 
        fn s => 
            (typeCheckSignature 
            (Context (topLevelStructureName, true, 
                    []))
            s true []) >>= (fn (csig, ctx) => 
                (* resolveAllMetaVarsInCSig ctx csig >>= (fn csig =>  *)
                (
                    (* DebugPrint.p ("DEBUG1070: " ^ PrettyPrint.show_typecheckingCSig csig) ;  *)
                    Success(csig))
                (* ) *)
            )
                (* val _ = DebugPrint.p "Type checked top level\n"
                val _ = DebugPrint.p (PrettyPrint.show_typecheckingCSig res) *)
    end
end
