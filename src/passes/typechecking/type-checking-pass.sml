structure TypeCheckingPass = struct
open TypeCheckingAST
open TypeCheckingASTOps
open StaticErrorStructure
infix 5 >>= 
infix 5 >> 
infix 6 =/=
infix 5 <?>

    (* val DEBUG = true *)
    val DEBUG = false


    (*  !!! we assume the context is well formed in the sense that 
    all term type judgments have no free type variables !!! *) 
    (* so before anything is added to context, must perform substitution first ! *)
    (* the context is not a telescope !!! *)
    (* This also applies to type definitions! They must be expanded as well! *)
    (* ^^^ THis is not true !!! *)
    (* this is called the closed-world assumption and in practice, this 
    helps to reduce bugs during type checking *)
    (* also assume no name clash  *)
           
   
                    (* curStructure, curVisibility and mapping *)
  val addToCtxA = appendAbsoluteMappingToCurrentContext (* A for relative *)
  val addToCtxR = appendRelativeMappingToCurrentContext (* R for relative *)
    val addToCtxAL = appendAbsoluteMappingsToCurrentContext (* L for list *)
    val addToCtxRL = appendRelativeMappingsToCurrentContext (* L for list *)
    

 
    fun getMapping (c: context ):mapping list = 
        case c of  (Context(cSname, cVis, m)) => m

    fun showctx x = SOME(case x of 
    Context(curSName, curVis, m) =>  
    "当前结构名：" ^ StructureName.toStringPlain curSName ^
    "\n当前已定义的值及其类型：\n"  ^
            String.concatWith "；\n" (map (fn x => case x of
    TermTypeJ(e, t,_) => StructureName.toStringPlain e ^ " ，其类型为 " ^ PrettyPrint.show_typecheckingType t
    | TypeDef(s, t, _) => StructureName.toStringPlain s ^ " ，其为 " ^ PrettyPrint.show_typecheckingType t) m) ^ "\n"
          )

(* require lookup to add name qualification if references local structure *)
    fun lookup (Context(curSName, v, ctx) : context) (n : StructureName.t) : (StructureName.t * Type) witherrsoption= 
        let exception LookupNotFound
            fun lookupMapping (ctx : mapping list) (n : StructureName.t) (curSName : StructureName.t ): (StructureName.t * Type) witherrsoption= 
                case ctx of 
                (* WARNING: toUTF8String discards the separator information, but I guess it is fine because 
                    as long as all components are of the same name, we're fine*)
                    [] => raise LookupNotFound
                    (* ("name " ^ StructureName.toStringPlain n ^ " not found in context") *)
                    | TermTypeJ(n1, t1, u)::cs => 
                        (case StructureName.checkRefersTo n1 n curSName 
                        of SOME(cname) => Success (cname, t1)
                        | NONE => lookupMapping cs n curSName
                        )
                    | TypeDef(_) :: cs => lookupMapping cs n curSName
            val ntp = lookupMapping ctx n curSName 
                handle LookupNotFound =>
                genSingletonError (StructureName.toString n) ("名称`" ^ StructureName.toStringPlain n ^ "`未找到") (showctx (Context(curSName, v, ctx)))
        in 
            ntp 
        end
        (* handle LookupNotFound s1 => 
            (* (let val tp = lookupMapping ctx (curSName@n)
             (* try both absolute and relative path *)
             in (curSName@n, tp) end *)
            (* handle LookupNotFound s2 =>  *)
            raise TypeCheckingFailure (s1 ^ ", \n " ) *)
            (* ) *)
        
    fun nextContextOfOpenStructure  (curName : StructureName.t) (curVis : bool) (bindings : 'a gmapping list) 
    (openName : StructureName.t)=

     Context(curName, curVis, 
            (* extract all bindings from bindings in order and put them into the current context *)
                    List.mapPartial (fn x => 
                    case x of TermTypeJ(name, t, u) => 
                    if StructureName.isPrefix (curName@openName) name   (* relative path *)
                    then SOME (TermTypeJ(curName@(StructureName.stripPrefix (curName@openName) name), t, u))
                    else 
                    if StructureName.isPrefix openName name   (* absolute path *)
                    then SOME (TermTypeJ((StructureName.stripPrefix openName name), t, u))
                    else  NONE
                    | TypeDef(name, t, u) =>
                    if StructureName.isPrefix (curName@openName) name (* relative path *)
                    then SOME (TypeDef(curName@(StructureName.stripPrefix (curName@openName) name), t, u))
                    else 
                    if StructureName.isPrefix (openName) name (* absolute path *)
                    then SOME (TypeDef((StructureName.stripPrefix openName name), t, u))
                    else NONE   
                    ) bindings @ bindings
                )

        

    fun applyContextTo (ctx : context) (subst : Type -> StructureName.t -> 'a -> 'a) (t : 'a) : 'a = 
    (
        (* print ("apply ctx to gen called"  ^ Int.toString(case ctx of (Context(_, _, l)) => length l)^ "\n") ; *)
        case ctx of Context(curName, curVis, mapl) =>
        (case mapl of
            [] => t
            | TypeDef(n1, t1, u)::cs => (
                let val stepOne = (subst t1 n1 t)
                    val stepTwo = (subst t1 (StructureName.stripPrefixOnAgreedParts curName n1) stepOne)
                    val rest = applyContextTo (Context(curName, curVis, cs)) subst stepTwo
                    in rest end
                )
                (* print "HHHH"; *)
            (* the current subsituting name is a prefix! we need also to perform local subsitution *)
            (* always eagerly perform prefix-stripped substitutions *)
            | TermTypeJ(_) :: cs => applyContextTo (Context(curName, curVis, cs)) subst t)
            (* to get the semantics correct, context need to be applied in reverse order *)
            (* no reverse function is called because context is in reverse order *)
    )
    fun applyContextToType (ctx : context) (t : Type) : Type = 
    (
        (* print "apply ctx to type called\n"; *)
        applyContextTo ctx (fn t => fn  l => fn t1 => 
        (let 
        val _ = 
        if DEBUG then  print (" apply context subsituting "^ PrettyPrint.show_typecheckingType t  
        ^ " for " ^ StructureName.toStringPlain l ^ " in " ^ PrettyPrint.show_typecheckingType t1 ^  "\n") else ()
            val res = substTypeInType t l t1
            val _ =if DEBUG then print (" res is " ^ PrettyPrint.show_typecheckingType res ^ "\n") else ()
            in res end
            )) t
    )
    fun applyContextToExpr (ctx : context) (e : RExpr) : RExpr = 
        applyContextTo ctx (fn t => fn  l => fn e1 => 
        (let 
        val _ = 
        if DEBUG then  print (" apply context subsituting "^ PrettyPrint.show_typecheckingType t  
        ^ " for " ^ StructureName.toStringPlain l ^ " in " ^ PrettyPrint.show_typecheckingRExpr e1 ^  "\n") else ()
            val res = substTypeInRExpr t l e1
            val _ =if DEBUG then print (" res is " ^ PrettyPrint.show_typecheckingRExpr res ^ "\n") else ()
            in res end
            )) e
    fun applyContextToSignature (ctx : context) (s : RSignature) : RSignature = 
        applyContextTo ctx (substituteTypeInRSignature) s


    fun lookupLabel ( ctx : (Label * Type) list) (l : Label) : Type witherrsoption = 
        case ctx of 
            [] =>  genSingletonError l ("标签`" ^ UTF8String.toString l ^ "`未找到") NONE
            (* raise TypeCheckingFailure ("label " ^ UTF8String.toString l ^ " not found in prod type") *)
            | (n1, t1)::cs => if UTF8String.semanticEqual n1 l then Success t1 else lookupLabel cs l

      fun lookupLabel3 ( ctx : (Label * EVar *Type) list) (l : Label) : Type witherrsoption = 
        case ctx of 
            [] => genSingletonError l ("标签`" ^ UTF8String.toString l ^ "`未找到") NONE
            (* raise TypeCheckingFailure ("label " ^ UTF8String.toString l ^ " not found in sum type") *)
            | (n1, _, t1)::cs => if UTF8String.semanticEqual n1 l then Success t1 else lookupLabel3 cs l



    fun typeUnify (e : RExpr) (a : Type list) : Type witherrsoption =
        case a of
            [] => raise Fail ("INternal error: empty sum")
            | [t] => Success t
            | (x::y :: xs) => if typeEquiv []  x y then typeUnify e (x :: xs)
            else genSingletonError (reconstructFromRExpr e) "类型不相等"  (SOME 
                ("第一类型：" ^  (PrettyPrint.show_typecheckingType x) 
                ^ "\n第二类型：" ^  (PrettyPrint.show_typecheckingType x)
            ))
            (* raise TypeCheckingFailure ("Type unify failed") *)
    
    structure Errors = struct 
        fun attemptToProjectNonProd e = genSingletonError (reconstructFromRExpr e) ("试图从非乘积类型中投射(attempt to project out of product type)") NONE
        fun attemptToCaseNonSum e = genSingletonError (reconstructFromRExpr e) ("试图对非总和类型进行分析(attempt to case on non-sum types)") NONE
        fun attemptToApplyNonFunction e = genSingletonError (reconstructFromRExpr e) ("试图使用非函数(attempt to apply on nonfunction types)") NONE
        fun attemptToApplyNonUniversal e = genSingletonError (reconstructFromRExpr e) ("试图使用非通用类型(attempt to apply on nonuniversal types)") NONE
        fun openTypeCannotExitScope e = genSingletonError (reconstructFromRExpr e) ("‘打开’的类型不能退出作用域(open's type cannot exit scope)") NONE
        fun attemptToOpenNonExistentialTypes e = genSingletonError (reconstructFromRExpr e) "试图打开非存在类型(attempt to open non existential types)" NONE
        fun attemptToUnfoldNonRecursiveTypes e = genSingletonError (reconstructFromRExpr e) "试图展开非递归类型(attempt to unfold non recursive type)" NONE
        fun expressionDoesNotSupportTypeSynthesis e = genSingletonError (reconstructFromRExpr e) "表达式不支持类型合成，请指定类型" NONE
        fun prodTupleLengthMismatch e = genSingletonError (reconstructFromRExpr e) "数组长度与类型不匹配(prod tuple length mismatch)" NONE
        fun expectedProdType e = genSingletonError (reconstructFromRExpr e) "期待的类型是乘积类型(expected prod)" NONE
        fun expectedSumType e = genSingletonError (reconstructFromRExpr e) "期待总和类型(expected sum types)" NONE
        fun expectedFunctionType e = genSingletonError (reconstructFromRExpr e) "期待函数类型(expected sum types)" NONE
        fun expectedExistentialType e = genSingletonError (reconstructFromRExpr e) "期待通用类型(expected function types)" NONE
        fun expectedUniversalType e = genSingletonError (reconstructFromRExpr e) "期待存在类型(expected universal types)" NONE
        fun expectedRecursiveType e = genSingletonError (reconstructFromRExpr e) "期待递归类型(expected existential types)" NONE
        fun firstArgumentOfCCallMustBeStringLiteral e = genSingletonError (reconstructFromRExpr e) "C调用的第一个参数必须是字符串(first argument of ccall must be a string literal)" NONE
        fun ccallArgumentsMustBeImmediate e = genSingletonError (reconstructFromRExpr e) "C调用的参数必须是直接值(arguments of ccall must be immediate)" NONE
        fun typeDeclContainsFreeVariables s = genSingletonError s "类型声明不可以包含未定义的类型(type decl cannot contain free variables)" NONE
        fun termTypeDeclContainsFreeVariables s = genSingletonError s "值类型声明不可以包含未定义的类型(type decl cannot contain free variables)" NONE
    end


    fun configureAndTypeCheckSignature(
        getTypeCheckedAST:  (FileResourceURI.t * StructureName.t) -> TypeCheckingAST.CSignature witherrsoption
    )
    :  RSignature -> CSignature witherrsoption =
    let

            fun synthesizeType (ctx : context)(e : RExpr) : (CExpr * Type) witherrsoption =
            (
                let val _ = if DEBUG then print ("synthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e ^ "\n") else ()
                val originalExpr = e
                val res = case e of
                    RExprVar v => lookup ctx v >>= (fn (canonicalName, tp) =>
                                        Success (CExprVar canonicalName, tp))
                    | RUnitExpr(soi) => Success (CUnitExpr, UnitType)
                    | RProj(e, l, soi) => synthesizeType ctx e >>= (fn t =>  case t of 
                            (ce, Prod ls) => fmap (fn x => (CProj(ce, l, Prod ls),x)) (lookupLabel ls l)
                            | _ => Errors.attemptToProjectNonProd e
                            (* raise TypeCheckingFailure "Attempt to project out of non product type" *)
                    )
                    | RCase(e,cases, soi) => (synthesizeType ctx e) >>= (fn t => case t of
                            (ce, Sum ls) => let 
                            val checkedCases = collectAll (map (fn (l, ev, e) => 
                                (lookupLabel ls l) >>= (fn lookedUpType => 
                                        synthesizeType (addToCtxA (TermTypeJ([ev], lookedUpType, ())) ctx) e
                                    )
                                ) 
                            cases)
                            val casesTypes : Type list witherrsoption =  fmap (map (#2)) checkedCases 
                            val checkedExprs : CExpr list witherrsoption =  fmap (map (#1)) checkedCases
                            val returnType : Type witherrsoption = casesTypes >>= typeUnify originalExpr
                            in 
                                (* I don't know how to make these look nice, this is just to unroll the witherrsoption *)
                                checkedCases >>= (fn checkedCases =>
                                casesTypes >>= (fn casesTypes  =>
                                checkedExprs >>= (fn checkedExprs => 
                                returnType >>= (fn returnType =>
                                Success (CCase ((Sum ls, ce), (List.tabulate(length cases, fn i => 
                                    let val (label, evar, _) = List.nth(cases, i)
                                    in (label, evar, #1 (List.nth(checkedCases, i)))
                                    end
                                    )), returnType), returnType)
                                )
                                )
                                )
                                )
                            end
                            (* | _ => raise TypeCheckingFailure "Attempt to case on non sum types") *)
                            | _ => Errors.attemptToCaseNonSum e
                            )
                    | RLamWithType (t, ev, e, soi) => 
                        synthesizeType (addToCtxA (TermTypeJ([ev], t, ())) ctx) e >>= (fn (bodyExpr, returnType) =>
                        Success(CLam(ev, bodyExpr, Func (t, returnType)), Func(t, returnType))
                        )
                    | RApp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn t => case t
                        of (ce1, Func (t1, t2)) => 
                        (checkType ctx e2 t1) >>= (fn ce2 => 
                        Success(CApp (ce1,ce2, Func(t1,t2)), t2)
                        )
                        | (_, t) => Errors.attemptToApplyNonFunction e
                    )
                        (* raise TypeCheckingFailure ("Application on nonfunction, got " ^ PrettyPrint.show_typecheckingType t)) *)
                    | RTAbs (tv, e2, soi) =>   synthesizeType ctx  e2 >>= (fn (ce2, bodyType) => 
                    Success (CTAbs(tv, ce2, Forall (tv, bodyType)), Forall (tv, bodyType)) )
                    | RTApp (e2, t, soi) => synthesizeType ctx e2 >>= (fn st => case st of
                        (ce2, Forall (tv, tb)) => Success(CTApp(ce2,t, Forall(tv, tb)), substTypeInType t [tv] tb)
                        | _ => Errors.attemptToApplyNonUniversal e
                        (* raise TypeCheckingFailure "TApp on non universal types" *)
                        )
                    (* | Pack (t, e2) => *)
                    | ROpen (e1, (tv, ev, e2), soi) => synthesizeType ctx e1 >>= (fn synt => case synt  of
                                (ce1, Exists (tv', tb)) => 
                        synthesizeType (addToCtxA (TermTypeJ([ev], 
                        substTypeInType (TypeVar [tv]) [tv'] tb,())) ctx) e2 >>= (fn (ce2, synthesizedType) =>
                        if List.exists (fn t => t = [tv]) (freeTVar synthesizedType)
                            then Errors.openTypeCannotExitScope e
                            (* raise TypeCheckingFailure "Open's type cannot exit scope" *)
                            else Success(COpen((Exists(tv', tb), ce1), (tv, ev, ce2), synthesizedType), synthesizedType)
                        )
                            | _ => Errors.attemptToOpenNonExistentialTypes e)
                    (* | Fold e2 => Fold (substTypeInExpr tS x e2) *)
                    | RUnfold (e2, soi) => synthesizeType ctx e2 >>= (fn synt => case synt of
                        (ce2, Rho (tv, tb)) => Success (CUnfold(ce2, Rho(tv, tb)), substTypeInType (Rho (tv, tb)) [tv] tb)
                        | _ => Errors.attemptToUnfoldNonRecursiveTypes e
                        )
                    | RStringLiteral(l, soi) => Success(CStringLiteral l, BuiltinType(BIString))
                    | RIntConstant(i, soi) => Success(CIntConstant i, BuiltinType(BIInt))
                    | RRealConstant (r, soi) => Success(CRealConstant r, BuiltinType(BIReal))
                    

                    | RLetIn(decls, e, soi) => (case ctx of 
                        Context(curName, curVis, bindings) => 
                typeCheckSignature (Context(curName@StructureName.localName(), curVis, bindings)) decls [] >>= 
                (fn (Context(localName, _, newBindings), csig) =>
                        synthesizeType (Context(localName,curVis, newBindings)) e >>= 
                        (fn (ce, synthesizedType) =>
                                Success (CLetIn(csig, ce, synthesizedType), synthesizedType)
                        )
                        )
                    )
                    (* | Fix (ev, e)=> Fix (ev, substTypeInExpr tS x e) *)
                    | _ => Errors.expressionDoesNotSupportTypeSynthesis e
                    
                    (* raise TypeCheckingFailure "Expression does type support type synthesis, please specify type" *)

                    val _ = if DEBUG then print ( "synthesize got result " ^
                    PrettyPrint.show_static_error res (fn res => PrettyPrint.show_typecheckingType (#2 res))^
                    " whensynthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e ^ "\n") else ()
                    in res
                    end )
                (* handle TypeCheckingFailure s => 
                    raise TypeCheckingFailure (s ^ "\n when synthesizing the type for " ^ PrettyPrint.show_typecheckingRExpr e 
                    ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx) *)

            and assertTypeEquiv (expr: RExpr) (synthesized : Type) (checked : Type) : unit witherrsoption =
                if typeEquiv [] synthesized checked 
                then Success() 
                else genSingletonError (reconstructFromRExpr expr)
                ("类型不匹配(type mismatch) \n 推断的类型(synthesized type) : " ^ PrettyPrint.show_typecheckingType synthesized
                ^ " \n 检查的类型(checked type) : " ^ PrettyPrint.show_typecheckingType checked) NONE
            and checkType (ctx : context) (e : RExpr) (tt: Type) (* tt target type *) : CExpr witherrsoption =
                (let 
                    val _ = if DEBUG then  print(  "checking the expr " ^ PrettyPrint.show_typecheckingRExpr e ^ 
                        " against type " ^ PrettyPrint.show_typecheckingType tt ^ "\n") else ()
                    val originalExpr = e
                    val res = 
                    case e of
                    RExprVar v => 
                    (synthesizeType ctx e) >>= (fn (synthExpr, synthType) =>
                    assertTypeEquiv e (synthType) tt  >> (Success (synthExpr))
                    (* = false 
                                then raise TypeCheckingFailure ("var type mismatch var is " ^ StructureName.toStringPlain v 
                                ^ " synthesized : " ^ PrettyPrint.show_typecheckingType (#2 (synthesizeType ctx e)) ^ " against : " 
                                ^ PrettyPrint.show_typecheckingType tt
                                ) *)
                                (* else synthExpr *)
                    )
                    | RUnitExpr(soi) => assertTypeEquiv e UnitType  tt >> (Success(CUnitExpr))
                        (* raise TypeCheckingFailure "unit expr will have unit type" *)
                    | RTuple (l, soi) => (case tt of 
                        Prod ls => if List.length l <> List.length ls
                                    then Errors.prodTupleLengthMismatch e
                                    (* raise TypeCheckingFailure "Prod tuple length mismatch" *)
                                    else collectAll (List.tabulate(List.length l, (fn i => 
                                    checkType ctx (List.nth(l, i)) (#2 (List.nth(ls, i)))))) >>= (fn checkedElems => 
                                    Success(CTuple ( checkedElems, (Prod ls))))
                        | _ => Errors.expectedProdType e
                        (* raise TypeCheckingFailure "Expected Prod" *)
                        )
                    | RProj(e, l, soi) =>
                    synthesizeType ctx (RProj(e, l, soi)) >>= (fn synt => case synt of
                        (CProj(ce, l, prodType), synthType) => assertTypeEquiv originalExpr synthType tt >> 
                        Success(CProj(ce, l, prodType))
                        | _ => raise Fail "tcp229")

                    | RInj (l, e, soi) => (case tt of
                        Sum ls => (lookupLabel ls l) >>= (fn lookedupType => 
                                checkType ctx e lookedupType >>= (fn checkedExpr => 
                                    Success(CInj(l, checkedExpr, Sum ls))
                                ))
                        | _ => Errors.expectedSumType originalExpr
                    )
                    | RCase(e,cases, soi) => (synthesizeType ctx e) >>= (fn synt => case synt of
                            (ce, Sum ls) => 
                            (collectAll (map (fn (l, ev, e) => 
                                fmap (fn ce => (l, ev, ce)) 
                                    ((lookupLabel ls l) >>= (fn lookedUpType => 
                                        checkType (addToCtxA (TermTypeJ([ev], lookedUpType , ())) ctx) e tt))
                                ) cases)) >>= (fn checkedCases  
                                    => Success(CCase((Sum ls, ce), checkedCases , tt)))
                            | _ => Errors.attemptToCaseNonSum originalExpr)
                    | RLam(ev, eb, soi) => (case tt of
                        Func(t1,t2) => 
                            checkType (addToCtxA (TermTypeJ([ev], t1,())) ctx) eb t2
                            >>= (fn checkedExpr => Success(CLam(ev, checkedExpr, tt)))
                        | _ => Errors.expectedFunctionType e
                        (* raise TypeCheckingFailure ("Lambda is not function got " ^ PrettyPrint.show_typecheckingType tt) *)
                        )
                    | RLamWithType (t, ev, eb, soi) => (case tt of
                        Func(t1,t2) => (assertTypeEquiv e t t1 >>
                            (checkType (addToCtxA (TermTypeJ([ev], t1, ())) ctx) eb t2 >>= (fn checkedBody => 
                                Success(CLam(ev, checkedBody , tt)))
                                )
                            )
                        | _ => Errors.expectedFunctionType e
                        (* raise TypeCheckingFailure "Lambda is not function" *)
                        )
                    | RApp (e1, e2, soi) => synthesizeType ctx e1 >>= (fn synt => case synt 
                        of (ce1, Func (t1, t2)) => (
                        assertTypeEquiv e t2 tt >> (
                                checkType ctx e2 t1 >>= (fn checkedArg => 
                                    Success (CApp(ce1, checkedArg, Func(t1, t2)))
                                )
                            )
                        )
                        | _ => Errors.attemptToApplyNonFunction e)
                        (* raise TypeCheckingFailure "Application on nonfunction") *)
                    | RTAbs (tv, e2, soi) => (case tt of
                        Forall (tv', tb) => 
                                checkType ctx e2 (substTypeInType (TypeVar [tv]) [tv'] tb) >>= (fn ce2 => 
                                            Success(CTAbs (tv, ce2, tt))
                                )
                        | _ => Errors.expectedUniversalType e 
                        (* raise TypeCheckingFailure "Encountered TAbs" *)
                    )
                    | RTApp (e2, t, soi) => synthesizeType ctx e2  >>= (fn synt => case synt of
                        (ce2, Forall (tv, tb)) => (assertTypeEquiv e tt (substTypeInType t [tv] tb) >>
                        Success(CTApp(ce2, t, Forall(tv, tb))))
                        | _ => Errors.attemptToApplyNonUniversal e
                        (* raise TypeCheckingFailure "TApp on non universal types" *)
                        )
                    | RPack (t, e2, soi) => (case tt of
                        Exists (tv, tb) => 
                                checkType ctx e2 (substTypeInType t [tv]  tb) >>= (fn ce2 => 
                                                Success(CPack(t, ce2, tt)))
                        | _ => Errors.expectedExistentialType e
                        (* raise TypeCheckingFailure "Pack <-> Exists" *)
                    )
                    | ROpen (e1, (tv, ev, e2), soi) => synthesizeType ctx e1 >>= (fn synt => case synt of
                        (ce1, Exists (tv', tb)) => 
                        checkType (addToCtxA (TermTypeJ([ev], substTypeInType (TypeVar [tv]) [tv'] tb, ())) ctx) e2 tt
                        >>= (fn ce2 => 
                        Success(COpen((Exists (tv', tb), ce1), (tv, ev, ce2), tt))
                        )
                        | _ => Errors.attemptToOpenNonExistentialTypes e
                    )
                        (* raise TypeCheckingFailure "cannot open non existential types") *)
                    | RFold (e2, soi) => (case tt
                        of 
                        Rho (tv ,tb) => 
                        checkType ctx e2 (substTypeInType (Rho(tv, tb)) [tv] tb)
                        >>= (fn ce2 => Success (CFold(ce2, tt)))
                        | _ => Errors.expectedRecursiveType e
                        (* raise TypeCheckingFailure "Expected Rho" *)
                            )
                    | RUnfold (e2,soi) => synthesizeType ctx e2  >>= (fn synt => case synt of
                        (ce2, Rho (tv, tb)) =>(
                            assertTypeEquiv e (substTypeInType (Rho (tv, tb)) [tv] tb) tt >>
                            Success(CUnfold(ce2, Rho(tv,tb))))
                        | _ => Errors.attemptToUnfoldNonRecursiveTypes e
                        (* raise TypeCheckingFailure "Cannot unfold non recursive type" *)
                        )
                    | RFix (ev, e, soi)=> checkType (addToCtxA (TermTypeJ([ev] , tt, ())) ctx) e tt
                                        >>= (fn ce => Success(CFix(ev,ce, tt)))
                    | RStringLiteral (s, soi) => (assertTypeEquiv e (BuiltinType(BIString)) tt >> (Success (CStringLiteral s)))
                    | RIntConstant (i, soi) => (assertTypeEquiv e (BuiltinType(BIInt)) tt >> (Success ( CIntConstant i)))
                    | RRealConstant (r, soi) => (assertTypeEquiv e (BuiltinType(BIReal)) tt >> (Success (CRealConstant r)))
                    | RFfiCCall (e1, e2, soi) => (
                        case e1 of
                            RStringLiteral (cfuncName, soi) => 
                                let fun elaborateArguments  (args : StructureName.t list ) : CExpr witherrsoption = 
                                    fmap CFfiCCall(Success cfuncName =/= 
                                    collectAll (map (fn a => fmap (#1) (lookup ctx a)) args))
                                in
                                            (case e2 of 
                                                RExprVar v => (Success ([v])) >>= elaborateArguments
                                                | RTuple (l, soi) => (collectAll (map (fn arg => case arg of 
                                                    RExprVar v => Success (v)
                                                    | _ => Errors.ccallArgumentsMustBeImmediate arg
                                                    (* raise TypeCheckingFailure "ccall arguments must be immediate values" *)
                                                    ) l)) >>= elaborateArguments
                                                | _ => raise Fail "tcp439"
                                            )
                                end
                            | _ => Errors.firstArgumentOfCCallMustBeStringLiteral e1
                            (* raise TypeCheckingFailure "First argument of the ccall must be string literal" *)

                    )
                    | RLetIn(decls, e, soi) => (case ctx of 
                Context(curName, curVis, bindings) => 
                    typeCheckSignature (Context(curName@StructureName.localName(), curVis, bindings)) decls []
                    >>= (fn(Context(localName, _, newBindings), csig) =>
                        (* assume the typeChecking is behaving properly, 
                        no conflicting things will be added to the signature *)
                        (* sub context will be determined by whether the signature is private or not ? *)
                            checkType (Context(localName,curVis, newBindings)) e tt >>= (fn ce => 
                                        Success(CLetIn(csig, ce, tt))
                            )
                    )
                    
                )
                in res
                end 
        )
                    (* handle TypeCheckingFailure s =>
                    raise TypeCheckingFailure (s ^ "\n when checking the expr " ^ PrettyPrint.show_typecheckingRExpr e ^ 
                        " against type " ^ PrettyPrint.show_typecheckingType tt
                    ^ " in context " ^ PrettyPrint.show_typecheckingpassctx ctx
                    ) *)
                    (* type check signature will return all bindings *)
            and typeCheckSignature(ctx : context) (s : RSignature) (acc : CSignature) : (context * CSignature) witherrsoption =

                (
                    if DEBUG then print ("DEBUG " ^ PrettyPrint.show_typecheckingRSig s ^
                    " in context " ^ PrettyPrint.show_typecheckingpassctx ctx ^"\n") else (); 

                    case s of
                    [] => Success(ctx, acc)
                    (* normalize should not change the set of free variables *)
                | RTypeMacro (n, t)::ss => 
                let val freeTVars = freeTVar (applyContextToType ctx t) in if freeTVars <> [] then 
                    Errors.typeDeclContainsFreeVariables (StructureName.toString (hd freeTVars))
                    else 
                    normalizeType (applyContextToType ctx t) >>= (fn normalizedType => 
                    typeCheckSignature (addToCtxR (TypeDef([n], normalizedType, ())) ctx) ss 
                        (acc@[CTypeMacro((getCurSName ctx)@[n], normalizedType)]))
                    end
                | RTermTypeJudgment(n, t):: ss => if freeTVar (applyContextToType ctx t) <> [] 
                    then Errors.termTypeDeclContainsFreeVariables n
                    (* raise SignatureCheckingFailure ("TermType decl contains free var" ^ PrettyPrint.show_sttrlist (freeTVar (applyContextToType ctx t)) ^" in "^ PrettyPrint.show_typecheckingType (applyContextToType ctx t))  *)
                    else 
                    normalizeType (applyContextToType ctx t) >>= (fn normalizedType => 
                    typeCheckSignature (addToCtxR (TermTypeJ([n], normalizedType, ())) ctx) ss (acc))
                | RTermMacro(n, e) :: ss => 
                    synthesizeType ctx (applyContextToExpr ctx e) >>= 
                    (fn (transformedExpr , synthesizedType)  =>
                        typeCheckSignature (addToCtxR (TermTypeJ([n], synthesizedType, ())) ctx) ss 
                            (acc@[CTermDefinition((getCurSName ctx)@[n], transformedExpr, synthesizedType)])
                    )
                | RTermDefinition(n, e) :: ss => 
        (lookup ctx [n]) >>= (fn (_, lookedUpType) => 
                let val transformedExprOrFailure = checkType ctx (applyContextToExpr ctx e) lookedUpType
                in 
                case transformedExprOrFailure of
                Success(transformedExpr) => typeCheckSignature ctx ss 
                                                (acc@[CTermDefinition((getCurSName ctx)@[n], transformedExpr, lookedUpType)])
                | DErrors(l) => (case typeCheckSignature ctx ss (acc) of 
                            Success _ => DErrors(l)
                            | DErrors l2 => DErrors(l @l2)
                            | _ => raise Fail "tcp458"
                    )
                | _ => raise Fail "tcp457"
                end
        )
                | RStructure (vis, sName, decls) :: ss => 
                (case ctx of 
                Context(curName, curVis, bindings) => 
                    typeCheckSignature (Context(curName@[sName], vis, bindings)) decls [] >>=
                    (fn(Context(_, _, newBindings), checkedSig) =>
                        (* assume the typeChecking is behaving properly, 
                        no conflicting things will be added to the signature *)
                        (* sub context will be determined by whether the signature is private or not ? *)
                    typeCheckSignature (Context(curName, curVis, newBindings)) ss (acc@checkedSig)
                    )
                    
                )
                | ROpenStructure openName :: ss =>
                (case ctx of 
                Context(curName, curVis, bindings) => 
                    let val nextContext = nextContextOfOpenStructure curName curVis bindings openName
                        (* assume the typeChecking is behaving properly, 
                        no conflicting things will be added to the signature *)
                        (* sub context will be determined by whether the signature is private or not ? *)
                    in typeCheckSignature nextContext ss (acc)
                    end
                )
                | RImportStructure(importName, path) :: ss => 
                    (getTypeCheckedAST (path, importName)
                    <?> (genSingletonError (StructureName.toString importName) "导入模块时出错" NONE)
                    )
                     >>= (fn csig => 
                        typeCheckSignature 
                        (addToCtxAL (List.mapPartial (fn x => case x of 
                            CTypeMacro(sname, t) => SOME(TypeDef(importName@sname, t, ()))
                            | CTermDefinition(sname, e, t) => SOME(TermTypeJ(importName@sname, t,()))
                            | CDirectExpr _ => NONE
                            | CImport _ => NONE
                            ) csig) ctx)
                        ss (acc@[CImport(importName, path)])
                    )
                | RDirectExpr e :: ss=> 
                    let 
                    val synthedExprOrFailure = (synthesizeType ctx (applyContextToExpr ctx e))
                    in case synthedExprOrFailure of 
                        Success(checkedExpr, synthesizedType) => typeCheckSignature ctx ss (acc@[CDirectExpr(checkedExpr, synthesizedType)])
                        | DErrors l => (case typeCheckSignature ctx ss (acc) of
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
        let val res =  (typeCheckSignature 
            (Context (StructureName.topLevelName, true, 
                    []))
            s [])
                (* val _ = DebugPrint.p "Type checked top level\n"
                val _ = DebugPrint.p (PrettyPrint.show_typecheckingCSig res) *)
        in fmap (#2) res end
    end
end
