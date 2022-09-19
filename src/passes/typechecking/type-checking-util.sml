
structure TypeCheckingUtil = struct
    open TypeCheckingAST
    open TypeCheckingASTOps
    open StaticErrorStructure
    infix 5 >>=
    
    structure Errors = TypeCheckingErrors

    fun assertTypeEquiv (ctx : context) (expr: RExpr) (synthesized : CType) (checked : CType) : unit witherrsoption =
    TypeCheckingErrors.genericError expr ctx ("Assert Type Equiv called on " ^ PrettyPrint.show_typecheckingCExpr synthesized ^ " and "  ^
        PrettyPrint.show_typecheckingCExpr checked  ^ ", replace with type unify" )
        
        fun foldMapCtx (ctx : context) 
                   (f : ('a * context) -> ('b * context) witherrsoption) 
                   (l : 'a list) : ('b list * context) witherrsoption = 
        case l of  
        [] => Success([], ctx)
        | (x :: xs) => f (x, ctx) >>= (fn (y, ctx) => foldMapCtx ctx f xs >>= (fn (ys, ctx) => Success(y::ys, ctx)))

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





        (* returned CExpr is CVar or CStructureProj *)
    fun resolveRVar (ctx : context) (rvarName : StructureName.t) : (CExpr * CType * judgmentType) witherrsoption = 
    let fun findCorrectType i (acctm, acctp, accjtp) (* acc for accumulation *) = 
            if i > length rvarName then raise Fail "not possible tcp225"
            else
            if i = length rvarName
            then 
            Success(acctm, acctp, accjtp)
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
                                    dec as ( CPureDeclaration(_, tp)
                                    | CConstructorDecl(_, tp, _)
                                    )
                                    , idx) =>
                                    let 
                                        val nextAccTm = CBlockProj(acctm, curNameComp, idx)
                                        (* val _ = DebugPrint.p (PrettyPrint.show_typecheckingCExpr tp) *)
                                        val nextAccTp = tp (*TODO: dependent case *)
                                        val nextI = i +1
                                    in findCorrectType nextI (nextAccTm, nextAccTp, 
                                        (case dec of 
                                        CPureDeclaration _ => JTLocalBinder
                                        | CConstructorDecl (_, _, cinfo) => JTConstructor cinfo
                                        | _ => raise Fail "tcu81"
                                        )
                                    )
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
            else Success((CVar(canonicalName, judgmentTypeToCVarType canonicalName jtp), tp, jtp)))
        else (case findCtx ctx (List.take(rvarName, i)) of 
            NONE => lookupModuleName (i+1)
            | SOME(canonicalName, tp, jtp) => 
            if not (StructureName.semanticEqual canonicalName (List.take(rvarName, i)))
            then raise Fail "canonical name different from rvar name(2)"
            else findCorrectType (i) (CVar(canonicalName, judgmentTypeToCVarType canonicalName jtp), tp, jtp))
    in lookupModuleName 1
    end

end