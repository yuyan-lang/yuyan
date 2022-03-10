
structure TypeCheckingContext = struct

open TypeCheckingAST
open StaticErrorStructure
fun show_jt defop = 
(case defop of JTDefinition(def) =>  " (定义) "
        | JTConstructor (CConsInfoTypeConstructor) => " （类型构造器）"
        | JTConstructor (CConsInfoElementConstructor _) => " （元素构造器）"
        | JTLocalBinder => "（局部绑定）"
        | JTPending => "（pending）"
        | JTMetaVarPendingResolve _ => "(metavar pending resolve)"
        | JTMetaVarResolved e => "(resolved metavar)"
    )
    fun showctx (x : context) (full : bool) = (case x of 
    Context(curSName, curVis, m) =>  
    "当前结构名：" ^ StructureName.toStringPlain curSName ^
    "\n当前已定义的值及其类型：\n"  ^(
        let val allDecls = (map (fn x => case x of
    TermTypeJ(e, t, defop, _) => StructureName.toStringPlain e ^ "：" ^ PrettyPrint.show_typecheckingCType t 
    ^ (show_jt defop ^ 
        (if full 
    then (case defop of JTDefinition(def) =>  "\n" ^ StructureName.toStringPlain e ^" = " ^PrettyPrint.show_typecheckingCExpr def 
         | _ => "")
    else ""))) m)
    (* | TermDefJ(s, t, _) => StructureName.toStringPlain s ^ " = " ^ PrettyPrint.show_typecheckingCType t) m) *)
        
        val condensedDecls = 
        if full then allDecls else (if length allDecls > 10 then List.take(allDecls, 10)@["以及之后的" ^ Int.toString (length allDecls - 10)^"个值或类型"]
        else allDecls)
    in 
            String.concatWith "；\n" condensedDecls  ^ "\n"
            end
    )
          )

    (* fun showctxSome x = SOME(showctx x false) *)
    fun showctxSome x = SOME(showctx x true)



    fun findCtx (Context(curSName, v, ctx) : context) (n : StructureName.t) : (StructureName.t * CType * judgmentType) option = 
        let exception LookupNotFound
            fun lookupMapping (ctx : mapping list) (n : StructureName.t) (curSName : StructureName.t ): (StructureName.t * CType * judgmentType) = 
                case ctx of 
                (* WARNING: toUTF8String discards the separator information, but I guess it is fine because 
                    as long as all components are of the same name, we're fine*)
                    [] => raise LookupNotFound
                    (* ("name " ^ StructureName.toStringPlain n ^ " not found in context") *)
                    | TermTypeJ(n1, t1, defop1,  u)::cs => 
                        (case StructureName.checkRefersTo n1 n curSName 
                        of SOME(cname) => (case u of NONE => cname | SOME(x, _) => x, t1, defop1)
                        | NONE => lookupMapping cs n curSName
                        )
            val ntp = SOME(lookupMapping ctx n curSName)
                handle LookupNotFound => NONE
        in 
            ntp 
        end

    fun findCtxForType (Context(curSName, v, ctx) : context) (n : StructureName.t) : (StructureName.t * CType) option = 
        Option.map(fn (x,t,eop) => (x, t)) (findCtx (Context(curSName, v, ctx)) n)

    fun modifyCtx (Context(curSName, v, ctx) : context) (cname : StructureName.t)
        (jopf : judgmentType -> judgmentType) : context = 
        case ctx of 
            [] => raise Fail "tcc52: key not found"
            | (currentj as TermTypeJ(n1, t1, jtp,  u))::cs => 
                if StructureName.semanticEqual n1 cname
                then ( Context(curSName, v, TermTypeJ(n1, t1, jopf jtp, u) :: cs)
                    )
                else 
                     case modifyCtx (Context(curSName, v, cs)) cname jopf of 
                        Context(cname', v', cs') => Context(cname', v', currentj::cs')

    (* the name must be absolute name *)
    fun modifyCtxAddDef(ctx : context) (cname : StructureName.t) (newDef : CExpr) : context = 
        modifyCtx ctx cname (fn jtp => case jtp of 
                JTPending => JTDefinition newDef
                | _ => raise Fail ("tcc58: jtp is not pending " ^ (StructureName.toStringPlain cname))
            )

    fun modifyCtxResolveMetaVar (ctx : context) (cname : StructureName.t) (resolvedExpr : CExpr) : context = 
        modifyCtx ctx cname (fn jtp => case jtp of 
                JTMetaVarPendingResolve _ => JTMetaVarResolved resolvedExpr
                | _ => raise Fail ("tcc58: jtp is not metavar pending resolve but is " ^ show_jt jtp ^ " at " ^ (StructureName.toStringPlain cname))
            )
        

    fun lookupCtx (ctxg as Context(curSName, v, ctx) : context) (n : StructureName.t) : (StructureName.t * CType * judgmentType) witherrsoption = 
    case findCtx ctxg n of 
        SOME(v) => Success(v)
        | NONE =>  genSingletonError (StructureName.toString n) ("名称`" ^ StructureName.toStringPlain n ^ "`未找到") (showctxSome (Context(curSName, v, ctx)))


(* require lookup to add name qualification if references local structure *)
    fun lookupCtxForType (ctxg as Context(curSName, v, ctx): context) (n : StructureName.t) : (StructureName.t * CType) witherrsoption= 
    case findCtxForType ctxg n of
        SOME(st) => Success(st)
        | NONE => genSingletonError (StructureName.toString n) ("名称`" ^ StructureName.toStringPlain n ^ "`未找到") (showctxSome (Context(curSName, v, ctx)))

     fun findCtxForDef (Context(curSName, v, ctx) : context) (n : StructureName.t) : (StructureName.t * CType) option = 
        case (findCtx (Context(curSName, v, ctx)) n) of 
            SOME(x,t,eop) => (case eop of JTDefinition(e) => SOME(x, e) | _ => NONE)
            | NONE => NONE

(* require lookup to add name qualification if references local structure *)
    fun lookupCtxForDef (ctxg as Context(curSName, v, ctx): context) (n : StructureName.t) : (StructureName.t * CType) witherrsoption= 
    case findCtxForDef ctxg n of
        SOME(st) => Success(st)
        | NONE => genSingletonError (StructureName.toString n) ("名称`" ^ StructureName.toStringPlain n ^ "`未找到") (showctxSome (Context(curSName, v, ctx)))

    fun appendAbsoluteMappingToCurrentContext (m : 'a gmapping) (ctx : 'a gcontext) : 'a gcontext = 
        case ctx of
            Context(curSName, vis, l) => Context(curSName, vis, 
            (case m of 
                TermTypeJ(e, t, defop, u) => TermTypeJ(e, t, defop, u)
                (* | TermDefJ(tname, t, u) => TermDefJ(tname, t, u) *)
                ):: l
            )

   fun appendRelativeMappingToCurrentContext (m : 'a gmapping) (ctx : 'a gcontext) : 'a gcontext = 
        case ctx of
            Context(curSName, vis, l) => Context(curSName, vis, 
            (case m of 
                TermTypeJ(e, t, defop, u) => TermTypeJ(curSName@e, t, defop, u)
                (* | TermDefJ(tname, t, u) => TermDefJ(curSName@tname, t, u) *)
                ):: l
            )

    fun appendAbsoluteMappingsToCurrentContext (m : 'a gmapping list) (ctx : 'a gcontext) : 'a gcontext = 
        foldl (fn (map, acc) => appendAbsoluteMappingToCurrentContext map acc) ctx m

    fun appendRelativeMappingsToCurrentContext (m : 'a gmapping list) (ctx : 'a gcontext) : 'a gcontext = 
        foldl (fn (map, acc) => appendRelativeMappingToCurrentContext map acc) ctx m

   val addToCtxA = appendAbsoluteMappingToCurrentContext (* A for relative *)
  val addToCtxR = appendRelativeMappingToCurrentContext (* R for relative *)
    val addToCtxAL = appendAbsoluteMappingsToCurrentContext (* L for list *)
    val addToCtxRL = appendRelativeMappingsToCurrentContext (* L for list *)
    

  

        

end