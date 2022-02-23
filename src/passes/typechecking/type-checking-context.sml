
structure TypeCheckingContext = struct

open TypeCheckingAST
open StaticErrorStructure
    fun showctx (x : context) (full : bool) = (case x of 
    Context(curSName, curVis, m) =>  
    "当前结构名：" ^ StructureName.toStringPlain curSName ^
    "\n当前已定义的值及其类型：\n"  ^(
        let val allDecls = (map (fn x => case x of
    TermTypeJ(e, t,_) => StructureName.toStringPlain e ^ "：" ^ PrettyPrint.show_typecheckingCType t
    | TermDefJ(s, t, _) => StructureName.toStringPlain s ^ " = " ^ PrettyPrint.show_typecheckingCType t) m)
        
        val condensedDecls = 
        if full then allDecls else (if length allDecls > 10 then List.take(allDecls, 10)@["以及之后的" ^ Int.toString (length allDecls - 10)^"个值或类型"]
        else allDecls)
    in 
            String.concatWith "；\n" condensedDecls  ^ "\n"
            end
    )
          )

    fun showctxSome x = SOME(showctx x true)

    fun findCtxForType (Context(curSName, v, ctx) : context) (n : StructureName.t) : (StructureName.t * CType) option = 
        let exception LookupNotFound
            fun lookupMapping (ctx : mapping list) (n : StructureName.t) (curSName : StructureName.t ): (StructureName.t * CType) = 
                case ctx of 
                (* WARNING: toUTF8String discards the separator information, but I guess it is fine because 
                    as long as all components are of the same name, we're fine*)
                    [] => raise LookupNotFound
                    (* ("name " ^ StructureName.toStringPlain n ^ " not found in context") *)
                    | TermTypeJ(n1, t1, u)::cs => 
                        (case StructureName.checkRefersTo n1 n curSName 
                        of SOME(cname) => (case u of NONE => cname | SOME(x) => x, t1)
                        | NONE => lookupMapping cs n curSName
                        )
                    | TermDefJ(_) :: cs => lookupMapping cs n curSName
            val ntp = SOME(lookupMapping ctx n curSName)
                handle LookupNotFound => NONE
        in 
            ntp 
        end
(* require lookup to add name qualification if references local structure *)
    fun lookupCtxForType (ctxg as Context(curSName, v, ctx): context) (n : StructureName.t) : (StructureName.t * CType) witherrsoption= 
    case findCtxForType ctxg n of
        SOME(st) => Success(st)
        | NONE => genSingletonError (StructureName.toString n) ("名称`" ^ StructureName.toStringPlain n ^ "`未找到") (showctxSome (Context(curSName, v, ctx)))

     fun findCtxForDef (Context(curSName, v, ctx) : context) (n : StructureName.t) : (StructureName.t * CType) option = 
        let exception LookupNotFound
            fun lookupMapping (ctx : mapping list) (n : StructureName.t) (curSName : StructureName.t ): (StructureName.t * CType) = 
                case ctx of 
                    [] => raise LookupNotFound
                    | TermDefJ(n1, t1, u)::cs => 
                        (case StructureName.checkRefersTo n1 n curSName 
                        of SOME(cname) => (cname, t1)
                        | NONE => lookupMapping cs n curSName
                        )
                    | TermTypeJ(_) :: cs => lookupMapping cs n curSName
            val ntp = SOME(lookupMapping ctx n curSName)
                handle LookupNotFound => NONE
        in 
            ntp 
        end

(* require lookup to add name qualification if references local structure *)
    fun lookupCtxForDef (ctxg as Context(curSName, v, ctx): context) (n : StructureName.t) : (StructureName.t * CType) witherrsoption= 
    case findCtxForDef ctxg n of
        SOME(st) => Success(st)
        | NONE => genSingletonError (StructureName.toString n) ("名称`" ^ StructureName.toStringPlain n ^ "`未找到") (showctxSome (Context(curSName, v, ctx)))

        fun appendAbsoluteMappingToCurrentContext (m : 'a gmapping) (ctx : 'a gcontext) : 'a gcontext = 
        case ctx of
            Context(curSName, vis, l) => Context(curSName, vis, 
            (case m of 
                TermTypeJ(e, t, u) => TermTypeJ(e, t, u)
                | TermDefJ(tname, t, u) => TermDefJ(tname, t, u)
                ):: l
            )

   fun appendRelativeMappingToCurrentContext (m : 'a gmapping) (ctx : 'a gcontext) : 'a gcontext = 
        case ctx of
            Context(curSName, vis, l) => Context(curSName, vis, 
            (case m of 
                TermTypeJ(e, t, u) => TermTypeJ(curSName@e, t, u)
                | TermDefJ(tname, t, u) => TermDefJ(curSName@tname, t, u)
                ):: l
            )

    fun appendAbsoluteMappingsToCurrentContext (m : 'a gmapping list) (ctx : 'a gcontext) : 'a gcontext = 
        foldl (fn (map, acc) => appendAbsoluteMappingToCurrentContext map acc) ctx m

    fun appendRelativeMappingsToCurrentContext (m : 'a gmapping list) (ctx : 'a gcontext) : 'a gcontext = 
        foldl (fn (map, acc) => appendRelativeMappingToCurrentContext map acc) ctx m

  

        

end