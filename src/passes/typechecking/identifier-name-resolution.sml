structure IdentifierNameResolution =
struct
    (* open TypeCheckingAST
    
    type typecontext = StructureName.t list
    type termcontext = StructureName.t list

    structure StructureNameSet = ListSet(structure Elem = StructureNameOrdered)
    open StructureNameSet

    fun  ***(a,b) = StructureNameSet.union a b
    infix 7 *** 

    fun addToCtxR(name : UTF8String.t) ( ctx : StructureName.t list) (currentSName : StructureName.t) = 
        (currentSName@[name])::ctx

    fun addToCtxA(name : UTF8String.t) ( ctx : StructureName.t list)  = 
        ([name])::ctx
    fun addToCtxOpen(openName : StructureName.t) ( ctx : StructureName.t list)(currentSName : StructureName.t)  = 
        (List.mapPartial (fn x => if StructureName.isPrefix (currentSName@openName) x(* for global structures (e.g. structures and substructures on the top level )*)
                            then SOME (StructureName.stripPrefix (currentSName@openName) x)
                            else NONE) ctx)
        @(List.mapPartial (fn x => if StructureName.isPrefix openName x (* for local structures e.g. structures in let *)
                            then SOME (StructureName.stripPrefix openName x)
                            else NONE) ctx)
        @ctx
    fun lookupCtx (ctx : StructureName.t list) (n : StructureName.t) (currentSName) : bool = 
    (
        (* (
        DebugPrint.p ("Looking up " ^ StructureName.toStringPlain currentSName ^ " -> " ^ 
            StructureName.toStringPlain n ^ " in " ^ String.concatWith ",\n" (map StructureName.toStringPlain ctx) ^ " \n\n")

    )
    ; *)
            List.exists (fn x => 
            Option.isSome(TypeCheckingASTOps.checkRefersTo x n currentSName)
            ) ctx
    )

        


    fun getUnresolvedIdentifiersType (t : Type) (typectx : typecontext) 
        (termctx : termcontext) (currentSName : StructureName.t) : StructureNameSet.set = 
        case t of
                    TypeVar v => if lookupCtx typectx v currentSName 
                                 then StructureNameSet.empty else StructureNameSet.singleton v 
                    | UnitType => StructureNameSet.empty  
                    | Prod l => 
                    List.foldr (op *** ) StructureNameSet.empty (map (fn (lbl, t) => getUnresolvedIdentifiersType t typectx termctx currentSName) l)
                    | NullType => StructureNameSet.empty  
                    | Sum l =>  
                    List.foldr (op *** ) StructureNameSet.empty (map (fn (lbl, t) => getUnresolvedIdentifiersType t typectx termctx currentSName) l)
                    | Func (t1, t2) => 
                        getUnresolvedIdentifiersType t1 typectx termctx currentSName
                    ***
                        getUnresolvedIdentifiersType t2 typectx termctx currentSName
                    | TypeInst (t1, t2) => 
                        getUnresolvedIdentifiersType t1 typectx termctx currentSName
                    ***
                        getUnresolvedIdentifiersType t2 typectx termctx currentSName
                    | Forall(t1, t2) => 
                        getUnresolvedIdentifiersType t2 ([t1]::typectx) termctx currentSName
                    | Exists (t1, t2) => 
                        getUnresolvedIdentifiersType t2 ([t1]::typectx) termctx currentSName
                    | Rho (t1, t2) => 
                        getUnresolvedIdentifiersType t2 ([t1]::typectx) termctx currentSName
                    | BuiltinType (BIString) =>StructureNameSet.empty  
                    | BuiltinType (BIBool) =>StructureNameSet.empty  
                    | BuiltinType (BIInt) => StructureNameSet.empty  
                    | BuiltinType (BIReal) => StructureNameSet.empty  
    fun getUnresolvedIdentifiersExpr (expr : RExpr) (typectx : typecontext) 
        (termctx : termcontext) (currentSName : StructureName.t) : StructureNameSet.set = 
         (let 
            (* val _ = DebugPrint.p "e" *)
            val res = 
            case expr of
            RVar v => 
            if lookupCtx termctx v currentSName
            then StructureNameSet.empty else StructureNameSet.singleton v
            | RUnitExpr => StructureNameSet.empty 
            | RTuple l => List.foldr (op *** ) StructureNameSet.empty (map (fn e => getUnresolvedIdentifiersExpr e typectx termctx currentSName) l)
            | RProj(e, l) => getUnresolvedIdentifiersExpr e typectx termctx currentSName
            | RInj (l, e) => getUnresolvedIdentifiersExpr e typectx termctx currentSName
            | RCase(e,cases) => getUnresolvedIdentifiersExpr e typectx termctx currentSName ***
                    List.foldr (op *** ) StructureNameSet.empty (map (fn (l, ev, e) => 
                getUnresolvedIdentifiersExpr e typectx ([ev]::termctx) currentSName) cases)
            | RLam(ev, eb) =>  getUnresolvedIdentifiersExpr eb typectx ([ev]::termctx) currentSName 
            | RLamWithType (t, ev, eb) => 
                getUnresolvedIdentifiersType t typectx termctx currentSName  ***
                getUnresolvedIdentifiersExpr eb typectx ([ev]::termctx) currentSName 
            | RApp (e1, e2) => 
                getUnresolvedIdentifiersExpr e1 typectx termctx currentSName ***
                getUnresolvedIdentifiersExpr e2 typectx termctx currentSName
            | RTAbs (tv, e2) => 
                getUnresolvedIdentifiersExpr e2 ([tv]::typectx) termctx currentSName 
            | RTApp (e2, t) => 
                getUnresolvedIdentifiersType t typectx termctx currentSName  ***
                getUnresolvedIdentifiersExpr e2 typectx termctx currentSName 
            | RPack (t, e2) => 
                getUnresolvedIdentifiersType t typectx termctx currentSName  ***
                getUnresolvedIdentifiersExpr e2 typectx termctx currentSName 
            | ROpen (e1, (tv, ev, e2)) => 
                getUnresolvedIdentifiersExpr e1 typectx termctx currentSName  ***
                getUnresolvedIdentifiersExpr e2 ([tv]::typectx) ([ev]::termctx) currentSName 
            | RFold e2 => 
                getUnresolvedIdentifiersExpr e2 typectx termctx currentSName 
            | RUnfold e2 => 
                getUnresolvedIdentifiersExpr e2 typectx termctx currentSName 
            | RFix (ev, e)=> 
                getUnresolvedIdentifiersExpr e typectx ([ev]::termctx) currentSName 
            | RStringLiteral s => StructureNameSet.empty 
            | RIntConstant s => StructureNameSet.empty 
            | RRealConstant s => StructureNameSet.empty 
            | RFfiCCall (str, e2) => 
                getUnresolvedIdentifiersExpr e2 typectx termctx currentSName 

            | RLetIn(decls, e) => (
                let val localName = StructureName.localName()
                in case getUnresolvedIdentifiersSignature decls typectx termctx (currentSName@localName) of
            (* this is a trick, the newly added signature can almost never be referenced *)
            (freeSNames, newtypecontext, newtermcontext) =>
        freeSNames ***
            getUnresolvedIdentifiersExpr e newtypecontext newtermcontext (currentSName@localName)
            end
         )
        in res
        end 
)
    and getUnresolvedIdentifiersSignature (s : RSignature) (typectx : typecontext) 
        (termctx : termcontext) (currentSName : StructureName.t) : (StructureNameSet.set * typecontext * termcontext) = 
        let 
        (* val _ = DebugPrint.p ("g" ^ Int.toString (length s)) *)
        fun ****(a, (b, c, d)) = (a***b, c,d)
        infix 7 ****
        in
          (
            case s of
            [] => (StructureNameSet.empty, typectx, termctx)
        | RTypeMacro (n, t)::ss => 
        getUnresolvedIdentifiersType t typectx termctx currentSName ****
            getUnresolvedIdentifiersSignature ss (addToCtxR n typectx currentSName) termctx currentSName
        | RTermTypeJudgment(n, t):: ss => 
        getUnresolvedIdentifiersType t typectx termctx currentSName ****
            getUnresolvedIdentifiersSignature ss typectx (addToCtxR n termctx currentSName) currentSName
        | RTermMacro(n, e) :: ss => 
        getUnresolvedIdentifiersExpr e typectx termctx currentSName ****
            getUnresolvedIdentifiersSignature ss typectx (addToCtxR n termctx currentSName) currentSName
        | RTermDefinition(n, e) :: ss => 
        getUnresolvedIdentifiersExpr e typectx termctx currentSName ****
            getUnresolvedIdentifiersSignature ss typectx (addToCtxR n termctx currentSName) currentSName
        | RStructure (vis, sName, decls) :: ss => 
        (case getUnresolvedIdentifiersSignature decls typectx termctx (currentSName@[sName]) of
            (freeSNames, newtypecontext, newtermcontext) =>
        freeSNames ****
            getUnresolvedIdentifiersSignature ss newtypecontext newtermcontext currentSName)
        | ROpenStructure openName :: ss =>
            getUnresolvedIdentifiersSignature ss 
                (addToCtxOpen openName typectx currentSName)
                (addToCtxOpen openName termctx currentSName) currentSName
        | RDirectExpr e :: ss=> 
            getUnresolvedIdentifiersSignature ss typectx termctx currentSName
          )
        end

    
    fun getUnresolvedIdentifiersSignatureTopLevel (s : RSignature)  (currentSName : StructureName.t) : StructureName.t list = 
        (StructureNameSet.toList (#1(getUnresolvedIdentifiersSignature s [] [] currentSName)))


(* attempts to find the identifer in the current strucuture *)
    fun findIdentifierInSignature (rsig : RSignature) ( sname : StructureName.t) : StructureName.t option = 
    let 
        fun findIdInTopLevel (rsig : RSignature) ( name : UTF8String.t) : UTF8String.t option = 
        case rsig of
            [] => NONE
            | RTypeMacro (n, t)::ss => if UTF8String.semanticEqual n name then SOME(n) else findIdInTopLevel ss name
            | RTermTypeJudgment(n, t):: ss => if UTF8String.semanticEqual n name then SOME(n) else findIdInTopLevel ss name
            | RTermMacro(n, e) :: ss => if UTF8String.semanticEqual n name then SOME(n) else findIdInTopLevel ss name
            | RTermDefinition(n, e) :: ss => if UTF8String.semanticEqual n name then SOME(n) else findIdInTopLevel ss name
            | _ => NONE
        fun findStructureInTopLevel (rsig : RSignature) ( name : UTF8String.t) : (UTF8String.t * RSignature) option = 
        case rsig of
            [] => NONE
            | RStructure (vis, sName, decls) :: ss => if UTF8String.semanticEqual sName name then SOME(sName, decls) else findStructureInTopLevel ss name
            | _ => NONE
    in
    case sname of 
        [] => raise Fail "inr151: sname is empty"
        | [s] => Option.map (fn x => [x]) (findIdInTopLevel rsig s)
        | (x :: xs) => case findStructureInTopLevel rsig x of 
                    SOME(sname, decls) => Option.map (fn x  => sname::x) (findIdentifierInSignature decls xs)
                    | NONE => NONE
    end

 *)

end
