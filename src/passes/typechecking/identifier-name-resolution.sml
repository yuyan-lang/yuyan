structure IdentifierNameResolution =
struct
    open TypeCheckingAST
    
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
            List.exists (fn x => StructureName.semanticEqual x n) ctx
            orelse
            List.exists (fn x => StructureName.semanticEqual x (currentSName@n)) ctx

        


    fun getUnresolvedIdentifiersType (t : Type) (typectx : typecontext) 
        (termctx : termcontext) (currentSName : StructureName.t) : StructureNameSet.set = 
        case t of
                    TypeVar v => if lookupCtx typectx v currentSName 
                                 then StructureNameSet.empty else StructureNameSet.singleton v 
                    | UnitType => StructureNameSet.empty  
                    | Prod l => 
                    List.foldr (op ***) StructureNameSet.empty (map (fn (lbl, t) => getUnresolvedIdentifiersType t typectx termctx currentSName) l)
                    | NullType => StructureNameSet.empty  
                    | Sum l =>  
                    List.foldr (op ***) StructureNameSet.empty (map (fn (lbl, t) => getUnresolvedIdentifiersType t typectx termctx currentSName) l)
                    | Func (t1, t2) => 
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
    fun getUnresolvedIdentifiersExpr (e : RExpr) (typectx : typecontext) 
        (termctx : termcontext) (currentSName : StructureName.t) : StructureNameSet.set = 
         (let 
            val res = 
            case e of
            RExprVar v => 
            if lookupCtx termctx v currentSName
            then StructureNameSet.empty else StructureNameSet.singleton v
            | RUnitExpr => StructureNameSet.empty 
            | RTuple l => List.foldr (op ***) StructureNameSet.empty (map (fn e => getUnresolvedIdentifiersExpr e typectx termctx currentSName) l)
            | RProj(e, l) => getUnresolvedIdentifiersExpr e typectx termctx currentSName
            | RInj (l, e) => getUnresolvedIdentifiersExpr e typectx termctx currentSName
            | RCase(e,cases) => getUnresolvedIdentifiersExpr e typectx termctx currentSName ***
                    List.foldr (op ***) StructureNameSet.empty (map (fn (l, ev, e) => 
                getUnresolvedIdentifiersExpr e typectx ([ev]::termctx) currentSName) cases)
            | RLam(ev, eb) =>  getUnresolvedIdentifiersExpr e typectx ([ev]::termctx) currentSName 
            | RLamWithType (t, ev, eb) => 
                getUnresolvedIdentifiersType t typectx termctx currentSName  ***
                getUnresolvedIdentifiersExpr e typectx ([ev]::termctx) currentSName 
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
            | RLetIn(decls, e) => (case getUnresolvedIdentifiersSignature decls typectx termctx (currentSName@StructureName.localName()) of
            (* this is a trick, the newly added signature can almost never be referenced *)
            (freeSNames, newtypecontext, newtermcontext) =>
        freeSNames ***
            getUnresolvedIdentifiersExpr e newtypecontext newtermcontext currentSName
         )
        in res
        end 
)
    and getUnresolvedIdentifiersSignature (s : RSignature) (typectx : typecontext) 
        (termctx : termcontext) (currentSName : StructureName.t) : (StructureNameSet.set * typecontext * termcontext) = 
        let fun ****(a, (b, c, d)) = (a***b, c,d)
        infix 7 ****
        in
          (
            case s of
            [] => (StructureNameSet.empty, typectx, termctx)
        | RTypeMacro (n, t)::ss => 
        getUnresolvedIdentifiersType t typectx termctx currentSName ****
            getUnresolvedIdentifiersSignature s (addToCtxR n typectx currentSName) termctx currentSName
        | RTermTypeJudgment(n, t):: ss => 
        getUnresolvedIdentifiersType t typectx termctx currentSName ****
            getUnresolvedIdentifiersSignature s typectx (addToCtxR n termctx currentSName) currentSName
        | RTermMacro(n, e) :: ss => 
        getUnresolvedIdentifiersExpr e typectx termctx currentSName ****
            getUnresolvedIdentifiersSignature s typectx (addToCtxR n termctx currentSName) currentSName
        | RTermDefinition(n, e) :: ss => 
        getUnresolvedIdentifiersExpr e typectx termctx currentSName ****
            getUnresolvedIdentifiersSignature s typectx (addToCtxR n termctx currentSName) currentSName
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

end
