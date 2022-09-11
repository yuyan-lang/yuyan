structure FileDependencyResolution =
struct

open CompilationStructure

    fun constructFileDependencyInfo (tc : TypeCheckingAST.CSignature) : dependency list = 
    let open TypeCheckingAST
        fun recurExpr (e : TypeCheckingAST.CExpr) : dependency list = 
        case e of 
            CBlock(decls) => recurSig decls
            | CBlockProj(e, lbl, idx) => recurExpr e
            | CVar(_) => []
            | CStringLiteral _ => []
            | CPiType(t2, x, t1, _) => recurExpr t2 @ recurExpr t1
            | CUniverse => [] 
            | CBoolConstant _ => [] 
            | CBuiltinType _ => []
            | CUnitExpr => []
            | CUnitType => []
            | CApp(e1, e2, _) => recurExpr e1 @ recurExpr e2
            | CLam(ev, e, _) => recurExpr e
            | CFfiCCall(_) => []
            | _ => raise Fail ("fdr11 : dependency for cexpr ni for " ^ PrettyPrint.show_typecheckingCExpr e)
        and recurSig (x : TypeCheckingAST.CDeclaration list) : dependency list = 
        List.concat (List.map (fn x => case x of 
            TypeCheckingAST.CImport(x, y) => [(y,x)]
            | CTermDefinition(name, tm, tp) => recurExpr (tm) @ recurExpr tp
            | CConstructorDecl(name, tp, cconsinfo) => recurExpr (tp) 
            | _ => []
        ) x)
    in
        recurSig tc
    end



    


end
