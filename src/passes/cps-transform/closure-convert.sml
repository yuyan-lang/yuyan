structure ClosureConvert = struct


open CPSAst
open CPSAstOps

fun cpsvarToL i = (
        case i of CPSVarLocal i => [i]
        | CPSVarGlobal _ => [])

fun closureConvertCont ( (i, c) : cpscontinuation) : IntSet.set * cpscontinuation =
    let val (cfree, c') = closureConvert c
    in (remove cfree (cpsvarToL i) , (i, c')) end
and closureConvert (s : cpscomputation) : (IntSet.set * cpscomputation) =
let val cck  = closureConvertCont
fun fv (CPSValueVar v) = fromList (cpsvarToL v)
fun fvi (CPSValueVar v) : int list = cpsvarToL v
fun ***(a,b) = IntSet.union a b
infix 4 ***
in
    case s of   
        CPSUnit(k) => let val (kfree, k') = cck k 
                      in (kfree, CPSUnit(k')) end
        | CPSProj(v, i, k) => 
                let val (kfree, k') = cck k
                in (fv v *** kfree, CPSProj(v, i, k'))
                end
        | CPSCases(v, ks) => 
            let val fks' = map cck ks
            in ((fv v *** (foldr (op***) (fromList []) (map (fn x => #1 x) fks')),
                CPSCases(v, map (fn x => #2 x) fks')))
            end
        | CPSUnfold(v, k) => 
            let val (kfree, k') = cck k
            in (fv v *** kfree, CPSUnfold(v, k'))
            end
        | CPSApp(a, (b, c)) => (fromList (List.concat [fvi a, fvi b, fvi c]), CPSApp(a, (b,c)))
        | CPSAppSingle(a, b) => (fromList (List.concat [fvi a, fvi b]), CPSAppSingle(a,b))
        (* | CPSFix((a, b, c), k) => remove (freeVars c) [a, b] *** fk k *)
        | CPSTuple(l, k) =>
            let val (kfree, k') = cck k
            in (fromList (List.concat (map fvi l)) *** kfree, CPSTuple(l, k'))
            end
        | CPSInj(l, i, v, k) => 
            let val (kfree, k') = cck k
        in (fv v *** kfree, CPSInj(l, i, v, k'))
        end
        | CPSFold(v, k) => 
            let val (kfree, k') = cck k
            in (fv v *** kfree, CPSFold(v, k'))
             end
        | CPSAbsSingle((a,c), NONE , k) => 
            let val (cfree, c') = closureConvert c
            val (kfree, k') = cck k
            val cfreeExcepta = remove cfree ([a])
            in (cfreeExcepta *** kfree, CPSAbsSingle((a,c'), SOME (IntSet.toList cfreeExcepta), k'))
            end
        | CPSAbs((a, b, c),NONE, k) => 
            let val (cfree, c') = closureConvert c
            val (kfree, k') = cck k
            val cfreeExceptab = remove cfree ([a, b])
            in
            (cfreeExceptab *** kfree, CPSAbs((a,b,c'), SOME (IntSet.toList cfreeExceptab), k'))
            end
        | CPSBuiltinValue(v, k) => 
            let val (kfree, k') = cck k
            in (kfree, CPSBuiltinValue(v,k'))
            end
        | CPSDone v => (fv v, CPSDone v)
        | CPSFfiCCall (name, vs, k) => 
         let val (kfree, k') = cck k
            in (fromList (List.concat (map fvi vs)) *** kfree, CPSFfiCCall(name, vs, k'))
             end
        | CPSAbsSingle(_, SOME _, _) => raise Fail "cvt69"
        | CPSAbs(_, SOME _, _) => raise Fail "cvt70"
            
end
fun closureConvertTopLevel (s : cpscomputation) :  cpscomputation =
    #2 (closureConvert s)
end
