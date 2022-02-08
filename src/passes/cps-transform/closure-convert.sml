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

    fun closureConvertCPSPop(x : cpsprimitiveop) : (IntSet.set * cpscomputation) = 
        let fun closureConvertBinaryPop((v1, v2, k) : cpsvalue * cpsvalue * cpscontinuation) 
            (f : cpsvalue * cpsvalue * cpscontinuation -> cpsprimitiveop) : (IntSet.set * cpscomputation)
        = let
            val (kfree, k') = cck k
            in (fv v1 *** fv v2 *** kfree, CPSPrimitiveOp(f (v1, v2, k')))
        end
    in case x of 
        CPSPOpIntEq(i) => closureConvertBinaryPop i CPSPOpIntEq
        | CPSPOpIntSub(i) => closureConvertBinaryPop i CPSPOpIntSub
    end

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
        | CPSIfThenElse(v, kt,kf) => 
            let val (ktfree, kt') = closureConvert kt
                val (kffree, kf') = closureConvert kf
            in (fv v *** ktfree *** kffree, CPSIfThenElse(v, kt', kf'))
            end
        | CPSUnfold(v, k) => 
            let val (kfree, k') = cck k
            in (fv v *** kfree, CPSUnfold(v, k'))
            end
        | CPSApp(a, (b, c)) => (fromList (List.concat [fvi a, fvi b, fvi c]), CPSApp(a, (b,c)))
        | CPSAppSingle(a, b) => (fromList (List.concat [fvi a, fvi b]), CPSAppSingle(a,b))
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
        (* | CPSSequence l => 
         let val fs = map closureConvert l
            in (((foldr (op*** ) (fromList []) (map (fn x => #1 x) fs)),
                CPSSequence(map (fn x => #2 x) fs)))
            end  *)
        | CPSStore (dst, src, cc) =>
            let val (fcc, cc') = closureConvert cc
            in
                (fromList (cpsvarToL dst) *** fv src *** fcc, CPSStore(dst,src, cc'))
            end
        | CPSDynClsfdIn(v, i, v2, k) => 
            let 
                val (kfree, k') = cck k
            in (kfree *** (fv v) *** (fv v2), CPSDynClsfdIn(v, i, v2, k')) 
            end
        | CPSDynClsfdMatch(v, (i, (a, c1)), c2) =>
            let 
                val (c1free, c1') = closureConvert c1
                val (c2free, c2') = closureConvert c2
                val c1free' = remove c1free (cpsvarToL a)
            in (fv v *** c1free' *** c2free, 
                CPSDynClsfdMatch(v, (i, (a, c1')), c2')
            )
            end
        | CPSPrimitiveOp(cpspop) => 
            closureConvertCPSPop cpspop

        | CPSAbsSingle(_, SOME _, _) => raise Fail "cvt69"
        | CPSAbs(_, SOME _, _) => raise Fail "cvt70"
            
end
fun closureConvertTopLevel (s : cpscomputation) :  cpscomputation =
    #2 (closureConvert s)
end
