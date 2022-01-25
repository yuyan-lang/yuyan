structure CPSAstOps =
struct

open CPSAst

structure IntSet = ListSet(structure Elem =IntOrdered)

type freevars = IntSet.set

fun remove( fv : freevars) (elems : int list) : freevars = 
    foldr (fn (e, acc) => IntSet.remove acc e) fv elems
fun insert( fv : freevars) (elems : int list) : freevars = 
    foldr (fn (e, acc) => IntSet.insert acc e) fv elems
fun fromList (elems : int list) : freevars  = 
    insert (IntSet.empty) elems

fun insertAndRemove( fv : freevars) (inserts : int list) (removes: int list) : freevars = 
    remove (insert fv inserts) (removes)

fun freeVarsCont ( (v, c) : cpscontinuation) : freevars=
    remove  (freeVars c) (case v of 
        CPSVarLocal i => [i]
        | CPSVarGlobal _ => [])

and freeVars (s : cpscomputation) : freevars =

let val fk  = freeVarsCont
fun fvarl (v) = case v of
      CPSVarLocal i => [i]
    | CPSVarGlobal _ => []

fun fv (CPSValueVar v) : freevars = 
       fromList (fvarl v)
fun fvi (CPSValueVar v) : int list =  fvarl v

fun ***(a,b) = IntSet.union a b
infix 4 ***
in
    case s of   
        CPSUnit(k) => fk k
        | CPSProj(v, i, k) => fv v  *** fk k
        | CPSCases(v, ks) => fv v *** (foldr (op***) (fromList []) (map fk ks))
        | CPSUnfold(v, k) => fv v *** fk k
        | CPSApp(a, (b, c)) => fromList (List.concat [fvi a, fvi b, fvi c])
        | CPSAppSingle(a, b) => fromList (List.concat [fvi a, fvi b])
        (* | CPSFix((a, b, c), k) => remove (freeVars c) [a, b] *** fk k *)
        | CPSTuple(l, k) => fromList (List.concat (map fvi l)) *** fk k
        | CPSInj(l, i, v, k) => fv v *** fk k
        | CPSFold(v, k) => fv v *** fk k
        | CPSAbsSingle((a,c),_, k) => remove (freeVars c) ( [a])*** fk k
        | CPSAbs((a, b, c),_, k) => remove (freeVars c) ( [a, b]) *** fk k
        | CPSBuiltinValue(_, k) => fk k
        | CPSDone v => fv v
        | CPSFfiCCall (n, l, k) => fromList (List.concat (map fvi l)) *** fk k
        | CPSStore(dst, src, cc )=> fromList (fvarl dst) *** fv src ***freeVars  cc
        | CPSSequence l =>  foldr (op***) (fromList []) (map freeVars l)
            
end
end
