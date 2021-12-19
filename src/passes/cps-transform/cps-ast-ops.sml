structure CPSAstOps =
struct

open CPSAst

structure IntSet = ListSet(structure Elem =IntOrdered)

type freevars = IntSet.set

fun remove( fv : freevars) (elems : int list) = 
    foldr (fn (e, acc) => IntSet.remove acc e) fv elems
fun insert( fv : freevars) (elems : int list) = 
    foldr (fn (e, acc) => IntSet.insert acc e) fv elems
fun fromList (elems : int list) = 
    insert (IntSet.empty) elems

fun insertAndRemove( fv : freevars) (inserts : int list) (removes: int list)= 
    remove (insert fv inserts) (removes)

fun freeVarsCont ( (i, c) : cpscontinuation) : freevars=
    remove  (freeVars c) ([i])

and freeVars (s : cpscomputation) : freevars =

let val fk  = freeVarsCont
fun fv (CPSVar v) = fromList [v]
fun fvi (CPSVar v) = v
fun ***(a,b) = IntSet.union a b
infix 4 ***
in
    case s of   
        CPSUnit(k) => fk k
        | CPSProj(v, i, k) => fv v  *** fk k
        | CPSCases(v, ks) => fv v *** (foldr (op***) (fromList []) (map fk ks))
        | CPSUnfold(v, k) => fv v *** fk k
        | CPSApp(a, (b, c)) => fromList [fvi a, fvi b, fvi c]
        | CPSAppSingle(a, b) => fromList [fvi a, fvi b]
        | CPSFix((a, b, c), k) => remove (freeVars c) [a, b] *** fk k
        | CPSTuple(l, k) => fromList (map fvi l) *** fk k
        | CPSInj(l, i, v, k) => fv v *** fk k
        | CPSFold(v, k) => fv v *** fk k
        | CPSAbsSingle(f, k) => fk f *** fk k
        | CPSAbs((a, b, c), k) => remove (freeVars c) [a,b] *** fk k
        | CPSDone  => fromList []
        | CPSBuiltinValue(_, k) => fk k
            
end
end
