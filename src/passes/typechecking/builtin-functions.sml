structure BuiltinFunctions = struct

open TypeCheckingAST

val typeBinderA = UTF8String.fromString "甲"
val typeBinderB = UTF8String.fromString "乙"
val typeBinderC = UTF8String.fromString "丙"

(* 'b. (('c. 'b -> 'c) -> 'b) -> 'b) *)
val callccType : Type = Forall (typeBinderB, 
    Func( Func( Forall(typeBinderC, Func(
        TypeVar [typeBinderB], TypeVar [typeBinderC]
    )) , TypeVar [typeBinderB]), TypeVar [typeBinderB])
    )
   

   fun typeOf (x : BuiltinFunc) : Type = case x of
    CallCC => callccType
end