structure BuiltinFunctions = struct

open TypeCheckingAST

val typeBinderA = UTF8String.fromString "甲"
val typeVarA = TypeVar [typeBinderA]
val typeBinderB = UTF8String.fromString "乙"
val typeVarB = TypeVar [typeBinderB]
val typeBinderC = UTF8String.fromString "丙"
val typeVarC = TypeVar [typeBinderC]

(* 'b. (('c. 'b -> 'c) -> 'b) -> 'b) *)
val callccType : Type = Forall (typeBinderB, 
    Func( Func( Forall(typeBinderC, Func(
        TypeVar [typeBinderB], TypeVar [typeBinderC]
    )) , TypeVar [typeBinderB]), TypeVar [typeBinderB])
    )
   
val newDynClsfdType : Type = 
    Forall(typeBinderB,
        Func(BuiltinType(BIString),
            Prod([
                ((UTF8String.fromString "创造值"), 
                    Func(TypeVar [typeBinderB], BuiltinType(BIDynClsfd))),
                ((UTF8String.fromString "分析值"), 
                    Forall(typeBinderC,
                        Func(Prod([
                            (UTF8String.fromString "值", BuiltinType(BIDynClsfd)),
                            (UTF8String.fromString "符合", Func(TypeVar [typeBinderB], TypeVar [typeBinderC])),
                            (UTF8String.fromString "不符合", Func(UnitType, TypeVar [typeBinderC]))
                        ])
                        , TypeVar ([typeBinderC])
                    )))
            ])
        )
    )


    val raiseType : Type = 
        Forall(typeBinderB, 
            Func(BuiltinType(BIDynClsfd),
            typeVarB)
        )
    val handleType : Type = 
        Forall(typeBinderB, 
            Func(Prod([
                ((UTF8String.fromString "尝试"), 
                    Func(UnitType, typeVarB)),
                ((UTF8String.fromString "遇异"),
                    Func(BuiltinType(BIDynClsfd), 
                    typeVarB
                    )
                )
            ]), typeVarB)
        )
   fun typeOf (x : BuiltinFunc) : Type = case x of
    BFCallCC => callccType
    | BFNewDynClsfdValueWithString => newDynClsfdType
    | BFRaise => raiseType
    | BFHandle => handleType


    fun parseStr(x : string) : BuiltinFunc option = case x of
          "《《内建：以当前续延调用》》" => SOME(BFCallCC)
        | "《《内建：新建动态分类》》" => SOME(BFNewDynClsfdValueWithString)
        | "《《内建：抛出异常》》" =>  SOME(BFRaise)
        | "《《内建：尝试运行》》" =>  SOME(BFHandle)
        | _ => NONE


end