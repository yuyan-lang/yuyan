structure BuiltinFunctions = struct

open TypeCheckingAST

val typeBinderA = UTF8String.fromString "甲"
val typeVarA = RVar [typeBinderA]
val typeBinderB = UTF8String.fromString "乙"
val typeVarB = RVar [typeBinderB]
val typeBinderC = UTF8String.fromString "丙"
val typeVarC = RVar [typeBinderC]

(* 'b. (('c. 'b -> 'c) -> 'b) -> 'b) *)
val callccType : RType = RForall (typeBinderB, 
    RFunc( RFunc( RForall(typeBinderC, RFunc(
        RVar [typeBinderB], RVar [typeBinderC]
    )) , RVar [typeBinderB]), RVar [typeBinderB])
    )
   
val newDynClsfdType : RType = 
    RForall(typeBinderB,
        RFunc(RBuiltinType(BIString),
            RProd([
                ((UTF8String.fromString "创造值"), 
                    RFunc(RVar [typeBinderB], RBuiltinType(BIDynClsfd))),
                ((UTF8String.fromString "分析值"), 
                    RForall(typeBinderC,
                        RFunc(RProd([
                            (UTF8String.fromString "值", RBuiltinType(BIDynClsfd)),
                            (UTF8String.fromString "符合", RFunc(RVar [typeBinderB], RVar [typeBinderC])),
                            (UTF8String.fromString "不符合", RFunc(RUnitType, RVar [typeBinderC]))
                        ])
                        , RVar ([typeBinderC])
                    )))
            ])
        )
    )


    val raiseType : RType = 
        RForall(typeBinderB, 
            RFunc(RBuiltinType(BIDynClsfd),
            typeVarB)
        )
    val handleType : RType = 
        RForall(typeBinderB, 
            RFunc(RProd([
                ((UTF8String.fromString "尝试"), 
                    RFunc(RUnitType, typeVarB)),
                ((UTF8String.fromString "遇异"),
                    RFunc(RBuiltinType(BIDynClsfd), 
                    typeVarB
                    )
                )
            ]), typeVarB)
        )

    val intSubType : RType = RFunc(RBuiltinType(BIInt), RFunc(RBuiltinType(BIInt), RBuiltinType(BIInt)))
    val intEqType : RType = RFunc(RBuiltinType(BIInt), RFunc(RBuiltinType(BIInt), RBuiltinType(BIBool)))

   fun typeOf (x : BuiltinFunc) : RType = case x of
    BFCallCC => callccType
    | BFNewDynClsfdValueWithString => newDynClsfdType
    | BFRaise => raiseType
    | BFHandle => handleType
    | BFIntSub => intSubType
    | BFIntEq => intEqType


    fun parseStr(x : string) : BuiltinFunc option = case x of
          "《《内建函数：以当前续延调用》》" => SOME(BFCallCC)
        | "《《内建函数：新建动态分类》》" => SOME(BFNewDynClsfdValueWithString)
        | "《《内建函数：抛出异常》》" =>  SOME(BFRaise)
        | "《《内建函数：尝试运行》》" =>  SOME(BFHandle)
        | "《《内建函数：整数：减》》" =>  SOME(BFIntSub)
        | "《《内建函数：整数：相等》》" =>  SOME(BFIntEq)
        | _ => NONE


end