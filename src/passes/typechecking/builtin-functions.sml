structure BuiltinFunctions = struct

open TypeCheckingAST

val typeBinderA = UTF8String.fromString "甲"
val typeVarA = CVar ([typeBinderA], CVarTypeBinder)
val typeBinderB = UTF8String.fromString "乙"
val typeVarB = CVar ([typeBinderB], CVarTypeBinder)
val typeBinderC = UTF8String.fromString "丙"
val typeVarC = CVar ([typeBinderC], CVarTypeBinder)

(* 'b. (('c. 'b -> 'c) -> 'b) -> 'b) *)
val callccType : CType = CForall (typeBinderB, 
    CFunc( CFunc( CForall(typeBinderC, CFunc(
        typeVarB, typeVarC
    )) , typeVarB), typeVarB)
    )
   
val newDynClsfdType : CType = 
    CForall(typeBinderB,
        CFunc(CBuiltinType(BIString),
            CProd([
                ((UTF8String.fromString "创造值"), 
                    CFunc(typeVarB, CBuiltinType(BIDynClsfd))),
                ((UTF8String.fromString "分析值"), 
                    CForall(typeBinderC,
                        CFunc(CProd([
                            (UTF8String.fromString "值", CBuiltinType(BIDynClsfd)),
                            (UTF8String.fromString "符合", CFunc(typeVarB, typeVarC)),
                            (UTF8String.fromString "不符合", CFunc(CUnitType, typeVarC))
                        ])
                        , typeVarC
                    )))
            ])
        )
    )


    val raiseType : CType = 
        CForall(typeBinderB, 
            CFunc(CBuiltinType(BIDynClsfd),
            typeVarB)
        )
    val handleType : CType = 
        CForall(typeBinderB, 
            CFunc(CProd([
                ((UTF8String.fromString "尝试"), 
                    CFunc(CUnitType, typeVarB)),
                ((UTF8String.fromString "遇异"),
                    CFunc(CBuiltinType(BIDynClsfd), 
                    typeVarB
                    )
                )
            ]), typeVarB)
        )

    val intSubType : CType = CFunc(CBuiltinType(BIInt), CFunc(CBuiltinType(BIInt), CBuiltinType(BIInt)))
    val intEqType : CType = CFunc(CBuiltinType(BIInt), CFunc(CBuiltinType(BIInt), CBuiltinType(BIBool)))

   fun typeOf (x : BuiltinFunc) : CType = case x of
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