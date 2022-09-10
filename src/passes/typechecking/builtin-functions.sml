structure BuiltinFunctions = struct

open TypeCheckingAST

val typeBinderA = UTF8String.fromString "甲"
val typeVarA = CVar ([typeBinderA], CVTBinder)
val typeBinderB = UTF8String.fromString "乙"
val typeVarB = CVar ([typeBinderB], CVTBinder)
val typeBinderC = UTF8String.fromString "丙"
val typeVarC = CVar ([typeBinderC], CVTBinder)

fun cFunc (t1, t2) = CPiType(t1, NONE, t2, Explicit)

fun cForall (tv, t) = CPiType(CUniverse, SOME tv, t, Explicit)
fun cForallImplicit (tv, t) = CPiType(CUniverse, SOME tv, t, Implicit)
(* 'b. (('c. 'b -> 'c) -> 'b) -> 'b) *)
val callccType : CType = cForallImplicit (typeBinderB, 
    cFunc( cFunc( cForallImplicit(typeBinderC, cFunc(
        typeVarB, typeVarC
    )) , typeVarB), typeVarB)
    )
   
val newDynClsfdType : CType = 
    cForall(typeBinderB,
        cFunc(CBuiltinType(BIString),
            CLabeledProd([
                ((UTF8String.fromString "创造值"), 
                    cFunc(typeVarB, CBuiltinType(BIDynClsfd))),
                ((UTF8String.fromString "分析值"), 
                    cForall(typeBinderC,
                        cFunc(CLabeledProd([
                            (UTF8String.fromString "值", CBuiltinType(BIDynClsfd)),
                            (UTF8String.fromString "符合", cFunc(typeVarB, typeVarC)),
                            (UTF8String.fromString "不符合", cFunc(CUnitType, typeVarC))
                        ])
                        , typeVarC
                    )))
            ])
        )
    )


    val raiseType : CType = 
        cForall(typeBinderB, 
            cFunc(CBuiltinType(BIDynClsfd),
            typeVarB)
        )
    val handleType : CType = 
        cForall(typeBinderB, 
            cFunc(CLabeledProd([
                ((UTF8String.fromString "尝试"), 
                    cFunc(CUnitType, typeVarB)),
                ((UTF8String.fromString "遇异"),
                    cFunc(CBuiltinType(BIDynClsfd), 
                    typeVarB
                    )
                )
            ]), typeVarB)
        )

    val intSubType : CType = cFunc(CBuiltinType(BIInt), cFunc(CBuiltinType(BIInt), CBuiltinType(BIInt)))
    val intEqType : CType = cFunc(CBuiltinType(BIInt), cFunc(CBuiltinType(BIInt), CBuiltinType(BIBool)))

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