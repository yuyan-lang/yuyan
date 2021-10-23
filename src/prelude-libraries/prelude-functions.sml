structure PreludeFunctions = struct

    open TypeCheckingAST
    open KMachine
    val label1 = UTF8String.fromString "__BUILTIN_1"
    val label2 = UTF8String.fromString "__BUILTIN_2"

    val typeInt = BuiltinType(BIInt)
    val typeBool = BuiltinType(BIBool)
    val typeStr = BuiltinType(BIString)
    val typeReal = BuiltinType(BIReal)
    datatype preludeFunc = PFunc of string * Type * (kvalue -> kcomputation)

    val intFromString= PFunc("__BUILTIN_INT_FROM_STRING",
             Func(typeStr, typeInt), 
                (fn (KBuiltinValue(KbvString s)) => case Int.fromString (UTF8String.toString s) of
                    SOME(s) => KRet(KBuiltinValue(KbvInt s))
                    | NONE => raise KBuiltinFuncExecException ("String " ^ UTF8String.toString s ^ " is not an int")
                    ))
    val intAdd= PFunc("__BUILTIN_INT_ADD",
        Func(typeInt, Func(typeInt,typeInt)),
        (fn (KBuiltinValue(KbvInt i1)) => KRet(KAbs(fn (KBuiltinValue(KbvInt i2))=> 
            KRet(KBuiltinValue(KbvInt (i1 + i2)))))))
    val intLessThan= PFunc("__BUILTIN_INT_LESS_THAN",
        Func(typeInt, Func(typeInt,typeBool)),
        (fn (KBuiltinValue(KbvInt i1)) => KRet(KAbs(fn (KBuiltinValue(KbvInt i2))=> 
            KRet(KBuiltinValue(KbvBool (i1 < i2)))))))
    val intToString= PFunc("__BUILTIN_INT_TO_STRING",
        Func(typeInt, typeStr),
        (fn (KBuiltinValue(KbvInt i1)) => 
            KRet(KBuiltinValue(KbvString (UTF8String.fromString (Int.toString i1))))))
    val stringAppend= PFunc("__BUILTIN_STRING_APPEND",
        Func(typeStr, Func(typeStr, typeStr)),
        (fn (KBuiltinValue(KbvString s1)) => 
            KRet(KAbs(fn KBuiltinValue(KbvString s2) => 
                KRet(KBuiltinValue(KbvString( s1 @ s2 )))))))
    val printF= PFunc("__BUILTIN_PRINT",
        Func(typeStr, UnitType),
        (fn (KBuiltinValue(KbvString s1)) => 
            (print (UTF8String.toString s1); KRet(KUnit))
            )
        )
    val allPreludeFuncs = [
        intFromString, 
        intAdd,
        intLessThan,
        intToString,
        stringAppend,
        printF
    ]

    val preludeTypes = [
        ("__BUILTIN_TYPE_INT", BuiltinType(BIInt)),
        ("__BUILTIN_TYPE_BOOL", BuiltinType(BIInt)),
        ("__BUILTIN_TYPE_REAL", BuiltinType(BIInt)),
        ("__BUILTIN_TYPE_STRING", BuiltinType(BIString))
    ]
    val typeCheckingPrelude = map (fn (x, t) => TypeCheckingPass.TypeDef(UTF8String.fromString x, t)) preludeTypes @
        map (fn PFunc(name, t, impl) => TypeCheckingPass.TermTypeJ(UTF8String.fromString name, t)) allPreludeFuncs
    val kmachinePrelude = 
        map (fn PFunc(name, t, impl) => (UTF8String.fromString name,
         KBuiltinValue(KbvFunc(UTF8String.fromString name, impl)))) 
        allPreludeFuncs

end