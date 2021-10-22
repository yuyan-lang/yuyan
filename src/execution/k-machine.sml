structure KMachine =
struct
    open TypeCheckingAST

    datatype kvalue = KUnit 
                  (* only used durning persistence, should not be used anywhere else *)
                  | KVar of int (* FOR DEBUG PURPOSES ONLY, do not use for compiling practical programs !!! *)
                  (* We should only run closed programs so KVar shouldn't appear for any practical purposes *)
                  | KTuple of (kvalue) list (* label is only for printing purposes *)
                  | KInj of  Label *int * kvalue (* label is only for printing purposes *)
                  | KFold of kvalue
                  | KAbs of (kvalue -> kcomputation)
                  | KComp of kcomputation
    and kcomputation = 
                  KProj of kcomputation * int
                | KCases of kcomputation * (kvalue -> kcomputation) list
                | KUnfold of kcomputation 
                | KApp of kcomputation  * kcomputation
                (* only used intermediately by the step machine, should not be used outside this module *)
                | KAppWithEvaledFun of (kvalue -> kcomputation)  * kcomputation (* only used intermediately *)
                | KRet of kvalue
                | KFix of kvalue -> kcomputation


    fun kvalueToString (prevPred : int ) (v : kvalue) : UTF8String.t = 
    let fun pack curPred s = 
        if curPred < prevPred
        then SpecialChars.leftSingleQuote :: s @ [SpecialChars.rightSingleQuote]
        else s
        in
    case
    v of
        KUnit => pack 72 (UTF8String.fromString "元")
        | KVar i => UTF8String.fromString "ERR[please report this as a bug on github]"
        | KTuple l => pack 68 ( UTF8String.concatWith (UTF8String.fromString "与") (map (kvalueToString 68) l))
        | KInj (l, i, kv) => pack 67 (l @ UTF8String.fromString "临"@ kvalueToString 67 kv)
        | KFold e => pack 66 (UTF8String.fromString "卷" @ kvalueToString 66 e)
        | KAbs f => pack 52 (UTF8String.fromString "会？而？？？")
        | KComp c => UTF8String.fromString "ERR[please report this as a bug on github]"
end


                  

    type kcont = (kvalue -> kcomputation) list

    datatype kmachine = Run of kcont * kcomputation
                      | NormalRet of kcont * kvalue
                      (* | ExceptionRet of kcont * kvalue *)


    exception RunAfterFinal
    fun runOneStep (m : kmachine): kmachine = 
        case m of
            (* this is just hacking to compile fixed points *)
            NormalRet (s , KComp(c)) => Run(s, c)
            | NormalRet ([], v) => raise RunAfterFinal
            | NormalRet ((f :: s), v) => Run (s,(f v))
            | Run (s, KRet(v)) => NormalRet (s, v)
            | Run (s, KProj(p,i)) => Run ( ((fn (KTuple l) => KRet(List.nth(l, i))) ::s), p)
            | Run (s, KCases(p,l)) => Run ( ((fn (KInj(_, i,v)) => (List.nth(l, i)(v))) ::s), p)
            | Run (s, KUnfold(p)) => Run ( ((fn (KFold(v)) => KRet(v)) ::s), p)
            | Run (s, KApp(f, arg)) => Run ( ((fn (KAbs f') => KAppWithEvaledFun(f', arg)) ::s), f)
            | Run (s, KAppWithEvaledFun(f', arg)) => Run ( ((fn argv => f' argv) ::s), arg)
            | Run (s, KFix(f)) => Run ( s, f(KComp(KFix(f))))


    fun runUntilCompletion (m : kmachine) (prt : kmachine -> unit) : kvalue = 
    (
        (* prt m; *)
        case m of 
            NormalRet ([], v) => v
            | _ => runUntilCompletion (runOneStep m) prt
    )

end