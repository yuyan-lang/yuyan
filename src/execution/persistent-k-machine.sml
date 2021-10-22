
structure PersistentKMachine =
struct
  open KMachine
    datatype pkvalue = PKUnit 
                  | PKVar of int
                  | PKTuple of pkvalue list
                  | PKInj of Label*  int * pkvalue
                  | PKFold of pkvalue
                  | PKAbs of (int * pkcomputation)
                  | PKComp of pkcomputation
    and pkcomputation = 
                  PKProj of pkcomputation * int
                | PKCases of pkcomputation * (int * pkcomputation) list
                | PKUnfold of pkcomputation 
                | PKApp of pkcomputation  * pkcomputation
                | PKAppWithEvaledFun of (int * pkcomputation) * pkcomputation
                | PKRet of pkvalue
                | PKFix of (int * pkcomputation)
        
    fun substitutePKValueInValue (v2 : pkvalue) (x: int) (vc : pkvalue ) : pkvalue =
    case vc of
        PKUnit => PKUnit
        | PKVar i => if i = x then v2 else vc
        | PKTuple l => PKTuple (map (substitutePKValueInValue v2 x) l)
        | PKInj (l, i, kv) => PKInj (l, i, if i = x then kv else substitutePKValueInValue v2 x kv)
        | PKFold e => PKFold (substitutePKValueInValue v2 x  e)
        | PKAbs (i, c) => if i = x then vc else PKAbs(i, substitutePKValueInComp v2 x c)
        | PKComp c => PKComp(substitutePKValueInComp v2 x  c)
        (* assume every x is unique so no capture *)
    and substitutePKValueInComp (v : pkvalue) (x : int) (c : pkcomputation) : pkcomputation= 
      case c of
        PKProj(k, i) => PKProj(substitutePKValueInComp v x k, i)
        | PKCases(e, l) => PKCases ((substitutePKValueInComp v x e),(map (fn (i,c) => 
                    if i = x then (i, c) else (i,substitutePKValueInComp v x c)) l ))
        | PKUnfold(e) => PKUnfold (substitutePKValueInComp v x e)
        | PKApp(c1, c2) => PKApp(substitutePKValueInComp v x c1, substitutePKValueInComp v x c2)
        | PKRet(vc) => PKRet(substitutePKValueInValue v x vc)
        | PKFix(id, e) => if x = id then PKFix(id, e) else PKFix(id, substitutePKValueInComp v x e)
    
    fun fromKValue (kv : kvalue) : pkvalue = 
      case kv of
        KUnit => PKUnit
        | KVar i => PKVar i
        | KTuple l => PKTuple (map fromKValue l)
        | KInj (l, i, kv) => PKInj (l, i, fromKValue kv)
        | KFold e => PKFold (fromKValue e)
        | KAbs f => 
        let val boundId = UID.next()
                    in PKAbs(boundId, fromKComp (f (KVar boundId))) end
        | KComp c => PKComp (fromKComp c)

    and fromKComp (kv : kcomputation) : pkcomputation = 
    case kv of
        KProj(k, i) => PKProj(fromKComp k, i)
        | KCases(e, l) => PKCases ((fromKComp e),(map (fn f => 
                    let val boundId = UID.next()
                    in (boundId, fromKComp (f (KVar boundId))) end) l))
        | KUnfold(e) => PKUnfold (fromKComp e)
        | KApp(c1, c2) => PKApp(fromKComp c1, fromKComp c2)
        | KRet(v) => PKRet(fromKValue v)
        | KFix(f) => let 
              val boundId = UID.next() 
              in PKFix(boundId, fromKComp(f(KVar boundId))) end


    (* we should not need to convert pkmachine to naive k machine as this 
    conversion will slow down the actual computation *)
    (* structure Ctx = RedBlackDict(structure Key=IntOrdered)
    type context = kvalue Ctx.dict

      fun emptyCtx() = Ctx.empty
      fun lookup (ctx : context) (i : int ) : kvalue= 
        Ctx.lookup ctx i
      fun insert (ctx : context) ((i, v) : (int * kvalue)) = 
        Ctx.insert ctx i v

    fun toKValue (ctx : context) (kv : pkvalue) : kvalue = 
      case kv of
        PKUnit => KUnit
        | PKVar i => lookup ctx i
        | PKTuple l => KTuple (map (toKValue ctx) l)
        | PKInj (l, i, kv) => KInj (l, i, toKValue ctx kv)
        | PKFold e => KFold (toKValue ctx e)
        | PKAbs (i, c) => KAbs(fn v => toKComp (insert ctx (i, v)) c)
        | PKComp c => KComp(toKComp ctx c)

    and toKComp (ctx: context) (kv : pkcomputation) : kcomputation = 
     case kv of
        PKProj(k, i) => KProj(toKComp ctx k, i)
        | PKCases(e, l) => KCases ((toKComp ctx e),(map (fn (i,c) => 
                    fn v => toKComp (insert ctx (i, v)) c) l ))
        | PKUnfold(e) => KUnfold (toKComp ctx e)
        | PKApp(c1, c2) => KApp(toKComp ctx c1, toKComp ctx c2)
        | PKRet(v) => KRet(toKValue ctx v)
        | PKFix(id, e) => KFix(fn v => toKComp (insert ctx (id, v)) e)

                 *)

    type pkcont = (pkvalue -> pkcomputation) list

    datatype pkmachine = Run of pkcont * pkcomputation
                      | NormalRet of pkcont * pkvalue
                      (* | ExceptionRet of kcont * kvalue *)


    exception RunAfterFinal
    fun runOneStep (m : pkmachine): pkmachine = 
        case m of
            (* this is just hacking to compile fixed points *)
            NormalRet (s , PKComp(c)) => Run(s, c)
            | NormalRet ([], v) => raise RunAfterFinal
            | NormalRet ((f :: s), v) => Run (s,(f v))
            | Run (s, PKRet(v)) => NormalRet (s, v)
            | Run (s, PKProj(p,i)) => Run ( ((fn (PKTuple l) => PKRet(List.nth(l, i))) ::s), p)
            | Run (s, PKCases(p,l)) => Run ( ((fn (PKInj(_, i,v)) => ( case List.nth(l, i) of
                                                          (x, c) => substitutePKValueInComp v x c
                                                                        )) ::s), p)
            | Run (s, PKUnfold(p)) => Run ( ((fn (PKFold(v)) => PKRet(v)) ::s), p)
            | Run (s, PKApp(f, arg)) => Run ( ((fn (PKAbs f') => PKAppWithEvaledFun(f', arg)) ::s), f)
            | Run (s, PKAppWithEvaledFun((x,f), arg)) => Run ( ((fn argv => substitutePKValueInComp argv x f) ::s), arg)
            | Run (s, PKFix(x, f)) => Run ( s, substitutePKValueInComp (PKComp(PKFix(x, f))) x f)


    fun runUntilCompletion (m : pkmachine) (prt : pkmachine -> unit) : pkvalue = 
    (
        (* prt m; *)
        case m of 
            NormalRet ([], v) => v
            | _ => runUntilCompletion (runOneStep m) prt
    )

    fun pkvalueToString (prevPred : int ) (v : pkvalue) : UTF8String.t = 
    let fun pack curPred s = 
        if curPred < prevPred
        then SpecialChars.leftSingleQuote :: s @ [SpecialChars.rightSingleQuote]
        else s
        in
    case
    v of
        PKUnit => pack 72 (UTF8String.fromString "元")
        | PKVar i => UTF8String.fromString "ERR[Var: please report this as a bug on github]"
        | PKTuple l => pack 68 ( UTF8String.concatWith (UTF8String.fromString "与") (map (pkvalueToString 68) l))
        | PKInj (l, i, kv) => pack 67 (l @ UTF8String.fromString "临"@ pkvalueToString 67 kv)
        | PKFold e => pack 66 (UTF8String.fromString "卷" @ pkvalueToString 66 e)
        | PKAbs f => pack 52 (UTF8String.fromString "会？而？？？")
        | PKComp c => UTF8String.fromString "ERR[Comp: please report this as a bug on github]"
end
end