
structure PersistentKMachine =
struct
  open KMachine
    datatype pkvalue = PKUnit 
                  | PKVar of int
                  | PKTuple of pkvalue list
                  | PKInj of  int * pkvalue
                  | PKFold of pkvalue
                  | PKAbs of (int * pkcomputation)
    and pkcomputation = 
                  PKProj of pkcomputation * int
                | PKCases of pkcomputation * (int * pkcomputation) list
                | PKUnfold of pkcomputation 
                | PKApp of pkcomputation  * pkcomputation
                | PKRet of pkvalue
    
    fun fromKValue (kv : kvalue) : pkvalue = 
      case kv of
        KUnit => PKUnit
        | KVar i => PKVar i
        | KTuple l => PKTuple (map fromKValue l)
        | KInj (i, kv) => PKInj (i, fromKValue kv)
        | KFold e => PKFold (fromKValue e)
        | KAbs f => 
        let val boundId = UID.next()
                    in PKAbs(boundId, fromKComp (f (KVar boundId))) end

    and fromKComp (kv : kcomputation) : pkcomputation = 
    case kv of
        KProj(k, i) => PKProj(fromKComp k, i)
        | KCases(e, l) => PKCases ((fromKComp e),(map (fn f => 
                    let val boundId = UID.next()
                    in (boundId, fromKComp (f (KVar boundId))) end) l))
        | KUnfold(e) => PKUnfold (fromKComp e)
        | KApp(c1, c2) => PKApp(fromKComp c1, fromKComp c2)
        | KAppWithEvaledFun (v, c2) => PKApp(PKRet(fromKValue (KAbs v)), fromKComp c2)
        | KRet(v) => PKRet(fromKValue v)

    structure Ctx = RedBlackDict(structure Key=IntOrdered)
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
        | PKInj (i, kv) => KInj (i, toKValue ctx kv)
        | PKFold e => KFold (toKValue ctx e)
        | PKAbs (i, c) => KAbs(fn v => toKComp (insert ctx (i, v)) c)

    and toKComp (ctx: context) (kv : pkcomputation) : kcomputation = 
     case kv of
        PKProj(k, i) => KProj(toKComp ctx k, i)
        | PKCases(e, l) => KCases ((toKComp ctx e),(map (fn (i,c) => 
                    fn v => toKComp (insert ctx (i, v)) c) l ))
        | PKUnfold(e) => KUnfold (toKComp ctx e)
        | PKApp(c1, c2) => KApp(toKComp ctx c1, toKComp ctx c2)
        | PKRet(v) => KRet(toKValue ctx v)


end