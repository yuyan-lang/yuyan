structure KMachineOps = struct 
open KMachine
open PersistentKMachine
    exception InternalFailure of string
    (* we should not need to convert pkmachine to naive k machine as this 
    conversion will slow down the actual computation *)
    structure Ctx = RedBlackDict(structure Key=IntOrdered)
    type context = kvalue Ctx.dict

      fun emptyCtx() = Ctx.empty
      fun lookup (ctx : context) (i : int ) : kvalue= 
        Ctx.lookup ctx i
        handle Ctx.Absent =>
          raise InternalFailure ("INTERNAL FAILURE when looking up " ^ (Int.toString i) ^ " in context of size "^
          Int.toString(Ctx.size ctx) ^ "\n" ^ 
          String.concatWith "\n" (map (fn (i1, kvalue) => "(" ^ Int.toString i ^ " -> " ^PrettyPrint.show_pkvalue (fromKValue
        kvalue)) 
          (Ctx.toList ctx)) ^ "\n\n"
          )

      fun insert (ctx : context) ((i, v) : (int * kvalue)) = 
        Ctx.insert ctx i v

    fun toKValue (ctx : context) (kv : pkvalue) : kvalue = 
      (case kv of
        PKUnit => KUnit
        | PKVar i => lookup ctx i
        | PKTuple l => KTuple (map (toKValue ctx) l)
        | PKInj (l, i, kv) => KInj (l, i, toKValue ctx kv)
        | PKFold e => KFold (toKValue ctx e)
        | PKAbs (i, c) => KAbs(fn v => toKComp (insert ctx (i, v)) c)
        | PKComp c => KComp(toKComp ctx c)
        | PKBuiltinValue c => KBuiltinValue c)
        handle InternalFailure s =>
          raise InternalFailure (s ^ "\n when converting pkvalue " ^ PrettyPrint.show_pkvalue kv ^ " to value")

    and toKComp (ctx: context) (kv : pkcomputation) : kcomputation = 
     (case kv of
        PKProj(k, i) => KProj(toKComp ctx k, i)
        | PKCases(e, l) => KCases ((toKComp ctx e),(map (fn (i,c) => 
                    fn v => toKComp (insert ctx (i, v)) c) l ))
        | PKUnfold(e) => KUnfold (toKComp ctx e)
        | PKApp(c1, c2) => KApp(toKComp ctx c1, toKComp ctx c2)
        | PKRet(v) => KRet(toKValue ctx v)
        | PKFix(id, e) => KFix(fn v => toKComp (insert ctx (id, v)) e))
        handle InternalFailure s =>
          raise InternalFailure (s ^ "\n when converting pkcomp " ^ PrettyPrint.show_pkcomputation kv ^ " to value")

      
end
