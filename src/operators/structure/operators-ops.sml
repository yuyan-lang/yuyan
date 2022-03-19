
structure OperatorsOps = struct
    open Operators

    fun reconstructWithArgs(oper as Operator(_, fix, _, comps, _) : operator) (args : UTF8String.t list) = 
    let val allComps = case fix of
          Prefix =>  comps @[OpCompExpr] 
          | Infix => OpCompExpr :: comps @[ OpCompExpr ]
          | Postfix =>OpCompExpr :: comps
          | Closed => comps
    val _ = if length allComps <> length (getStringComponents oper) + length args  then 
          raise Fail ("Operator has an incorrect number of arguments expecting " ^ 
            Int.toString (length allComps) ^ " actual " ^ Int.toString (length (getStringComponents oper) + length args)
            ^ " " ^ " in " ^ PrettyPrint.show_op oper ^ " they are " ^ String.concatWith " ;; " (map UTF8String.toString args) ^ " respectively " 
            ^ "TODO: fix in tcerrs line 43" )
          else ()
        val res = foldl (fn (elem , (rargs, acc))=>
          case elem of 
            OpCompString s => (rargs, acc@[s])
            | _ => (case rargs of 
                (h:: trargs) => (trargs, acc@[h])
                | _ => raise Fail "impossible by length counting"
              )
        ) (args, []) allComps
      in
      List.concat (#2 res)
      end 

end