

functor MixFixParser (P : sig 
        val allOps :Operators.allOperators 
        end)  =
struct
 

    open RawAST
    open Operators

    structure Parser = PrecedenceParser(P)
    fun parseMixfixExpression (exp : RawAST.RawAST list) : OpAST list = 
        case exp of
            l =>  (case Parser.parseExpWithEOF() l of
                                [] => raise Fail "noParse 20"
                                | l => map (fn (x, _) => ElaboratePrecedence.elaborate x)
                                     (List.filter (fn (x, s) => 
                                     let 
                                     (* val w = print ("REST : " ^ String.concatWith ", " (map PrettyPrint.show_rawast s) ^ "\n");  *)
                                     (* val x = print (PrettyPrint.show_opast (ElaboratePrecedence.elaborate x));  *)
                                     val y = false
                                     in List.length s = 0 
                                     end) l))
            (* | RawID _ => raise Fail "Must be a list 22" *)

end