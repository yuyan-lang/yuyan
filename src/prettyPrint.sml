structure PrettyPrint : 
sig
  val show_rawast : RawAST.RawAST -> string
  val show_opast : Operators.OpAST-> string
end =
struct


  fun show_rawast x = let 
    open RawAST
    in case x of 
       RawList l => "("^ (String.concatWith ", " (map show_rawast l)) ^")"
      | RawID s => s
      end

  fun show_op x = let 
    open Operators
    in case x of
      Operator(p, fix, assoc, names) =>
      let val baseName = String.concatWith underscoreChar names
      in case fix of
          Prefix => underscoreChar ^ baseName
          | Infix => underscoreChar ^ baseName ^ underscoreChar 
          | Postfix => baseName ^ underscoreChar 
          | Closed => baseName
        end
    end

  fun show_opast x = let 
    open Operators
    in case x of 
      OpAST (oper, l) => (show_op oper) ^ "[" ^ String.concatWith ", " (map show_opast l) ^ "]"
    end

end