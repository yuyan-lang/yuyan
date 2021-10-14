structure PrettyPrint :> 
sig
  val show_rawast : RawAST.RawAST -> string
end =
struct

  fun show_rawast x = let 
    open RawAST
    in case x of 
      Period => "■"
      | RawList l => "("^ (String.concatWith ", " (map show_rawast l)) ^")"
      | RawID s => s
      end

end