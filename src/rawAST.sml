
structure RawAST
= struct
	datatype RawAST = RawID of string  (* We always want RawID to be a single Unicode Character! *)
                | RawList of RawAST list

    fun isRawASTEmpty (r : RawAST) : bool = case r of 
         RawList [] => true
        | _ => false
end


