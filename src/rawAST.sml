
structure RawAST
= struct
	datatype RawAST = RawID of string 
                | RawList of RawAST list
                | Period

    fun isRawASTEmpty (r : RawAST) : bool = case r of 
        Period => True
        | RawList [] => True
        | _ => False
end


