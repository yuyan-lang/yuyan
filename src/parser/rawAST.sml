
structure RawAST
= struct

	datatype RawAST = RawID of UTF8Char.utf8char  (* We always want RawID to be a single Unicode Character! *)

	fun unId (RawID c) = c
end


