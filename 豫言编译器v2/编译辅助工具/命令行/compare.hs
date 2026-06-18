
processCmdLineArgs : List String -> Unit
processCmdLineArgs = \arguments ->
	case arguments of 
		[] -> ()
		| "--"::t -> (runtimeCommandLineArgs := t)
		| "-v" :: t -> (debugPrintRoughMessages := True; processCmdLineArgs t)
		| "-vv" :: t -> (debugPrintDetailedMessages := True; 
						debugPrintRoughMessages := True;
						processCmdLineArgs t
				)
		| arg :: t -> (
				if (String.startsWith arg "-")
				then raise Fail "Unrecognized Command Line Option : " ^ ar
				else (filesToCompile := (!filesToCompile) @ [arg]; 
						processCmdLineArgs t
					)
        )
			