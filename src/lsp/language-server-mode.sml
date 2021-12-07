

structure LanguageServerMode =
struct


    fun lspLog(f: string) : string -> unit = 
    let 
    val fhandle = TextIO.openOut f
    (* val fhandle = TextIO.stdErr *)
    in fn s => (TextIO.output (fhandle, (s)); TextIO.flushOut fhandle)
    end


    val print = lspLog "lsplog.txt"
    fun printLine l = case l of SOME j => print j | NONE => print "EMPTYLINE"

    fun sendJSON (x : JSON.value) : unit = 
    (print "will print";
        JSONPrinter.print(TextIO.stdErr, x);
        TextIO.output (TextIO.stdOut, "Content-Length: 73\r\n\r\n");
        JSONPrinter.print(TextIO.stdOut, x); TextIO.output (TextIO.stdOut, ""); TextIO.flushOut TextIO.stdOut)

    exception InputLineException 
    exception InputNException of int
    exception InputFormatError of string
    fun inputLine() : string = 
        let 
        val line = TextIO.inputLine TextIO.stdIn
        in case line of 
            SOME s =>s
            | NONE => raise InputLineException 
        end





    fun handleJSONRPC (x: JSON.value ): unit = 
    let open JSON
    val _ = print "PARSED SUCCESFULLY"
    in 
    sendJSON(
        OBJECT[
            ("jsonrpc", STRING "2.0"),
            ("id", INT 0),
            ("result", OBJECT[
                ("capabilities", OBJECT[
                    ("textDocumentSync", 
                    (* OBJECT[
                        ("openClose", BOOL true),
                        ("change", INT 1)
                    ] *)
                    INT 2
                    )
                ])
            ])
        ]
    )
    end
    fun startLSP() : unit = 
        let 
        val _ = print "read line"
        val line = inputLine()
        val _ = print "done read line"
        val _ = print line
        in if String.isPrefix "Content-Length: " line
            then let 
                val SOME contentLength = Int.fromString (String.extract (line, String.size "Content-Length: ", NONE))
                val emptyLine = inputLine()
                val jsonContent = TextIO.inputN (TextIO.stdIn, contentLength)
                val _ = print jsonContent
                val jsonValue = JSONParser.parse (JSONParser.openString jsonContent)
            in (handleJSONRPC jsonValue;
            startLSP())
            end
        else raise InputFormatError line

        end


end 