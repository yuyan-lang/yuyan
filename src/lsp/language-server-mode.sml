

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
    (
        let (* hack to get content length *)
        val acc = ref (nil : TextPrimIO.vector_slice list) 
        val writer = TextPrimIO.WR { name = "jsonhack", chunkSize = 1, writeVec = SOME (fn v => (  acc := !acc@[v]; 1)), writeArr = NONE, writeVecNB = NONE, writeArrNB = NONE, block = NONE, canOutput = NONE, getPos = NONE, setPos = NONE, endPos = NONE, verifyPos = NONE, close = fn () => (), ioDesc = NONE }
        val stream = TextIO.mkOutstream (TextIO.StreamIO.mkOutstream (TextPrimIO.augmentWriter writer, IO.NO_BUF));
        val _ = JSONPrinter.print (stream, x);
        val contentLength = length(!acc)
        in
        TextIO.output (TextIO.stdOut, "Content-Length: "^Int.toString contentLength ^"\r\n\r\n");
        print ("Content-Length: "^Int.toString contentLength ^"\r\n\r\n");
        JSONPrinter.print(TextIO.stdOut, x); 
        (* TextIO.output (TextIO.stdOut, "\r\n");  *)
        TextIO.flushOut TextIO.stdOut
        end)

    exception InputLineException 
    exception InputNException of int
    exception InputFormatError of string
    exception MethodError of string
    fun inputLine() : string = 
        let 
        val line = TextIO.inputLine TextIO.stdIn
        in case line of 
            SOME s =>s
            | NONE => raise InputLineException 
        end





    fun handleInitJSONRPC (params: JSON.value ): LanguageServer.t = 
    let open JSON
    val _ = print "JSON PARSED SUCCESSFULLY"
    val _ = 
    sendJSON(
        OBJECT[
            ("jsonrpc", STRING "2.0"),
            ("id", INT 0),
            ("result", OBJECT[
                ("capabilities", OBJECT[
                    ("textDocumentSync", 
                        OBJECT[
                            ("openClose", BOOL true),
                            ("change", INT 1),
                            ("save", OBJECT [
                                ("includeText", BOOL true)
                            ])
                        ]
                    ),
                    (* features should be developed incrementally *)
                    (* ("hoverProvider", BOOL true),
                    ("declarationProvider", BOOL true),
                    ("definitionProvider", BOOL true),
                    ("typeDefinitionProvider", BOOL true),
                    ("implementationProvider", BOOL true),
                    ("referencesProvider", BOOL true),
                    ("documentHighlightProvider", BOOL true),
                    ("documentSymbolProvider", BOOL true), *)
                    ("semanticTokensProvider", OBJECT[
                        ("full", BOOL true),
                        ("legend", OBJECT[
                            ("tokenTypes", ARRAY
                                (map (fn x => STRING (SyntaxHighlight.TokenType.getString x)) SyntaxHighlight.TokenType.allTokenTypes)
                            ),
                            ("tokenModifiers", ARRAY
                                (map (fn x => STRING (SyntaxHighlight.TokenModifier.getString x)) 
                                SyntaxHighlight.TokenModifier.allTokenModifiers)
                            )
                        ])
                    ])
                ])
            ])
        ]
    )
    open JSONUtil
    in 
    LanguageServer.Server 
    ((*cm*) CompilationManager.initWithWorkingDirectory (asString (lookupField params "rootPath")))
    end

    fun getDocumentPath (params : JSON.value option) : string =
    let 
    open JSON
    open JSONUtil
        val fileUri = asString (get ((Option.valOf params),[SEL "textDocument", SEL "uri"]))
        val filePath = (URIUtil.uriToPath fileUri)
    in 
    filePath
    end



    fun handleJSONRequest (server: LanguageServer.t) (method: string) (params: JSON.value option)  : JSON.value = 
    let 
    open JSON
    open JSONUtil
    in 
    case method of
     "textDocument/semanticTokens/full" => SyntaxHighlight.highlightFile (getDocumentPath params) (LanguageServer.getCM server)
     | _ => OBJECT[]
    end
        
    fun handleJSONNotification (server :LanguageServer.t) (method: string) (params : JSON.value option) : unit = 
    let 
    open JSON
    open JSONUtil

    fun updateFileBufferContent(content : string) =
        CompilationManager.updateContentForFilepath
            (getDocumentPath params)
            (UTF8String.fromStringAndFile content 
                    (getDocumentPath params))
            (LanguageServer.getCM server)
    in 
    case method of
     "textDocument/didOpen" => updateFileBufferContent((asString (get ((Option.valOf params),[SEL "textDocument", SEL "text"]))))
     | "textDocument/didChange" => updateFileBufferContent((asString (get ((Option.valOf params),[SEL "contentChanges", SUB 0, SEL "text"]))))
     | _ => ()
    end


    fun readJSONFromLSP() : JSON.value = 
        let 
        val line = inputLine()
        in if String.isPrefix "Content-Length: " line
            then let 
                    val SOME contentLength = Int.fromString (String.extract (line, String.size "Content-Length: ", NONE))
                    val emptyLine = inputLine()
                    val jsonContent = TextIO.inputN (TextIO.stdIn, contentLength)
                    val _ = print jsonContent
                    val jsonValue = JSONParser.parse (JSONParser.openString jsonContent)
                in 
                    jsonValue 
                end
        else raise InputFormatError line
        end


        
    fun continueLSP (server : LanguageServer.t) : unit = 
    let 
    open JSON
    open JSONUtil
    val jsonValue = readJSONFromLSP()
    val method = asString (lookupField jsonValue "method") 
    val params = (findField jsonValue "params") 
    in  case findField jsonValue "id" of 
        SOME (id) => (let
                    val result = handleJSONRequest server method params
                    val jsonToSend = OBJECT[
                            ("jsonrpc", STRING "2.0"),
                            ("id", id),
                            ("result", result)
                        ]
                    val _ = sendJSON jsonToSend
                in
                    continueLSP(server)
                end)
    | NONE => (handleJSONNotification server method params; continueLSP(server))
    end
    handle JSONUtil.FieldNotFound (jv, s) => (print "Field Not Found Exeption"; print s; raise JSONUtil.FieldNotFound (jv, s))




    fun startLSP() : unit = 
        let 
        open JSONUtil
        val jsonValue = readJSONFromLSP()
    in
    if asString (lookupField jsonValue "method") = "initialize" 
    andalso
    asInt (lookupField jsonValue "id") = 0
    then 
        (continueLSP (handleInitJSONRPC (lookupField jsonValue "params")))
    else raise MethodError "Expecting the first json call to be method initialize, or the id must be zero" 
    end


end 