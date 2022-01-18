
structure LspDiagnostics = struct

    open CompilationStructure
    open CompilationManager
    open StaticErrorStructure
    fun collectAndSendAllDiagnosticsJSONs (cm : compilationmanager) : JSON.value list = 
        let val ferrsl : (string * StaticErrorStructure.errlist) list = CompilationManager.collectAllDiagnostics cm
        open JSON
        fun severityToInt (severity : diagnosticseverity) = case severity of
     DiagnosticError  =>  1
     | DiagnosticWarning  => 2 
     | DiagnosticInformation  => 3
     | DiagnosticHint => 4

        fun toDiagnosticJSON (err : StaticErrorStructure.staticerror) =
            case err of 
                StaticError(locStr, severity, msgHd, msgBody) =>
                    case UTF8String.getSourceRange locStr of
                        SourceRange.StartEnd(_, ls, cs, le, ce) =>
                            OBJECT[
                                ("range", OBJECT [
                                    ("start", OBJECT [
                                        ("line", INT (IntInf.fromInt ls)),
                                        ("character", INT (IntInf.fromInt cs))
                                    ]),
                                    ("end", OBJECT [
                                        ("line", INT (IntInf.fromInt le)),
                                        ("character", INT (IntInf.fromInt ce))
                                    ])
                                ]),
                                ("severity",INT (IntInf.fromInt (severityToInt severity))),
                                (* ("code", STRING msgHd), *)
                                ("message", STRING (case msgBody of SOME s => (msgHd ^ "\n" ^s) | NONE => msgHd))
                            ]


        fun toErrJson (path, errl) = 
        OBJECT [
            ("jsonrpc", STRING "2.0"),
            ("method", STRING "textDocument/publishDiagnostics"),
            ("params", OBJECT [
                ("uri", STRING (URIUtil.pathToUri path)),
                ("diagnostics", ARRAY 
                    (map toDiagnosticJSON errl)
                )
            ])
        ]
        in 
            map toErrJson ferrsl
        end


end
