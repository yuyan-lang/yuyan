
structure PrintDiagnostics = struct 
open StaticErrorStructure
    fun showErr (err : staticerror)(cm : CompilationStructure.compilationmanager) : string = 
    let  val StaticError (pos, severity, msg) = err
        val SourceRange.StartEnd(fp, sl, sc, el, ec) = UTF8String.getSourceRange pos
        val relativePath = PathUtil.makeRelative fp (#pwd cm)
        val serverityMsg = case severity of
                DiagnosticError  => "错误(error)："
                | DiagnosticWarning  => "警告(warning)： "
                | DiagnosticInformation  => "信息(information)："
                | DiagnosticHint => "提示(hint)："
    in 
    relativePath ^ ":" ^  Int.toString (sl+1) ^ "." ^ Int.toString (sc +1)
    ^ "-" ^ Int.toString (el+1) ^ "." ^ Int.toString (ec+1)
    ^ ":" ^ serverityMsg ^ msg
    end
    fun showErrs (errl : errlist)(cm : CompilationStructure.compilationmanager) : string = 
    String.concatWith "\n" (map (fn e => showErr e cm) errl) ^ "\n"

end