
structure PrintDiagnostics = struct 
open StaticErrorStructure

    fun getSourceTextInfo ((sl, sc, el, ec) : int * int * int * int)(fileContent : UTF8String.t) : string = 
    let val lines = UTF8String.fields (fn c => UTF8Char.semanticEqual c SpecialChars.newline) fileContent
        val targetLine = List.nth(lines, sl)
        val (endIndexC, targetTildeLine) = List.foldl (fn (c, (index, acc)) => 
            if sl = el andalso  index >= ec  (* do nothing if already satisfied *)
            then (index+1, acc)
            else
            if UTF8Char.semanticEqual c SpecialChars.tab orelse UTF8Char.semanticEqual c SpecialChars.space
            then (index + 1,acc@[c])
            else 
            let 
                val wordWidth = UTF8Char.getWidth c
                val text = List.tabulate(wordWidth, (fn _ => 
                    if index >= sc then SpecialChars.tilde else SpecialChars.space
                ))
            in 
            (index+1, acc@text)
            end
        ) (0, []) targetLine
        val trailing = if sl <> el then ["及以后的" ^ Int.toString (el - sl) ^ "行"] else []
    in 
        String.concatWith "\n" ([UTF8String.toString targetLine, UTF8String.toString targetTildeLine]@trailing)
    end

    fun showErr (err : staticerror)(cm : CompilationStructure.compilationmanager) : string = 
    let  val StaticError (pos, severity, msghd, msgdetail) = err
        val SourceRange.StartEnd(fp, sl, sc, el, ec) = UTF8String.getSourceRange pos
        val CompilationStructure.CompilationFile file = CompilationManager.lookupFileByPath (FileResourceURI.make fp) cm
        val fileContent = case (#content file) of Success (c, _) => c | _ => raise Fail "pd13"
        val sourceTextInfo = getSourceTextInfo (sl, sc, el, ec) fileContent
        val relativePath = PathUtil.makeRelative fp (#pwd cm)
        (* https://misc.flogisoft.com/bash/tip_colors_and_formatting *)
        (* https://smlfamily.github.io/Basis/char.html *)
        val serverityMsg = case severity of
                DiagnosticError  => "\027[1;34m错误(error)：\027[0;49m"
                | DiagnosticWarning  => "警告(warning)： "
                | DiagnosticInformation  => "信息(information)："
                | DiagnosticHint => "提示(hint)："
        val detailMessage = case msgdetail of SOME t => "\n" ^ t | NONE => ""
    in 
    relativePath ^ ":" ^  Int.toString (sl+1) ^ "." ^ Int.toString (sc +1)
    ^ "-" ^ Int.toString (el+1) ^ "." ^ Int.toString (ec+1)
    ^ ":" ^ serverityMsg ^ msghd ^ "\n" 
    ^ sourceTextInfo ^  detailMessage
    end
    fun showErrs (errl : errlist)(cm : CompilationStructure.compilationmanager) : string = 
    String.concatWith "\n" (map (fn e => showErr e cm) errl) ^ "\n"

end