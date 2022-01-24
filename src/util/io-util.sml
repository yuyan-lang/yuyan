structure IOUtil =
struct

    fun writeFile(fp : string) (content : string) : unit = 
        let val os = TextIO.openOut fp
            val _ = TextIO.output(os, content)
            val _ = TextIO.flushOut os
            val _ = TextIO.closeOut os
        in () end

    fun writeFileFpUTF8(fp : FileResourceURI.t) (content : UTF8String.t) : unit = 
    writeFile (FileResourceURI.access fp) (UTF8String.toString content)


end