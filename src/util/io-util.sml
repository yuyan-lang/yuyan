structure IOUtil =
struct

    fun makeAllParentDirectories(fp : string) = 
        let val baseDir = PathUtil.getBaseDir fp
            fun recCreateFolder (folderPath :string) = 
                if PathUtil.exists folderPath
                then ()
                else let val parentDir = PathUtil.getBaseDir folderPath
                     in (recCreateFolder parentDir; OS.FileSys.mkDir  folderPath)
                     end
        in  recCreateFolder baseDir end

    fun writeFile(fp : string) (content : string) : unit = 
        let val _ = makeAllParentDirectories fp
            val os = TextIO.openOut fp
            val _ = TextIO.output(os, content)
            val _ = TextIO.flushOut os
            val _ = TextIO.closeOut os
        in () end



    fun writeFileFpUTF8(fp : FileResourceURI.t) (content : UTF8String.t) : unit = 
        writeFile (FileResourceURI.access fp) (UTF8String.toString content)


end