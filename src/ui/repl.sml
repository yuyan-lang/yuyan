
structure IRepl =
struct

    fun addUIDynamic (s : string) = "豫☯ " ^ s ^ "\n"
    fun addUIStatic (s : string) = "豫䷏ " ^ s ^ "\n"


    fun process (input : string ) (options : ReplOptions.t ): string =
    let 
        val res = TypeCheckAndEval.typeCheckAndEval input options
    in
        ""
    end

    fun computeLocalFilename (fileNameOfLib : string) (fileNameInLib : string) : string = 
        OS.Path.concat (OS.Path.getParent(fileNameOfLib), fileNameInLib)

    fun readFile (filename : string) : string =
        let 
        val content = (TextIO.inputAll (TextIO.openIn filename))
        in if String.isSuffix "。豫库" filename
           then (* rec process filecontent *)
                (let val names = String.tokens (fn c => Char.isSpace c) content
                in String.concatWith "\n" (map (fn x =>readFile 
                (computeLocalFilename filename x)) names)
                end)
            else content
        end
    fun inputFile  (filename : string) (options : ReplOptions.t): unit = 
         (process  (readFile (filename)) options; ())

    val aboutText = "豫言 ☯  (v0.1.0alpha) 以：yy r[kvv] filename.yuyan\n"

    fun main() : unit = 
        (* arg.sml *)
        let
        val args = CommandLine.arguments()
        in case args of
             (cmd :: fname :: []) => (inputFile  fname
             {
                 verbose=(
                if String.isSubstring "vv" cmd
                then 2
                else 
                if String.isSubstring "v" cmd
                then 1 else 0
                ),
                usekmachine = String.isSubstring "k" cmd
             })
            | _ => (print aboutText; OS.Process.exit OS.Process.failure : unit)
            end

     fun testMain()= 
        inputFile "testfiles/test。豫库" {
            verbose=2, usekmachine=true
        }

end