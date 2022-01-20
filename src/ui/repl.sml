
structure IRepl =
struct

    fun addUIDynamic (s : string) = "豫☯ " ^ s ^ "\n"
    fun addUIStatic (s : string) = "豫䷏ " ^ s ^ "\n"


    (* fun process (input : string ) (options : ReplOptions.t ) (filename : string): string =
    let 
        val res = TypeCheckAndEval.typeCheckAndEval input options filename
    in
        ""
    end *)

    fun computeLocalFilename (fileNameOfLib : string) (fileNameInLib : string) : string = 
        OS.Path.concat (OS.Path.getParent(fileNameOfLib), fileNameInLib)

    (* fun readFile (filename : string) : string =
        let 
        val content = (TextIO.inputAll (TextIO.openIn filename))
        in if String.isSuffix "。豫库" filename
           then (* rec process filecontent *)
                (let val names = String.tokens (fn c => Char.isSpace c) content
                in String.concatWith "\n" (map (fn x =>readFile 
                (computeLocalFilename filename x)) names)
                end)
            else content
        end *)
    fun inputFile  (filename : string) (options : ReplOptions.t): unit = 
         (TypeCheckAndEval.typeCheckAndEval options filename NONE; ())

    val aboutText = "豫言 ☯  (v0.1.0alpha) 以：yy r[kvv] filename.yuyan\n"

    fun main() : unit = 
        (* arg.sml *)
        let
        val args = CommandLine.arguments()
        in case args of
             [cmd] => if cmd = "lsp" 
             then LanguageServerMode.startLSP()
             else (print aboutText; OS.Process.exit OS.Process.failure : unit)
             | (cmd :: fname :: []) => (TypeCheckAndEval.typeCheckAndEval 
                {
                    verbose=(
                    if String.isSubstring "vv" cmd
                    then 2
                    else 
                    if String.isSubstring "v" cmd
                    then 1 else 0
                    ),
                    usekmachine = String.isSubstring "k" cmd, 
                    exitOnFailure = true
                } fname (
                    if String.isSubstring "profileLSPTokens"  cmd
                    then SOME (ReplDebug.profileLSPTokens)
                    else NONE
                ))
            | _ => (print aboutText; OS.Process.exit OS.Process.failure : unit)
            end

     fun testMain()= 
        inputFile "testfiles/test。豫库" {
            verbose=2, usekmachine=true, exitOnFailure= false
        }
    fun test f = 
        inputFile f {
            verbose=2, usekmachine=false, exitOnFailure= false
        }
    fun testk f = 
        inputFile f {
            verbose=2, usekmachine=true, exitOnFailure= false
        }

end