
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

    fun smlnjMain ((name , args) : string * string list) =
        case args of
             [cmd] => if cmd = "lsp" 
             then (LanguageServerMode.startLSP(); OS.Process.success)
             else (print aboutText;  OS.Process.failure )
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
                    exitOnFailure = not (String.isSubstring "noExitOnFail" cmd),
                    compileOnly = String.isSubstring "compileOnly" cmd
                } fname (
                    if String.isSubstring "profileLSPTokens"  cmd
                    then SOME (ReplDebug.profileLSPTokens)
                    else if String.isSubstring "genDocs" cmd
                    then SOME(fn _ => fn cm => DocsGeneration.generateDocs 
                    (FileResourceURI.make (PathUtil.concat [(#pwd cm), "yylib"]))
                    (FileResourceURI.make (PathUtil.concat [(#pwd cm), ".yybuild", "docs"]))
                    cm
                    )
                    else NONE
                ); OS.Process.success)
            | _ => (DebugPrint.p aboutText;  OS.Process.failure )

    fun main() : unit = 
        (* arg.sml *)
        let
        val name = CommandLine.name()
        val args = CommandLine.arguments()
        in 
OS.Process.exit (smlnjMain(name, args))
            end

     (* fun testMain()= 
        inputFile "testfiles/test。豫库" {
            verbose=2, usekmachine=true, exitOnFailure= false
        } *)
    (* fun test f = 
        inputFile f {
            verbose=2, usekmachine=false, exitOnFailure= false
        } *)
    fun test f = 
        smlnjMain("<name>",["rv", f])

    fun testcmd cmd f = 
        smlnjMain("<name>",[cmd, f])
        
        
    (* fun testk f = 
        inputFile f {
            verbose=2, usekmachine=true, exitOnFailure= false
        } *)

end