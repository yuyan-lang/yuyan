
structure Repl =
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
    (* fun inputFile  (filename : string) (options : ReplOptions.t): unit = 
         (TypeCheckAndEval.typeCheckAndEval options filename NONE; ()) *)

    val aboutText = "豫言 ☯  (v0.1.0alpha) 以：yy filename.yuyan\n"

    fun outputPrint s = print (s)
    fun errPrint s = DebugPrint.p s

    open ReplOptions
    fun smlnjMain ((name , args) : string * string list) =
             if args = ["lsp", "--stdio"]
             then (LanguageServerMode.startLSP(); OS.Process.success)
             else 
                let val options = ArgumentParser.parseArgumentsTopLevel args
                val exitSt = 
                if getShowHelp options 
                then (outputPrint ReplHelpText.helpText; OS.Process.success)
                else if getShowVersion options
                then (outputPrint ReplHelpText.versionText; OS.Process.success)
                else if getShowAbout options
                then (outputPrint ReplHelpText.aboutText; OS.Process.success)
                else
                if length (getInputFiles options) < 1 
                then (errPrint ("错误，请至少指定一个文件\n" ^ ReplHelpText.helpText);
                        OS.Process.failure)
                else 
                    TypeCheckAndEval.typeCheckAndEval 
                        options
                        (* (
                            if String.isSubstring "showTokens"  cmd
                            then SOME (ReplDebug.showTokens)
                            else
                            if String.isSubstring "profileLSPTokens"  cmd
                            then SOME (ReplDebug.profileLSPTokens)
                            else if String.isSubstring "genDocs" cmd
                            then SOME(fn _ => fn cm => DocsGeneration.generateDocs 
                            (FileResourceURI.make (PathUtil.concat [(#pwd cm), "yylib"]))
                            (FileResourceURI.make (PathUtil.concat [(#pwd cm), ".yybuild", "docs"]))
                            cm
                            )
                            else NONE
                        ) *)
                in 
                    exitSt
                end
                handle ArgumentParser.ArgumentParseFailure f => (
                    errPrint (f ^ "\n" ^ ReplHelpText.helpText );
                        OS.Process.failure
                )

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
    (* fun test f = 
        smlnjMain("<name>",["rv", f]) *)

    fun testcmd l  = 
        smlnjMain("<name>",l)
        
        
    (* fun testk f = 
        inputFile f {
            verbose=2, usekmachine=true, exitOnFailure= false
        } *)

end