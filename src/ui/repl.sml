
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

    fun inputFile (filename : string) (options : ReplOptions.t): unit = 
         (process (TextIO.inputAll (TextIO.openIn filename)) options; ())

    val aboutText = "豫言 ☯  (v0.1.0alpha) 以：yy r[kvv] filename.yuyan\n"

    fun main() : unit = 
        (* arg.sml *)
        let
        val args = CommandLine.arguments()
        in case args of
             (cmd :: fname :: []) => (inputFile fname 
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
        inputFile "testfiles/test.yuyan" {
            verbose=2, usekmachine=false
        }

end