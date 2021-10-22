
structure IRepl =
struct

    fun addUIDynamic (s : string) = "豫☯ " ^ s ^ "\n"
    fun addUIStatic (s : string) = "豫䷏ " ^ s ^ "\n"

    fun process (input : string ) (verbose : int ): string =
    let 
        val res = TypeCheckAndEval.typeCheckAndEval input verbose
    in
        ""
    end

    fun replHelper (input : string ) (verbose : int) : string =
        let 
        val startTime = Time.now()
        val res = process input verbose
        val endTime = Time.now()
        val duration : Time.time = Time.-(endTime,startTime)
        in 
            (res ^ "\n" ^ "Took " ^ (LargeInt.toString(Time.toMilliseconds(duration))) ^ "ms to complete\n")
        end

    fun inputFile (filename : string) (verbose : int): unit = 
         (replHelper (TextIO.inputAll (TextIO.openIn filename)) verbose; ())

    val aboutText = "豫言 ☯  (v0.1.0alpha) 以：yy r filename.yuyan\n"

    fun main() : unit = 
        (* arg.sml *)
        let
        val args = CommandLine.arguments()
        in case args of
             (cmd :: fname :: []) => (inputFile fname (
                if String.isSubstring "vv" cmd
                then 2
                else 
                if String.isSubstring "v" cmd
                then 1 else 0
                ))
            | _ => (print aboutText; OS.Process.exit OS.Process.failure : unit)
            end

     fun testMain()= 
        inputFile "testfiles/test.yuyan" 2

end