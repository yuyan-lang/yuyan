structure PathUtil =
struct
fun makeAbsolute(possiblyRelative : string) (pwd : string) : string = 
let
            val absFp = (OS.Path.mkAbsolute {path=possiblyRelative, relativeTo=pwd})
        in absFp end

fun lsDir(dir : string) : string list = 
    let val dirStream = OS.FileSys.openDir dir
        fun files() = case OS.FileSys.readDir dirStream of 
            NONE => []
            | SOME f => f :: files()
        val paths = map (fn f => OS.Path.concat(dir, f))  (files())
    in paths end


(* list all files satisfying the predicate *)
fun globDir(dir : string) 
    (p : string -> bool) 
    : string list = 
    let val allPaths = lsDir dir
        val (directories, files) = List.partition OS.FileSys.isDir allPaths
    in List.filter p files @ List.concat (map (fn d => globDir d p) directories)
    end

fun concat (dir : string list) = 
    (* (DebugPrint.p ("concat: " ^ (PrettyPrint.show_plainstrlist dir)  ^" \n"); *)
    case dir of 
        [] => ""
        | [x] => x
        | (x :: y :: xs) => OS.Path.concat(x, concat(y :: xs))
    

fun makeRelative(path : string) (relativeTo : string)= 
      OS.Path.mkRelative {path = path, relativeTo = relativeTo}
end
