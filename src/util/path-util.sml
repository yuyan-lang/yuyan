structure PathUtil =
struct
fun makeAbsolute(possiblyRelative : string) (pwd : string) : string = 
let
            val absFp = (OS.Path.mkAbsolute {path=possiblyRelative, relativeTo=pwd})
        in absFp end
end
