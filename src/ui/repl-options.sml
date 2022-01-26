structure ReplOptions =
struct
    type reploptions = {verbose : int, usekmachine : bool, exitOnFailure: bool, compileOnly: bool, optimize:bool}
type t = reploptions
end
