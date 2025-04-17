
module YYNode  = struct
  type builtin = String of string
               | Int of int
               | Bool of bool
               | Unit

  type parsing_elem = UnknownChar of string
                       | Keyword of string

  type t = Builtin of builtin
         | ParsingElem of parsing_elem

  let arity (t : t) : int list option = 
    match t with
    | Builtin (_) -> Some([])
    | ParsingElem (_) -> Some([])


  let show_builtin (b : builtin) : string = 
    match b with
    | String (s) -> "\"" ^ s ^ "\""
    | Int (i) -> string_of_int i
    | Bool (b) -> string_of_bool b
    | Unit -> "unit"

  let show_parsing_elem (p : parsing_elem) : string =
    match p with
    | UnknownChar (s) -> "UnknownChar(" ^ s ^ ")"
    | Keyword (s) -> "Keyword(" ^ s ^ ")"

  let show (t : t) : string =
    match t with
    | Builtin (b) -> "Builtin(" ^ show_builtin b ^ ")"
    | ParsingElem (p) -> "ParsingElem(" ^ show_parsing_elem p ^ ")"

end
module N = YYNode

module YYAbt = AbtLib.Abt(YYNode)
module A = YYAbt
             
type t = A.t

let get_unknown_char_t (c : AbtLib.Extent.t_str) : t = 
  A.annotate_with_extent
  (A.fold(A.N(N.ParsingElem(N.UnknownChar(AbtLib.Extent.get_str_content c)), [])))
  (AbtLib.Extent.get_str_extent c)