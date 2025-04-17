
module YYNode  = struct
  type builtin = String of string
               | Int of int
               | Bool of bool
               | Unit
               | UnderscorePattern (* 「」  or （） *)

  type parsing_elem = ScannedChar of string
                    | Keyword of string * int (* id of the operator responsible for this keyword *)

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
    | UnderscorePattern -> "_"

  let show_parsing_elem (p : parsing_elem) : string =
    match p with
    | ScannedChar (s) -> "UnknownChar(" ^ s ^ ")"
    | Keyword (s, _) -> "Keyword(" ^ s ^ ")"

  let show (t : t) : string =
    match t with
    | Builtin (b) -> "Builtin(" ^ show_builtin b ^ ")"
    | ParsingElem (p) -> "ParsingElem(" ^ show_parsing_elem p ^ ")"

end
module N = YYNode

module YYAbt = AbtLib.Abt(YYNode)
module A = YYAbt
module Ext = AbtLib.Extent
             
type t = A.t

let get_scanned_char_t (c : AbtLib.Extent.t_str) : t = 
  A.annotate_with_extent
  (A.fold(A.N(N.ParsingElem(N.ScannedChar(AbtLib.Extent.get_str_content c)), [])))
  (AbtLib.Extent.get_str_extent c)

let get_identifier_t (c : AbtLib.Extent.t_str) : t = 
  A.annotate_with_extent
  (A.fold(A.FreeVar(Ext.get_str_content c)))
  (AbtLib.Extent.get_str_extent c)

let get_keyword_t (keyword : Ext.t_str) (id : int) : t = 
  A.annotate_with_extent
  (A.fold(A.N(N.ParsingElem(N.Keyword(Ext.get_str_content keyword, id)), [])))
  (Ext.get_str_extent keyword)