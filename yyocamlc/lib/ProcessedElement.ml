

open EngineData
module A = YYAbt
module Ext = AbtLib.Extent
module CS = CharStream
             
let get_scanned_char_t ((c, ext) : CS.t_char * Ext.t ) : input_acc_elem = 
  ParsingElem(ScannedChar(c), ext)

let get_bound_scanned_string_t ((c, ext) : CS.t_string * Ext.t) : input_acc_elem = 
  ParsingElem(BoundScannedString(c), ext)


let get_identifier_t ((c, ext) : CS.t_string * Ext.t) : input_acc_elem = 
  Expr(
    A.annotate_with_extent
    (A.fold(A.FreeVar(CS.get_t_string c)))
    (ext)
  )

let get_keyword_t ((keyword, ext) : CS.t_string * Ext.t)  : input_acc_elem = 
  ParsingElem(Keyword(keyword), ext)



let is_keyword (x : input_acc_elem) (string : string)  : bool = 
  match x with
  | ParsingElem(Keyword(s), _) -> CS.get_t_string s = string 
  | _ -> false
