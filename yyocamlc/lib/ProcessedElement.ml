

open EngineData
module A = YYAbt
module Ext = AbtLib.Extent
module CS = CharStream
             
type t = A.t
let get_scanned_char_t ((c, ext) : CS.t_char * Ext.t ) : t = 
  A.annotate_with_extent
  (A.fold(A.N(N.ParsingElem(N.ScannedChar(c)), [])))
  ( ext)

let get_identifier_t ((c, ext) : CS.t_string * Ext.t) : t = 
  A.annotate_with_extent
  (A.fold(A.FreeVar(CS.get_t_string c)))
  (ext)

let get_keyword_t ((keyword, ext) : CS.t_string * Ext.t)  : t = 
  A.annotate_with_extent
  (A.fold(A.N(N.ParsingElem(N.Keyword(keyword)), [])))
  (ext)

let is_keyword (x : t) (string : string)  : bool = 
  match A.view x with
  | A.N(N.ParsingElem(N.Keyword(s)), _) -> CS.get_t_string s = string 
  | _ -> false