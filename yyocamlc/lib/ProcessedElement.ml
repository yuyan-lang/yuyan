

open EngineData
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

let is_keyword (x : t) (string : string) (id : int) : bool = 
  match A.view x with
  | A.N(N.ParsingElem(N.Keyword(s, id')), _) -> s = string && id = id'
  | _ -> false