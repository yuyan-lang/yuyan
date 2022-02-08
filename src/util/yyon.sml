structure YYON = struct
(* similar to json structure, but uses UTF8String.t *)

datatype value
  = OBJECT of (UTF8String.t * value) list
  | ARRAY of value list
  | NULL
  | BOOL of bool
  | INT of int
  | FLOAT of real
  | STRING of UTF8String.t

end