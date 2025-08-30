let escaped_unicode (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
       match c with
       | '"' -> Buffer.add_string buf "\\\""
       | '\\' -> Buffer.add_string buf "\\\\"
       | '\n' -> Buffer.add_string buf "\\n"
       | '\t' -> Buffer.add_string buf "\\t"
       | '\r' -> Buffer.add_string buf "\\r"
       | c when Char.code c < 0x20 ->
         (* control chars -> decimal escape *)
         Buffer.add_string buf (Printf.sprintf "\\%03d" (Char.code c))
       | c ->
         (* leave everything else (including UTF-8) untouched *)
         Buffer.add_char buf c)
    s;
  Buffer.contents buf
;;
