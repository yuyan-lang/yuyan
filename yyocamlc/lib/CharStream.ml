type t =
  { filename : string
  ; str : string
  ; idx : int
  ; line : int
  ; col : int
  }

type t_char = { c : string }

let new_t_char (c : string) : t_char =
  assert (String.length c = Uchar.utf_decode_length (String.get_utf_8_uchar c 0));
  { c }
;;

type t_string = t_char list

let get_t_char (c : t_char) : string = c.c
let get_t_string (s : t_string) : string = String.concat "" (List.map get_t_char s)
let new_cs (filename : string) (s : string) : t = { filename; str = s; idx = 0; line = 0; col = 0 }

let get_next_char (cs : t) : (AbtLib.Extent.t_str * t) option =
  if cs.idx >= String.length cs.str
  then None
  else (
    let c = String.get_utf_8_uchar cs.str cs.idx in
    let c_size = Uchar.utf_decode_length c in
    let c_str = String.sub cs.str cs.idx c_size in
    let new_line = if c_str = "\n" then cs.line + 1 else cs.line in
    let new_col = if c_str = "\n" then 0 else cs.col + 1 in
    let next_cs = { filename = cs.filename; str = cs.str; idx = cs.idx + c_size; line = new_line; col = new_col } in
    let ext_str = AbtLib.Extent.str_with_extent c_str (cs.filename, (cs.line, cs.col), (new_line, new_col)) in
    Some (ext_str, next_cs))
;;

let has_next_char (cs : t) : bool = cs.idx < String.length cs.str

let peek_next_char (cs : t) : (t_char * AbtLib.Extent.t) option =
  if cs.idx >= String.length cs.str
  then None
  else (
    let c = String.get_utf_8_uchar cs.str cs.idx in
    let c_size = Uchar.utf_decode_length c in
    let c_str = String.sub cs.str cs.idx c_size in
    let new_line = if c_str = "\n" then cs.line + 1 else cs.line in
    let new_col = if c_str = "\n" then 0 else cs.col + 1 in
    let ext = cs.filename, (cs.line, cs.col), (new_line, new_col) in
    Some (new_t_char c_str, ext))
;;

let new_t_string (s : string) : t_char list =
  let stream = ref (new_cs "" s) in
  let result = ref [] in
  let exception Break in
  try
    while true do
      match get_next_char !stream with
      | None -> raise Break
      | Some (c, next_cs) ->
        stream := next_cs;
        result := new_t_char (AbtLib.Extent.get_str_content c) :: !result
    done
  with
  | Break -> List.rev !result
;;

let show_next_char (cs : t) : string =
  match get_next_char cs with
  | None -> "[EOF]"
  | Some (c, _) -> " " ^ AbtLib.Extent.get_str_content c
;;

let show_current_position (cs : t) : string =
  let current_pos = cs.idx in
  "line "
  ^ string_of_int cs.line
  ^ ", col "
  ^ string_of_int cs.col
  ^ ", char "
  ^ string_of_int current_pos
  ^ show_next_char cs
;;

let print_vscode_position (cs : t) : string = Printf.sprintf "%s:%d:%d" cs.filename (cs.line + 1) (cs.col + 1)

let show_cs (cs : t) : string =
  "CS [idx "
  ^ string_of_int cs.idx
  ^ ", line "
  ^ string_of_int cs.line
  ^ ", col "
  ^ string_of_int cs.col
  ^ ", nextchar "
  ^ show_next_char cs
  ^ "]"
;;

let print_cs (cs : t) : string =
  "CS [ "
  ^ cs.filename
  ^ ":"
  ^ string_of_int (cs.line + 1)
  ^ ":"
  ^ string_of_int (cs.col + 1)
  ^ ", nextchar "
  ^ show_next_char cs
  ^ "]"
;;
