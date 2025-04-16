
type t = {
  str : string;
  idx : int;
  line : int;
  col : int;
}

type t_char = string

let new_cs (s : string) : t = 
  { str = s; idx = 0; line = 0; col = 0 }

let get_next_char (cs : t) : (t_char * t) option = 
  if cs.idx >= String.length cs.str then
    None
  else
    let c = String.get_utf_8_uchar cs.str cs.idx in
    let c_size= Uchar.utf_decode_length c in
    let c_str = String.sub cs.str cs.idx c_size in
    let new_line = if c_str = "\n" then cs.line + 1 else cs.line in
    let new_col = if c_str = "\n" then 0 else cs.col + 1 in
    let next_cs = { str = cs.str; idx = cs.idx + c_size; line=new_line; col=new_col  } in
    Some (c_str, next_cs)

let has_next_char (cs : t) : bool = 
  cs.idx < String.length cs.str
  

let to_utf8_list (s : string) : t_char list = 
  let stream = ref (new_cs s) in
  let result = ref [] in
  let exception Break in
  try 
    while true do
      match get_next_char !stream with
      | None -> raise Break
      | Some (c, next_cs) -> 
          stream := next_cs;
          result := c :: !result
    done;
  with Break ->
    List.rev !result


let show_current_position (cs : t) : string = 
  let current_pos = cs.idx in
  "line " ^ string_of_int cs.line ^
  ", col " ^ string_of_int cs.col ^
  ", char " ^ string_of_int current_pos 
  ^ (
    match get_next_char cs with
    | None -> "[EOF]"
    | Some (c, _) -> " " ^ c
  )