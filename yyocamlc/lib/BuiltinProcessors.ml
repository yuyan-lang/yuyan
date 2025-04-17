open EngineData
open ProcCombinators

let top_level_identifier_pusher : unit proc_state_m = 
  read_any_char_except_and_push (CharStream.to_utf8_list "。（）「」『』\n\t\r"@[" "])

let top_level_empty_space_ignore : unit proc_state_m = 
  read_one_of_char (CharStream.to_utf8_list " \n\t\r") >> ignore



  
let default_registry = [
  (* to_processor TopLevel top_level_identifier_pusher; *)
  to_processor Expression top_level_empty_space_ignore;
]