open EngineData
open ProcCombinators

module A = ProcessedElement.A
module Ext = AbtLib.Extent
module PElem = ProcessedElement
module CS = CharStream
  

let yy_keyword_chars = CharStream.to_utf8_list "。（）「」『』"
(* let top_level_identifier_pusher : unit proc_state_m = 
  read_any_char_except_and_push (CharStream.to_utf8_list "。（）「」『』\n\t\r"@[" "]) *)

let top_level_empty_space_ignore : unit proc_state_m = 
  read_one_of_char (CharStream.to_utf8_list " \n\t\r") >> ignore

(* parses a single identifier identifier is something that is quoted between 「 and 」 and without special chars 
*)
let identifier_parser : Ext.t_str proc_state_m = 
  let* _ = read_one_of_char ["「"] in
  let* (middle, terminal) = scan_past_one_of_char yy_keyword_chars in
  if Ext.get_str_content terminal = "」" then
    match middle with
    | [] -> failwith ("Unit pattern not implemented")
    | _ -> return (Ext.join_t_str_list middle)
  else
    pfail ("ET100: Expected '」' but got " ^ Ext.get_str_content terminal)


let identifier_parser_pusher : unit proc_state_m = 
  let* id = identifier_parser in
  push_scanned_char id

let comment_start : unit proc_state_m = 
  let* read_start = read_string (CS.to_utf8_list "「：") in
  let* () = push_elem_on_input_acc (PElem.get_keyword_t read_start 0) in



  
let default_registry = [
  to_processor Expression top_level_empty_space_ignore;
]