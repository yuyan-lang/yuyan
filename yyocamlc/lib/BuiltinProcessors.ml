open EngineData
open ProcCombinators

module A = ProcessedElement.A
module Ext = AbtLib.Extent
module PElem = ProcessedElement
module CS = CharStream
module Env = Environment
  

let yy_keyword_chars = CharStream.to_utf8_list "。（）「」『』"
(* let top_level_identifier_pusher : unit proc_state_m = 
  read_any_char_except_and_push (CharStream.to_utf8_list "。（）「」『』\n\t\r"@[" "]) *)

let top_level_empty_space_ignore : unit proc_state_m = 
  read_one_of_char (CharStream.to_utf8_list " \n\t\r") >> ignore ()

(* parses a single identifier identifier is something that is quoted between 「 and 」 and without special chars 
*)
let identifier_parser : Ext.t_str proc_state_m = 
  let* _ = pnot (read_string (CS.to_utf8_list "「：")) in
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
  push_expect_state (Scanning InComment)

let comment_end : unit proc_state_m =
  let* cur_state = get_expect_state () in
  let* () = assertb (cur_state = Scanning InComment) in
  let* _read_end = read_string (CS.to_utf8_list "：」") in
  let* _ = pop_expect_state () in
  let* _comments = pop_input_acc_past (fun elem -> PE.is_keyword elem "「：" 0) in
  ignore ()

(* keep reading and ignore result when *)
let comment_middle : unit proc_state_m = 
  let* _ = pnot (comment_end) in
  let* read_char  = read_any_char () in
  let* _ = push_scanned_char read_char in
  ignore ()

let import_start : unit proc_state_m = 
  let* read_start = read_one_of_string 
    [CS.to_utf8_list "寻观";
     CS.to_utf8_list "寻" 
    ] in
  push_elem_on_input_acc (PElem.get_keyword_t read_start 0)

let import_end : unit proc_state_m = 
  let* read_end = read_one_of_string [CS.to_utf8_list "之书"] in
  let* content = pop_input_acc_past (fun elem -> PE.is_keyword elem "寻" 0 
    || PE.is_keyword elem "寻观" 0) in            
  push_elem_on_input_acc (PElem.get_keyword_t read_end 0)
  
let default_registry = [
  to_processor Expression "top_level_empty_space_ignore" top_level_empty_space_ignore;
  to_processor Expression "comment_start" comment_start;
  to_processor (Scanning InComment) "comment_middle" comment_middle;
  to_processor (Scanning InComment) "comment_end" comment_end;
] @ List.concat [
to_processor_list [Expression] "identifier_parser_pusher" identifier_parser_pusher;
 ]