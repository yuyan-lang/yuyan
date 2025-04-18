open EngineData
open ProcCombinators

module A = ProcessedElement.A
module Ext = AbtLib.Extent
module PElem = ProcessedElement
module CS = CharStream
module Env = Environment
  

let yy_keyword_chars = CharStream.new_t_string "。（）「」『』"
(* let top_level_identifier_pusher : unit proc_state_m = 
  read_any_char_except_and_push (CharStream.new_t_string "。（）「」『』\n\t\r"@[" "]) *)

let top_level_empty_space_ignore : unit proc_state_m = 
  read_one_of_char (CharStream.new_t_string " \n\t\r") >> ignore ()

(* parses a single identifier identifier is something that is quoted between 「 and 」 and without special chars 
*)
let identifier_parser : (CS.t_string * Ext.t) proc_state_m = 
  let* _ = pnot (read_string (CS.new_t_string "「：")) in
  let* _ = read_one_of_char [CS.new_t_char "「"] in
  let* ((middle, middle_ext), (terminal, _)) = scan_past_one_of_char yy_keyword_chars in
  if CS.get_t_char terminal = "」" then
    match middle with
    | [] -> failwith ("Unit pattern not implemented")
    | _ -> return (middle, middle_ext)
  else
    pfail ("ET100: Expected '」' but got " ^ CS.get_t_char terminal)


let identifier_parser_pusher : unit proc_state_m = 
  let* id = identifier_parser in
  push_elem_on_input_acc (PElem.get_identifier_t id)

let comment_start : unit proc_state_m = 
  let* read_start = read_string (CS.new_t_string "「：") in
  let* () = push_elem_on_input_acc (PElem.get_keyword_t read_start) in
  push_expect_state (Scanning InComment)

let comment_end : unit proc_state_m =
  let* cur_state = get_expect_state () in
  let* () = assertb (cur_state = Scanning InComment) in
  let* _read_end = read_string (CS.new_t_string "：」") in
  let* _ = pop_expect_state () in
  let* _comments = pop_input_acc_past (fun elem -> PE.is_keyword elem ("「：")) in
  ignore ()

(* keep reading and ignore result when *)
let comment_middle : unit proc_state_m = 
  let* _ = pnot (comment_end) in
  let* read_char  = read_any_char () in
  let* _ = push_scanned_char read_char in
  ignore ()

let import_start : unit proc_state_m = 
  let* read_start = read_one_of_string 
    [CS.new_t_string "寻观";
     CS.new_t_string "寻" 
    ] in
  push_elem_on_input_acc (PElem.get_keyword_t read_start)

let import_end : binary_op = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "之书";
    left_precedence = 50;
    right_precedence = 50;
    fixity = Postfix;
    reduction = 
      let* prev_comp = pop_input_acc () in
      let* module_expr = Imports.get_module_expr prev_comp in
      push_elem_on_input_acc module_expr
  }

let definition_middle : binary_op = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "者";
    left_precedence = 0;
    right_precedence = 0;
    fixity = Infix;
    reduction = 
      let* (name, defn) = pop_bin_operand () in
      push_elem_on_input_acc (A.fold(A.N(N.Declaration(N.ConstantDefn), [[], name; [], defn])))
  }

  (* let* read_end = read_one_of_string [CS.new_t_string "之书"] in
  let* (content, directive) = pop_input_acc_past (fun elem -> PE.is_keyword elem "寻"
    || PE.is_keyword elem "寻观") in            
  push_elem_on_input_acc (PElem.get_keyword_t read_end) *)
  
let default_registry = [
  to_processor_complex Expression "top_level_empty_space_ignore" top_level_empty_space_ignore;
  to_processor_complex Expression "comment_start" comment_start;
  to_processor_complex (Scanning InComment) "comment_middle" comment_middle;
  to_processor_complex (Scanning InComment) "comment_end" comment_end;
  ProcBinOp import_end;
] @ List.concat [
to_processor_complex_list [Expression] "identifier_parser_pusher" identifier_parser_pusher;
 ]