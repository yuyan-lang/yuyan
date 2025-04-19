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
  let* _read_end = read_string (CS.new_t_string "：」") in
  let* _ = pop_expect_state (Scanning InComment) in
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

let import_end_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "之书";
    left_precedence = 50;
    right_precedence = 50;
    fixity = Postfix;
  }
let import_end : binary_op = 
  {
    meta = import_end_meta;
    reduction = 
      let* (prev_comp, ext) = pop_postfix_operand (import_end_meta) in
      let* module_expr = Imports.get_module_expr prev_comp in
      push_elem_on_input_acc (A.annotate_with_extent module_expr ext)
  }

let definition_middle_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "者";
    left_precedence = 10;
    right_precedence = 10;
    fixity = Infix;
  }

let definition_middle : binary_op = 
  {
    meta = definition_middle_meta;
    reduction = 
      let* ((name, defn), ext) = pop_bin_operand definition_middle_meta in
      push_elem_on_input_acc (A.annotate_with_extent(A.fold(A.N(N.Declaration(N.ConstantDefn), [[], name; [], defn]))) ext)
  }

let library_root_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "藏书阁";
    left_precedence = 0;
    right_precedence = 0;
    fixity = ClosedIdentifier;
  }

let library_root : binary_op = 
  {
    meta = library_root_meta;
    reduction = 
    (
      let* ext = pop_closed_identifier_operand library_root_meta in 
      let default_path = Filename.concat (Sys.getcwd()) "藏书阁" in
      if Sys.file_exists default_path && Sys.is_directory default_path  then
        push_elem_on_input_acc (A.annotate_with_extent (A.fold(A.N(N.Builtin(N.Library default_path), []))) ext)
      else
        pfail ("Directory not found: " ^ default_path)
    )
  }

let known_structure_deref : unit proc_state_m =
  let* (_, _) = read_one_of_string [CS.new_t_string "之"] in
  let* input_top = pop_input_acc () in
  match A.view input_top with
  | A.N(N.Builtin(N.Library path), _) -> 
    (* check ending *)
    choice  (
      let* (_, ext_end) = read_one_of_char [CS.new_t_char "书"] in
      push_elem_on_input_acc (A.annotate_with_extent input_top (Ext.combine_extent (A.get_extent_some input_top) ext_end))
    )
    (* ls dir *)
    (
      if Sys.file_exists path && Sys.is_directory path then
        let new_expt_state = (Scanning (InLibrarySearch path)) in
        let* _ = push_expect_state new_expt_state in
        let files = Array.to_list (Sys.readdir path) in
        let all_new_procs = List.map (fun file -> 
          let file_name_without_ext = if String.ends_with ~suffix:"。豫" file then 
            String.sub file 0 (String.length file - String.length "。豫")
          else file in
          let file_name_id_parse : unit proc_state_m = (
              let* (_read_filename, read_filename_ext) = read_one_of_string [CS.new_t_string file_name_without_ext] in
              let* input_top = pop_input_acc () in
              let* () = pop_expect_state new_expt_state in
              let* () = remove_all_proc_registry_with_input_expect_state new_expt_state in
              let new_lib_node = A.fold(A.N(N.Builtin(N.Library(path ^ "/" ^ file_name_without_ext)), [])) in
              let new_lib_node_ext = Ext.combine_extent (A.get_extent_some input_top) read_filename_ext in
              push_elem_on_input_acc (A.annotate_with_extent new_lib_node new_lib_node_ext) 
            )
          in
          to_processor_complex new_expt_state "filename_id_parse" file_name_id_parse
        ) files in
        let* _ = add_processor_entry_list all_new_procs in
        let* _ = push_elem_on_input_acc input_top in
        return ()
      else
        pfail ("BP154: Expected a directory but got " ^ path)
    )
  | _ ->
    pfail ("BP157: Expected a library but got " ^ A.show_view input_top)
    
let unknown_structure_deref_meta : binary_op_meta = 
  {
      id = Uid.next();
      keyword = CS.new_t_string "之";
      left_precedence = 100;
      right_precedence = 100;
      fixity = Infix;
  }
let unknown_structure_deref : binary_op =
  {
    meta = unknown_structure_deref_meta;
    reduction = 
      let* ((lo, proj_label), ext) = pop_bin_operand unknown_structure_deref_meta in
      match A.view proj_label with
      | A.FreeVar(label) -> 
        let new_node = A.fold(A.N(N.StructureDeref(label), [[],lo])) in
        push_elem_on_input_acc (A.annotate_with_extent new_node ext)
      | _ -> pfail ("ET102: Expected a free variable but got " ^ A.show_view proj_label)
  }

let statement_end_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "也";
    left_precedence = 5;
    right_precedence = 0;
    fixity = Postfix;
  }
let statement_end : binary_op = 
  {
    meta = statement_end_meta;
    reduction = 
      let* (oper, per_ext) = pop_postfix_operand statement_end_meta in
      push_elem_on_input_acc (A.annotate_with_extent oper per_ext)
  }

let sentence_end : unit proc_state_m =
  let* _ = read_one_of_string [CS.new_t_string "。"] in
  let* module_expr, decl = pop_input_acc_2 () in 
  match A.view module_expr, A.view decl with
  | A.N(N.ModuleDef, args), A.N(N.Declaration(_), _) -> 
    push_elem_on_input_acc
      (A.annotate_with_extent
        (A.fold(A.N(N.ModuleDef, args@[[], decl]))) 
        (Ext.combine_extent (A.get_extent_some module_expr) (A.get_extent_some decl))
      )
  | _ -> pfail ("ET103: Expected a module defn and a decl but got " ^ A.show_view module_expr ^ " and " ^ A.show_view decl)
  





  (* let* read_end = read_one_of_string [CS.new_t_string "之书"] in
  let* (content, directive) = pop_input_acc_past (fun elem -> PE.is_keyword elem "寻"
    || PE.is_keyword elem "寻观") in            
  push_elem_on_input_acc (PElem.get_keyword_t read_end) *)
  
let default_registry = [
  to_processor_complex Expression "top_level_empty_space_ignore" top_level_empty_space_ignore;
  to_processor_complex Expression "comment_start" comment_start;
  to_processor_complex (Scanning InComment) "comment_middle" comment_middle;
  to_processor_complex (Scanning InComment) "comment_end" comment_end;
  to_processor_binary_op Expression "definition_middle" definition_middle;
  to_processor_binary_op Expression "import_end" import_end;
  to_processor_binary_op Expression "library_root" library_root;
  to_processor_binary_op Expression "unknown_structure_deref" unknown_structure_deref;
  to_processor_complex Expression "known_structure_deref" known_structure_deref;
  to_processor_binary_op Expression "statement_end" statement_end;
  to_processor_complex Expression "sentence_end" sentence_end;

] @ List.concat [
to_processor_complex_list [Expression] "identifier_parser_pusher" identifier_parser_pusher;
 ]