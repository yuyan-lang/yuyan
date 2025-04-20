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

let string_parser_pusher : unit proc_state_m = 
  let* (_, start_ext) = read_one_of_char [CS.new_t_char "『"] in
  let* ((middle, _middle_ext), (_end, end_ext)) = scan_past_one_of_char [CS.new_t_char "』"] in
  push_elem_on_input_acc (
    A.annotate_with_extent
      (A.fold(A.N(N.Builtin(N.String(CS.get_t_string middle)), [])))
      (Ext.combine_extent start_ext end_ext)
  )

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
  let* _ = pnot (comment_start) in
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
    left_fixity = FxOp 90;
    right_fixity = FxNone;
  }
let import_end : binary_op = 
  {
    meta = import_end_meta;
    reduction = 
      let* (prev_comp, ext) = pop_postfix_operand (import_end_meta) in
      let* module_expr = Imports.get_module_expr prev_comp in
      push_elem_on_input_acc (A.annotate_with_extent module_expr ext)
  }

let assert_is_free_var (x : A.t) : unit proc_state_m = 
  match A.view x with
  | A.FreeVar(_) -> return ()
  | _ -> pfail ("ET101: Expected a free variable but got " ^ A.show_view x)

let definition_middle_uid = Uid.next()
let definition_end_uid = Uid.next()
let definition_middle_meta : binary_op_meta = 
  {
    id = definition_middle_uid;
    keyword = CS.new_t_string "者";
    left_fixity = FxOp 10;
    right_fixity = FxComp definition_end_uid;
  }
let definition_end_meta : binary_op_meta =
  {
    id = Uid.next();
    keyword = CS.new_t_string "也";
    left_fixity = FxComp definition_middle_uid;
    right_fixity = FxNone;
  }

let definition_middle : binary_op = 
  {
    meta = definition_middle_meta;
    reduction = p_internal_error "BP104: definition_middle reduction";
  }

let definition_end : binary_op = 
{
  meta = definition_end_meta;
  reduction = 
    let* ((name, defn), ext) = pop_op_operands_from_top_2 definition_end_meta in
    push_elem_on_input_acc (A.annotate_with_extent(A.fold(A.N(N.Declaration(N.ConstantDefn), [[], name; [], defn]))) ext)
}

let library_root_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "藏书阁";
    left_fixity = FxNone;
    right_fixity = FxNone;
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

(* let known_structure_deref : unit proc_state_m =
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
     *)
let unknown_structure_deref_meta : binary_op_meta = 
  {
      id = Uid.next();
      keyword = CS.new_t_string "之";
      left_fixity = FxOp 99;
      right_fixity = FxOp 100;
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

let builtin_op_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "内建";
    left_fixity = FxNone;
    right_fixity = FxOp 200;
  }
let builtin_op : binary_op = 
  {
    meta = builtin_op_meta;
    reduction = 
      let* (oper, per_ext) = pop_prefix_operand builtin_op_meta in
      let* node = (
        match A.view oper with
        | A.FreeVar(x) -> 
          (
            match x with
            | "《《内建类型：字符串》》" ->
              return (A.fold(A.N(N.Builtin(N.StringType), [])))
            | "《《内建类型：整数》》" ->
              return (A.fold(A.N(N.Builtin(N.IntType), [])))
            | "《《内建类型：小数》》" ->
              return (A.fold(A.N(N.Builtin(N.FloatType), [])))
            | "《《内建类型：动态分类值》》" ->
              return (A.fold(A.N(N.Builtin(N.Type), [])))
            | "《《内建类型：有》》" ->
              return (A.fold(A.N(N.Builtin(N.UnitType), [])))
            | "《《内建类型：爻》》" ->
              return (A.fold(A.N(N.Builtin(N.BoolType), [])))
            | "《《内建类型：元类型》》" ->
              return (A.fold(A.N(N.Builtin(N.Type), [])))
            | "《《内建爻：阳》》" ->
              return (A.fold(A.N(N.Builtin(N.Bool true), [])))
            | "《《内建爻：阴》》" ->
              return (A.fold(A.N(N.Builtin(N.Bool false), [])))
            | "《《内建有：元》》" ->
              return (A.fold(A.N(N.Builtin(N.Unit), [])))
            | "《《内建函数：抛出异常字符串》》" ->
              return (A.fold(A.N(N.Builtin(N.RaiseException), [])))
            | "《《内建函数：尝试运行字符串》》" ->
              return (A.fold(A.N(N.Builtin(N.TryCatch), [])))
            
            | _ -> pfail ("ET104: Expected a builtin val but got >" ^ x ^ "<")
          )
        | _ -> pfail ("ET105: Builtin Expected a free variable but got " ^ A.show_view oper)
      ) in
      push_elem_on_input_acc (A.annotate_with_extent node per_ext)
  }

let module_open_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "观";
    left_fixity = FxNone;
    right_fixity = FxOp 80;
  }

let get_file_ref (file_path : string) : A.t proc_state_m = 
  match !compilation_manager_get_file_hook file_path with
  | Some (result) -> return (result)
  | None -> failwith ("Im30: Module not found: " ^ file_path ^ " fileRefs should only contain checked modules")

let get_module_expr_defined_names (m : A.t) : (string * Ext.t) list proc_state_m = 
  let all_names = 
    (match A.view m with
    | A.N(N.ModuleDef, args) -> (
      List.filter_map (fun (_, arg) -> 
        match A.view arg with
        | A.N(N.Declaration(N.ConstantDefn), ([], name)::_) 
        | A.N(N.Declaration(N.ConstructorDecl), ([], name)::_) 
        | A.N(N.Declaration(N.ConstantDecl), ([], name)::_) ->
          (
          match A.view name with
          | A.FreeVar(x) -> Some (x, A.get_extent_some name)
          | _ -> failwith ("BP280: ConstantDefn should be a free variable but got " ^ A.show_view name)
          )
        | _ -> print_failwith ("BP281: Expected a ConstantDefn but got " ^ A.show_view arg)
      ) args
    )
    | _ -> failwith ("BP282: Expecting moduleDef: " ^ A.show_view m)
    ) in
  return (ListUtil.remove_duplicates all_names)

let module_open : binary_op = 
  {
    meta = module_open_meta;
    reduction = 
      let* (module_expr, _per_ext) = pop_prefix_operand module_open_meta in
      let* () = (
        match A.view module_expr with
        | A.N(N.FileRef(path), []) -> (
          let* file_content = get_file_ref path in
          let* all_names = get_module_expr_defined_names file_content in
          (* add new operators corresponding to the names in the file *)
          let ops = List.map (fun (name, _ext) -> 
            let meta = {
              id = Uid.next();
              keyword = CS.new_t_string name;
              left_fixity = FxNone;
              right_fixity = FxNone;
            } in
            let name_oper = {
              meta = meta;
              reduction = 
                let* (per_ext) = pop_closed_identifier_operand meta in
                let node = A.fold(A.N(N.StructureDeref(name), [([], module_expr)])) in
                push_elem_on_input_acc (A.annotate_with_extent node per_ext)
            } in
            to_processor_binary_op Expression ("open_module_"^name) name_oper
            ) all_names in
          add_processor_entry_list ops
          (* DO WE NEED TO PUSH SOMETHING TO THE INPUT ACCUM? *)
        )
        (* | A.FreeVar(x) -> 
          return (A.fold(A.N(N.Builtin(N.String x), []))) *)
        | _ -> pfail ("BP273: Expected a module Expression but got " ^ A.show_view module_expr)
      ) in
      return ()
      (* push_elem_on_input_acc (A.annotate_with_extent node per_ext) *)
  }

let const_decl_middle_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "乃";
    left_fixity = FxOp 10;
    right_fixity = FxOp 10;
  }
let const_decl_middle : binary_op = 
  {
    meta = const_decl_middle_meta;
    reduction = 
      let* ((name, defn), ext) = pop_bin_operand const_decl_middle_meta in
      let* () = assert_is_free_var name in
      push_elem_on_input_acc (A.annotate_with_extent(A.fold(A.N(N.Declaration(N.ConstantDecl), [[], name; [], defn]))) ext)
  }


let constructor_decl_middle_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "立";
    left_fixity = FxOp 10;
    right_fixity = FxOp 10;
  }

let constructor_decl_middle : binary_op = {
    meta = constructor_decl_middle_meta;
    reduction = 
      let* ((name, defn), ext) = pop_bin_operand constructor_decl_middle_meta in
      let* () = assert_is_free_var name in
      push_elem_on_input_acc (A.annotate_with_extent(A.fold(A.N(N.Declaration(N.ConstructorDecl), [[], name; [], defn]))) ext)
}

let left_parenthesis_uid = Uid.next()
let right_parenthesis_uid = Uid.next()
let left_parenthesis_meta : binary_op_meta = 
  {
    id = left_parenthesis_uid;
    keyword = CS.new_t_string "（";
    left_fixity = FxNone;
    right_fixity = FxComp right_parenthesis_uid;
  }
let right_parenthesis_meta : binary_op_meta = 
  {
    id = right_parenthesis_uid;
    keyword = CS.new_t_string "）";
    left_fixity = FxComp left_parenthesis_uid;
    right_fixity = FxNone;
  }
let left_parenthesis : binary_op = 
  {
    meta = left_parenthesis_meta;
    reduction = p_internal_error "BP104: left_parenthesis reduction";
  }
let right_parenthesis : binary_op = 
  {
    meta = right_parenthesis_meta;
    reduction = 
      let* (oper, per_ext) = pop_op_operands_from_top_1 right_parenthesis_meta in
      push_elem_on_input_acc (A.annotate_with_extent oper per_ext)
  }

let double_parenthesis_left_uid = Uid.next()
let double_parenthesis_right_uid = Uid.next()
let double_parenthesis_left_meta : binary_op_meta = 
  {
    id = double_parenthesis_left_uid;
    keyword = CS.new_t_string "「「";
    left_fixity = FxNone;
    right_fixity = FxComp double_parenthesis_right_uid;
  }
let double_parenthesis_right_meta : binary_op_meta = 
  {
    id = double_parenthesis_right_uid;
    keyword = CS.new_t_string "」」";
    left_fixity = FxComp double_parenthesis_left_uid;
    right_fixity = FxNone;
  }
let double_parenthesis_left : binary_op = 
  {
    meta = double_parenthesis_left_meta;
    reduction = p_internal_error "BP104: double_parenthesis_left reduction";
  }
let double_parenthesis_right : binary_op = 
  {
    meta = double_parenthesis_right_meta;
    reduction = 
      let* (oper, per_ext) = pop_op_operands_from_top_1 double_parenthesis_right_meta in
      push_elem_on_input_acc (A.annotate_with_extent oper per_ext)
  }


let get_binding_name (x : A.t) : string proc_state_m = 
  match A.view x with
  | A.N(N.ParsingElem(N.BoundScannedString(s)), []) -> return (CS.get_t_string s)
  | _ -> pfail ("ET107: Expected a bound scanned string but got " ^ A.show_view x)

let explicit_pi_start_uid = Uid.next()
let explicit_pi_middle_1_uid = Uid.next()
let explicit_pi_middle_2_uid = Uid.next()
let explicit_pi_start_meta = 
  {
    id = explicit_pi_start_uid;
    keyword = CS.new_t_string "化";
    left_fixity = FxNone;
    right_fixity = FxComp explicit_pi_middle_1_uid;
  }
let explicit_pi_middle_1_meta = 
  {
    id = explicit_pi_middle_1_uid;
    keyword = CS.new_t_string "者";
    left_fixity = FxComp explicit_pi_start_uid;
    right_fixity = FxBinding explicit_pi_middle_2_uid;
  }
let explicit_pi_middle_2_meta = 
  {
    id = explicit_pi_middle_2_uid;
    keyword = CS.new_t_string "而";
    left_fixity = FxBinding explicit_pi_middle_1_uid;
    right_fixity = FxOp 40;
  }
let explicit_pi_start : binary_op = 
  {
    meta = explicit_pi_start_meta;
    reduction = p_internal_error "BP104: explicit_pi_start reduction";
  }
let explicit_pi_middle_1 : binary_op = 
  {
    meta = explicit_pi_middle_1_meta;
    reduction = p_internal_error "BP104: explicit_pi_middle_1 reduction";
  }
let explicit_pi_middle_2 : binary_op = 
  {
    meta = explicit_pi_middle_2_meta;
    reduction = 
      let* ((tp_name, bnd_name, range_expr), per_ext) = pop_op_operands_from_second_top_3 explicit_pi_middle_2_meta in
      let* binding_name = get_binding_name bnd_name in
      let result_expr = A.fold_with_extent(A.N(N.ExplicitPi, [[], tp_name; [binding_name], range_expr])) per_ext in
      push_elem_on_input_acc result_expr 
  }

let implicit_pi_start_uid = Uid.next()
let implicit_pi_middle_1_uid = Uid.next()
let implicit_pi_middle_2_uid = Uid.next()
let implicit_pi_start_meta = 
  {
    id = implicit_pi_start_uid;
    keyword = CS.new_t_string "承";
    left_fixity = FxNone;
    right_fixity = FxComp implicit_pi_middle_1_uid;
  }
let implicit_pi_middle_1_meta = 
  {
    id = implicit_pi_middle_1_uid;
    keyword = CS.new_t_string "者";
    left_fixity = FxComp implicit_pi_start_uid;
    right_fixity = FxBinding implicit_pi_middle_2_uid;
  }
let implicit_pi_middle_2_meta = 
  {
    id = implicit_pi_middle_2_uid;
    keyword = CS.new_t_string "而";
    left_fixity = FxBinding implicit_pi_middle_1_uid;
    right_fixity = FxOp 40;
  }
let implicit_pi_start : binary_op = 
  {
    meta = implicit_pi_start_meta;
    reduction = p_internal_error "BP104: implicit_pi_start reduction";
  }
let implicit_pi_middle_1 : binary_op = 
  {
    meta = implicit_pi_middle_1_meta;
    reduction = p_internal_error "BP104: implicit_pi_middle_1 reduction";
  }
let implicit_pi_middle_2 : binary_op = 
  {
    meta = implicit_pi_middle_2_meta;
    reduction = 
      let* ((tp_name, bnd_name, range_expr), per_ext) = pop_op_operands_from_second_top_3 implicit_pi_middle_2_meta in
      let* binding_name = get_binding_name bnd_name in
      let result_expr = A.fold_with_extent(A.N(N.ImplicitPi, [[], tp_name; [binding_name], range_expr])) per_ext in
      push_elem_on_input_acc result_expr 
  }

let arrow_start_uid = Uid.next()
let arrow_middle_uid = Uid.next()
let arrow_start_meta = 
  {
    id = arrow_start_uid;
    keyword = CS.new_t_string "化";
    left_fixity = FxNone;
    right_fixity = FxComp arrow_middle_uid;
  }
let arrow_middle_meta = 
  {
    id = arrow_middle_uid;
    keyword = CS.new_t_string "而";
    left_fixity = FxComp arrow_start_uid;
    right_fixity = FxOp 40;
  }
let arrow_start : binary_op = 
  {
    meta = arrow_start_meta;
    reduction = p_internal_error "BP104: arrow_start reduction";
  }
let arrow_middle : binary_op = 
  {
    meta = arrow_middle_meta;
    reduction = 
      let* ((tp_name, range_expr), per_ext) = pop_op_operands_from_second_top_2 arrow_middle_meta in
      let result_expr = A.fold(A.N(N.Arrow, [[], tp_name; [], range_expr])) in
      push_elem_on_input_acc (A.annotate_with_extent result_expr per_ext)
  }

let implicit_lam_abs_start_uid = Uid.next()
let implicit_lam_abs_middle_uid = Uid.next()
let implicit_lam_abs_start_meta = 
  {
    id = implicit_lam_abs_start_uid;
    keyword = CS.new_t_string "受";
    left_fixity = FxNone;
    right_fixity = FxBinding implicit_lam_abs_middle_uid;
  }
let implicit_lam_abs_middle_meta = 
  {
    id = implicit_lam_abs_middle_uid;
    keyword = CS.new_t_string "而";
    left_fixity = FxBinding implicit_lam_abs_start_uid;
    right_fixity = FxOp 50;
  }
let implicit_lam_abs_start : binary_op = 
  {
    meta = implicit_lam_abs_start_meta;
    reduction = p_internal_error "BP104: implicit_lam_abs_start reduction";
  }
let implicit_lam_abs_middle : binary_op = 
  {
    meta = implicit_lam_abs_middle_meta;
    reduction = 
      let* ((bnd_name, range_expr), per_ext) = pop_op_operands_from_second_top_2 implicit_lam_abs_middle_meta in
      let* binding_name = get_binding_name bnd_name in
      let result_expr = A.fold_with_extent (A.N(N.Lam, [[binding_name], range_expr])) per_ext in
      push_elem_on_input_acc result_expr 
  }

let explicit_lam_abs_start_uid = Uid.next()
let explicit_lam_abs_middle_uid = Uid.next()
let explicit_lam_abs_start_meta = 
  {
    id = explicit_lam_abs_start_uid;
    keyword = CS.new_t_string "会";
    left_fixity = FxNone;
    right_fixity = FxBinding explicit_lam_abs_middle_uid;
  }
let explicit_lam_abs_middle_meta = 
  {
    id = explicit_lam_abs_middle_uid;
    keyword = CS.new_t_string "而";
    left_fixity = FxBinding explicit_lam_abs_start_uid;
    right_fixity = FxOp 50;
  }
let explicit_lam_abs_start : binary_op = 
  {
    meta = explicit_lam_abs_start_meta;
    reduction = p_internal_error "BP104: explicit_lam_abs_start reduction";
  }
let explicit_lam_abs_middle : binary_op = 
  {
    meta = explicit_lam_abs_middle_meta;
    reduction = 
      let* ((tp_name, range_expr), per_ext) = pop_op_operands_from_second_top_2 explicit_lam_abs_middle_meta in
      let* binding_name = get_binding_name tp_name in
      let result_expr = A.fold_with_extent (A.N(N.Lam, [[binding_name], range_expr])) per_ext in
      push_elem_on_input_acc result_expr 
  }

let implicit_ap_uid = Uid.next()
let implicit_ap_meta = 
  {
    id = implicit_ap_uid;
    keyword = CS.new_t_string "授以";
    left_fixity = FxOp 79;
    right_fixity = FxOp 80;
  }
let implicit_ap : binary_op = 
  {
    meta = implicit_ap_meta;
    reduction = 
      let* ((f, arg), per_ext) = pop_bin_operand implicit_ap_meta in
      push_elem_on_input_acc (A.fold_with_extent(A.N(N.Ap, [[], f; [], arg])) per_ext)
  }

let explicit_ap_uid = Uid.next()
let explicit_ap_meta = 
  {
    id = explicit_ap_uid;
    keyword = CS.new_t_string "于";
    left_fixity = FxOp 79;
    right_fixity = FxOp 80;
  }
let explicit_ap : binary_op = 
  {
    meta = explicit_ap_meta;
    reduction = 
      let* ((f, arg), per_ext) = pop_bin_operand explicit_ap_meta in
      push_elem_on_input_acc (A.fold_with_extent(A.N(N.Ap, [[], f; [], arg])) per_ext)
  }


  let statement_end_meta : binary_op_meta = 
    {
      id = Uid.next();
      keyword = CS.new_t_string "也";
      left_fixity = FxOp 5;
      right_fixity = FxNone
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
    (* reduce all existing expressions*)
    let* _ = operator_precedence_reduce (-1) in
    let* input_acc_size = get_input_acc_size () in
    if input_acc_size = 1  then
      let* module_expr = pop_input_acc () in
      match A.view module_expr with
      | A.N(N.ModuleDef, _) -> 
        let* () = push_elem_on_input_acc module_expr in
        return ()
      | _ -> pfail ("BP207: Expected a module defn but got " ^ A.show_view module_expr)
    else if input_acc_size > 1 then
      let* module_expr, decl = pop_input_acc_2 () in 
      match A.view module_expr, A.view decl with
      | A.N(N.ModuleDef, args), A.N(N.Declaration(_), _) -> 
        push_elem_on_input_acc
          (A.annotate_with_extent
            (A.fold(A.N(N.ModuleDef, args@[[], decl]))) 
            (Ext.combine_extent (A.get_extent_some module_expr) (A.get_extent_some decl))
          )
      (* also for 「「 name *)
      | A.N(N.ParsingElem(N.OpKeyword({id=opid;_})), []), A.N(N.Declaration(_), _) -> 
        if opid = double_parenthesis_left_uid || opid = left_parenthesis_uid then 
          (* push 「「 back onto the stack *)
          let* _ = push_elem_on_input_acc module_expr in
          let* _ = push_elem_on_input_acc (A.fold_with_extent (A.N(N.ModuleDef, [[],decl])) (A.get_extent_some decl)) in
          return ()
        else  pfail ("ET103: Expected a module defn and a decl but got " ^ A.show_view module_expr ^ " and " ^ A.show_view decl)
      (* also for 「「 name *)
      | _ -> pfail ("ET103: Expected a module defn and a decl but got " ^ A.show_view module_expr ^ " and " ^ A.show_view decl)
    else
      pfail ("ET106: Expected at least 2 elements in the input acc but got " ^ string_of_int input_acc_size)
    

let external_call_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "《《外部调用》》";
    left_fixity = FxNone;
    right_fixity = FxOp 200;
  }
let external_call : binary_op = 
  {
    meta = external_call_meta;
    reduction = 
      let* (oper, per_ext) = pop_prefix_operand external_call_meta in
        match A.view oper with
        | A.N(N.Builtin(N.String(x)), []) -> 
          (
            push_elem_on_input_acc (A.fold_with_extent (A.N(N.ExternalCall(x), []) ) per_ext)
          )
        | _ -> pfail ("BP693: Builtin Expected a string but got " ^ A.show_view oper)
  }
  

let default_registry = [
  to_processor_complex Expression "top_level_empty_space_ignore" top_level_empty_space_ignore;
  to_processor_complex Expression "comment_start" comment_start;
  to_processor_complex (Scanning InComment) "comment_start" comment_start;
  to_processor_complex (Scanning InComment) "comment_middle" comment_middle;
  to_processor_complex (Scanning InComment) "comment_end" comment_end;
  to_processor_complex Expression "string_parser_pusher" string_parser_pusher;
  to_processor_binary_op Expression "definition_middle" definition_middle;
  to_processor_binary_op Expression "definition_end" definition_end;
  to_processor_binary_op Expression "import_end" import_end;
  to_processor_binary_op Expression "library_root" library_root;
  to_processor_binary_op Expression "unknown_structure_deref" unknown_structure_deref;
  (* to_processor_complex Expression "known_structure_deref" known_structure_deref; *)
  to_processor_binary_op Expression "statement_end" statement_end;
  to_processor_complex Expression "sentence_end" sentence_end;
  to_processor_binary_op Expression "builtin_op" builtin_op;
  to_processor_binary_op Expression "module_open" module_open;
  to_processor_binary_op Expression "const_decl_middle" const_decl_middle;
  to_processor_binary_op Expression "constructor_decl_middle" constructor_decl_middle;
  to_processor_binary_op Expression "left_parenthesis" left_parenthesis;
  to_processor_binary_op Expression "right_parenthesis" right_parenthesis;
  to_processor_binary_op Expression "explicit_pi_start" explicit_pi_start;
  to_processor_binary_op Expression "explicit_pi_middle_1" explicit_pi_middle_1;
  to_processor_binary_op Expression "explicit_pi_middle_2" explicit_pi_middle_2;
  to_processor_binary_op Expression "implicit_pi_start" implicit_pi_start;
  to_processor_binary_op Expression "implicit_pi_middle_1" implicit_pi_middle_1;
  to_processor_binary_op Expression "implicit_pi_middle_2" implicit_pi_middle_2;
  to_processor_binary_op Expression "arrow_start" arrow_start;
  to_processor_binary_op Expression "arrow_middle" arrow_middle;
  to_processor_binary_op Expression "implicit_lam_abs_start" implicit_lam_abs_start;
  to_processor_binary_op Expression "implicit_lam_abs_middle" implicit_lam_abs_middle;
  to_processor_binary_op Expression "explicit_lam_abs_start" explicit_lam_abs_start;
  to_processor_binary_op Expression "explicit_lam_abs_middle" explicit_lam_abs_middle;
  to_processor_binary_op Expression "implicit_ap" implicit_ap;
  to_processor_binary_op Expression "explicit_ap" explicit_ap;
  to_processor_binary_op Expression "double_parenthesis_left" double_parenthesis_left;
  to_processor_binary_op Expression "double_parenthesis_right" double_parenthesis_right;
  to_processor_binary_op Expression "external_call" external_call;

] @ List.concat [
to_processor_complex_list [Expression] "identifier_parser_pusher" identifier_parser_pusher;
 ]