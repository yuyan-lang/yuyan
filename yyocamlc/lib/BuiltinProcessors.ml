open EngineData
open ProcCombinators

module Ext = AbtLib.Extent
module CS = CharStream
module Env = Environment
open BasicParsing
  

(* let get_binding_name (x : input_acc_elem) : string proc_state_m = 
  match x with
  | ParsingElem(BoundScannedString(s), _) -> return (CS.get_t_string s)
  | _ -> pfail ("ET107: Expected a bound scanned string but got " ^ (show_input_acc_elem x)) *)

let get_binding_name (x : A.t) : string proc_state_m = 
  match A.view x with
  | A.FreeVar(name) -> return name
  | _ -> pfail ("ET107: Expected a free variable but got " ^ (A.show_view x))

(* let top_level_identifier_pusher : unit proc_state_m = 
  read_any_char_except_and_push (CharStream.new_t_string "。（）「」『』\n\t\r"@[" "]) *)

let top_level_empty_space_ignore : unit proc_state_m = 
  read_one_of_char (CharStream.new_t_string " \n\t\r") >> ignore ()


let identifier_parser_pusher : unit proc_state_m = 
  let* (id, ext) = identifier_parser () in
  if List.for_all (fun x -> List.mem x yy_number_words) id
    then (let number = get_int_from_t_string id in
      push_elem_on_input_acc (Expr (A.fold_with_extent(A.N(N.Builtin(N.Int number), [])) ext))
    ) else
    push_elem_on_input_acc (get_identifier_t (id, ext))

let string_escape_sequence_scanner : (CS.t_char * Ext.t) proc_state_m = 
  let* (_backslash, ext) = read_one_of_char [CS.new_t_char "\\"] in
  let* (next_char, ext2) = read_any_char () in
  match CS.get_t_char next_char with
  | "\\" -> return (next_char, Ext.combine_extent ext ext2)
  | "』" -> return (CS.new_t_char "』", Ext.combine_extent ext ext2)
  | _ -> pfail_with_ext ("ET108: Expected a valid escape sequence but got \\" ^ CS.get_t_char next_char) ext2

let string_character_scanner : (CS.t_char * Ext.t) proc_state_m = 
  pcut (choice string_escape_sequence_scanner (read_any_char ()))

let scan_string_body () : ((CS.t_string * Ext.t) * Ext.t) proc_state_m = 
  let rec aux acc = 
    pcut (choice (
      let* (c, ext) = string_escape_sequence_scanner in
      aux (acc@[(c, ext)])
    ) (
    let* (c, ext) = read_any_char () in
    if CS.get_t_char c = "』" then
      if List.is_empty acc 
      then
        return (([],ext), ext)
      else
        return ((remap_t_char_list_with_ext acc), ext)
    else 
      aux (acc@[(c, ext)])
    ))
  in
  aux []

let string_parser_pusher : unit proc_state_m = 
  let* (_, start_ext) = read_one_of_char [CS.new_t_char "『"] in
  let* ((middle, _middle_ext), (end_ext)) = scan_string_body () in 
  push_elem_on_input_acc (
    Expr (
      A.annotate_with_extent
        (A.fold(A.N(N.Builtin(N.String(CS.get_t_string middle)), [])))
        (Ext.combine_extent start_ext end_ext)
    )
  )


let rec single_comment () : unit proc_state_m = 
  let* _ = read_string (CS.new_t_string "「：") in
  (read_until_comment_end ())
(* using a choice is problematic because of backtracking (things either don't work 
or dont terminate.).  I don't know why.
I need a decision unambiguously without backtracking *)
and read_until_comment_end () : unit proc_state_m = 
  let* (char1,_) = read_any_char () in
  let* (char2,_) = peek_any_char () in
  match CS.get_t_char char1, CS.get_t_char char2 with
  | "：", "」" -> (
    let* _ = read_any_char () in (* read the ending comment char*)
    return ()
  )
  | "「", "：" -> (
    let* _ = read_any_char () in (* read the colon char*)
    let* () = read_until_comment_end() in (* two levels of comments now, need to read twice*)
    read_until_comment_end ()
  )
  | _ -> read_until_comment_end ()

(* let comment_end : unit proc_state_m =
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
  ignore () *)
(* 
let import_start : unit proc_state_m = 
  let* read_start = read_one_of_string 
    [CS.new_t_string "寻观";
     CS.new_t_string "寻" 
    ] in
  push_elem_on_input_acc (get_keyword_t read_start) *)

let import_end_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "之书";
    left_fixity = FxOp (Some 90);
    right_fixity = FxNone;
  }
let import_end : binary_op = 
  {
    meta = import_end_meta;
    reduction = 
    (
      let* (prev_comp, ext) = pop_postfix_operand (import_end_meta) in
      let* module_expr = Imports.get_module_expr prev_comp in
      push_elem_on_input_acc (Expr (A.annotate_with_extent module_expr ext))
    );
    shift_action = do_nothing_shift_action;
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
    left_fixity = FxOp (Some 10);
    right_fixity = FxComp definition_end_uid;
  }
let definition_end_meta : binary_op_meta =
  {
    id = definition_end_uid;
    keyword = CS.new_t_string "也";
    left_fixity = FxComp definition_middle_uid;
    right_fixity = FxNone;
  }

let definition_middle : binary_op = 
  {
    meta = definition_middle_meta;
    reduction = p_internal_error "BP104: definition_middle reduction";
    shift_action = do_nothing_shift_action;
  }

let definition_end : binary_op = 
{
  meta = definition_end_meta;
  reduction = 
  (
    let* ((name, defn), ext) = pop_postfix_op_operands_2 definition_end_meta in
    push_elem_on_input_acc_expr (A.annotate_with_extent(A.fold(A.N(N.Declaration(N.ConstantDefn), [[], name; [], defn]))) ext)
  );
  shift_action = do_nothing_shift_action;
}

let definition2_start_uid = Uid.next()
let definition2_middle_uid = Uid.next()
let definition2_end_uid = Uid.next()
let definition2_start_meta : binary_op_meta = 
  {
    id = definition2_start_uid;
    keyword = CS.new_t_string "夫";
    left_fixity = FxNone;
    right_fixity = FxBinding definition2_middle_uid;
  }
let definition2_middle_meta : binary_op_meta = 
  {
    id = definition2_middle_uid;
    keyword = CS.new_t_string "者";
    left_fixity = FxBinding definition2_start_uid;
    right_fixity = FxComp definition2_end_uid;
  }
let definition2_end_meta : binary_op_meta = 
  {
    id = definition2_end_uid;
    keyword = CS.new_t_string "也";
    left_fixity = FxComp definition2_middle_uid;
    right_fixity = FxNone;
  }
let definition2_start : binary_op = 
  {
    meta = definition2_start_meta;
    reduction = p_internal_error "BP104: definition2_start reduction";
    shift_action = do_nothing_shift_action;
  }
let definition2_middle : binary_op = 
  {
    meta = definition2_middle_meta;
    reduction = p_internal_error "BP104: definition2_middle reduction";
    shift_action = add_prev_identifier_shift_action;
  }
let definition2_end : binary_op = 
  {
    meta = definition2_end_meta;
    reduction = 
    (
      let* ((name, defn), ext) = pop_postfix_op_operands_2 definition2_end_meta in
      let* bnd_name = get_binding_name name in
      push_elem_on_input_acc_expr (A.annotate_with_extent(A.fold(A.N(N.Declaration(N.ConstantDefn), 
      [[], A.annotate_with_extent (A.free_var bnd_name) (A.get_extent_some name); [], defn]))) ext)
    );
    shift_action = do_nothing_shift_action;
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
        push_elem_on_input_acc_expr (A.annotate_with_extent (A.fold(A.N(N.Builtin(N.Library default_path), []))) ext)
      else
        pfail ("Directory not found: " ^ default_path)
    );
    shift_action = do_nothing_shift_action;
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
      left_fixity = FxOp (Some 999);
      right_fixity = FxOp (Some 1000);
  }
let unknown_structure_deref : binary_op =
  {
    meta = unknown_structure_deref_meta;
    reduction = 
    (
      let* ((lo, proj_label), ext) = pop_bin_operand unknown_structure_deref_meta in
      match A.view proj_label with
      | A.FreeVar(label) -> 
        let new_node = A.fold(A.N(N.StructureDeref(label), [[],lo])) in
        push_elem_on_input_acc_expr (A.annotate_with_extent new_node ext)
      | A.N(N.Builtin(N.Int i), []) -> 
        let new_node = A.fold(A.N(N.TupleDeref(i), [[],lo])) in
        push_elem_on_input_acc_expr (A.annotate_with_extent new_node ext)
      | _ -> pfail ("ET102: Expected a free variable but got " ^ A.show_view proj_label)
    );
    shift_action = do_nothing_shift_action;
  }

let builtin_op_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "内建";
    left_fixity = FxNone;
    right_fixity = FxOp (Some 2000);
  }
let builtin_op : binary_op = 
  {
    meta = builtin_op_meta;
    reduction = 
    (
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
      push_elem_on_input_acc_expr (A.annotate_with_extent node per_ext)
  );
  shift_action = do_nothing_shift_action;
  }

let module_open_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "观";
    left_fixity = FxNone;
    right_fixity = FxOp (Some 80);
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
        | A.N(N.Declaration(N.CustomOperatorDecl), _) -> None
        | _ -> print_failwith ("BP281: Expected a ConstantDefn but got " ^ A.show_view arg)
      ) args
    )
    | _ -> failwith ("BP282: Expecting moduleDef: " ^ A.show_view m)
    ) in
  return (ListUtil.remove_duplicates all_names)
let get_module_expr_defined_custom_ops (m : A.t) : binary_op list proc_state_m = 
  let all_names = 
    (match A.view m with
    | A.N(N.ModuleDef, args) -> (
      List.filter_map (fun (_, arg) -> 
        match A.view arg with
        | A.N(N.Declaration(N.ConstantDefn), _)
        | A.N(N.Declaration(N.ConstructorDecl), _)
        | A.N(N.Declaration(N.ConstantDecl), _) ->
          (
            None
          )
        | A.N(N.Declaration(N.CustomOperatorDecl), ([], op)::([], elab)::[]) -> (
          match A.view op with
          | A.N(N.Builtin(N.CustomOperatorString(x)), []) -> 
            let all_ops = UserDefinedOperators.get_operators x elab in
            Some(all_ops)

          | _ -> failwith ("BP279: Expected a string but got " ^ A.show_view op)
        )
        | _ -> print_failwith ("BP281: Expected a ConstantDefn but got " ^ A.show_view arg)
      ) args
    )
    | _ -> failwith ("BP282: Expecting moduleDef: " ^ A.show_view m)
    ) in
  return (List.concat all_names)
  
let module_open : binary_op = 
  {
    meta = module_open_meta;
    reduction = 
    (
      let* (module_expr, _per_ext) = pop_prefix_operand module_open_meta in
      let* () = (
        match A.view module_expr with
        | A.N(N.FileRef(path), []) -> (
          let* file_content = get_file_ref path in
          let* all_names = get_module_expr_defined_names file_content in
          let* all_custom_ops = get_module_expr_defined_custom_ops file_content in
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
              (
                let* (per_ext) = pop_closed_identifier_operand meta in
                let node = A.fold(A.N(N.StructureDeref(name), [([], module_expr)])) in
                push_elem_on_input_acc_expr (A.annotate_with_extent node per_ext)
              );
              shift_action = do_nothing_shift_action;
            } in
            to_processor_binary_op ("open_module_"^name) name_oper
            ) all_names in
          add_processor_entry_list (ops @
          (List.map (to_processor_binary_op "imported_ops") all_custom_ops))
          (* add new operators corresponding to the custom ops in the file *)
          (* DO WE NEED TO PUSH SOMETHING TO THE INPUT ACCUM? *)
        )
        (* | A.FreeVar(x) -> 
          return (A.fold(A.N(N.Builtin(N.String x), []))) *)
        | _ -> pfail ("BP273: Expected a module Expression but got " ^ A.show_view module_expr)
      ) in
      return ()
      (* push_elem_on_input_acc (A.annotate_with_extent node per_ext) *)
    );
    shift_action = do_nothing_shift_action;
  }


let module_reexport_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "诵";
    left_fixity = FxNone;
    right_fixity = FxOp (Some 80);
  }
let module_reexport : binary_op = 
  {
    meta = module_reexport_meta;
    reduction = 
    (
      let* (module_expr, per_ext) = pop_prefix_operand module_reexport_meta in
      let* (cur_module_expr, cur_ext) = pop_input_acc_expr () in 
      match A.view cur_module_expr with
      | A.N(N.ModuleDef, args) -> (
          match A.view module_expr with
          | A.N(N.FileRef(path), []) -> (
              let* file_content = get_file_ref path in
              let rec aux acc decls = 
                match decls with
                | [] -> push_elem_on_input_acc_expr (A.fold_with_extent(A.N(N.ModuleDef, acc))
                    (Ext.combine_extent cur_ext per_ext)
                )
                | x::xs -> (
                  match A.view x with
                  | A.N((N.Declaration(N.ConstantDefn) as hd), ([], name)::_) 
                  | A.N((N.Declaration(N.ConstructorDecl) as hd), ([], name)::_) ->
                    (
                      match A.view name with
                      | A.FreeVar(x) -> 
                        let new_node = A.fold_with_extent(
                          A.N(hd, [([],name); [], A.fold(A.N(N.StructureDeref(x), [([], module_expr)]))])
                        ) per_ext in
                        aux (acc@[[], new_node]) xs
                      | _ -> pfail ("BP280: ConstantDefn should be a free variable but got " ^ A.show_view name)
                    )
                  | A.N(N.Declaration(N.CustomOperatorDecl), _) -> (
                    aux (acc@[[], x]) xs
                  )
                  | A.N(N.Declaration(N.ConstantDecl), _) -> aux acc xs
                  | _ -> print_failwith ("BP281: Expected a ConstantDefn but got " ^ A.show_view x)
                )
              in
              match A.view file_content with
              | A.N(N.ModuleDef, margs) -> aux args (List.map snd margs)
              | _ -> print_failwith ("BP282: Expecting moduleDef: " ^ A.show_view file_content)
              )
            | _ -> pfail ("BP273: Expected a module Expression but got " ^ A.show_view module_expr)
      )
      | _ -> pfail ("BP273: Expected a module Expression but got " ^ A.show_view module_expr)
    );
    shift_action = do_nothing_shift_action;
  }

let const_decl_middle_uid = Uid.next()
let const_decl_end_uid = Uid.next()
let const_decl_middle_meta : binary_op_meta = 
  {
    id = const_decl_middle_uid;
    keyword = CS.new_t_string "乃";
    left_fixity = FxOp (Some 10);
    right_fixity = FxComp const_decl_end_uid;
  }
let const_decl_end_meta : binary_op_meta = 
  {
    id = const_decl_end_uid;
    keyword = CS.new_t_string "也";
    left_fixity = FxComp const_decl_middle_uid;
    right_fixity = FxNone;
  }
let const_decl_middle : binary_op = 
  {
    meta = const_decl_middle_meta;
    reduction = p_internal_error "BP104: const_decl_middle reduction";
    shift_action = do_nothing_shift_action;
  }
let const_decl_end : binary_op =
  {
    meta = const_decl_end_meta;
    reduction =
    (
      let* ((name, defn), ext) = pop_postfix_op_operands_2 const_decl_end_meta in
      let* () = assert_is_free_var name in
      push_elem_on_input_acc_expr (A.annotate_with_extent(A.fold(A.N(N.Declaration(N.ConstantDecl), [[], name; [], defn]))) ext)
    );
    shift_action = do_nothing_shift_action;
  }

let const_decl2_start_uid = Uid.next()
let const_decl2_middle_uid = Uid.next()
let const_decl2_end_uid = Uid.next()
let const_decl2_start_meta : binary_op_meta = 
  {
    id = const_decl2_start_uid;
    keyword = CS.new_t_string "夫";
    left_fixity = FxNone;
    right_fixity = FxBinding const_decl2_middle_uid;
  }
let const_decl2_middle_meta : binary_op_meta = 
  {
    id = const_decl2_middle_uid;
    keyword = CS.new_t_string "乃";
    left_fixity = FxBinding const_decl2_start_uid;
    right_fixity = FxComp const_decl2_end_uid;
  }
let const_decl2_end_meta : binary_op_meta = 
  {
    id = const_decl2_end_uid;
    keyword = CS.new_t_string "也";
    left_fixity = FxComp const_decl2_middle_uid;
    right_fixity = FxNone;
  }
let const_decl2_start : binary_op = 
  {
    meta = const_decl2_start_meta;
    reduction = p_internal_error "BP104: const_decl2_start reduction";
    shift_action = do_nothing_shift_action;
  }
let const_decl2_middle : binary_op = 
  {
    meta = const_decl2_middle_meta;
    reduction = p_internal_error "BP104: const_decl2_middle reduction";
    shift_action = do_nothing_shift_action;
  }
let const_decl2_end : binary_op = 
  {
    meta = const_decl2_end_meta;
    reduction =
    (
      let* ((name, defn), ext) = pop_postfix_op_operands_2 const_decl2_end_meta in
      let* bnd_name = get_binding_name name in
      push_elem_on_input_acc_expr (A.annotate_with_extent(A.fold(A.N(N.Declaration(N.ConstantDecl), 
      [[], A.annotate_with_extent (A.free_var bnd_name) (A.get_extent_some name); [], defn]))) ext)
    );
    shift_action = do_nothing_shift_action;
  }

let constructor_decl_middle_uid = Uid.next()
let constructor_decl_end_uid = Uid.next()

let constructor_decl_middle_meta : binary_op_meta = 
  {
    id = constructor_decl_middle_uid;
    keyword = CS.new_t_string "立";
    left_fixity = FxOp (Some 10);
    right_fixity = FxComp constructor_decl_end_uid;
  }
let constructor_decl_end_meta : binary_op_meta = 
  {
    id = constructor_decl_end_uid;
    keyword = CS.new_t_string "也";
    left_fixity = FxComp constructor_decl_middle_uid;
    right_fixity = FxNone;
  }

let constructor_decl_middle : binary_op = {
    meta = constructor_decl_middle_meta;
    reduction = p_internal_error "BP104: constructor_decl_middle reduction";
    shift_action = do_nothing_shift_action;
  }
let constructor_decl_end : binary_op =
  {
    meta = constructor_decl_end_meta;
    reduction =
    (
      let* ((name, defn), ext) = pop_postfix_op_operands_2 constructor_decl_end_meta in
      let* () = assert_is_free_var name in
      push_elem_on_input_acc_expr (A.annotate_with_extent(A.fold(A.N(N.Declaration(N.ConstructorDecl), [[], name; [], defn]))) ext)
    );
    shift_action = do_nothing_shift_action;
}

let constructor_decl2_start_uid = Uid.next()
let constructor_decl2_middle_uid = Uid.next()
let constructor_decl2_end_uid = Uid.next()
let constructor_decl2_start_meta : binary_op_meta = 
  {
    id = constructor_decl2_start_uid;
    keyword = CS.new_t_string "夫";
    left_fixity = FxNone;
    right_fixity = FxBinding constructor_decl2_middle_uid;
  }
let constructor_decl2_middle_meta : binary_op_meta = 
  {
    id = constructor_decl2_middle_uid;
    keyword = CS.new_t_string "立";
    left_fixity = FxBinding constructor_decl2_start_uid;
    right_fixity = FxComp constructor_decl2_end_uid;
  }
let constructor_decl2_end_meta : binary_op_meta = 
  {
    id = constructor_decl2_end_uid;
    keyword = CS.new_t_string "也";
    left_fixity = FxComp constructor_decl2_middle_uid;
    right_fixity = FxNone;
  }
let constructor_decl2_start : binary_op = 
  {
    meta = constructor_decl2_start_meta;
    reduction = p_internal_error "BP104: constructor_decl2_start reduction";
    shift_action = do_nothing_shift_action;
  }
let constructor_decl2_middle : binary_op = 
  {
    meta = constructor_decl2_middle_meta;
    reduction = p_internal_error "BP104: constructor_decl2_middle reduction";
    shift_action = add_prev_identifier_shift_action;
  }
let constructor_decl2_end : binary_op = 
  {
    meta = constructor_decl2_end_meta;
    reduction =
    (
      let* ((name, defn), ext) = pop_postfix_op_operands_2 constructor_decl2_end_meta in
      let* bnd_name = get_binding_name name in
      push_elem_on_input_acc_expr (A.annotate_with_extent(A.fold(A.N(N.Declaration(N.ConstructorDecl), 
      [[], A.annotate_with_extent (A.free_var bnd_name) (A.get_extent_some name); [], defn]))) ext)
    );
    shift_action = do_nothing_shift_action;
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
    shift_action = do_nothing_shift_action;
  }
let right_parenthesis : binary_op = 
  {
    meta = right_parenthesis_meta;
    reduction = 
    (
      let* (oper, per_ext) = pop_postfix_op_operands_1 right_parenthesis_meta in
      push_elem_on_input_acc_expr (A.annotate_with_extent oper per_ext)
    );
    shift_action = do_nothing_shift_action;
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
    shift_action = do_nothing_shift_action;
  }
let double_parenthesis_right : binary_op = 
  {
    meta = double_parenthesis_right_meta;
    reduction = 
    (
      let* (oper, per_ext) = pop_postfix_op_operands_1 double_parenthesis_right_meta in
      push_elem_on_input_acc_expr (A.annotate_with_extent oper per_ext)
    );
    shift_action = do_nothing_shift_action;
  }


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
    right_fixity = FxOp (Some 40);
  }
let explicit_pi_start : binary_op = 
  {
    meta = explicit_pi_start_meta;
    reduction = p_internal_error "BP104: explicit_pi_start reduction";
    shift_action = do_nothing_shift_action;
  }
let explicit_pi_middle_1 : binary_op = 
  {
    meta = explicit_pi_middle_1_meta;
    reduction = p_internal_error "BP104: explicit_pi_middle_1 reduction";
    shift_action = do_nothing_shift_action;
  }
let explicit_pi_middle_2 : binary_op = 
  {
    meta = explicit_pi_middle_2_meta;
    reduction = 
    (
      let* ((tp_name, bnd_name, range_expr), per_ext) = pop_prefix_op_operands_3 explicit_pi_middle_2_meta in
      let* binding_name = get_binding_name bnd_name in
      let result_expr = A.fold_with_extent(A.N(N.ExplicitPi, [[], tp_name; [binding_name], range_expr])) per_ext in
      push_elem_on_input_acc_expr result_expr 
    );
    shift_action = add_prev_identifier_shift_action;
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
    right_fixity = FxOp (Some 40);
  }
let implicit_pi_start : binary_op = 
  {
    meta = implicit_pi_start_meta;
    reduction = p_internal_error "BP104: implicit_pi_start reduction";
    shift_action = do_nothing_shift_action;
  }
let implicit_pi_middle_1 : binary_op = 
  {
    meta = implicit_pi_middle_1_meta;
    reduction = p_internal_error "BP104: implicit_pi_middle_1 reduction";
    shift_action = do_nothing_shift_action;
  }
let implicit_pi_middle_2 : binary_op = 
  {
    meta = implicit_pi_middle_2_meta;
    reduction = 
    (
      let* ((tp_name, bnd_name, range_expr), per_ext) = pop_prefix_op_operands_3 implicit_pi_middle_2_meta in
      let* binding_name = get_binding_name bnd_name in
      let result_expr = A.fold_with_extent(A.N(N.ImplicitPi, [[], tp_name; [binding_name], range_expr])) per_ext in
      push_elem_on_input_acc_expr result_expr 
  );
    shift_action = add_prev_identifier_shift_action;
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
    right_fixity = FxOp (Some 40);
  }
let arrow_start : binary_op = 
  {
    meta = arrow_start_meta;
    reduction = p_internal_error "BP104: arrow_start reduction";
    shift_action = do_nothing_shift_action;
  }
let arrow_middle : binary_op = 
  {
    meta = arrow_middle_meta;
    reduction = 
    (
      let* ((tp_name, range_expr), per_ext) = pop_prefix_op_operands_2 arrow_middle_meta in
      let result_expr = A.fold(A.N(N.Arrow, [[], tp_name; [], range_expr])) in
      push_elem_on_input_acc_expr (A.annotate_with_extent result_expr per_ext)
    );
    shift_action = do_nothing_shift_action;
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
    right_fixity = FxOp (Some 50);
  }
let implicit_lam_abs_start : binary_op = 
  {
    meta = implicit_lam_abs_start_meta;
    reduction = p_internal_error "BP104: implicit_lam_abs_start reduction";
    shift_action = do_nothing_shift_action;
  }
let implicit_lam_abs_middle : binary_op = 
  {
    meta = implicit_lam_abs_middle_meta;
    reduction = 
    (
      let* ((bnd_name, range_expr), per_ext) = pop_prefix_op_operands_2 implicit_lam_abs_middle_meta in
      let* binding_name = get_binding_name bnd_name in
      let result_expr = A.fold_with_extent (A.N(N.Lam, [[binding_name], range_expr])) per_ext in
      push_elem_on_input_acc_expr result_expr 
    );
    shift_action = add_prev_identifier_shift_action;
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
    right_fixity = FxOp (Some 50);
  }
let explicit_lam_abs_start : binary_op = 
  {
    meta = explicit_lam_abs_start_meta;
    reduction = p_internal_error "BP104: explicit_lam_abs_start reduction";
    shift_action = do_nothing_shift_action;
  }
let explicit_lam_abs_middle : binary_op = 
  {
    meta = explicit_lam_abs_middle_meta;
    reduction = 
    (
      let* ((tp_name, range_expr), per_ext) = pop_prefix_op_operands_2 explicit_lam_abs_middle_meta in
      let* binding_name = get_binding_name tp_name in
      let result_expr = A.fold_with_extent (A.N(N.Lam, [[binding_name], range_expr])) per_ext in
      push_elem_on_input_acc_expr result_expr 
  );
    shift_action = add_prev_identifier_shift_action;
  }

let typed_lam_abs_start_uid = Uid.next()
let typed_lam_abs_middle1_uid = Uid.next()
let typed_lam_abs_middle2_uid = Uid.next()
let typed_lam_abs_start_meta = 
  {
    id = typed_lam_abs_start_uid;
    keyword = CS.new_t_string "遇";
    left_fixity = FxNone;
    right_fixity = FxComp typed_lam_abs_middle1_uid;
  }
let typed_lam_abs_middle1_meta = 
  {
    id = typed_lam_abs_middle1_uid;
    keyword = CS.new_t_string "者";
    left_fixity = FxComp typed_lam_abs_start_uid;
    right_fixity = FxBinding typed_lam_abs_middle2_uid;
  }
let typed_lam_abs_middle2_meta = 
  {
    id = typed_lam_abs_middle2_uid;
    keyword = CS.new_t_string "而";
    left_fixity = FxBinding typed_lam_abs_middle1_uid;
    right_fixity = FxOp (Some 50);
  }
let typed_lam_abs_start : binary_op = 
  {
    meta = typed_lam_abs_start_meta;
    reduction = p_internal_error "BP104: typed_lam_abs_start reduction";
    shift_action = do_nothing_shift_action;
  }
let typed_lam_abs_middle1 : binary_op = 
  {
    meta = typed_lam_abs_middle1_meta;
    reduction = p_internal_error "BP104: typed_lam_abs_middle1 reduction";
    shift_action = do_nothing_shift_action;
  }
let typed_lam_abs_middle2 : binary_op = 
  {
    meta = typed_lam_abs_middle2_meta;
    reduction = 
    (
      let* ((tp_name, bnd_name, body_expr), per_ext) = pop_prefix_op_operands_3 typed_lam_abs_middle2_meta in
      let* binding_name = get_binding_name bnd_name in
      let result_expr = A.fold_with_extent (A.N(N.TypedLam, [[], tp_name;[binding_name], body_expr])) per_ext in
      push_elem_on_input_acc_expr result_expr 
  );
    shift_action = add_prev_identifier_shift_action;
  }


let implicit_ap_uid = Uid.next()
let implicit_ap_meta = 
  {
    id = implicit_ap_uid;
    keyword = CS.new_t_string "授以";
    left_fixity = FxOp (Some 799);
    right_fixity = FxOp (Some 800);
  }
let implicit_ap : binary_op = 
  {
    meta = implicit_ap_meta;
    reduction = 
    (
      let* ((f, arg), per_ext) = pop_bin_operand implicit_ap_meta in
      push_elem_on_input_acc_expr (A.fold_with_extent(A.N(N.Ap, [[], f; [], arg])) per_ext)
  );
    shift_action = do_nothing_shift_action;
  }

let explicit_ap_uid = Uid.next()
let explicit_ap_meta = 
  {
    id = explicit_ap_uid;
    keyword = CS.new_t_string "于";
    left_fixity = FxOp (Some 799);
    right_fixity = FxOp (Some 800);
  }
let explicit_ap : binary_op = 
  {
    meta = explicit_ap_meta;
    reduction = 
    (
      let* ((f, arg), per_ext) = pop_bin_operand explicit_ap_meta in
      push_elem_on_input_acc_expr (A.fold_with_extent(A.N(N.Ap, [[], f; [], arg])) per_ext)
  );
    shift_action = do_nothing_shift_action;
  }


let add_declaration_name_identifier (declaration : A.t) : unit proc_state_m = 
  match A.view declaration with
  | A.N(N.Declaration(N.ConstantDecl), [[], name; _ ]) 
  | A.N(N.Declaration(N.ConstructorDecl), [[], name; _ ])
  | A.N(N.Declaration(N.ConstantDefn), [[], name; _ ])
  -> 
    (
      match A.view name with
      | A.FreeVar(name) -> (
        add_identifier_processor_no_repeat (CS.new_t_string name)
      )
      | _ -> pfail ("BP679: Expected a constant declaration but got " ^ A.show_view declaration)
    )
  | A.N(N.Declaration(N.CustomOperatorDecl), [[], _; _ ]) -> 
    (
      return ()
    )
  | _ -> failwith ("BP679: Expected a constant declaration but got " ^ A.show_view declaration)

  let sentence_end_fail (module_expr : input_acc_elem) (decl_expr : input_acc_elem) : unit proc_state_m = 
    let* st = get_proc_state () in
    pfail ("BP678: Expected a module defn and a decl but got " ^ (show_input_acc_elem module_expr) ^ " and " ^ (show_input_acc_elem decl_expr)
    ^ "input_acc = " ^ show_input_acc st.input_acc) 

  let sentence_end : unit proc_state_m =
    let* _ = read_one_of_string [CS.new_t_string "。"] in
    (* reduce all existing expressions*)
    let* _ = operator_precedence_reduce_always () in
    let* input_acc_size = get_input_acc_size () in
    let* () = (
      if input_acc_size = 1  then
        let* (module_expr, _) = pop_input_acc_expr () in
        match A.view module_expr with
        | A.N(N.ModuleDef, _) -> 
          let* () = push_elem_on_input_acc_expr module_expr in
          return ()
        | _ -> pfail ("BP207: Expected a module defn but got " ^ A.show_view module_expr)
      else if input_acc_size > 1 then
        let* (poped_left, poped_right) = pop_input_acc_2 () in 
        match poped_left, poped_right with
        | Expr(module_expr), Expr(decl) -> 
          (
            match A.view module_expr, A.view decl with
            | A.N(N.ModuleDef, args), A.N(N.Declaration(_), _) -> 
              let* () = add_declaration_name_identifier decl in
              push_elem_on_input_acc_expr
                (A.annotate_with_extent
                  (A.fold(A.N(N.ModuleDef, args@[[], decl]))) 
                  (Ext.combine_extent (A.get_extent_some module_expr) (A.get_extent_some decl))
                )
            
            | A.N(N.ModuleDef, args), _ -> 
              let direct_expr_decl = A.fold_with_extent (A.N(N.Declaration(N.DirectExpr), [[], decl])) (A.get_extent_some decl) in
              push_elem_on_input_acc_expr
                (A.annotate_with_extent
                  (A.fold(A.N(N.ModuleDef, args@[[], direct_expr_decl]))) 
                  (Ext.combine_extent (A.get_extent_some module_expr) (A.get_extent_some decl))
                )
            | _ -> 
              sentence_end_fail (Expr module_expr) (Expr decl)
          )
        | ParsingElem(start_op, _start_op_ext), Expr(decl) -> 
          (
            match start_op, A.view decl with
            (* also for 「「 name *)
            | OpKeyword({id=opid;_}, _), A.N(N.Declaration(_), _) -> 
              if opid = double_parenthesis_left_uid || opid = left_parenthesis_uid then 
                (* push 「「 back onto the stack *)
                let* _ = push_elem_on_input_acc poped_left in
                let* _ = add_declaration_name_identifier decl in (* we choose not to descope this *)
                let* _ = push_elem_on_input_acc_expr (A.fold_with_extent (A.N(N.ModuleDef, [[],decl])) (A.get_extent_some decl)) in
                return ()
              else  sentence_end_fail poped_left poped_right
            | OpKeyword({id=opid;_}, _), _ ->
              if opid = double_parenthesis_left_uid || opid = left_parenthesis_uid then 
                let direct_expr_decl = A.fold_with_extent (A.N(N.Declaration(N.DirectExpr), [[], decl])) (A.get_extent_some decl) in
                (* push 「「 back onto the stack *)
                let* _ = push_elem_on_input_acc poped_left in
                let* _ = push_elem_on_input_acc_expr (A.fold_with_extent (A.N(N.ModuleDef, [[],direct_expr_decl])) (A.get_extent_some decl)) in
                return ()
              else  sentence_end_fail poped_left poped_right
            | _ -> 
              sentence_end_fail poped_left poped_right
          )
        (* also for 「「 name *)
        | _ -> sentence_end_fail poped_left poped_right
      else
        pfail ("ET106: Expected at least 2 elements in the input acc but got " ^ string_of_int input_acc_size)
    ) in
    (* we want to commit once we successfully handled 。 if subsequent error occurs, 
      do not backtrack over interpretation of sentences *)
    pcommit ()
    

let external_call_start_uid = Uid.next()
let external_call_end_uid = Uid.next()
let external_call_start_meta : binary_op_meta = 
  {
    id = external_call_start_uid;
    keyword = CS.new_t_string "《《外部调用";
    left_fixity = FxNone;
    right_fixity = FxComp external_call_end_uid;
  }
let external_call_end_meta : binary_op_meta = 
  {
    id = external_call_end_uid;
    keyword = CS.new_t_string "》》";
    left_fixity = FxComp external_call_start_uid;
    right_fixity = FxNone;
  }
let external_call_start : binary_op = 
  {
    meta = external_call_start_meta;
    reduction = p_internal_error "BP104: external_call_start reduction";
    shift_action = do_nothing_shift_action;
  }
let external_call_end : binary_op = 
  {
    meta = external_call_end_meta;
    reduction = 
    (
      let* (oper, per_ext) = pop_postfix_operand external_call_end_meta in
        match A.view oper with
        | A.N(N.Builtin(N.String(x)), []) -> 
          (
            push_elem_on_input_acc_expr (A.fold_with_extent (A.N(N.ExternalCall(x), []) ) per_ext)
          )
        | _ -> pfail ("BP693: Builtin Expected a string but got " ^ A.show_view oper)
    );
    shift_action = do_nothing_shift_action;
  }
  
let if_then_else_start_uid = Uid.next()
let if_then_else_mid1_uid = Uid.next()
let if_then_else_mid2_uid = Uid.next()

let if_then_else_start_meta = 
  {
    id = if_then_else_start_uid;
    keyword = CS.new_t_string "若";
    left_fixity = FxNone;
    right_fixity = FxComp if_then_else_mid1_uid;
  }
let if_then_else_mid1_meta = 
  {
    id = if_then_else_mid1_uid;
    keyword = CS.new_t_string "则";
    left_fixity = FxComp if_then_else_start_uid;
    right_fixity = FxComp if_then_else_mid2_uid;
  }
let if_then_else_mid2_meta = 
  {
    id = if_then_else_mid2_uid;
    keyword = CS.new_t_string "否则";
    left_fixity = FxComp if_then_else_mid1_uid;
    right_fixity = FxOp (Some 80);
  }

let if_then_else_start : binary_op = 
  {
    meta = if_then_else_start_meta;
    reduction = p_internal_error "BP104: if_then_else_start reduction";
    shift_action = do_nothing_shift_action;
  }
let if_then_else_mid1 : binary_op = 
  {
    meta = if_then_else_mid1_meta;
    reduction = p_internal_error "BP104: if_then_else_mid1 reduction";
    shift_action = do_nothing_shift_action;
  }
let if_then_else_mid2 : binary_op = 
  {
    meta = if_then_else_mid2_meta;
    reduction = 
    (
      let* ((cond, then_expr, else_expr), per_ext) = pop_prefix_op_operands_3 if_then_else_mid2_meta in
      let result_expr = A.fold_with_extent (A.N(N.IfThenElse, [[], cond; [], then_expr; [], else_expr])) per_ext in
      push_elem_on_input_acc_expr result_expr
  );
    shift_action = do_nothing_shift_action;
  }

let match_subject_start_uid = Uid.next()
let match_subject_end_uid = Uid.next()
let match_subject_start_meta = 
  {
    id = match_subject_start_uid;
    keyword = CS.new_t_string "鉴";
    left_fixity = FxNone;
    right_fixity = FxComp match_subject_end_uid;
  }
let match_subject_end_meta = 
  {
    id = match_subject_end_uid;
    keyword = CS.new_t_string "而";
    left_fixity = FxComp match_subject_start_uid;
    right_fixity = FxNone;
  }
let match_subject_start : binary_op = 
  {
    meta = match_subject_start_meta;
    reduction = p_internal_error "BP104: match_subject_start reduction";
    shift_action = do_nothing_shift_action;
  }
let match_subject_end : binary_op = 
  {
    meta = match_subject_end_meta;
    reduction = 
    (
      let* (oper, per_ext) = pop_postfix_op_operands_1 match_subject_end_meta in
      push_elem_on_input_acc_expr (A.fold_with_extent (A.N(N.Match, [[], oper])) per_ext)
    );
    shift_action = do_nothing_shift_action;
  }

let match_case_start_uid = Uid.next()
let match_case_mid_uid = Uid.next()
let match_case_start_meta = 
  {
    id = match_case_start_uid;
    keyword = CS.new_t_string "有";
    left_fixity = FxNone;
    right_fixity = FxComp match_case_mid_uid;
  }
let match_case_mid_meta = 
  {
    id = match_case_mid_uid;
    keyword = CS.new_t_string "则";
    left_fixity = FxComp match_case_start_uid;
    right_fixity = FxOp (Some 70);
  }
let match_case_start : binary_op = 
  {
    meta = match_case_start_meta;
    reduction = p_internal_error "BP104: match_case_start reduction";
    shift_action = do_nothing_shift_action;
  }
let match_case_mid : binary_op = 
  {
    meta = match_case_mid_meta;
    reduction = 
    (
      let* ((case_expr, then_expr), per_ext) = pop_prefix_op_operands_2 match_case_mid_meta in
      let result_expr = A.fold_with_extent (A.N(N.MatchCase, [[], case_expr; [], then_expr])) per_ext in
      push_elem_on_input_acc_expr result_expr
    );
    shift_action = add_prev_expr_shift_action;
  }

let match_case_alternative_meta : binary_op_meta = 
  {
    id = Uid.next();
    keyword = CS.new_t_string "或";
    left_fixity = FxOp (Some 59);
    right_fixity = FxOp (Some 60);
  }
let match_case_alternative : binary_op = 
  {
    meta = match_case_alternative_meta;
    reduction = 
    (
      let* ((case_expr, then_expr), per_ext) = pop_bin_operand match_case_alternative_meta in
      match A.view case_expr with
      | A.N(N.Match, args) -> 
        let new_case_expr = A.fold(A.N(N.Match, args@[[], then_expr])) in
        push_elem_on_input_acc_expr (A.annotate_with_extent new_case_expr per_ext)
      | _ -> pfail ("ET108: Expected a match case but got " ^ A.show_view case_expr)
    );
    shift_action = do_nothing_shift_action;
  }

let comma_char = "，"

let comma_sequence_meta : binary_op_meta = {
      id = Uid.next();
      keyword = CS.new_t_string comma_char;
      left_fixity = FxOp (Some 89);
      right_fixity = FxOp (Some 90);
    }

let comma_sequence : binary_op =
  {
    meta = comma_sequence_meta;
    reduction = 
    (
      let* ((x, y), per_ext) = pop_bin_operand comma_sequence_meta in
      match A.view x with
      | A.N(N.Sequence "，", args) -> 
        push_elem_on_input_acc_expr (A.fold_with_extent (A.N(N.Sequence comma_char, args@[[], y])) per_ext)
      | _ ->
        push_elem_on_input_acc_expr (A.fold_with_extent(A.N(N.Sequence comma_char, [[], x; [], y])) per_ext)
    );
    shift_action = do_nothing_shift_action;
  }


let enumeration_comma_char = "、"
let enumeration_comma_sequence_meta : binary_op_meta = {
      id = Uid.next();
      keyword = CS.new_t_string enumeration_comma_char;
      left_fixity = FxOp (Some 109);
      right_fixity = FxOp (Some 110);
    }
let enumeration_comma_sequence : binary_op =
  {
    meta = enumeration_comma_sequence_meta;
    reduction = 
    (
      let* ((x, y), per_ext) = pop_bin_operand enumeration_comma_sequence_meta in
      match A.view x with
      | A.N(N.Sequence "、", args) -> 
        push_elem_on_input_acc_expr (A.fold_with_extent (A.N(N.Sequence enumeration_comma_char, args@[[], y])) per_ext)
      | _ ->
        push_elem_on_input_acc_expr (A.fold_with_extent(A.N(N.Sequence enumeration_comma_char, [[], x; [], y])) per_ext)
    );
    shift_action = do_nothing_shift_action;
  }



let custom_operator_decl_start : unit proc_state_m = 
  let* _ = read_one_of_string [CS.new_t_string "术"] in
  let* (defn, defn_ext) = scan_until_one_of_string [CS.new_t_string "盖谓"] in
  (* precheck if operators can be get *)
  let* _ = UserDefinedOperators.get_operators_m defn (A.fold_with_extent(A.FreeVar("TRIVIAL")) defn_ext) in
  (* if we can get operators (meaning this is at least well-formed, push the scanned thing onto the stack)*)
  push_elem_on_input_acc_expr (A.fold_with_extent(A.N(N.Builtin(N.CustomOperatorString defn), [])) defn_ext)
  (* then we can scan the rest of the line *)
  (* reduce the stack *)

let custom_operator_decl_middle_uid = Uid.next()
let custom_operator_decl_end_uid = Uid.next()
let custom_operator_decl_middle_meta = 
  {
    id = custom_operator_decl_middle_uid;
    keyword = CS.new_t_string "盖谓";
    left_fixity = FxNone;
    right_fixity = FxComp custom_operator_decl_end_uid;
  }
let custom_operator_decl_end_meta = 
  {
    id = custom_operator_decl_end_uid;
    keyword = CS.new_t_string "也";
    left_fixity = FxComp custom_operator_decl_middle_uid;
    right_fixity = FxNone;
  }
let custom_operator_decl_middle : binary_op = 
  {
    meta = custom_operator_decl_middle_meta;
    reduction = p_internal_error "BP104: custom_operator_decl_middle reduction";
    shift_action = do_nothing_shift_action;
  }
let custom_operator_decl_end : binary_op = 
  {
    meta = custom_operator_decl_end_meta;
    reduction = 
    (
      let* (result, per_ext) = pop_postfix_operand custom_operator_decl_end_meta in
      let* (defn, defn_ext) = pop_input_acc_expr () in
      match A.view defn with
      | A.N(N.Builtin(N.CustomOperatorString oper_str), []) -> 
        let* new_ops = UserDefinedOperators.get_operators_m oper_str result in
        let* () = push_elem_on_input_acc_expr (A.fold_with_extent
                                    (A.N(N.Declaration(N.CustomOperatorDecl), [[], defn; [], result]))
                                    (Ext.combine_extent defn_ext per_ext)
                                    ) in
        print_endline ("Adding new Ops: " ^ String.concat ", " (List.map (fun x -> show_binary_op_meta x.meta) new_ops));
        (* add new operators to the registry *)
        let* () = add_processor_entry_list (List.map (to_processor_binary_op "custom_ops") new_ops) in
        return ()
      | _ -> pfail ("ET109: Expected a scanned operator but got " ^ A.show_view defn)
    );
    shift_action = do_nothing_shift_action;
  }

let let_in_start_uid = Uid.next()
let let_in_mid1_uid = Uid.next()
let let_in_mid2_uid = Uid.next()

let let_in_start_meta = 
  {
    id = let_in_start_uid;
    keyword = CS.new_t_string "虑";
    left_fixity = FxNone;
    right_fixity = FxBinding let_in_mid1_uid;
  }
let let_in_mid1_meta = 
  {
    id = let_in_mid1_uid;
    keyword = CS.new_t_string "者";
    left_fixity = FxBinding let_in_start_uid;
    right_fixity = FxComp let_in_mid2_uid;
  }
let let_in_mid2_meta = 
  {
    id = let_in_mid2_uid;
    keyword = CS.new_t_string "而";
    left_fixity = FxComp let_in_mid1_uid;
    right_fixity = FxOp (Some 55);
  }
let let_in_start : binary_op = 
  {
    meta = let_in_start_meta;
    reduction = p_internal_error "BP104: let_in_start reduction";
    shift_action = do_nothing_shift_action;
  }
let let_in_mid1 : binary_op = 
  {
    meta = let_in_mid1_meta;
    reduction = p_internal_error "BP104: let_in_mid1 reduction";
    shift_action = add_prev_identifier_shift_action;
  }
let let_in_mid2 : binary_op = 
  {
    meta = let_in_mid2_meta;
    reduction = 
    (
      let* ((bnd_name, domain_expr, range_expr), per_ext) = pop_prefix_op_operands_3 let_in_mid2_meta in
      let* binding_name = get_binding_name bnd_name in
      let result_expr = A.fold_with_extent (A.N(N.LetIn, [[], domain_expr;[binding_name], range_expr])) per_ext in
      push_elem_on_input_acc_expr result_expr 
    );
    shift_action = do_nothing_shift_action;
  }

let typing_annotation_middle_uid = Uid.next()
let typing_annotation_end_uid = Uid.next()
let typing_annotation_middle_meta = 
  {
    id = typing_annotation_middle_uid;
    keyword = CS.new_t_string "其";
    left_fixity = FxOp (Some 120);
    right_fixity = FxComp typing_annotation_end_uid;
  }
let typing_annotation_end_meta = 
  {
    id = typing_annotation_end_uid;
    keyword = CS.new_t_string "也";
    left_fixity = FxComp typing_annotation_middle_uid;
    right_fixity = FxNone;
  }
let typing_annotation_middle : binary_op = 
  {
    meta = typing_annotation_middle_meta;
    reduction = p_internal_error "BP104: typing_annotation_middle reduction";
    shift_action = do_nothing_shift_action;
  }
let typing_annotation_end : binary_op = 
  {
    meta = typing_annotation_end_meta;
    reduction = 
    (
      let* ((body_expr, type_expr), per_ext) = pop_postfix_op_operands_2 typing_annotation_end_meta in
      let result_expr = A.fold_with_extent (A.N(N.TypingAnnotation, [[], body_expr; [], type_expr])) per_ext in
      push_elem_on_input_acc_expr result_expr 
    );
    shift_action = do_nothing_shift_action;
  }


let default_registry = [
  to_processor_complex "top_level_empty_space_ignore" top_level_empty_space_ignore;
  to_processor_complex "single_comment" (single_comment ());
  (* to_processor_complex (Scanning InComment) "comment_start" comment_start;
  to_processor_complex (Scanning InComment) "comment_middle" comment_middle;
  to_processor_complex (Scanning InComment) "comment_end" comment_end; *)
  to_processor_complex "string_parser_pusher" string_parser_pusher;
  to_processor_binary_op "definition_middle" definition_middle;
  to_processor_binary_op "definition_end" definition_end;
  to_processor_binary_op "import_end" import_end;
  to_processor_binary_op "library_root" library_root;
  to_processor_binary_op "unknown_structure_deref" unknown_structure_deref;
  (* to_processor_complex Expression "known_structure_deref" known_structure_deref; *)
  (* to_processor_binary_op Expression "statement_end" statement_end; *)
  to_processor_complex "sentence_end" sentence_end;
  to_processor_binary_op "builtin_op" builtin_op;

  to_processor_binary_op "module_open" module_open;
  to_processor_binary_op "module_reexport" module_reexport;

  to_processor_binary_op "const_decl_middle" const_decl_middle;
  to_processor_binary_op "const_decl_end" const_decl_end;
  to_processor_binary_op "constructor_decl_middle" constructor_decl_middle;
  to_processor_binary_op "constructor_decl_end" constructor_decl_end;
  to_processor_binary_op "left_parenthesis" left_parenthesis;
  to_processor_binary_op "right_parenthesis" right_parenthesis;
  to_processor_binary_op "explicit_pi_start" explicit_pi_start;
  to_processor_binary_op "explicit_pi_middle_1" explicit_pi_middle_1;
  to_processor_binary_op "explicit_pi_middle_2" explicit_pi_middle_2;
  to_processor_binary_op "implicit_pi_start" implicit_pi_start;
  to_processor_binary_op "implicit_pi_middle_1" implicit_pi_middle_1;
  to_processor_binary_op "implicit_pi_middle_2" implicit_pi_middle_2;
  to_processor_binary_op "arrow_start" arrow_start;
  to_processor_binary_op "arrow_middle" arrow_middle;

  (* lambdas *)
  to_processor_binary_op "implicit_lam_abs_start" implicit_lam_abs_start;
  to_processor_binary_op "implicit_lam_abs_middle" implicit_lam_abs_middle;
  to_processor_binary_op "explicit_lam_abs_start" explicit_lam_abs_start;
  to_processor_binary_op "explicit_lam_abs_middle" explicit_lam_abs_middle;
  to_processor_binary_op "typed_lam_abs_start" typed_lam_abs_start;
  to_processor_binary_op "typed_lam_abs_middle1" typed_lam_abs_middle1;
  to_processor_binary_op "typed_lam_abs_middle2" typed_lam_abs_middle2;

  (* application *)
  to_processor_binary_op "implicit_ap" implicit_ap;
  to_processor_binary_op "explicit_ap" explicit_ap;
  to_processor_binary_op "double_parenthesis_left" double_parenthesis_left;
  to_processor_binary_op "double_parenthesis_right" double_parenthesis_right;
  to_processor_binary_op "external_call_start" external_call_start;
  to_processor_binary_op "external_call_end" external_call_end;

  (* if *)
  to_processor_binary_op "if_then_else_start" if_then_else_start;
  to_processor_binary_op "if_then_else_mid1" if_then_else_mid1;
  to_processor_binary_op "if_then_else_mid2" if_then_else_mid2;

  (* match*)
  to_processor_binary_op "match_subject_start" match_subject_start;
  to_processor_binary_op "match_subject_end" match_subject_end;
  to_processor_binary_op "match_case_start" match_case_start;
  to_processor_binary_op "match_case_mid" match_case_mid;
  to_processor_binary_op "match_case_alternative" match_case_alternative;

  (* lists *)
  to_processor_binary_op "comma_sequence" comma_sequence;
  to_processor_binary_op "enumeration_comma_sequence" enumeration_comma_sequence;

  (* custom operators*)
  to_processor_complex "custom_operator_decl_start" custom_operator_decl_start;
  to_processor_binary_op "custom_operator_decl_middle" custom_operator_decl_middle;
  to_processor_binary_op "custom_operator_decl_end" custom_operator_decl_end;

  (* let in*)
  to_processor_binary_op "let_in_start" let_in_start;
  to_processor_binary_op "let_in_mid1" let_in_mid1;
  to_processor_binary_op "let_in_mid2" let_in_mid2;


  (* defn 2*)
  to_processor_binary_op "definition2_start" definition2_start;
  to_processor_binary_op "definition2_middle" definition2_middle;
  to_processor_binary_op "definition2_end" definition2_end;

  (* decl 2*)
  to_processor_binary_op "const_decl2_start" const_decl2_start;
  to_processor_binary_op "const_decl2_middle" const_decl2_middle;
  to_processor_binary_op "const_decl2_end" const_decl2_end;
  to_processor_binary_op "constructor_decl2_start" constructor_decl2_start;
  to_processor_binary_op "constructor_decl2_middle" constructor_decl2_middle;
  to_processor_binary_op "constructor_decl2_end" constructor_decl2_end;

  (* typing annotation *)
  to_processor_binary_op "typing_annotation_middle" typing_annotation_middle;
  to_processor_binary_op "typing_annotation_end" typing_annotation_end;

  to_processor_complex "identifier_parser_pusher" identifier_parser_pusher;
  to_processor_complex "number_parser" (integer_number_parser ());
  to_processor_complex "decimal_number_parser" (decimal_number_parser ());

] @ List.concat [
 ]