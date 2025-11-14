open EngineData
open ProcCombinators
module Ext = AbtLib.Extent
module CS = CharStream
module Env = Environment
open BasicParsing
open EngineDataPrint

(* let get_binding_name (x : input_acc_elem) : string proc_state_m = 
  match x with
  | ParsingElem(BoundScannedString(s), _) -> return (CS.get_t_string s)
  | _ -> Fail.failwith ("ET107: Expected a bound scanned string but got " ^ (show_input_acc_elem x)) *)

let get_binding_name (x : A.t) : Ext.t_str proc_state_m =
  match A.view x with
  | A.FreeVar name -> return name
  | _ -> Fail.failwith ("ET107: Expected a free variable but got " ^ A.show_view x)
;;

(* let top_level_identifier_pusher : unit proc_state_m = 
  read_any_char_except_and_push (CharStream.new_t_string "。（）「」『』\n\t\r"@[" "]) *)

let top_level_empty_space_ignore : unit proc_state_m = read_one_of_char (CharStream.new_t_string " \n\t\r") >> ignore ()

let identifier_parser_pusher : unit proc_state_m =
  let* id, ext = identifier_parser () in
  (* Check for consecutive identifiers *)
  let* acc_top = peek_input_acc 0 in
  let* () =
    match acc_top with
    | Some (Expr x) ->
      (match A.view x with
       | A.FreeVar prev_id ->
         pfail_with_ext
           ("Syntax Error: Consecutive quoted identifiers are not allowed. Found 「"
            ^ Ext.get_str_content prev_id
            ^ "」「"
            ^ CS.get_t_string id
            ^ "」. "
            ^ "An operator is required between identifiers.")
           ext
       | _ -> return ())
    | _ -> return ()
  in
  if List.for_all (fun x -> List.mem x yy_number_words) id
  then (
    let number = get_int_from_t_string id in
    push_elem_on_input_acc (Expr (A.fold_with_extent (A.N (N.Builtin (N.Int number), [])) ext)))
  else push_elem_on_input_acc (get_identifier_t (id, ext))
;;

let string_escape_sequence_scanner : (CS.t_char * Ext.t) proc_state_m =
  let* _backslash, ext = read_one_of_char [ CS.new_t_char "\\" ] in
  let* next_char, ext2 = read_any_char () in
  match CS.get_t_char next_char with
  | "\\" -> return (next_char, Ext.combine_extent ext ext2)
  | "』" -> return (CS.new_t_char "』", Ext.combine_extent ext ext2)
  | _ -> pfail_with_ext ("ET108: Expected a valid escape sequence but got \\" ^ CS.get_t_char next_char) ext2
;;

let string_character_scanner : (CS.t_char * Ext.t) proc_state_m =
  pcut (choice string_escape_sequence_scanner (read_any_char ()))
;;

let scan_string_body () : ((CS.t_string * Ext.t) * Ext.t) proc_state_m =
  let rec aux acc =
    pcut
      (choice
         (let* c, ext = string_escape_sequence_scanner in
          aux (acc @ [ c, ext ]))
         (let* c, ext = read_any_char () in
          if CS.get_t_char c = "』"
          then if List.is_empty acc then return (([], ext), ext) else return (remap_t_char_list_with_ext acc, ext)
          else aux (acc @ [ c, ext ])))
  in
  aux []
;;

let string_parser_pusher : unit proc_state_m =
  let* start_char, start_ext = read_one_of_char [ CS.new_t_char "『" ] in
  let* (middle, _middle_ext), end_ext = scan_string_body () in
  let* () =
    TokenInfo.add_token_info
      (Ext.str_with_extent
         (CS.get_t_string ([ start_char ] @ middle @ [ CS.new_t_char "』" ]))
         (Ext.combine_extent start_ext end_ext))
      (SemanticToken StringConstant)
  in
  push_elem_on_input_acc
    (Expr
       (A.annotate_with_extent
          (A.fold (A.N (N.Builtin (N.String (CS.get_t_string middle)), [])))
          (Ext.combine_extent start_ext end_ext)))
;;

let rec single_comment () : unit proc_state_m =
  let* comment_text, comment_ext = read_string (CS.new_t_string "「：") in
  let* () =
    TokenInfo.add_token_info (Ext.str_with_extent (CS.get_t_string comment_text) comment_ext) (SemanticToken Comment)
  in
  read_until_comment_end ()

(* using a choice is problematic because of backtracking (things either don't work 
or dont terminate.).  I don't know why.
I need a decision unambiguously without backtracking *)
and read_until_comment_end () : unit proc_state_m =
  let* char1, char1_ext = read_any_char () in
  let* char2, char2_ext = peek_any_char () in
  match CS.get_t_char char1, CS.get_t_char char2 with
  | "：", "」" ->
    let* _ = read_any_char () in
    (* read the ending comment char*)
    let* () =
      TokenInfo.add_token_info
        (Ext.str_with_extent (CS.get_t_string [ char1; char2 ]) (Ext.combine_extent char1_ext char2_ext))
        (SemanticToken Comment)
    in
    return ()
  | "「", "：" ->
    let* _ = read_any_char () in
    let* () =
      TokenInfo.add_token_info
        (Ext.str_with_extent (CS.get_t_string [ char1; char2 ]) (Ext.combine_extent char1_ext char2_ext))
        (SemanticToken Comment)
    in
    (* read the colon char*)
    let* () = read_until_comment_end () in
    (* two levels of comments now, need to read twice*)
    read_until_comment_end ()
  | _ ->
    let* () =
      TokenInfo.add_token_info (Ext.str_with_extent (CS.get_t_string [ char1 ]) char1_ext) (SemanticToken Comment)
    in
    read_until_comment_end ()
;;

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
  { id = Uid.next ()
  ; keyword = CS.new_t_string "之书"
  ; left_fixity = FxOp (Some 90)
  ; right_fixity = FxNone
  ; classification = Structural
  }
;;

let import_end : binary_op =
  { meta = import_end_meta
  ; reduction =
      (let* prev_comp, ext = pop_postfix_operand import_end_meta in
       let* module_path = Imports.get_module_expr prev_comp in
       let* () =
         TokenInfo.add_token_info (Ext.str_with_extent "<import>" ext) (Definition (module_path, (0, 0), (0, 0)))
       in
       let* () = TokenInfo.add_token_info (Ext.str_with_extent "<import>" ext) (Hover module_path) in
       push_elem_on_input_acc (Expr (A.annotate_with_extent (A.fold (A.N (N.FileRef module_path, []))) ext)))
  ; shift_action = do_nothing_shift_action
  }
;;

let assert_is_free_var (x : A.t) : unit proc_state_m =
  match A.view x with
  | A.FreeVar _ -> return ()
  | _ -> Fail.failwith ("ET101: Expected a free variable but got " ^ A.show_view x)
;;

let definition_middle_uid = Uid.next ()
let definition_end_uid = Uid.next ()

let definition_middle_meta : binary_op_meta =
  { id = definition_middle_uid
  ; keyword = CS.new_t_string "者"
  ; left_fixity = FxOp (Some 10)
  ; right_fixity = FxComp definition_end_uid
  ; classification = Structural
  }
;;

let definition_end_meta : binary_op_meta =
  { id = definition_end_uid
  ; keyword = CS.new_t_string "也"
  ; left_fixity = FxComp definition_middle_uid
  ; right_fixity = FxNone
  ; classification = Structural
  }
;;

let definition_middle : binary_op =
  { meta = definition_middle_meta
  ; reduction = p_internal_error "BP104: definition_middle reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let definition_end : binary_op =
  { meta = definition_end_meta
  ; reduction =
      (let* (name, defn), ext = pop_postfix_op_operands_2 definition_end_meta in
       (* *)
       let* () = pcommit () in
       let* defn_name_str = get_free_var name in
       (* let* () = TokenInfo.add_token_info defn_name_str (Definition (Ext.get_str_extent decl_name_str)) in *)
       let* checked_defn_body, tp_expr = TypeChecking.synth_top defn in
       let* () =
         let* aka_print_tp = ProcCombinators.aka_print_expr tp_expr in
         TokenInfo.add_token_info defn_name_str (Hover aka_print_tp)
       in
       let* () = TypeChecking.assert_no_free_vars checked_defn_body in
       let* def_id =
         Environment.add_constant (DataExpression { tp = tp_expr; tm = checked_defn_body; name = Some defn_name_str })
       in
       push_elem_on_input_acc_expr
         (A.annotate_with_extent (A.fold (A.N (N.Declaration (CheckedConstantDefn (defn_name_str, def_id)), []))) ext))
  ; shift_action = do_nothing_shift_action
  }
;;

let unknown_structure_deref_meta : binary_op_meta =
  { id = Uid.next ()
  ; keyword = CS.new_t_string "之"
  ; left_fixity = FxOp (Some 999)
  ; right_fixity = FxOp (Some 1000)
  ; classification = Expression
  }
;;

let unknown_structure_deref : binary_op =
  { meta = unknown_structure_deref_meta
  ; reduction =
      (let* (lo, proj_label), ext = pop_bin_operand unknown_structure_deref_meta in
       match A.view proj_label with
       | A.FreeVar label ->
         let new_node = A.fold (A.N (N.StructureDeref label, [ [], lo ])) in
         push_elem_on_input_acc_expr (A.annotate_with_extent new_node ext)
       | A.N (N.Builtin (N.Int i), []) ->
         let new_node = A.fold (A.N (N.TupleDeref i, [ [], lo ])) in
         push_elem_on_input_acc_expr (A.annotate_with_extent new_node ext)
       | _ -> Fail.failwith ("ET102: Expected a free variable but got " ^ A.show_view proj_label))
  ; shift_action = do_nothing_shift_action
  }
;;

let builtin_op_meta : binary_op_meta =
  { id = Uid.next ()
  ; keyword = CS.new_t_string "内建"
  ; left_fixity = FxNone
  ; right_fixity = FxOp (Some 2000)
  ; classification = Expression
  }
;;

let builtin_op : binary_op =
  { meta = builtin_op_meta
  ; reduction =
      (let* oper, per_ext = pop_prefix_operand builtin_op_meta in
       let* node =
         match A.view oper with
         | A.N (N.Builtin (String x), []) ->
           (match x with
            | "《《内建类型：字符串》》" -> return (A.fold (A.N (N.Builtin N.StringType, [])))
            | "《《内建类型：整数》》" -> return (A.fold (A.N (N.Builtin N.IntType, [])))
            | "《《内建类型：小数》》" -> return (A.fold (A.N (N.Builtin N.FloatType, [])))
            | "《《内建类型：有》》" -> return (A.fold (A.N (N.Builtin N.UnitType, [])))
            | "《《内建类型：爻》》" -> return (A.fold (A.N (N.Builtin N.BoolType, [])))
            | "《《内建爻：阳》》" -> return (A.fold (A.N (N.Builtin (N.Bool true), [])))
            | "《《内建爻：阴》》" -> return (A.fold (A.N (N.Builtin (N.Bool false), [])))
            | "《《内建有：元》》" -> return (A.fold (A.N (N.Builtin N.Unit, [])))
            | _ -> Fail.failwith ("ET104: Expected a builtin val but got >" ^ x ^ "<"))
         | _ -> Fail.failwith ("ET105: Builtin Expected a free variable but got " ^ A.show_view oper)
       in
       push_elem_on_input_acc_expr (A.annotate_with_extent node per_ext))
  ; shift_action = do_nothing_shift_action
  }
;;

let single_word_op (expr : N.builtin) (name : string) : binary_op =
  let meta =
    { id = Uid.next ()
    ; keyword = CS.new_t_string name
    ; left_fixity = FxNone
    ; right_fixity = FxNone
    ; classification = Expression
    }
  in
  { meta
  ; reduction =
      (let* ext = pop_postfix_op_operands_0 meta in
       push_elem_on_input_acc_expr (A.annotate_with_extent (A.fold (A.N (N.Builtin expr, []))) ext))
  ; shift_action = do_nothing_shift_action
  }
;;

let builtin_type = single_word_op N.Type "元类型"
let builtin_data_type = single_word_op N.DataType "数据类型"
let builtin_data_constructor = single_word_op N.DataConstructor "数据构造"
let builtin_data_case = single_word_op N.DataCase "数据分析"
let builtin_recurse = single_word_op N.Recurse "递归"

let module_open_meta : binary_op_meta =
  { id = Uid.next ()
  ; keyword = CS.new_t_string "观"
  ; left_fixity = FxNone
  ; right_fixity = FxOp (Some 80)
  ; classification = Structural
  }
;;

let get_file_ref (file_path : string) : (A.t * t_constants) proc_state_m =
  match !compilation_manager_get_file_hook file_path with
  | Some result -> return result
  | None -> failwith ("Im30: Module not found: " ^ file_path ^ " fileRefs should only contain checked modules")
;;

let add_module_expr_defined_names_to_env (m : A.t) : unit proc_state_m =
  let all_names =
    match A.view m with
    | A.N (N.ModuleDef, args) ->
      List.filter_map
        (fun (_, arg) ->
           match A.view arg with
           | A.N (N.Declaration (N.CheckedConstantDefn (name, id)), _)
           | A.N (N.Declaration (N.ReexportedCheckedConstantDefn (name, id)), _)
           | A.N (N.Declaration (N.ModuleAliasDefn (name, id)), _) -> Some (Environment.add_binding name id)
           | A.N (N.Declaration (N.CheckedDirectExpr _), _) | A.N (N.Declaration N.CustomOperatorDecl, _) -> None
           | _ -> print_failwith ("BP483: Expected a Declaration but got " ^ A.show_view arg))
        args
    | _ -> failwith ("BP282: Expecting moduleDef: " ^ A.show_view m)
  in
  let* _ = psequence all_names in
  return ()
;;

let get_module_expr_defined_custom_ops (_m : A.t) : binary_op list proc_state_m =
  (* User-defined operators no longer supported *)
  return []
;;

let module_open : binary_op =
  { meta = module_open_meta
  ; reduction =
      (let* module_expr, _per_ext = pop_prefix_operand module_open_meta in
       let* () =
         match A.view module_expr with
         | A.N (N.FileRef path, []) ->
           let* file_content, _ = get_file_ref path in
           let* () = add_module_expr_defined_names_to_env file_content in
           let* all_custom_ops = get_module_expr_defined_custom_ops file_content in
           (* only add custom operators, no automatic name operators since all identifiers must be quoted *)
           add_processor_entry_list (List.map (to_processor_binary_op "imported_ops") all_custom_ops)
         (* add new operators corresponding to the custom ops in the file *)
         (* DO WE NEED TO PUSH SOMETHING TO THE INPUT ACCUM? *)
         (* | A.FreeVar(x) -> 
          return (A.fold(A.N(N.Builtin(N.String x), []))) *)
         | _ -> Fail.failwith ("BP273: Expected a module Expression but got " ^ A.show_view module_expr)
       in
       return ()
       (* push_elem_on_input_acc (A.annotate_with_extent node per_ext) *))
  ; shift_action = do_nothing_shift_action
  }
;;

let module_reexport_meta : binary_op_meta =
  { id = Uid.next ()
  ; keyword = CS.new_t_string "诵"
  ; left_fixity = FxNone
  ; right_fixity = FxOp (Some 80)
  ; classification = Structural
  }
;;

let module_reexport : binary_op =
  { meta = module_reexport_meta
  ; reduction =
      (let* module_expr, per_ext = pop_prefix_operand module_reexport_meta in
       let* cur_module_expr, cur_ext = pop_input_acc_expr () in
       match A.view cur_module_expr with
       | A.N (N.ModuleDef, args) ->
         (match A.view module_expr with
          | A.N (N.FileRef path, []) ->
            let* file_content, _ = get_file_ref path in
            let rec aux acc decls =
              match decls with
              | [] ->
                push_elem_on_input_acc_expr
                  (A.fold_with_extent (A.N (N.ModuleDef, acc)) (Ext.combine_extent cur_ext per_ext))
              | x :: xs ->
                (match A.view x with
                 | A.N (N.Declaration (N.ModuleAliasDefn _), _) | A.N (N.Declaration N.CustomOperatorDecl, _) ->
                   aux (acc @ [ [], x ]) xs
                 | A.N (N.Declaration (N.CheckedConstantDefn (name, id)), []) ->
                   aux (acc @ [ [], A.fold (A.N (N.Declaration (N.ReexportedCheckedConstantDefn (name, id)), [])) ]) xs
                 | A.N (N.Declaration (N.ReexportedCheckedConstantDefn _), []) -> aux (acc @ [ [], x ]) xs
                 | _ -> print_failwith ("BP281: Expected a Declaration but got " ^ A.show_view x))
            in
            (match A.view file_content with
             | A.N (N.ModuleDef, margs) -> aux args (List.map snd margs)
             | _ -> print_failwith ("BP282: Expecting moduleDef: " ^ A.show_view file_content))
          | _ -> Fail.failwith ("BP273: Expected a module Expression but got " ^ A.show_view module_expr))
       | _ -> Fail.failwith ("BP273: Expected a module Expression but got " ^ A.show_view module_expr))
  ; shift_action = do_nothing_shift_action
  }
;;

let module_alias_decl_start_uid = Uid.next ()
let module_alias_decl_middle_uid = Uid.next ()
let module_alias_decl_end_uid = Uid.next ()

let module_alias_decl_start_meta =
  { id = module_alias_decl_start_uid
  ; keyword = CS.new_t_string "模块"
  ; left_fixity = FxNone
  ; right_fixity = FxBinding module_alias_decl_middle_uid
  ; classification = Structural
  }
;;

let module_alias_decl_middle_meta : binary_op_meta =
  { id = module_alias_decl_middle_uid
  ; keyword = CS.new_t_string "即"
  ; left_fixity = FxBinding module_alias_decl_start_uid
  ; right_fixity = FxComp module_alias_decl_end_uid
  ; classification = Structural
  }
;;

let module_alias_decl_end_meta : binary_op_meta =
  { id = module_alias_decl_end_uid
  ; keyword = CS.new_t_string "也"
  ; left_fixity = FxComp module_alias_decl_middle_uid
  ; right_fixity = FxNone
  ; classification = Structural
  }
;;

let module_alias_decl_start : binary_op =
  { meta = module_alias_decl_start_meta
  ; reduction = p_internal_error "BP105: module_alias_decl_start reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let module_alias_decl_middle : binary_op =
  { meta = module_alias_decl_middle_meta
  ; reduction = p_internal_error "BP106: module_alias_decl_middle reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let module_alias_decl_end : binary_op =
  { meta = module_alias_decl_end_meta
  ; reduction =
      (let* (name, defn), ext = pop_postfix_op_operands_2 module_alias_decl_end_meta in
       let* name_str = get_free_var name in
       match A.view defn with
       | A.N (N.FileRef filepath, []) ->
         let* id = Environment.add_constant (ModuleAlias { name = name_str; filepath }) in
         push_elem_on_input_acc_expr
           (A.annotate_with_extent (A.fold (A.N (N.Declaration (N.ModuleAliasDefn (name_str, id)), []))) ext)
       | _ -> Fail.failwith ("BP283: Expected a file reference but got " ^ A.show_view defn))
  ; shift_action = do_nothing_shift_action
  }
;;

let left_parenthesis_uid = Uid.next ()
let right_parenthesis_uid = Uid.next ()

let left_parenthesis_meta : binary_op_meta =
  { id = left_parenthesis_uid
  ; keyword = CS.new_t_string "（"
  ; left_fixity = FxNone
  ; right_fixity = FxComp right_parenthesis_uid
  ; classification = Expression
  }
;;

let right_parenthesis_meta : binary_op_meta =
  { id = right_parenthesis_uid
  ; keyword = CS.new_t_string "）"
  ; left_fixity = FxComp left_parenthesis_uid
  ; right_fixity = FxNone
  ; classification = Expression
  }
;;

let left_parenthesis : binary_op =
  { meta = left_parenthesis_meta
  ; reduction = p_internal_error "BP104: left_parenthesis reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let right_parenthesis : binary_op =
  { meta = right_parenthesis_meta
  ; reduction =
      (let* oper, per_ext = pop_postfix_op_operands_1 right_parenthesis_meta in
       push_elem_on_input_acc_expr (A.annotate_with_extent oper per_ext))
  ; shift_action = do_nothing_shift_action
  }
;;

let explicit_pi_start_uid = Uid.next ()
let explicit_pi_middle_1_uid = Uid.next ()
let explicit_pi_middle_2_uid = Uid.next ()

let explicit_pi_start_meta =
  { id = explicit_pi_start_uid
  ; keyword = CS.new_t_string "化"
  ; left_fixity = FxNone
  ; right_fixity = FxComp explicit_pi_middle_1_uid
  ; classification = Expression
  }
;;

let explicit_pi_middle_1_meta =
  { id = explicit_pi_middle_1_uid
  ; keyword = CS.new_t_string "者"
  ; left_fixity = FxComp explicit_pi_start_uid
  ; right_fixity = FxBinding explicit_pi_middle_2_uid
  ; classification = Expression
  }
;;

let explicit_pi_middle_2_meta =
  { id = explicit_pi_middle_2_uid
  ; keyword = CS.new_t_string "而"
  ; left_fixity = FxBinding explicit_pi_middle_1_uid
  ; right_fixity = FxOp (Some 40)
  ; classification = Expression
  }
;;

let explicit_pi_start : binary_op =
  { meta = explicit_pi_start_meta
  ; reduction = p_internal_error "BP104: explicit_pi_start reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let explicit_pi_middle_1 : binary_op =
  { meta = explicit_pi_middle_1_meta
  ; reduction = p_internal_error "BP104: explicit_pi_middle_1 reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let explicit_pi_middle_2 : binary_op =
  { meta = explicit_pi_middle_2_meta
  ; reduction =
      (let* (tp_name, bnd_name, range_expr), per_ext = pop_prefix_op_operands_3 explicit_pi_middle_2_meta in
       let* binding_name = get_binding_name bnd_name in
       let result_expr =
         A.fold_with_extent (A.N (N.ExplicitPi, [ [], tp_name; [ binding_name ], range_expr ])) per_ext
       in
       push_elem_on_input_acc_expr result_expr)
  ; shift_action = do_nothing_shift_action
  }
;;

let arrow_start_uid = Uid.next ()
let arrow_middle_uid = Uid.next ()

let arrow_start_meta =
  { id = arrow_start_uid
  ; keyword = CS.new_t_string "化"
  ; left_fixity = FxNone
  ; right_fixity = FxComp arrow_middle_uid
  ; classification = Expression
  }
;;

let arrow_middle_meta =
  { id = arrow_middle_uid
  ; keyword = CS.new_t_string "而"
  ; left_fixity = FxComp arrow_start_uid
  ; right_fixity = FxOp (Some 40)
  ; classification = Expression
  }
;;

let arrow_start : binary_op =
  { meta = arrow_start_meta
  ; reduction = p_internal_error "BP104: arrow_start reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let arrow_middle : binary_op =
  { meta = arrow_middle_meta
  ; reduction =
      (let* (tp_name, range_expr), per_ext = pop_prefix_op_operands_2 arrow_middle_meta in
       let result_expr = A.fold (A.N (N.Arrow, [ [], tp_name; [], range_expr ])) in
       push_elem_on_input_acc_expr (A.annotate_with_extent result_expr per_ext))
  ; shift_action = do_nothing_shift_action
  }
;;

let explicit_lam_abs_start_uid = Uid.next ()
let explicit_lam_abs_middle_uid = Uid.next ()

let explicit_lam_abs_start_meta =
  { id = explicit_lam_abs_start_uid
  ; keyword = CS.new_t_string "会"
  ; left_fixity = FxNone
  ; right_fixity = FxBinding explicit_lam_abs_middle_uid
  ; classification = Expression
  }
;;

let explicit_lam_abs_middle_meta =
  { id = explicit_lam_abs_middle_uid
  ; keyword = CS.new_t_string "而"
  ; left_fixity = FxBinding explicit_lam_abs_start_uid
  ; right_fixity = FxOp (Some 50)
  ; classification = Expression
  }
;;

let explicit_lam_abs_start : binary_op =
  { meta = explicit_lam_abs_start_meta
  ; reduction = p_internal_error "BP104: explicit_lam_abs_start reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let explicit_lam_abs_middle : binary_op =
  { meta = explicit_lam_abs_middle_meta
  ; reduction =
      (let* (tp_name, range_expr), per_ext = pop_prefix_op_operands_2 explicit_lam_abs_middle_meta in
       let* binding_name = get_binding_name tp_name in
       let result_expr = A.fold_with_extent (A.N (N.Lam, [ [ binding_name ], range_expr ])) per_ext in
       push_elem_on_input_acc_expr result_expr)
  ; shift_action = do_nothing_shift_action
  }
;;

let typed_lam_abs_start_uid = Uid.next ()
let typed_lam_abs_middle1_uid = Uid.next ()
let typed_lam_abs_middle2_uid = Uid.next ()

let typed_lam_abs_start_meta =
  { id = typed_lam_abs_start_uid
  ; keyword = CS.new_t_string "会"
  ; left_fixity = FxNone
  ; right_fixity = FxComp typed_lam_abs_middle1_uid
  ; classification = Expression
  }
;;

let typed_lam_abs_middle1_meta =
  { id = typed_lam_abs_middle1_uid
  ; keyword = CS.new_t_string "者"
  ; left_fixity = FxComp typed_lam_abs_start_uid
  ; right_fixity = FxBinding typed_lam_abs_middle2_uid
  ; classification = Expression
  }
;;

let typed_lam_abs_middle2_meta =
  { id = typed_lam_abs_middle2_uid
  ; keyword = CS.new_t_string "而"
  ; left_fixity = FxBinding typed_lam_abs_middle1_uid
  ; right_fixity = FxOp (Some 50)
  ; classification = Expression
  }
;;

let typed_lam_abs_start : binary_op =
  { meta = typed_lam_abs_start_meta
  ; reduction = p_internal_error "BP104: typed_lam_abs_start reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let typed_lam_abs_middle1 : binary_op =
  { meta = typed_lam_abs_middle1_meta
  ; reduction = p_internal_error "BP104: typed_lam_abs_middle1 reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let typed_lam_abs_middle2 : binary_op =
  { meta = typed_lam_abs_middle2_meta
  ; reduction =
      (let* (tp_name, bnd_name, body_expr), per_ext = pop_prefix_op_operands_3 typed_lam_abs_middle2_meta in
       let* binding_name = get_binding_name bnd_name in
       let result_expr = A.fold_with_extent (A.N (N.TypedLam, [ [], tp_name; [ binding_name ], body_expr ])) per_ext in
       push_elem_on_input_acc_expr result_expr)
  ; shift_action = do_nothing_shift_action
  }
;;

let explicit_ap_uid = Uid.next ()

let explicit_ap_meta =
  { id = explicit_ap_uid
  ; keyword = CS.new_t_string "于"
  ; left_fixity = FxOp (Some 799)
  ; right_fixity = FxOp (Some 800)
  ; classification = Expression
  }
;;

let explicit_ap : binary_op =
  { meta = explicit_ap_meta
  ; reduction =
      (let* (f, arg), per_ext = pop_bin_operand explicit_ap_meta in
       match A.view f with
       | A.N (N.ExternalCall fname, cur_args) ->
         push_elem_on_input_acc_expr (A.fold_with_extent (A.N (N.ExternalCall fname, cur_args @ [ [], arg ])) per_ext)
       | _ -> push_elem_on_input_acc_expr (A.fold_with_extent (A.N (N.Ap, [ [], f; [], arg ])) per_ext))
  ; shift_action = do_nothing_shift_action
  }
;;

let sentence_end_fail (module_expr : input_acc_elem) (decl_expr : input_acc_elem) : unit proc_state_m =
  let* st = get_proc_state () in
  Fail.failwith
    ("BP678: Expected a module defn and a decl but got "
     ^ show_input_acc_elem module_expr
     ^ " and "
     ^ show_input_acc_elem decl_expr
     ^ "input_acc = "
     ^ show_input_acc st.input_acc)
;;

let check_and_create_direct_expr (expr : A.t) : A.t proc_state_m =
  let* checked_body, tp = TypeChecking.synth_top expr in
  let* id = Environment.add_constant (DataExpression { tp; tm = checked_body; name = None }) in
  return (A.fold (A.N (N.Declaration (CheckedDirectExpr id), [])))
;;

let check_and_append_module_defn (module_expr : A.t) (decl : A.t) : A.t proc_state_m =
  (* Original logic for appending to module *)
  match A.view module_expr, A.view decl with
  | A.N (N.ModuleDef, _args), A.N (N.Declaration N.ConstantDeclPlaceholder, _) -> return module_expr
  | A.N (N.ModuleDef, args), A.N (N.Declaration (N.CheckedDirectExpr _), _)
  | A.N (N.ModuleDef, args), A.N (N.Declaration CustomOperatorDecl, _)
  | A.N (N.ModuleDef, args), A.N (N.Declaration (N.ModuleAliasDefn _), _)
  | A.N (N.ModuleDef, args), A.N (N.Declaration (CheckedConstantDefn _), _) ->
    return (A.fold (A.N (N.ModuleDef, args @ [ [], decl ])))
  | _ ->
    Fail.failwith
      ("BP1308: Expected a module defn and a decl but got " ^ A.show_view module_expr ^ " and " ^ A.show_view decl)
;;

let sentence_end : unit proc_state_m =
  let* _, ext = read_one_of_string [ CS.new_t_string "。" ] in
  let* () = TokenInfo.add_token_info (Ext.str_with_extent "。" ext) (SemanticToken StructureKeyword) in
  (* reduce all existing expressions*)
  let* _ = operator_precedence_reduce_always () in
  let* input_acc_size = get_input_acc_size () in
  let* () =
    if input_acc_size = 1
    then
      let* module_expr, _ = pop_input_acc_expr () in
      match A.view module_expr with
      | A.N (N.ModuleDef, _) ->
        let* () = push_elem_on_input_acc_expr module_expr in
        return ()
      | _ -> Fail.failwith ("BP207: Expected a module defn but got " ^ A.show_view module_expr)
    else if input_acc_size > 1
    then
      let* poped_left, poped_right = pop_input_acc_2 () in
      match poped_left, poped_right with
      | Expr module_expr, Expr decl ->
        (match A.view module_expr, A.view decl with
         | A.N (N.ModuleDef, _), A.N (N.Declaration _, _) ->
           let* combined_expr = check_and_append_module_defn module_expr decl in
           push_elem_on_input_acc_expr
             (A.annotate_with_extent
                combined_expr
                (Ext.combine_extent (A.get_extent_some module_expr) (A.get_extent_some decl)))
         | A.N (N.ModuleDef, _), _ ->
           let* direct_expr_decl = check_and_create_direct_expr decl in
           let* combined_expr = check_and_append_module_defn module_expr direct_expr_decl in
           push_elem_on_input_acc_expr
             (A.annotate_with_extent
                combined_expr
                (Ext.combine_extent (A.get_extent_some module_expr) (A.get_extent_some decl)))
         | _ -> sentence_end_fail (Expr module_expr) (Expr decl))
      (* | ParsingElem (start_op, _start_op_ext), Expr decl ->
        (match start_op, A.view decl with
         (* also for 「「 name *)
         | OpKeyword ({ id = opid; _ }, _), A.N (N.Declaration _, _) ->
           if opid = double_parenthesis_left_uid || opid = left_parenthesis_uid
           then
             (* push 「「 back onto the stack *)
             let* _ = push_elem_on_input_acc poped_left in
             let* combined_expr = check_and_append_module_defn (A.n (N.ModuleDef, [])) decl in
             (* we choose not to descope this *)
             let* _ = push_elem_on_input_acc_expr (A.annotate_with_extent combined_expr (A.get_extent_some decl)) in
             return ()
           else sentence_end_fail poped_left poped_right
         | OpKeyword ({ id = opid; _ }, _), _ ->
           if opid = double_parenthesis_left_uid || opid = left_parenthesis_uid
           then
             let* direct_expr_decl = check_and_create_direct_expr decl in
             let* combined_expr = check_and_append_module_defn (A.n (N.ModuleDef, [])) direct_expr_decl in
             (* push 「「 back onto the stack *)
             let* _ = push_elem_on_input_acc poped_left in
             let* _ = push_elem_on_input_acc_expr (A.annotate_with_extent combined_expr (A.get_extent_some decl)) in
             return ()
           else sentence_end_fail poped_left poped_right
         | _ -> sentence_end_fail poped_left poped_right) *)
      (* also for 「「 name *)
      | _ -> sentence_end_fail poped_left poped_right
    else Fail.failwith ("ET106: Expected at least 2 elements in the input acc but got " ^ string_of_int input_acc_size)
  in
  (* we want to commit once we successfully handled 。 if subsequent error occurs, 
      do not backtrack over interpretation of sentences *)
  pcommit ()
;;

let external_call_start_uid = Uid.next ()
let external_call_end_uid = Uid.next ()

let external_call_start_meta : binary_op_meta =
  { id = external_call_start_uid
  ; keyword = CS.new_t_string "《《外部调用"
  ; left_fixity = FxNone
  ; right_fixity = FxComp external_call_end_uid
  ; classification = Expression
  }
;;

let external_call_end_meta : binary_op_meta =
  { id = external_call_end_uid
  ; keyword = CS.new_t_string "》》"
  ; left_fixity = FxComp external_call_start_uid
  ; right_fixity = FxNone
  ; classification = Expression
  }
;;

let external_call_start : binary_op =
  { meta = external_call_start_meta
  ; reduction = p_internal_error "BP104: external_call_start reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let external_call_end : binary_op =
  { meta = external_call_end_meta
  ; reduction =
      (let* oper, per_ext = pop_postfix_operand external_call_end_meta in
       match A.view oper with
       | A.N (N.Builtin (N.String x), []) ->
         push_elem_on_input_acc_expr (A.fold_with_extent (A.N (N.ExternalCall x, [])) per_ext)
       | _ -> Fail.failwith ("BP693: Builtin Expected a string but got " ^ A.show_view oper))
  ; shift_action = do_nothing_shift_action
  }
;;

let if_then_else_start_uid = Uid.next ()
let if_then_else_mid1_uid = Uid.next ()
let if_then_else_mid2_uid = Uid.next ()

let if_then_else_start_meta =
  { id = if_then_else_start_uid
  ; keyword = CS.new_t_string "若"
  ; left_fixity = FxNone
  ; right_fixity = FxComp if_then_else_mid1_uid
  ; classification = Expression
  }
;;

let if_then_else_mid1_meta =
  { id = if_then_else_mid1_uid
  ; keyword = CS.new_t_string "则"
  ; left_fixity = FxComp if_then_else_start_uid
  ; right_fixity = FxComp if_then_else_mid2_uid
  ; classification = Expression
  }
;;

let if_then_else_mid2_meta =
  { id = if_then_else_mid2_uid
  ; keyword = CS.new_t_string "否则"
  ; left_fixity = FxComp if_then_else_mid1_uid
  ; right_fixity = FxOp (Some 80)
  ; classification = Expression
  }
;;

let if_then_else_start : binary_op =
  { meta = if_then_else_start_meta
  ; reduction = p_internal_error "BP104: if_then_else_start reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let if_then_else_mid1 : binary_op =
  { meta = if_then_else_mid1_meta
  ; reduction = p_internal_error "BP104: if_then_else_mid1 reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let if_then_else_mid2 : binary_op =
  { meta = if_then_else_mid2_meta
  ; reduction =
      (let* (cond, then_expr, else_expr), per_ext = pop_prefix_op_operands_3 if_then_else_mid2_meta in
       let result_expr = A.fold_with_extent (A.N (N.IfThenElse, [ [], cond; [], then_expr; [], else_expr ])) per_ext in
       push_elem_on_input_acc_expr result_expr)
  ; shift_action = do_nothing_shift_action
  }
;;

let sum_case_start_uid = Uid.next ()
let sum_case_mid_uid = Uid.next ()

let sum_case_start_meta =
  { id = sum_case_start_uid
  ; keyword = CS.new_t_string "有"
  ; left_fixity = FxNone
  ; right_fixity = FxComp sum_case_mid_uid
  ; classification = Expression
  }
;;

let sum_case_mid_meta =
  { id = sum_case_mid_uid
  ; keyword = CS.new_t_string "则"
  ; left_fixity = FxOp (Some 70)
  ; right_fixity = FxOp (Some 70)
  ; classification = Expression
  }
;;

let sum_case_start : binary_op =
  { meta = sum_case_start_meta
  ; reduction = p_internal_error "BP104: match_case_start reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let sum_case_mid : binary_op =
  { meta = sum_case_mid_meta
  ; reduction =
      (let* (case_expr, tp_expr), per_ext = pop_prefix_op_operands_2 sum_case_mid_meta in
       let* label = get_binding_name case_expr in
       let result_expr = A.fold_with_extent (A.N (N.SumCase label, [ [], tp_expr ])) per_ext in
       push_elem_on_input_acc_expr result_expr)
  ; shift_action = do_nothing_shift_action
  }
;;

let comma_sequence_meta : binary_op_meta =
  { id = Uid.next ()
  ; keyword = CS.new_t_string "，"
  ; left_fixity = FxOp (Some 89)
  ; right_fixity = FxOp (Some 90)
  ; classification = Expression
  }
;;

let comma_sequence : binary_op =
  { meta = comma_sequence_meta
  ; reduction =
      (let* (x, y), per_ext = pop_bin_operand comma_sequence_meta in
       match A.view x with
       | A.N (N.Sequence Comma, args) ->
         push_elem_on_input_acc_expr (A.fold_with_extent (A.N (N.Sequence Comma, args @ [ [], y ])) per_ext)
       | _ -> push_elem_on_input_acc_expr (A.fold_with_extent (A.N (N.Sequence Comma, [ [], x; [], y ])) per_ext))
  ; shift_action = do_nothing_shift_action
  }
;;

let enumeration_comma_sequence_meta : binary_op_meta =
  { id = Uid.next ()
  ; keyword = CS.new_t_string "、"
  ; left_fixity = FxOp (Some 109)
  ; right_fixity = FxOp (Some 110)
  ; classification = Expression
  }
;;

let enumeration_comma_sequence : binary_op =
  { meta = enumeration_comma_sequence_meta
  ; reduction =
      (let* (x, y), per_ext = pop_bin_operand enumeration_comma_sequence_meta in
       match A.view x with
       | A.N (N.Sequence Dot, args) ->
         push_elem_on_input_acc_expr (A.fold_with_extent (A.N (N.Sequence Dot, args @ [ [], y ])) per_ext)
       | _ -> push_elem_on_input_acc_expr (A.fold_with_extent (A.N (N.Sequence Dot, [ [], x; [], y ])) per_ext))
  ; shift_action = do_nothing_shift_action
  }
;;

let let_in_start_uid = Uid.next ()
let let_in_mid1_uid = Uid.next ()
let let_in_mid2_uid = Uid.next ()

let let_in_start_meta =
  { id = let_in_start_uid
  ; keyword = CS.new_t_string "虑"
  ; left_fixity = FxNone
  ; right_fixity = FxBinding let_in_mid1_uid
  ; classification = Structural
  }
;;

let let_in_mid1_meta =
  { id = let_in_mid1_uid
  ; keyword = CS.new_t_string "者"
  ; left_fixity = FxBinding let_in_start_uid
  ; right_fixity = FxComp let_in_mid2_uid
  ; classification = Structural
  }
;;

let let_in_mid2_meta =
  { id = let_in_mid2_uid
  ; keyword = CS.new_t_string "而"
  ; left_fixity = FxComp let_in_mid1_uid
  ; right_fixity = FxOp (Some 55)
  ; classification = Structural
  }
;;

let let_in_start : binary_op =
  { meta = let_in_start_meta
  ; reduction = p_internal_error "BP104: let_in_start reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let let_in_mid1 : binary_op =
  { meta = let_in_mid1_meta
  ; reduction = p_internal_error "BP104: let_in_mid1 reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let let_in_mid2 : binary_op =
  { meta = let_in_mid2_meta
  ; reduction =
      (let* (bnd_name, domain_expr, range_expr), per_ext = pop_prefix_op_operands_3 let_in_mid2_meta in
       let* binding_name = get_binding_name bnd_name in
       let result_expr =
         A.fold_with_extent (A.N (N.LetIn, [ [], domain_expr; [ binding_name ], range_expr ])) per_ext
       in
       push_elem_on_input_acc_expr result_expr)
  ; shift_action = do_nothing_shift_action
  }
;;

let typing_annotation_start_uid = Uid.next ()
let typing_annotation_middle_uid = Uid.next ()

let typing_annotation_start_meta =
  { id = typing_annotation_start_uid
  ; keyword = CS.new_t_string "其"
  ; left_fixity = FxNone
  ; right_fixity = FxComp typing_annotation_middle_uid
  ; classification = Expression
  }
;;

let typing_annotation_middle_meta =
  { id = typing_annotation_middle_uid
  ; keyword = CS.new_t_string "焉"
  ; left_fixity = FxComp typing_annotation_start_uid
  ; right_fixity = FxOp (Some 20)
  ; classification = Expression
  }
;;

let typing_annotation_start : binary_op =
  { meta = typing_annotation_start_meta
  ; reduction = p_internal_error "BP104: typing_annotation_start reduction"
  ; shift_action = do_nothing_shift_action
  }
;;

let typing_annotation_middle : binary_op =
  { meta = typing_annotation_middle_meta
  ; reduction =
      (let* (type_expr, body_expr), per_ext = pop_prefix_op_operands_2 typing_annotation_middle_meta in
       let result_expr = A.fold_with_extent (A.N (N.TypingAnnotation, [ [], type_expr; [], body_expr ])) per_ext in
       push_elem_on_input_acc_expr result_expr)
  ; shift_action = do_nothing_shift_action
  }
;;

let default_registry =
  [ to_processor_complex "top_level_empty_space_ignore" top_level_empty_space_ignore
  ; to_processor_complex "single_comment" (single_comment ())
  ; to_processor_complex "string_parser_pusher" string_parser_pusher
  ; to_processor_binary_op "definition_middle" definition_middle
  ; to_processor_binary_op "definition_end" definition_end
  ; to_processor_binary_op "import_end" import_end (* ; to_processor_binary_op "library_root" library_root *)
  ; to_processor_binary_op "unknown_structure_deref" unknown_structure_deref
  ; to_processor_complex "sentence_end" sentence_end
  ; to_processor_binary_op "builtin_op" builtin_op
  ; to_processor_binary_op "module_open" module_open
  ; to_processor_binary_op "module_reexport" module_reexport
  ; to_processor_binary_op "module_alias_decl_start" module_alias_decl_start
  ; to_processor_binary_op "module_alias_decl_middle" module_alias_decl_middle
  ; to_processor_binary_op "module_alias_decl_end" module_alias_decl_end
  ; to_processor_binary_op "left_parenthesis" left_parenthesis
  ; to_processor_binary_op "right_parenthesis" right_parenthesis
  ; to_processor_binary_op "explicit_pi_start" explicit_pi_start
  ; to_processor_binary_op "explicit_pi_middle_1" explicit_pi_middle_1
  ; to_processor_binary_op "explicit_pi_middle_2" explicit_pi_middle_2
  ; to_processor_binary_op "arrow_start" arrow_start
  ; to_processor_binary_op "arrow_middle" arrow_middle
  ; (* lambdas *)
    to_processor_binary_op "explicit_lam_abs_start" explicit_lam_abs_start
  ; to_processor_binary_op "explicit_lam_abs_middle" explicit_lam_abs_middle
  ; to_processor_binary_op "typed_lam_abs_start" typed_lam_abs_start
  ; to_processor_binary_op "typed_lam_abs_middle1" typed_lam_abs_middle1
  ; to_processor_binary_op "typed_lam_abs_middle2" typed_lam_abs_middle2
  ; (* application *)
    to_processor_binary_op "explicit_ap" explicit_ap
  ; to_processor_binary_op "external_call_start" external_call_start
  ; to_processor_binary_op "external_call_end" external_call_end
  ; (* if *)
    to_processor_binary_op "if_then_else_start" if_then_else_start
  ; to_processor_binary_op "if_then_else_mid1" if_then_else_mid1
  ; to_processor_binary_op "if_then_else_mid2" if_then_else_mid2
  ; (* match*)
    to_processor_binary_op "sum_case_start" sum_case_start
  ; to_processor_binary_op "sum_case_mid" sum_case_mid
  ; (* lists *)
    to_processor_binary_op "comma_sequence" comma_sequence
  ; to_processor_binary_op "enumeration_comma_sequence" enumeration_comma_sequence
  ; (* let in*)
    to_processor_binary_op "let_in_start" let_in_start
  ; to_processor_binary_op "let_in_mid1" let_in_mid1
  ; to_processor_binary_op "let_in_mid2" let_in_mid2
  ; (* typing annotation *)
    to_processor_binary_op "typing_annotation_start" typing_annotation_start
  ; to_processor_binary_op "typing_annotation_middle" typing_annotation_middle
  ; to_processor_complex "identifier_parser_pusher" identifier_parser_pusher
  ; to_processor_complex "number_parser" (integer_number_parser ())
  ; to_processor_complex "decimal_number_parser" (decimal_number_parser ())
  ; (* type*)
    to_processor_binary_op "builtin_type" builtin_type
  ; to_processor_binary_op "builtin_data_type" builtin_data_type
  ; to_processor_binary_op "builtin_data_constructor" builtin_data_constructor
  ; to_processor_binary_op "builtin_data_case" builtin_data_case
  ; to_processor_binary_op "builtin_recurse" builtin_recurse
  ]
  @ List.concat []
;;
