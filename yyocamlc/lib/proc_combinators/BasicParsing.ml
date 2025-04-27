open EngineData
open ProcCombinators

(* pushes start elem (identifier or )
CHANGE: I decided that I still want a list of identifiers 
1. Function as applications if separator is 于 or 、 or 以 
    [The idea is that A以B、C、D = A于B于C于D]
2. Form a special sequence type if separator is ， or ；or。
*)
let push_elem_start (es : input_acc_elem) : unit proc_state_m = 
  let* acc_top = peek_input_acc 0 in
  match acc_top with
  | None -> push_elem_on_input_acc es
  | Some (x) -> 
    match x with
    | ParsingElem(OpKeyword(meta, _), _) -> 
      (
        match meta.right_fixity with
        | FxNone ->
          failwith ("PC322: expected prefix or infix but got " ^ (show_binary_op_meta meta) ^ " others should be reduced directly not remain on the stack")
        | _ ->
          push_elem_on_input_acc es
      )
    | Expr (x) -> (
      match A.view x with
      | A.N(N.ModuleDef, _)
      | A.N(N.Builtin(N.CustomOperatorString _), _)  (* special tricks for parsing custom operatos because final operators will pop one more thing from the stack
          TODO: maybe better if we have a shift-action in operators 
        *)
          -> push_elem_on_input_acc es
      | _ -> 
        pfail ("PC324: expected An element that allows ids to be pushed but got " ^ (A.show_view x) ^ " cannot push the next identifier, which is "
          ^ (show_input_acc_elem es) ^ " on the stack")
    )
    | _ -> 
      pfail ("PC324: expected An element that allows ids to be pushed but got " ^ (show_input_acc_elem x) ^ " cannot push the next identifier, which is "
        ^ (show_input_acc_elem es) ^ " on the stack")

let push_elem_continue (es : input_acc_elem) : unit proc_state_m = 
  let* acc_top = peek_input_acc 0 in
  match acc_top with
  | None -> pfail "PC243: empty stack"
  | Some (x) -> 
    match x with
    | ParsingElem(OpKeyword(_), _) ->
      pfail ("PC333: expected things  got " ^ (show_input_acc_elem x) ^ " cannot push the next identifier " ^ (show_input_acc_elem es))
    | _ -> 
      push_elem_on_input_acc es





(* this reduces postfix and closed operators already on the stack*)
let operator_right_most_reduce () : unit proc_state_m = 
  let* acc_top = peek_input_acc 0 in
  match acc_top with
  | None -> failwith "PC244: empty stack"
  | Some (x) -> 
    match x with
    | ParsingElem(OpKeyword(meta, _), _) -> 
      (
        match meta.right_fixity with
        | FxNone -> 
            let* bin_op = lookup_binary_op meta.id in
            (* no need to execute pop action because reduction will call pop which is responsible for executing the pop action *)
            bin_op.reduction
        | _ ->
            failwith ("PC362: expected postfix or closed identifier but got " ^ (show_binary_op_meta meta) ^ " others should be reduced directly not remain on the stack")
      )
    | _ -> 
      failwith ("PC364: expected OpKeyword but got " ^ (show_input_acc_elem x) ^ " this method should not be invoked when rightmost reduction is not possible")


(* reduces all operators A + B on the stack of higher precedence*)
let rec operator_precedence_reduce (limit : int option) : unit proc_state_m = 
  let* acc_top = peek_input_acc 1 in
  match acc_top with
  | None -> return ()
  | Some (x) -> 
    match x with
    | ParsingElem(OpKeyword(meta, _), _) -> 
      (
        let perform_reduction () = 
          let* bin_op = lookup_binary_op meta.id in
          let* _ = bin_op.reduction in
          operator_precedence_reduce limit
        in
        match meta.right_fixity, limit with
        | FxOp Some rp, Some limit ->
            if rp > limit then
              perform_reduction ()
            else
              return () (* cannot reduce, assume successful *)
        | FxOp None, Some limit -> 
          (* here we have an option of reducing or not reducing reducing if limit < 400 (more likely) *)
            if limit < 400 then
              choice (perform_reduction ()) (return ())
            else
              choice (return ()) (perform_reduction ())
        | FxOp (Some rp), None -> 
            if rp >= 400 then 
              choice (perform_reduction ()) (return ())
            else
              choice (return ()) (perform_reduction ())
        | FxOp None, None ->
          (* by default we do left-associative reductions [to rule out early failures] *)
            choice (return ()) (perform_reduction ())

        | _ -> 
          return () (* cannot reduce, assume successful *)
      )
    | _ -> 
      return () (* cannot reduce, assume successful *)


(* reduce until the stack 's second element is comp *)
let rec operator_component_reduce (comp_uid : int) : unit proc_state_m = 
  (* let* stack_size = get_input_acc_size () in
  if stack_size < 2 then 
    pfail ("PC399: Expecting at least 2 elements on the stack but got " ^ string_of_int stack_size)
  else *)
    let* acc_top = peek_input_acc 1 in
    match acc_top with
    | None -> pfail ("PC400: Expecting at least 2 elements on the stack but got " )
    | Some (x) -> 
      match x with
      | ParsingElem(OpKeyword(meta, _), _) -> 
        if meta.id = comp_uid then
          return ()
        else
          let* oper = lookup_binary_op meta.id in
          (* this component is eligible for reduction if the right_fixity is an expression-expecting construct, 
            if the right handside is a component exepctation that is different from comp_uid, 
              then it is an error, no parse*)
          (* print_endline ("Operator component reduction called on " ^ (show_binary_op_meta oper.meta) ^ " comp_uid= " ^ (string_of_int comp_uid) ^ " " ); *)
          (
            match oper.meta.right_fixity with
            | FxOp _ -> 
                let* _ = oper.reduction in
                operator_component_reduce comp_uid
            | FxBinding _ | FxComp _ ->
                let* comp_oper = lookup_binary_op comp_uid in
                  pfail ("PC401: Expecting " ^ (show_binary_op_meta comp_oper.meta) ^ " but got " ^ (show_binary_op_meta oper.meta) ^ " in the operator component reduction")
            | FxNone -> failwith ("PC559: FxNone should not be here " ^ (show_binary_op_meta oper.meta) ^ " " )
          )
      | _ -> 
        let* oper = lookup_binary_op comp_uid in
        pfail ("PC402: expecting " ^ (show_binary_op_meta oper.meta) ^ " but got " ^ (show_input_acc_elem x) ^ " ")





(* parses a single identifier identifier is something that is quoted between 「 and 」 and without special chars 
*)
let yy_keyword_chars = CharStream.new_t_string "。（）「」『』"
let yy_number_words = CharStream.new_t_string "零一二三四五六七八九"
let yy_number_str_to_numeric_str (number_list : CS.t_char list) : string = 
  String.concat "" (List.map (fun x -> 
    match List.find_index (fun y -> y = x ) yy_number_words with
    | None -> failwith ("ET101: Expected a number but got " ^ CS.get_t_char x)
    | Some i -> string_of_int i
    ) number_list)

let get_int_from_t_string (number_list : CS.t_char list) : int = 
  int_of_string (yy_number_str_to_numeric_str number_list)
  (* List.fold_left (fun acc x -> acc * 10 + (
    match List.find_index (fun y -> y = x ) yy_number_words with
    | None -> failwith ("ET101: Expected a number but got " ^ CS.get_t_char x)
    | Some i -> i
    )) 0 number_list *)
let get_decimal_part_from_t_string (number_list : CS.t_char list) : float = 
  float_of_string ("." ^ (yy_number_str_to_numeric_str number_list))
  (* List.fold_right (fun acc x -> acc *. 0.1 + (
    match List.find_index (fun y -> y = x ) yy_number_words with
    | None -> failwith ("ET101: Expected a number but got " ^ CS.get_t_char x)
    | Some i -> i *. 0.1
    )) number_list 0.0 *)


let integer_number_parser() : unit proc_state_m = 
  let* (top, top_ext) = peek_any_char () in
  if List.mem top yy_number_words then
    let* number_list = many1 (read_one_of_char yy_number_words) in
    let number = get_int_from_t_string (List.map fst number_list) in
    push_elem_on_input_acc (Expr (
      A.fold_with_extent (A.N(N.Builtin(N.Int number), []))
      (Ext.combine_extent_list (List.map snd number_list))
    )
  )
  else
    pfail_error (ErrExpectString {
      expecting = yy_number_words;
      actual = (top, top_ext);
    })

let decimal_number_parser() : unit proc_state_m = 
  let* integral_part = many1 (read_one_of_char yy_number_words) in
  let* _ = read_one_of_char [CS.new_t_char "点"] in
  let* decimal_part = many1 (read_one_of_char yy_number_words) in
  let integral_number = yy_number_str_to_numeric_str (List.map fst integral_part) in
  let decimal_number = yy_number_str_to_numeric_str (List.map fst decimal_part) in
  push_elem_on_input_acc (Expr (
      A.fold_with_extent (A.N(N.Builtin(N.Float (integral_number, decimal_number)), []))
      (Ext.combine_extent_list (List.map snd integral_part @ List.map snd decimal_part))
    )
  )
  

let identifier_parser () : (CS.t_string * Ext.t) proc_state_m = 
  let* _ = pnot (read_string (CS.new_t_string "「：")) in
  let* _ = read_one_of_char [CS.new_t_char "「"] in
  let* ((middle, middle_ext), (terminal, _)) = scan_past_one_of_char yy_keyword_chars in
  if CS.get_t_char terminal = "」" then
    match middle with
    | [] -> failwith ("Unit pattern not implemented")
    | _ -> return (middle, middle_ext)
  else
    pfail ("ET100: Expected '」' but got " ^ CS.get_t_char terminal ^ " expecting 「id」 or 「「expr」」 or 「：comments：」")

let process_read_operator (meta : binary_op_meta) (read_ext : Ext.t) : unit proc_state_m = 
  let {id=_;keyword;left_fixity;right_fixity} = meta in
  let* () = if !Flags.show_parse_tracing then (
    print_endline ("=========\n[OP] Read " ^ CS.get_t_string keyword );
    let* st = get_proc_state () in
    print_endline ("[OP] Current state " ^ show_proc_state st);
    return ()
  ) else return () in
  (* reduce existing stack based on the left_fixity of the operator element*)
  let* _ = (match left_fixity with
  | FxOp lp -> operator_precedence_reduce lp
  | FxNone -> return ()
  | FxBinding c | FxComp c -> operator_component_reduce c
  ) in
  (* shift operators onto the stack *)
  let* bin_op = lookup_binary_op meta.id in
  let* pop_action = bin_op.shift_action in
  let operator_elem = ParsingElem(OpKeyword(meta, pop_action), read_ext) in
  let* _ = (match left_fixity with
    | FxNone -> push_elem_start operator_elem
    | FxOp _ | FxBinding _ | FxComp _ -> push_elem_continue operator_elem
    ) in
  (* reduce right most thing for postfix and closed identifier*)
  let* _ = (match right_fixity with
    | FxNone -> operator_right_most_reduce ()
    | _ -> return ()) in
  (* also lookahead and parse a binding for start binding*)
  let* _ = (match right_fixity with
    | FxBinding end_str_op_uid -> 
        let* oper = lookup_binary_op end_str_op_uid in
        (* we have a single chance of succeeding, either parse an id or scan *)
        pcut (choice
          (
            let* (id, ext) = identifier_parser () in
            push_elem_on_input_acc (get_bound_scanned_string_t (id, ext))
          ) (* *)
          (
            let* ((middle_id, id_ext)) = scan_until_one_of_string [oper.meta.keyword] in
            (
              match middle_id with
              | [] -> pfail ("PC335: got empty string for binding")
              | _ -> 
                let disallowed_chars_in_binding = CharStream.new_t_string "。（）「」『』 \n\t，、" in
                  if List.exists (fun x -> List.mem x disallowed_chars_in_binding) middle_id then
                    pfail_with_ext ("PC336: got binding 『" ^ ( CS.get_t_string middle_id) ^ "』 which contains disallowed_chars " ^ (show_string (CS.get_t_string disallowed_chars_in_binding)) ) (id_ext)
                  else
                    let* _ = push_elem_on_input_acc (get_bound_scanned_string_t (middle_id, id_ext)) in
                    return ()
            )
          )
        )
    | _ -> return ()) in
  (* reduce the operator*)
  let* () = if !Flags.show_parse_tracing then (
    print_endline ("=========\n[OP] Successful " ^ CS.get_t_string keyword );
    let* st = get_proc_state () in
    print_endline ("[OP] Current state " ^ show_proc_state st);
        return ()
      ) else return () in
      return ()

let run_processor (proc : processor)  : unit proc_state_m = 
  match proc with
  | ProcComplex process -> process
  | ProcBinOp { meta;_} ->
      let* (_read_keyword, ext) = read_string meta.keyword in
      (* experiment once an semantic operator parses, no backtrack*)
      (* if meta.keyword = CS.new_t_string "之书"
        then pcut (process_read_operator meta ext)
        else  *)
        process_read_operator meta ext
  | ProcIdentifier id -> 
      let* string_read = read_string id in
      push_elem_on_input_acc (get_identifier_t string_read)

    
let add_processor_entry_list (proc : processor_entry list) : unit proc_state_m = 
    let* proc_state = get_proc_state () in
    let new_s = {proc_state with registry = proc @ proc_state.registry} in
    write_proc_state new_s
let remove_all_proc_registry_with_ids (ids : int list) : unit proc_state_m = 
    let* proc_state = get_proc_state () in
    let new_s = {proc_state with registry = List.filter (fun x -> not (List.mem x.id ids)) proc_state.registry} in
    write_proc_state new_s

let run_processor_entry (proc : processor_entry)  : unit proc_state_m = 
    let {id=_;name=_; processor} = proc in
    let* () = run_processor processor in
    (* print_endline ("Ran pentry " ^ show_processor_entry proc); *)
    if !Flags.show_parse_progress
      then (
        update_proc_state (fun st -> 
          {st with last_succeeded_processor = proc}
          )
      ) else return ()

let run_processor_entries (entries : processor_entry list) : unit proc_state_m = 
  choice_l (List.map run_processor_entry entries)

let collect_input_acc_identifiers() : CS.t_string list proc_state_m = 
  let* s = get_proc_state () in
  let all_scanned_ids = ListUtil.remove_duplicates (List.concat_map (fun x -> 
    (* aux potentially recursive *)
    let aux y = match y with 
      | ParsingElem(BoundScannedString s, _) -> [s]
      | Expr y ->  List.map (CS.new_t_string) (A.get_free_vars y) 
      | _ -> []
    in
      aux x
    ) s.input_acc) in
  let all_existing_ids = List.filter_map (fun x -> 
    match x.processor with
    | ProcIdentifier id -> Some id
    | _ -> None) s.registry in
  (* print_endline ("PC100: all scanned ids " ^ (String.concat "," (List.map CS.get_t_string all_scanned_ids))); *)
  (* print_endline ("PC100: all existing ids " ^ (String.concat "," (List.map CS.get_t_string all_existing_ids))); *)
  return (ListUtil.minus all_scanned_ids all_existing_ids)

let run_input_acc_identifiers () : unit proc_state_m = 
  let* all_scanned_ids = collect_input_acc_identifiers () in
  (* print_endline ("PC100: running identifiers " ^ (String.concat "," (List.map CS.get_t_string all_scanned_ids))); *)
  run_processor_entries (List.map (fun x -> 
    (to_processor_identifier ("input_acc_id_" ^ CS.get_t_string x)  x)
    ) all_scanned_ids)