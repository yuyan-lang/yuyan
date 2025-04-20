open EngineData
open ProcCombinators

(* pushes start elem (identifier or )
CHANGE: I decided that I still want a list of identifiers 
1. Function as applications if separator is 于 or 、 or 以 
    [The idea is that A以B、C、D = A于B于C于D]
2. Form a special sequence type if separator is ， or ；or。
*)
let push_elem_start (es : A.t) : unit proc_state_m = 
  let* acc_top = peek_input_acc 0 in
  match acc_top with
  | None -> push_elem_on_input_acc es
  | Some (x) -> 
    match A.view x with
    | A.N(N.ParsingElem(N.OpKeyword(meta)),[]) -> 
      (
        match meta.right_fixity with
        | FxNone ->
          failwith ("PC322: expected prefix or infix but got " ^ (show_binary_op_meta meta) ^ " others should be reduced directly not remain on the stack")
        | _ ->
          push_elem_on_input_acc es
      )
    | A.N(N.ModuleDef, _) 
    | A.N(N.Builtin(N.CustomOperatorString _), _)  (* special tricks for parsing custom operatos because final operators will pop one more thing from the stack
    TODO: maybe better if we have a shift-action in operators 
      *)
    ->
      push_elem_on_input_acc es
    | _ -> 
      pfail ("PC324: expected An element that allows ids to be pushed but got " ^ (A.show_view x) ^ " cannot push the next identifier, which is "
        ^ (A.show_view es) ^ " on the stack")

let push_elem_continue (es : A.t) : unit proc_state_m = 
  let* acc_top = peek_input_acc 0 in
  match acc_top with
  | None -> pfail "PC243: empty stack"
  | Some (x) -> 
    match A.view x with
    | A.N(N.ParsingElem(N.OpKeyword(_)),[]) -> 
      pfail ("PC333: expected things  got " ^ (A.show_view x) ^ " cannot push the next identifier " ^ A.show_view es)
    | _ -> 
      push_elem_on_input_acc es





(* this reduces postfix and closed operators already on the stack*)
let operator_right_most_reduce () : unit proc_state_m = 
  let* acc_top = peek_input_acc 0 in
  match acc_top with
  | None -> failwith "PC244: empty stack"
  | Some (x) -> 
    match A.view x with
    | A.N(N.ParsingElem(N.OpKeyword(meta)),[]) -> 
      (
        match meta.right_fixity with
        | FxNone -> 
            let* bin_op = lookup_binary_op meta.id in
            bin_op.reduction
        | _ ->
            failwith ("PC362: expected postfix or closed identifier but got " ^ (show_binary_op_meta meta) ^ " others should be reduced directly not remain on the stack")
      )
    | _ -> 
      failwith ("PC364: expected OpKeyword but got " ^ (A.show_view x) ^ " this method should not be invoked when rightmost reduction is not possible")


(* reduces all operators A + B on the stack of higher precedence*)
let rec operator_precedence_reduce (limit : int) : unit proc_state_m = 
  let* acc_top = peek_input_acc 1 in
  match acc_top with
  | None -> return ()
  | Some (x) -> 
    match A.view x with
    | A.N(N.ParsingElem(N.OpKeyword(meta)),[]) -> 
      (match meta.right_fixity with
        | FxOp rp ->
            if rp > limit then
              let* bin_op = lookup_binary_op meta.id in
              let* _ = bin_op.reduction in
              operator_precedence_reduce limit
            else
              return () (* cannot reduce, assume successful *)
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
      match A.view x with
      | A.N(N.ParsingElem(N.OpKeyword(meta)),[]) -> 
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
        pfail ("PC402: expecting " ^ (show_binary_op_meta oper.meta) ^ " but got " ^ (A.show_view x) ^ " ")





(* parses a single identifier identifier is something that is quoted between 「 and 」 and without special chars 
*)
let yy_keyword_chars = CharStream.new_t_string "。（）「」『』"
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
  let operator_elem = A.annotate_with_extent (A.fold(A.N(N.ParsingElem(N.OpKeyword (meta)), []))) read_ext in
  (* reduce existing stack based on the left_fixity of the operator element*)
  let* _ = (match left_fixity with
  | FxOp lp -> operator_precedence_reduce lp
  | FxNone -> return ()
  | FxBinding c | FxComp c -> operator_component_reduce c
  ) in
  (* shift operators onto the stack *)
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
            push_elem_on_input_acc (PE.get_bound_scanned_string_t (id, ext))
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
                    let* _ = push_elem_on_input_acc (PE.get_bound_scanned_string_t (middle_id, id_ext)) in
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
  | ProcBinOp { meta;reduction=_} ->
      let* (_read_keyword, ext) = read_string meta.keyword in
      (* experiment once an semantic operator parses, no backtrack*)
      if meta.keyword = CS.new_t_string "之书"
        then pcut (process_read_operator meta ext)
        else process_read_operator meta ext
  | ProcIdentifier id -> 
      let* string_read = read_string id in
      push_elem_on_input_acc (PE.get_identifier_t string_read)

    
let add_processor_entry_list (proc : processor_entry list) : unit proc_state_m = 
    let* proc_state = get_proc_state () in
    let new_s = {proc_state with registry = proc @ proc_state.registry} in
    write_proc_state new_s
let remove_all_proc_registry_with_input_expect_state (expect : expect) : unit proc_state_m = 
    let* proc_state = get_proc_state () in
    let new_s = {proc_state with registry = List.filter (fun x -> x.expect <> expect) proc_state.registry} in
    write_proc_state new_s

let run_processor_entry (proc : processor_entry)  : unit proc_state_m = 
    let {expect; name=_; processor} = proc in
    let* proc_state = get_proc_state () in
    if expect <> proc_state.input_expect 
    then pfail ("PC100: expected " ^ (show_input_expect expect) ^ " but got " ^ (show_input_expect proc_state.input_expect))
    else
      run_processor processor

let collect_input_acc_identifiers() : CS.t_string list proc_state_m = 
  let* s = get_proc_state () in
  let all_scanned_ids = ListUtil.remove_duplicates (List.concat_map (fun x -> 
    (* aux potentially recursive *)
    let aux y = match A.view y with 
    | A.N(N.ParsingElem(N.BoundScannedString s), []) -> [s]
    | _ ->  List.map (CS.new_t_string) (A.get_free_vars y) in
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
  choice_l (List.map (fun x -> 
    run_processor_entry (to_processor_identifier Expression "bid" x)
    ) all_scanned_ids)