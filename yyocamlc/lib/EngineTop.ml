

open EngineData
open ProcCombinators

(* let do_process_step  (proc_state : proc_state) : proc_state list = 
  List.filter_map (fun proc -> 
    match run_processor_entry proc proc_state  with
    | None -> None
    | Some ((), s) -> 
        Some({s with last_succeeded_processor = proc})
  ) proc_state.registry *)


(* let do_process_entire_stream (proc_state : proc_state) : (proc_state list , proc_state) result = 
  let last_failure = ref proc_state in
  let all_final_states = ref [] in
  let rec rec_proc_state ss  = 
    (if ss = [] then ()
    else
      (
        print_endline ("=================\nPCS18: " ^ (string_of_int (List.length ss)) ^ " Processing states: " ^ 
        String.concat "" (List.mapi (fun i s -> 
          "\n------------------\nAlternative " ^ string_of_int i ^ ": " ^
          show_proc_state s ^ "" 
        ) ss) ^ "\n=================");
        let new_states = List.concat_map (fun s -> 
          if s.input_future.idx > !last_failure.input_future.idx 
            (* if equal inputs, things that have more reductions are more likely to be the last failure *)
            || (s.input_future.idx = !last_failure.input_future.idx && List.length s.input_acc < List.length !last_failure.input_acc)
            then
            last_failure := s;

          if not (CharStream.has_next_char(s.input_future)) && List.length (s.input_acc) = 0 
            then 
              (all_final_states := s :: !all_final_states;
              [])
            else 
              let new_states = do_process_step s in
              new_states
        ) ss in
      rec_proc_state new_states
      )
    )
  in
  rec_proc_state [proc_state];
  (
  match !all_final_states with
  | [] -> Error !last_failure
  | ys -> Ok ys
  ) *)

let rec do_process_entire_stream () : A.t proc_state_m = 
  let* st = get_proc_state () in
  if not (CharStream.has_next_char st.input_future) && List.length (st.input_acc) = 1 
    then return (List.hd st.input_acc)
    else 
      let* () = choice_l (List.map run_processor_entry st.registry @[pfail "No more processors apply"])  in
      let* s = get_proc_state () in
      print_endline ("=========== STATE ======== \n" ^ show_proc_state s);
      
      do_process_entire_stream ()

let extract_all_result (st : proc_state) (processor : 'a proc_state_m) : ('a list, 'b) result = 
  let result = ref [] in
  let failure = ref None in
  processor st 
    (fun (msg, final_s) -> if msg <> "top_level_backtrack_backtrack" then failure := Some(msg, final_s)) 
    (fun (r, st') fail_c -> 
      result := r :: !result;
      fail_c ("top_level_backtrack_backtrack", st')
    );
  if !failure <> None then
    let (msg, final_s) = Option.get !failure in
    (* print_endline ("Final state: " ^ (Environment.show_environment final_s.store)); *)
    Error (msg, final_s)
  else
    (* print_endline ("Final state: " ^ (Environment.show_environment st.store)); *)
    Ok !result



let run_top_level (filename: string)(content : string) : A.t = 
  let input = CharStream.new_cs filename content in
  let initial_state = {
    input_future = input;
    input_expect = Expression;
    expect_state_stack = [];
    input_acc = [];
    store = Environment.default_environment;
    registry = BuiltinProcessors.default_registry;
    last_succeeded_processor = to_processor_identifier Expression "initial_none" (CS.new_t_string "[NONE]");
    failures = ([]);
  } in
  let final_state = extract_all_result initial_state (do_process_entire_stream ()) in
  match final_state  with
  | Ok [s] -> 
    (* print_endline ("Final state: " ^ (Environment.show_environment s.store)); *)
    s
  | Ok [] -> failwith ("ET76: No final state found")
  | Ok _ -> 
    (* print_endline ("Final states: " ^ (Environment.show_environment_list ys)); *)
    failwith ("ET79: Multiple final states found")

  | Error (msg, s) -> 
    print_endline ("Failure history has " ^ string_of_int (List.length s.failures) ^ " entries:\n" 
      ^ String.concat "\n" (List.mapi (fun i (msg, s) -> 
        string_of_int i ^ " failure: " ^
        msg ^ "\n" ^ show_proc_state s
     ) s.failures));
    Fail.failwith (msg ^ "\n" ^ filename ^ ":" ^ string_of_int s.input_future.line ^ ":" ^ string_of_int s.input_future.col ^
     "Processing failed at " ^ CharStream.show_current_position s.input_future
     ^ "\n"  
    )