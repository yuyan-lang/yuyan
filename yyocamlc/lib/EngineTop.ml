

open EngineData

let do_process_step  (proc_state : proc_state) : proc_state list = 
  List.filter_map (fun proc -> 
    if proc.expect <> proc_state.input_expect 
    then None
    else
      match (proc.process proc_state) with
      | None -> None
      | Some (new_state) -> 
        let new_state = {new_state with last_succeeded_processor = proc.name} in
        Some new_state
  ) proc_state.registry


let do_process_entire_stream (proc_state : proc_state) : (proc_state list , proc_state) result = 
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
  )



let run_top_level (filename: string)(content : string) : Environment.t = 
  let input = CharStream.new_cs filename content in
  let initial_state = {
    input_future = input;
    input_expect = Expression;
    input_acc = [];
    store = Environment.default_environment;
    registry = BuiltinProcessors.default_registry;
    last_succeeded_processor = "None";
  } in
  let final_state = do_process_entire_stream initial_state in
  match final_state with
  | Ok [s] -> 
    (* print_endline ("Final state: " ^ (Environment.show_environment s.store)); *)
    s.store
  | Ok [] -> failwith ("ET76: No final state found")
  | Ok _ -> 
    (* print_endline ("Final states: " ^ (Environment.show_environment_list ys)); *)
    failwith ("ET79: Multiple final states found")
  | Error s -> 
    Fail.failwith (filename ^ " Processing failed at " ^ CharStream.show_current_position s.input_future)
    (* print_endline ("Final state: " ^ (Environment.show_environment s.store)); *)