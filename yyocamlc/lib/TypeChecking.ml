open EngineData
open ProcCombinators

let assert_input_expect_top_level () : unit proc_state_m =
  let* proc_state = get_proc_state () in
  if proc_state.input_expect <> TopLevel then failwith "TypeChecking.assert_input_expect_top_level" else return ()
;;
