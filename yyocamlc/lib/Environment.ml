
open EngineData

module Ext = AbtLib.Extent
module PE = ProcessedElement
module A = ProcessedElement.A

type t = t_environment


let show (_x : t) : string = 
  "Environment"


let default_environment : t = 
  (* Initialize the environment here *)
  {
    expect_state_stack = [];
    constants = [];
  }

let wrap_store (f : t -> 'a * t) : 'a proc_state_m = 
  fun s -> 
    let (r, new_env) = f s.store in
    let new_s = {s with store = new_env} in
    Some (r, new_s)

let wrap_store_simple (f : t -> t ) : unit proc_state_m = 
  wrap_store (fun s -> ((), f s))



let push_env_state_stack (expect : expect) : unit proc_state_m = 
  wrap_store_simple (fun e -> {e with
    expect_state_stack = expect :: e.expect_state_stack;
  })

let pop_env_state_stack () : expect option proc_state_m =
  wrap_store (fun (e : t) ->
  match e.expect_state_stack with
  | [] -> (None, e)
  | x :: xs -> (Some x, {e with expect_state_stack = xs})
  )