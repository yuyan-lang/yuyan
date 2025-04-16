
open EngineData

let id_proc (x : 'a) : 'a proc_state_m = 
    fun s -> Some (x, s)

let bind  (m : 'a proc_state_m) (f: 'a -> 'b proc_state_m) : 'b proc_state_m = 
    fun s -> 
      match m s with
      | None -> None
      | Some (x, s') -> f x s'

let (>>=) = bind

let _then (m : 'a proc_state_m) (f: 'b proc_state_m) : 'b proc_state_m = 
  fun s ->
      match m s with
      | None -> None
      | Some (_, s') -> f s'

let (>>) = _then



