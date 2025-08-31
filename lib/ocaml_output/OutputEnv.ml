open EngineData

type t_data = A.t
type t_env = (string, t_data) map
type t = t_env ref

let new_env () : t = ref []
let add_entry (env : t) (name : string) (data : t_data) : unit = env := (name, data) :: !env

let find_entry_and_remove (env : t) (name : string) : t_data option =
  match ListUtil.find_elem_by_key !env name with
  | None -> None
  | Some data ->
    env := List.filter (fun (n, _) -> n <> name) !env;
    Some data
;;
