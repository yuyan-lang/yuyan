open EngineData

let compiled_files : (string, A.t * t_constants) map ref = ref []

let get_all_constants () : t_constants =
  List.fold_left (fun acc (_, (_, constants)) -> constants @ acc) [] !compiled_files
;;
