open EngineData
open ProcCombinators

module PE = ProcessedElement

let get_module_real_path (module_name_parsed : PE.t) : string proc_state_m (* path *)  = 
  match  A.view module_name_parsed with
  | A.N(N.Builtin(N.String path), _) ->
      let* path = 
      (if String.starts_with ~prefix:"./" path then
        let* cur_module_path = get_current_file_name () in
        let cur_module_dir = Filename.dirname cur_module_path in
        let path = Filename.concat cur_module_dir (String.sub path 2 (String.length path - 2)) in
          return path
      else 
        return path
      ) in
      let realpath = Unix.realpath path in
      return realpath
  | _ -> pfail ("ET101: Expected a string but got " ^ A.show_view module_name_parsed)

  



let get_module_expr (module_name_parsed : PE.t) : A.t proc_state_m = 
  let* path = get_module_real_path module_name_parsed in
  match !compilation_manager_get_file_hook path with
  | Some (result) -> return result
  | None -> pfail ("Im30: Module not found: " ^ path)

  
  
