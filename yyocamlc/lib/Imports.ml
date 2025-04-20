open EngineData
open ProcCombinators

module PE = ProcessedElement

let get_module_real_path (module_name_parsed : PE.t) : string proc_state_m (* path *)  = 
  let* cur_module_path = get_current_file_name () in
  let cur_module_dir = Unix.realpath (Filename.dirname cur_module_path) in
  match  A.view module_name_parsed with
  | A.N(N.Builtin(N.String path), _) ->
      let* path = 
      (if String.starts_with ~prefix:"./" path then
        let path = Filename.concat cur_module_dir (String.sub path 2 (String.length path - 2)) in
          return path
      else 
        return path
      ) in
      let realpath = Unix.realpath path in
      return realpath
  | A.N(N.StructureDeref _, _) | A.FreeVar(_) -> 
    (
      let head, spine = AbtUtil.get_head_spine_for_iterative_structure_deref module_name_parsed in
      let file_name = List.fold_left Filename.concat head spine ^ "。豫" in
      (* check if head exists in module_dir *)
      let head_name = if List.length spine = 0 then head^"。豫" else head in
      if not (Filename.is_relative file_name) && Sys.file_exists file_name then
        return file_name
      else if Sys.file_exists (Filename.concat cur_module_dir head_name) then
        return (Filename.concat cur_module_dir file_name)
      else if Sys.file_exists (Filename.concat (Sys.getcwd()) head_name) then
        return (Filename.concat (Sys.getcwd()) file_name)
      else
        pfail_with_ext ("Im33: Module not found: " ^ head_name) (A.get_extent_some module_name_parsed)
    )
  | _ -> pfail ("ET101: Expected a string but got " ^ A.show_view module_name_parsed)

  



let get_module_expr (module_name_parsed : PE.t) : A.t proc_state_m = 
  let* path = get_module_real_path module_name_parsed in
  match !compilation_manager_get_file_hook path with
  | Some (_result) -> return (A.annotate_with_extent(A.fold(A.N(N.FileRef(path), []))) (A.get_extent_some module_name_parsed))
  | None -> pfail_with_ext ("Im30: Module not found: " ^ path) (A.get_extent_some module_name_parsed)
  
  
