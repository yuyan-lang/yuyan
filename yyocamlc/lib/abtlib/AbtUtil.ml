open EngineData
open ProcCombinators

let rec get_head_spine_for_iterative_structure_deref (expr : A.t) : string * string list =
  match A.view expr with
  | A.N (N.StructureDeref label, ([], arg) :: []) ->
    let head, spine = get_head_spine_for_iterative_structure_deref arg in
    head, spine @ [ label ]
  | A.FreeVar label -> label, []
  | A.N (N.Builtin (N.Library path), []) -> path, []
  | _ ->
    print_failwith
      ("AU13: Expected a structure deref or free variable but got "
       ^ A.show_view expr
       ^ "\n"
       ^ Ext.show_extent_1_based (A.get_extent_some expr))
;;

let rec get_referenced_constant_ids (expr : A.t) : int list =
  match A.view expr with
  | A.N (N.Constant id, []) -> [ id ]
  | A.N (_, args) -> List.concat (List.map (fun (_, arg) -> get_referenced_constant_ids arg) args)
  | A.FreeVar _ -> []
;;
