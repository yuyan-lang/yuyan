

open EngineData

let rec get_head_spine_for_iterative_structure_deref (expr : A.t) : string * string list = 
  match A.view expr with
  | A.N(N.StructureDeref label, ([], arg)::[]) -> 
    let (head, spine) = get_head_spine_for_iterative_structure_deref arg in
    (head, spine@[label])
  | A.FreeVar label -> 
    (label, [])
  | _ ->
    failwith ("AU13: Expected a structure deref or free variable but got " ^ A.show_view expr)
  
