open EngineData
open ProcCombinators

let assert_input_expect_top_level () : unit proc_state_m =
  let* proc_state = get_proc_state () in
  if proc_state.input_expect <> TopLevel then failwith "TypeChecking.assert_input_expect_top_level" else return ()
;;

let group_type_constructor_declarations (decls : A.t list) : (A.t * A.t list) * A.t list =
  (* print_endline ("group_type_constructor_declarations: " ^ String.concat ", " (List.map A.show_view decls)); *)
  match decls with
  | [] -> failwith "TC11: Unexpected end of declaration list"
  | hd :: tl ->
    (match A.view hd with
     | A.N (N.Declaration N.TypeConstructorDecl, [ ([], _name); _ ]) ->
       let rec aux (sofar : A.t list) (decls : A.t list) : (A.t * A.t list) * A.t list =
         (* print_endline
           ("aux sofar: "
            ^ String.concat ", " (List.map A.show_view sofar)
            ^ " remaining: "
            ^ String.concat ", " (List.map A.show_view decls)); *)
         match decls with
         | [] -> (hd, sofar), []
         | tl_hd :: tl_tl ->
           (match A.view tl_hd with
            | A.N (N.Declaration N.ConstructorDecl, _) -> aux (sofar @ [ tl_hd ]) tl_tl
            | _ -> (hd, sofar), decls)
       in
       aux [] tl
     | _ -> failwith ("TC12: Expecting type constructor declaration but got " ^ A.show_view hd))
;;

let rec group_declarations (decls : A.t list) : (A.t * A.t list) list =
  (* print_endline ("group_declarations: " ^ String.concat ", " (List.map A.show_view decls)); *)
  match decls with
  | [] -> []
  | hd :: tl ->
    (match A.view hd with
     | A.N (N.Declaration N.ConstantDecl, _) ->
       (match tl with
        | [] -> failwith "OO245: Unexpected end of declaration list"
        | tl_hd :: tl_tl ->
          (match A.view tl_hd with
           | A.N (N.Declaration N.ConstantDefn, _) -> (hd, [ tl_hd ]) :: group_declarations tl_tl
           | _ -> Fail.failwith ("OO246: Expecting constant definition, got: " ^ A.show_view tl_hd)))
     | A.N (N.Declaration N.DirectExpr, _) -> (hd, []) :: group_declarations tl
     | A.N (N.Declaration N.CustomOperatorDecl, _) -> group_declarations tl
     | A.N (N.Declaration N.TypeConstructorDecl, _) ->
       let cons_decls, remaining = group_type_constructor_declarations decls in
       cons_decls :: group_declarations remaining
     | A.N (N.Declaration N.TypeDefn, _) -> (hd, []) :: group_declarations tl
     | _ -> Fail.failwith ("TC278: Expecting group leading constructor declaration, got: " ^ A.show_view hd))
;;

let rec get_constructor_tp_head (tp : A.t) : string =
  match A.view tp with
  | A.FreeVar name -> name
  | A.N (N.Ap, [ ([], head); _ ]) -> get_constructor_tp_head head
  | A.N ((N.ExplicitPi | N.ImplicitPi), [ ([], _); ([ _ ], cod) ]) -> get_constructor_tp_head cod
  | A.N (N.Arrow, [ ([], _); ([], cod) ]) -> get_constructor_tp_head cod
  | _ -> Fail.failwith ("TC41: Expecting constructor type but got " ^ A.show_view tp)
;;
