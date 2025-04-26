
module type NODE_CLASS = sig
  type t
  val arity: t -> int list option

  val show : t -> string
end


module type ABT = sig
  type t
  type node_t 
  type t_extent
  type t_view = FreeVar of string | N of node_t * (string list * t) list

  val free_var: string -> t
  val unbind_abt: t -> string * t
  val unbind_abt_list: t -> int -> string list * t
  val abstract_over: t -> string -> t
  val abstract_over_list: t -> string list -> t
  val abstract_over_no_name: t -> t
  val view: t -> t_view
  val fold: t_view -> t
  val fold_with_extent: t_view -> t_extent -> t
  (* val fold_direct_unsafe : ?check_arg_ctx:bool -> t_view -> t *)
  val subst: t -> string -> t -> t

  val appears_in_expr: string -> t -> bool
  val possibly_appears_in_expr: string -> t -> bool
  val get_free_vars: t -> string list


  val show_raw : t -> string
  val show_view : t -> string

  (* 
  extent is not preserved by view/fold pair 
  *)
  val annotate_with_extent : t -> t_extent -> t
  val get_extent : t -> t_extent option
  val get_extent_some : t -> t_extent 
  val operate_on_view : t -> (t_view -> t_view) -> t

  val eq_abt : t -> t -> bool

  
end
module type EXTENT = sig
  type t = string * (int * int) * (int * int)
  val combine_extent : t -> t -> t
  val combine_extent_list : t list -> t
  val show_extent : t -> string
  val show_extent_1_based : t -> string

  type t_str
  val str_with_extent : string -> t -> t_str
  val get_str_content : t_str -> string
  val get_str_extent : t_str -> t
  val join_t_str : t_str -> t_str -> t_str
  val join_t_str_list : t_str list -> t_str

end

module Extent : EXTENT = struct
  type t = string * (int * int) * (int * int)
  type t_extent = t


  let combine_extent (s1 : t_extent) (s2 : t_extent) : t_extent = 
    let (file1, (row1, col1), (_row1', _col1')) = s1 in
    let (_file2, (_row2, _col2), (row2', col2')) = s2 in
    (file1, (row1, col1), (row2', col2'))

  let combine_extent_list (s : t_extent list) : t_extent = 
    match s with
    | [] -> failwith "combine_extent_list: empty list"
    | [x] -> x
    | x :: xs -> List.fold_left combine_extent x xs

  
  let show_extent (s : t_extent) : string = 
    let (file, (row1, col1), (row2, col2)) = s in
    Printf.sprintf "%s:%d:%d-%d:%d" file row1 col1 row2 col2

  let show_extent_1_based (s : t_extent) : string = 
    let (file, (row1, col1), (row2, col2)) = s in
    if row1 = row2
      then
    Printf.sprintf "%s:%d:%d-%d" file (row1+1) (col1+1) (col2+1)
      else
    Printf.sprintf "%s:%d:%d - %d:%d" file (row1+1) (col1+1) (row2+1) (col2+1)

  type t_str = string * t_extent
  let str_with_extent (s : string) (e : t_extent) : t_str = (s, e)
  let get_str_content (s : t_str) : string = fst s
  let get_str_extent (s : t_str) : t_extent = snd s

  let join_t_str (s1 : t_str) (s2 : t_str) = 
    let (s1_str, s1_extent) = s1 in
    let (s2_str, s2_extent) = s2 in
    let new_extent = combine_extent s1_extent s2_extent in
    (s1_str ^ s2_str, new_extent)

  let join_t_str_list (s : t_str list) : t_str =
    match s with
    | [] -> failwith "join_t_str_list: empty list"
    | [x] -> x
    | x :: xs -> List.fold_left join_t_str x xs

  
end

module Flags = struct
  let use_lazy_substitution () = false
end


module Abt (NodeClass: NODE_CLASS) : ABT
  with type node_t = NodeClass.t
  and type t_extent = Extent.t
 = struct 

  type ctx = string list (* Bnd 1 means the first element in tis list *)
  type t_extent = string * (int * int) * (int * int)
  type node_t = NodeClass.t
  (*
  The idea is that ctx should only contain free vars of base_t, no extra. So the number of free var and the free vars can be accessed in O(1) time.
  For this to work, we need to for every subnode in the graph, we need to record how to map the outer freevars into the inner free vars.
  So each arg has an index list, inner freevars = outer freevars [inner list[i]-1] (for all i)
  *)
  type base_t = 
      BoundVar of int 
    | N of NodeClass.t * (base_t) list 
    | Binding of string * base_t 
    | Subst of base_t * subst_t
    | AnnotatedWithExtent of t_extent * base_t
  and subst_t = 
    | Id
    | Upshift
    | Cons of (base_t) * subst_t
    | Comp of subst_t * subst_t
  and t = ctx * base_t (* The ctx here is an over approximation, to get it precise requires more work than I wanted*)
  type t_view = FreeVar of string | N of NodeClass.t * (string list * t) list




  (* let _id_subst = Id *)
  let rec show_ctx (ctx: string list) : string = 
    "⟨" ^ String.concat "," (List.rev ctx) ^ "⟩"
  (* and show_arg_ctx (ctx: int list) : string = 
    "⟨" ^ String.concat "," (List.map string_of_int (List.rev ctx)) ^ "⟩" *)
  and show_raw_base_t (tm : base_t) : string = 
    match tm with
    | BoundVar i -> string_of_int i
    | Binding(name, i) -> 
      "" ^ name ^ "." ^ show_raw_base_t i
    | N(node_type, args) -> 
      let arg_str = List.map (fun (arg) -> 
        (* show_arg_ctx inner_ctx ^ "" ^ *)
         show_raw_base_t arg
      ) args in
      let arg_str = "[" ^ String.concat "; " arg_str ^ "]" in
      NodeClass.show node_type ^ "" ^ arg_str
    | AnnotatedWithExtent(_, inner_abt) -> "<" ^ show_raw_base_t inner_abt ^ ">"
    | Subst(b, s) -> "(" ^ show_raw_base_t b ^ "){" ^ show_raw_subst s ^ "}"
  and show_raw_subst (subst: subst_t) : string =
    match subst with
    | Id -> "id"
    | Upshift -> "up"
    (* | Cons((ctx, b), s) -> show_arg_ctx ctx ^ show_raw_base_t b ^ " . " ^ show_raw_subst s *)
    | Cons(b, s) ->  show_raw_base_t b ^ " . " ^ show_raw_subst s
    | Comp(s1, s2) -> "(" ^ show_raw_subst s1 ^ ") * (" ^ show_raw_subst s2 ^ ")"
    (* | AnnotatedWithExtent(extent, i) ->
      "WithExtent" ^ (let (_, (start_row, start_col), (end_row, end_col)) = extent in
        "<" ^ string_of_int start_row ^ ":" ^ string_of_int start_col ^ "-" ^ string_of_int end_row ^ ":" ^ string_of_int end_col ^ ">"
      ) ^ "(" ^ show_raw_base_t i ^ ")" *)
  and show_raw ((ctx,tm): t) : string = 
    show_ctx ctx ^ "" ^ show_raw_base_t tm
  


  let possibly_appears_in_expr (name: string) ((ctx, _): t) : bool = 
    List.mem name ctx

  let free_var(name: string) : t = ([name], BoundVar(1))

  let ctx_nth (ctx: string list) (i: int) : string = 
    try List.nth ctx (i ) with _ -> failwith ("ctx_nth: " ^ string_of_int i ^ " not found in " ^ show_ctx ctx)

  (* let int_ctx_nth (ctx: int list) (i: int) : int = 
    try List.nth ctx (i ) with _ -> failwith ("int_ctx_nth: " ^ string_of_int i ^ " not found in " ^ show_arg_ctx ctx) *)


  let rec find_uniq_name(name: string) (name_list: string list) : string = 
    if List.mem name name_list then
       (* append a random charcter from [A-Z][a-z][0-9] until this does not hold *)
      (* Randomly choose a character from [0-9], [A-Z], or [a-z] *)
    let random_char =
      match Random.int 3 with
      | 0 -> Char.chr (Random.int 10 + 48)   (* [0-9] *)
      | 1 -> Char.chr (Random.int 26 + 65)   (* [A-Z] *)
      | _ -> Char.chr (Random.int 26 + 97)   (* [a-z] *)
    in
    find_uniq_name (name ^ String.make 1 random_char) name_list
    else name

  (* let _unbind_abt((ctx, abt): t) : string * t = 
    match abt with
    | Binding(name, i) -> 
        let bnd_name = find_uniq_name name ctx in (bnd_name, (bnd_name::ctx, i))
    | _ -> failwith "unbind_expr_list: not a bound expression" *)

  (* reduce to a canonical subst form *)
  (* let arg_ctx_reduce ((arg_ctx, arg): int list * base_t) : base_t = 
    match arg with
    | BoundVar i -> (
      BoundVar (int_ctx_nth arg_ctx (i-1))
    )
    | _ ->
      (* let folded_subst = List.fold_right (fun num acc -> Cons (([num], BoundVar 1), acc)) arg_ctx Id in *)
      let folded_subst = List.fold_right (fun num acc -> Cons (BoundVar num, acc)) arg_ctx Id in
      Subst(arg, folded_subst) *)

  (* let rec subst_with_arg_ctx ((inner_ctx, arg) : int list * base_t) (s : subst_t) : int list * base_t = 
      let folded_subst = List.fold_right (fun num acc -> Cons (BoundVar num, acc)) inner_ctx s in
      let new_ctx = List.mapi (fun i _ -> i + 1) inner_ctx in
      (new_ctx, Subst(arg, folded_subst)) *)
  let rec subst_head_reduce (base_t : base_t) : base_t = 
    (* let _ = print_endline ("subst_head_reduce: " ^ show_raw_base_t base_t) in *)
    match base_t with
    | Subst(b, s) -> (
      match b, s with
      | BoundVar i, Id -> BoundVar i
      | BoundVar i, Cons(hd, s) -> (
        if i = 1 
          then subst_head_reduce (hd)
          else subst_head_reduce (Subst(BoundVar (i - 1), s))
      )
      | BoundVar i, Upshift -> BoundVar (i + 1)
      | BoundVar i, Comp(Upshift, s2) -> subst_head_reduce (Subst (BoundVar (i + 1), s2))
      | BoundVar _, Comp(s1, s2) -> subst_head_reduce (Subst (b, (subst_comp_reduce (Comp(s1, s2)))))
      | N(node_type, args), s -> N(node_type, List.map (fun ( arg) -> 
          (* subst_with_arg_ctx (inner_ctx, arg) s *)
          Subst(arg, s)
        ) args)
      | Binding(name, under), s -> Binding(name, 
        Subst(under, Cons(BoundVar 1, Comp(s, Upshift)))
      )
      | Subst(b, s1), s2 -> subst_head_reduce (Subst(b, Comp(s1, s2)))
      | AnnotatedWithExtent(extent, under), s -> AnnotatedWithExtent(extent, Subst(under, s))
    )
    | _ -> base_t
  and subst_comp_reduce (subst: subst_t) : subst_t = 
    match subst with
    | Comp(s1, s2) -> (
      match s1, s2 with
      | Id, _ -> s2
      | Upshift, Id -> Upshift
      | Upshift, Cons(_, s) -> subst_comp_reduce s
      | Cons(a, s), t -> Cons(Subst(a, t), Comp(s, t))
      | Comp(s1, s2), t -> Comp(s1, Comp(s2, t))
      | Upshift, _ -> subst
    )
    | _ -> subst
  (* let unbind_abt((ctx, abt): t) : string * t = 
    let rec bound_var_occurs_in_tm (var : int) (tm : base_t) : bool = 
      match tm with
      | Binding(_, inner_tm) -> bound_var_occurs_in_tm (var + 1) inner_tm
      | N(_, args) -> List.exists (fun (params, _) -> List.mem var params) args
      | BoundVar i -> i = var
      | AnnotatedWithExtent(_, inner_tm) -> bound_var_occurs_in_tm var inner_tm
      | Subst(_, _) -> bound_var_occurs_in_tm var (subst_head_reduce tm)
    in
    let rec decrement_all_bnd_var (abt: base_t): base_t = 
      match abt with
      | BoundVar(i) -> BoundVar(i - 1)
      | N(node_type, args) -> N(node_type, List.map (fun (inner_ctx, arg) -> (List.map (fun i -> i - 1) inner_ctx, arg)) args)
      | Binding(name, i) -> Binding(name, decrement_all_bnd_var i)
      | AnnotatedWithExtent(extent, i) -> AnnotatedWithExtent(extent, decrement_all_bnd_var i)
      | Subst(_, _) -> decrement_all_bnd_var (subst_head_reduce abt)
    in
    match abt with
    | Binding(name, under) -> 
      if bound_var_occurs_in_tm 1 under then 
        let bnd_name = find_uniq_name name ctx in (bnd_name, (bnd_name::ctx, under))
      else (name, (ctx, decrement_all_bnd_var under))
    | _ -> failwith "unbind_expr_list: not a bound expression" *)
  let rec hereditary_subst_head_reduce (base_t: base_t) : base_t = 
    match base_t with
    | Subst(_, _) -> (
      hereditary_subst_head_reduce (subst_head_reduce base_t)
    )
    | BoundVar _ -> base_t
    | AnnotatedWithExtent(ext, inner_abt) -> AnnotatedWithExtent(ext, hereditary_subst_head_reduce inner_abt)
    | Binding(name, inner_abt) -> Binding(name, hereditary_subst_head_reduce inner_abt)
    | N(node_type, args) -> N(node_type, List.map (fun (arg) -> hereditary_subst_head_reduce arg) args)

  let explicit_subst (base , subst: base_t * subst_t) : base_t = 
    if Flags.use_lazy_substitution()
      then Subst(base, subst)
  else hereditary_subst_head_reduce (Subst(base, subst))


  let rec unbind_abt((ctx, abt): t) : string * t = 
    match abt with
    | Binding(name, under) -> 
        let bnd_name = find_uniq_name name ctx in (bnd_name, (bnd_name::ctx, under))
    | Subst(_, _) -> unbind_abt (ctx, subst_head_reduce abt)
    | _ -> failwith "unbind_expr_list: not a bound expression"
  
  let unbind_abt_list(tm: t) (bnd_count: int) : string list * t = 
    let rec unbind_rec (sofar: string list) (tm: t) (bnd_count: int) : string list * t = 
      if bnd_count <= 0 then (sofar, tm)
      else 
        let (name, under_abt)  = unbind_abt tm
        in unbind_rec (sofar@[name]) under_abt (bnd_count - 1)
    in 
    unbind_rec [] tm bnd_count

  (* let map_inner_ctx (inner_ctx: int list) (outer_ctx: string list) : string list = 
    List.map (fun i -> ctx_nth outer_ctx (i - 1)) inner_ctx *)
  let rec view ((ctx, abt): t) : t_view = 
    (* let _ = print_endline ("view: " ^ show_raw (ctx, abt)) in *)
    try(
      match abt with
      | BoundVar(i) -> FreeVar(ctx_nth ctx (i - 1))
      | N(node_type, args) -> (
        match NodeClass.arity node_type with
        | None -> N(node_type, List.map (fun (arg) -> ([], (ctx, arg))) args)
        | Some(arity_l) -> N(node_type, List.map2 (fun arity (arg) -> unbind_abt_list (ctx,arg) arity) arity_l args) 
      )
      (* | N(node_type, args) -> (
        match NodeClass.arity node_type with
        | None -> N(node_type, List.map (fun (inner_ctx, arg) -> ([], (map_inner_ctx inner_ctx ctx, arg))) args)
        | Some(arity_l) -> N(node_type, List.map2 (fun arity (inner_ctx, arg) -> unbind_abt_list (map_inner_ctx inner_ctx ctx,arg) arity) arity_l args) 
      ) *)
      | Binding _ -> (
        (* prints a stack trace here*)
        Printexc.print_raw_backtrace Stdlib.stdout (Printexc.get_callstack 20);
        failwith ("view: cannot view a binding " ^ show_raw (ctx, abt)))
      | AnnotatedWithExtent(_, inner_abt) -> view (ctx, inner_abt)
      | Subst(_, _) -> view (ctx, subst_head_reduce abt)
    ) with Failure s -> failwith ("" ^ s ^ "\n when viewing " ^ show_raw (ctx, abt))

  
  let get_free_vars (tm: t) : string list = (
    let rec aux (tm : t) : string list = 
      (
        match view tm with
        | FreeVar(name) -> [name]
        | N(_, args) -> List.flatten (List.map (fun (bnds, arg) -> ListUtil.minus (aux arg) bnds) args)
      ) in
    ListUtil.remove_duplicates (aux tm)
  )
      
  let appears_in_expr (name: string) (tm: t) : bool = 
    List.mem name (get_free_vars tm)


  let iterative_upshifts(count: int) : subst_t = 
    List.fold_right (fun _ acc -> Comp(Upshift, acc)) (ListUtil.python_range 0 count) Id

  let unify_tm_contexts (tms : (string list * base_t) list) : string list * base_t list = 
    let final_ctx = ref [] in
    let final_tms = List.map (fun (arg_ctx, arg) -> (
      ( let subst =  List.fold_right (fun arg_free_var acc ->
          match List.find_index (fun x -> x = arg_free_var) (!final_ctx) with
          | None -> (let _ = final_ctx := (!final_ctx)@[arg_free_var] in Cons(BoundVar (List.length !final_ctx), acc))
          | Some(i) -> Cons(BoundVar (i + 1), acc)
        ) arg_ctx (iterative_upshifts (List.length arg_ctx)) in (* actually upshifts here are unnecessary because context enclose arg, no extra free args in arg except those in the context, so we never go above *)
        explicit_subst(arg, subst)
      )
    )) tms in
    (!final_ctx, final_tms)

   let abstract_over_no_name ((ctx, abt): t) : t = 
  (*  let rec increment_all_bnd_var (abt: base_t): base_t = 
      match abt with
      | BoundVar(i) -> BoundVar(i + 1)
      | N(node_type, args) -> N(node_type, List.map (fun (inner_ctx, arg) -> (List.map (fun i -> i + 1) inner_ctx, arg)) args)
      | Binding(name, i) -> Binding(name, increment_all_bnd_var i)
      | AnnotatedWithExtent(extent, i) -> AnnotatedWithExtent(extent, increment_all_bnd_var i)
      | Subst(_, _) -> increment_all_bnd_var (subst_head_reduce abt)
    in *)
    (ctx, Binding("__no_name", explicit_subst(abt, Upshift)))
  
  let abstract_over ((ctx, abt): t) (name: string) : t =
    match List.find_index (fun x -> x = name) ctx with
    | None -> abstract_over_no_name (ctx, abt)
    | Some(outer_name_idx_in_list) -> (
        (* Helper function to shift and replace bound variables as required, 
        shifts idx below name_idx and above outer_index by 1, and replaces name_idx with 1
         *)
      (* let rec adjust_bound_var (name_idx : int) (outer_index : int) (abt : base_t) : base_t =
        let adjust_idx (idx: int) = 
          if idx = name_idx then 1
          else if idx < name_idx && idx > outer_index then idx + 1
          else idx 
        in
        match abt with
        | BoundVar i -> BoundVar (adjust_idx i)
        | AnnotatedWithExtent(extent, inner_abt) -> AnnotatedWithExtent(extent, adjust_bound_var name_idx outer_index inner_abt)
        | Subst(_, _) -> adjust_bound_var name_idx outer_index (subst_head_reduce abt)
        | N (node_type, args) ->
          N (node_type, (List.map (fun (inner_ctx, arg) -> (List.map adjust_idx inner_ctx, arg)) args))
        | Binding (binding_name, inner_abt) ->
          Binding (binding_name, adjust_bound_var (name_idx + 1) (outer_index + 1) inner_abt)  (* Adjust inner part of Binding *)
      in *)
      (* Remove the found name from context and adjust bound variables in abt *)
      let outer_name_idx = outer_name_idx_in_list + 1 in
      let subst = List.fold_right 
              (fun this_idx acc -> Cons(BoundVar (this_idx + 1), acc)) 
              (ListUtil.python_range 1 (outer_name_idx )) 
              (Cons ((BoundVar 1), iterative_upshifts (outer_name_idx))) in
      let ctx_with_name_removed = List.filteri (fun i _ -> i <> outer_name_idx_in_list) ctx in
      (ctx_with_name_removed, Binding(name, explicit_subst(abt, subst)))
    )

  (* let abstract_over_debug (tm : t) (name : string) : t = 
    let result = abstract_over tm name in
    (* let _ = print_endline ("abstract_over: " ^ name ^ " in " ^ show_raw tm ^ " = " ^ show_raw result) in *)
    result *)

  let abstract_over_list (tm: t) (names: string list) : t = 
    List.fold_right (fun name acc -> abstract_over acc name) names tm 
  
  (* let remap_ctx((ctx, abt) : t) (given_ctx: string list) = 
    let remapped_idx = List.map (fun name -> match List.find_index (fun x -> x = name) ctx with Some i -> i | None -> failwith "abt99: remapped idx not found") new_ctx in
    let rec remap_bnd_var (abt: base_t) : base_t = 
      match abt with
      | BoundVar(i) -> BoundVar(ctx_nth remapped_idx i)
      | N(node_type, args) -> N(node_type, List.map remap_bnd_var args)
      | Binding(name, i) -> Binding(name, remap_bnd_var i)
    in
    (new_ctx, remap_bnd_var abt) *)
(*   
  let fold_direct_unsafe ?(check_arg_ctx=true) (view: t_view) : t = 
    match view with
    | FreeVar(name) -> ([name], BoundVar(1))
    | N(node_type, args) -> (
      if not check_arg_ctx || ListUtil.all_equal (List.map (fun (bnds, (ctx, _)) -> match bnds with | [] -> ctx | _ -> failwith "Cannot have nonempty binders in fold_direct_unsafe") args) then
        let final_ctx = if List.length args > 0 then (fst (snd (List.hd args))) else [] in
        (final_ctx, N(node_type, List.map (fun (_, (_, abt)) -> abt) args))
      else failwith "fold_direct_unsafe: contexts are not equal"
    ) *)


  let fold_no_arity_check (view: t_view) : t = 
    match view with
    | FreeVar(name) -> ([name], BoundVar(1))
    | N(node_type, args) -> 
      (
        let abstracted_args = List.map (fun (bnds, abt) -> (abstract_over_list abt bnds)) args in
        let final_ctx, final_args = unify_tm_contexts abstracted_args in
        (* let _ = print_endline ("fold_no_arity_check abstracted_args: " ^ String.concat "; " (List.map (fun (_, abt) -> show_raw abt) abstracted_args)) in *)
        (* let final_ctx = ref [] in
        let final_args = List.map (fun (_, (arg_ctx, arg)) -> (
          ( let subst =  List.fold_right (fun arg_free_var acc ->
              match List.find_index (fun x -> x = arg_free_var) (!final_ctx) with
              | None -> (let _ = final_ctx := (!final_ctx)@[arg_free_var] in Cons(BoundVar (List.length !final_ctx), acc))
              | Some(i) -> Cons(BoundVar (i + 1), acc)
            ) arg_ctx (iterative_upshifts (List.length arg_ctx)) in (* actually upshifts here are unnecessary because context enclose arg, no extra free args in arg except those in the context, so we never go above *)
            explicit_subst(arg, subst)
          )
        )) abstracted_args in
        (!final_ctx, N(node_type, final_args)) *)
        (final_ctx, N(node_type, final_args))
      )

  let fold (view: t_view) : t = 
  (* let _ = print_endline ("fold: " ^ (
    match view with
    | FreeVar(name) -> name
    | N(node_type, args) -> 
      let arg_str = List.map (fun (bound_vars, arg) -> 
        String.concat "" (List.map (fun v -> v ^ ".") bound_vars) ^ "" ^ show_raw arg
      ) args in
      let arg_str = "[" ^ String.concat "; " arg_str ^ "]" in
      NodeClass.show node_type ^ "" ^ arg_str
  )) in *)
  let result = (
  match view with
  | FreeVar(name) -> ([name], BoundVar(1))
  | N(node_type, args) -> 
    let arity = NodeClass.arity node_type in
    match arity with
    | None -> fold_no_arity_check view
    | Some(arity_l) -> 
      if List.length arity_l = List.length args && (List.for_all2 (fun (bnds, _) arity -> List.length bnds = arity) args arity_l)
        then fold_no_arity_check view
      else 
        (print_endline ("fold: arity mismatch for " ^ NodeClass.show node_type ^ " got " ^ show_raw(fold_no_arity_check view));
          failwith ("fold: arity mismatch for " ^ NodeClass.show node_type)
        )
  ) in 
  (* let _ = print_endline ("fold result: " ^ show_raw result) in *)
  result


   (* let rec switch_bnd_var( int_ctx : int list)  (abt : base_t) : base_t =
    match abt with
    | BoundVar i -> BoundVar (int_ctx_nth int_ctx (i - 1))
    | N(node_type, args) -> N(node_type, List.map (fun (inner_ctx, arg) -> (List.map(fun inner_idx -> int_ctx_nth int_ctx (inner_idx - 1)) inner_ctx, arg) ) args)
    | Binding(name, inner_abt) -> Binding(name, switch_bnd_var (1::int_ctx) inner_abt)
    | AnnotatedWithExtent(extent, inner_abt) -> AnnotatedWithExtent(extent, switch_bnd_var int_ctx inner_abt)
    | Subst(_, _) -> switch_bnd_var int_ctx (subst_head_reduce abt) *)
(*
  (* let upshift_base_t (final_upshift : int) (tm_b : base_t) : base_t = 
    let rec aux (limit : int) (cur_tm : base_t): base_t = 
      match cur_tm with
      | BoundVar i -> if i <= limit then BoundVar i else BoundVar (i + final_upshift)
      | Binding(name, inner_tm) -> Binding(name, aux (limit + 1) inner_tm)
      | N(node_type, args) -> N(node_type, List.map (fun (inner_ctx, arg) -> (List.map (fun i -> if i <= limit then i else i + final_upshift) inner_ctx, arg)) args)
      | AnnotatedWithExtent(extent, inner_tm) -> AnnotatedWithExtent(extent, aux limit inner_tm)
    in 
    aux 0 tm_b *)


  (* this function is auxiliary to instantiate, substitute the target index, shifting down everything above (so that subst works correctly)*)
  let rec substitute_base_t (final_upshift : int) ((tm_b_int_ctx, tm_b) : int list * base_t) (idx : int) (tm: base_t) (limit: int) : base_t = 
    let _ = print_endline ("substitute_base_t: " ^ show_arg_ctx tm_b_int_ctx ^ show_raw_base_t tm_b ^ " for " ^ string_of_int idx ^ " in " ^ show_raw_base_t tm ) in
    let map_idx i = if i > limit then i - 1 else i in
    let result = (
      match tm with
      | BoundVar i -> if i = idx then (switch_bnd_var (List.map (fun x -> x + final_upshift) tm_b_int_ctx) tm_b) else 
                BoundVar (map_idx i)
      | Binding(name, inner_tm) -> Binding(name, substitute_base_t (final_upshift + 1) (tm_b_int_ctx, tm_b) (idx + 1) inner_tm (limit + 1))
      | AnnotatedWithExtent(extent, inner_tm) -> AnnotatedWithExtent(extent, substitute_base_t final_upshift (tm_b_int_ctx, tm_b) idx inner_tm limit)
      | Subst(_, _) -> substitute_base_t final_upshift (tm_b_int_ctx, tm_b) idx (subst_head_reduce tm) limit
      | N(node_type, args) -> N(node_type, List.map (fun (inner_ctx, arg) -> 
        match  List.find_index (fun iidx -> iidx = idx) inner_ctx with
        | None -> (List.map map_idx inner_ctx, arg)
        | Some i -> (
          let new_ctx_ref = ref inner_ctx in
          let new_tm_b_int_ctx = List.map (fun name -> 
            match List.find_index (fun x -> x = name) inner_ctx with
            | None -> (
              new_ctx_ref := (!new_ctx_ref)@[name];
              List.length !new_ctx_ref
            )
            | Some idx -> (idx +1)
          ) tm_b_int_ctx in
        let _ = print_endline ("inner_ctx changed from " ^ show_arg_ctx inner_ctx ^ " to " ^ show_arg_ctx !new_ctx_ref ^ " for term " ^ show_raw_base_t tm) in
            !new_ctx_ref, substitute_base_t (final_upshift) (new_tm_b_int_ctx, tm_b) (i+1) arg (List.length inner_ctx))
        ) args)
    ) in 
    let _ = print_endline ("substitute_base_t: " ^ show_arg_ctx tm_b_int_ctx ^ show_raw_base_t tm_b ^ " for " ^ string_of_int idx ^ " in " ^ show_raw_base_t tm ^ " = " ^ show_raw_base_t result) in
    result
    *)

  (* subst a term into a binding (i.e. replace boundvar 1)*)
  let instantiate (tm_b : t) (tm : t) : t = 
    match unify_tm_contexts [tm_b; tm] with
    | unified_ctx, [under_tm_b; under_tm] -> 
      (
        match under_tm with
        | Binding(_, underlying) -> unified_ctx, explicit_subst(underlying, Cons(under_tm_b, Id))
        | _ -> failwith "Cannot subst into a non binding"
      )
    | _ -> failwith "489 Expecting 2 terms"
    
    (* match tm with
    | Binding(_, BoundVar 1) -> (tm_b_ctx, tm_b_tm)
    | Binding(_, BoundVar i) -> (ctx, BoundVar (i - 1)) (* ctx here cannot contain bound term *)
    | Binding(_, underlying) -> (
      (* let tm_b_ref = ref tm_b_tm in *)
      let new_ctx_ref = ref ctx in
      (* let tm_b_int_ctx = List.map (fun name -> 
        match List.find_index (fun x -> x = name) ctx with
        | None -> (
          new_ctx_ref := (!new_ctx_ref)@[name];
          List.length !new_ctx_ref
          (* tm_b_ref := switch_bnd_var (i + 1) (List.length !new_ctx_ref) !tm_b_ref *)
        )
        | Some idx -> (
          idx + 1
          (* tm_b_ref := switch_bnd_var (i + 1) (idx + 1) !tm_b_ref *)
        )
      ) tm_b_ctx in *)
      let tm_b_subst = List.fold_right (fun name acc -> 
        match List.find_index (fun x -> x = name) ctx with
        | None -> (
          new_ctx_ref := (!new_ctx_ref)@[name];
          Cons(BoundVar (List.length !new_ctx_ref), acc)
          (* tm_b_ref := switch_bnd_var (i + 1) (List.length !new_ctx_ref) !tm_b_ref *)
        )
        | Some idx -> (
          Cons(BoundVar (idx + 1), acc)
          (* idx + 1 *)
          (* tm_b_ref := switch_bnd_var (i + 1) (idx + 1) !tm_b_ref *)
        )
      ) tm_b_ctx (iterative_upshifts (List.length tm_b_ctx)) in (* Actually Id here suffices, because b has no free vars beyond those in the context*)
      
      (* let _ = print_endline ("instantiateing: " ^ show_raw (tm_b_ctx, tm_b_tm) ^ " for " ^ show_raw (ctx, tm)) in
      let _ = print_endline ("new_ctx: " ^ show_ctx !new_ctx_ref) in
      let _ = print_endline ("underlying: " ^ show_raw_base_t underlying) in
      let _ = print_endline ("tm_b_int_ctx: " ^ show_arg_ctx tm_b_int_ctx) in
      let _ = print_endline ("tm_b: " ^ show_raw_base_t tm_b_tm) in
      let _ = print_endline ("tm: " ^ show_raw (ctx, tm)) in *)
      (* (!new_ctx_ref, substitute_base_t 0 (tm_b_int_ctx, tm_b_tm) 1 underlying 1) *)
      (!new_ctx_ref, explicit_subst(underlying, Cons(explicit_subst(tm_b_tm, tm_b_subst), Id)))
    )
    | _ -> failwith "instantiate: not a binding"  *)
  
  let subst (tm_b : t) (var : string) ((ctx, tm): t) : t = 
    if possibly_appears_in_expr var (ctx, tm) then
        instantiate tm_b (abstract_over (ctx, tm) var)
    else (ctx, tm)
      


  let rec show_view (tm : t) : string = 
    match view tm with
    | FreeVar(name) -> name
    | N(node_type, args) -> 
      let arg_str = List.map (fun (bound_vars, arg) -> 
        String.concat "" (List.map (fun v -> v ^ ".") bound_vars) ^ "" ^ show_view arg
      ) args in
      let arg_str = "[" ^ String.concat "; " arg_str ^ "]" in
      NodeClass.show node_type ^ "" ^ arg_str

  let annotate_with_extent ((ctx, tm): t) (extent: t_extent) : t =
    match tm with
    | AnnotatedWithExtent(_, inner_tm) -> (ctx, AnnotatedWithExtent(extent, inner_tm))
    | _ -> (ctx, AnnotatedWithExtent(extent, tm))

  let fold_with_extent (tv : t_view) (extent: t_extent) : t = 
    annotate_with_extent (fold tv) extent

  let rec get_extent ((ctx, tm): t) : t_extent option =
    match tm with
    | AnnotatedWithExtent(extent, _) -> Some extent
    | Subst(base, _) -> get_extent (ctx, subst_head_reduce base)
    | _ -> (
      print_endline ("get_extent: no extent found in " ^ show_raw (ctx, tm));
      None
      )

  let get_extent_some ((ctx, tm): t) : t_extent =
    match get_extent (ctx, tm) with
    | Some extent -> extent
    | None -> failwith ("get_extent_some: no extent found in " ^ show_raw (ctx, tm))

  let operate_on_view (tm: t) (f: t_view -> t_view) : t =
    match get_extent tm with
    | None -> fold (f (view tm))
    | Some(extent) -> annotate_with_extent (fold (f (view tm))) extent


  let remove_all_extent ((ctx,tm): t) : t = 
    let rec aux (base_t : base_t) : base_t = 
      match base_t with
      | BoundVar _ -> base_t
      | Binding(name, inner_tm) -> Binding(name, aux inner_tm)
      | N(node_type, args) -> N(node_type, List.map (fun (arg) -> (aux arg)) args)
      | AnnotatedWithExtent(_, inner_tm) -> aux inner_tm
      | Subst(_, _) -> aux (subst_head_reduce base_t)
    in
    (ctx, aux tm)

  let eq_abt (tm1: t) (tm2: t) : bool = 
    match unify_tm_contexts [tm1; tm2] with
    | ctx, [tm1'; tm2'] -> (
      let _, tm1'' = remove_all_extent (ctx, tm1') in
      let _, tm2'' = remove_all_extent (ctx, tm2') in
      if Flags.use_lazy_substitution()
        then hereditary_subst_head_reduce tm1'' = hereditary_subst_head_reduce tm2''
      else tm1'' = tm2''

    )
    | _ -> failwith "eq_abt: expect 2 terms"


end