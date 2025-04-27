module CS = CharStream
module Ext = AbtLib.Extent
open EngineData
open ProcCombinators


type operator_component = Parenthetical of CS.t_string
                       | Keyword of CS.t_string
                      
                      
 let show_operator_component (x : operator_component) : string =
  match x with
  | Parenthetical s -> "Parenthetical(" ^ CS.get_t_string s ^ ")"
  | Keyword s -> "Keyword(" ^ CS.get_t_string s ^ ")"

                      

let is_parenthetical (x : operator_component) : bool = 
  match x with
  | Parenthetical _ -> true
  | Keyword _ -> false

let is_keyword (x : operator_component) : bool =
  match x with
  | Parenthetical _ -> false
  | Keyword _ -> true



let rec parse_operator_name (cur_acc : CS.t_string) (input : CS.t_string) : operator_component list = 
  (* print_endline ("Parsing operator name: " ^ CS.get_t_string input);
  print_endline ("Current acc: " ^ CS.get_t_string cur_acc);  *)
  match input with
  | [] -> (
    match cur_acc with
    | [] -> []
    | _ -> [Keyword (cur_acc)]
  )
  | (x :: xs) -> (
    if x = CS.new_t_char "「"
      then
        let rec submatch subacc xs = 
          (* print_endline ("Submatching: " ^ CS.get_t_string xs);
          print_endline ("Subacc: " ^ CS.get_t_string subacc); *)
          match xs with
          | [] -> failwith "Unmatched opening parenthesis"
          | (y :: ys) -> (
            if y = CS.new_t_char "」" then
              match subacc with
              | [] -> failwith "Empty parenthesis, need to have a name or a precedence number"
              | _ -> Parenthetical (subacc) :: parse_operator_name [] ys
            else
              submatch (subacc @ [y]) ys
          )
        in
        (match cur_acc with
        | [] -> submatch [] xs
        | _ -> Keyword cur_acc :: submatch [] xs)
      else
        parse_operator_name (cur_acc @ [x]) xs
  )
            

let get_operators (input : CS.t_string) (result : A.t) : binary_op list = 
  let components = ref (parse_operator_name [] input) in
  (* print_endline ("Components: " ^ String.concat ", " (List.map show_operator_component !components)); *)
  let global_leftfix, global_left_parameter_name = (
    match !components with
    | Parenthetical precedence :: Parenthetical pname :: ys -> 
      (
        components := ys;
        match  int_of_string_opt (CS.get_t_string precedence) with
        | Some i -> FxOp (Some i), Some pname
        | None -> failwith ("ET102: Expected a number but got " ^ CS.get_t_string precedence)
      )
    | Parenthetical pname :: ys -> 
      (
        components := ys;
        FxOp None, Some pname
      )
    | Keyword _ :: _ -> 
      (
        FxNone, None
      )
    | _ -> failwith ("UDO54: Must have at least one keyword operator but got  " ^ CS.get_t_string input)
  ) in
  let global_rightfix, global_right_parameter_name = (
    match List.rev !components with
    | Parenthetical precedence :: Parenthetical pname :: ys -> 
      (
        components := List.rev ys;
        match  int_of_string_opt (CS.get_t_string precedence) with
        | Some i -> FxOp (Some i), Some pname
        | None -> failwith ("ET102: Expected a number but got " ^ CS.get_t_string precedence)
      )
    | Parenthetical pname :: ys -> 
      (
        components := List.rev ys;
        FxOp None, Some pname
      )
    | Keyword _ :: _ -> 
      (
        FxNone, None
      )
    | _ -> failwith ("UDO54: Must have at least one keyword operator but got  " ^ CS.get_t_string input)
  ) in
  (* now check all odd-indexed components are (), and even-indexed are kw*)
  if not (ListUtil.forall_i (fun i x -> if i mod 2 = 0 then is_keyword x else is_parenthetical x) !components) then
    failwith ("UDO86: Not correctly alternated " ^ CS.get_t_string input);
  let parameter_names = List.map CS.get_t_string (List.filter_map (fun x -> x) (
      global_left_parameter_name ::
      List.map (function
      | Parenthetical x -> Some x
      | Keyword _ -> None
    ) !components  @ [global_right_parameter_name]
  )) in
  let components = List.filter_map (function
    | Parenthetical _ -> None
    | Keyword x -> Some x
  ) !components in
  let component_ids = List.map (fun _ -> Uid.next()) components in
  let component_metas : binary_op_meta list = List.mapi (fun i x -> 
    {
      id = List.nth component_ids i;
      keyword = x;
      left_fixity = if i = 0 then global_leftfix else FxComp (List.nth component_ids (i - 1));
      right_fixity = if i = List.length component_ids - 1 then global_rightfix else FxComp (List.nth component_ids (i + 1));
    }
  ) components in
  let final_components : binary_op list = List.mapi (fun i x -> 
    {
      meta = List.nth component_metas i;
      reduction = (
        if i <> List.length component_metas - 1 then
          p_internal_error ("UDO105: No reduction for non-terminal operator " ^ CS.get_t_string x)
        else
          let* (operands, oper_ext) = (match global_rightfix with
          | FxNone -> pop_postfix_op_operands (List.nth component_metas i)
          | FxOp _ -> pop_prefix_op_operands (List.nth component_metas i)
          | _ -> failwith ("UDO110: Should not occur")
          ) in
          if not (List.length operands = List.length parameter_names) then failwith ("UDO119: Number of operands does not match number of parameter names " ^ CS.get_t_string x);
          (* if the freevariable of any operands clashes with parameter names, rename parameter names in result*)
          let result_ref = ref result in
          let operands_free_vars = ListUtil.remove_duplicates (List.concat_map A.get_free_vars operands) in
          let new_param_name = List.map (fun param_name -> 
            let param_name_ref = ref param_name in
            while List.mem !param_name_ref operands_free_vars do
              param_name_ref := Uid.next_name(param_name);
              result_ref := A.subst (A.fold_with_extent(A.FreeVar(!param_name_ref)) (A.get_extent_some result)) param_name !result_ref;
            done;
            !param_name_ref
            ) parameter_names in
          (* now new_param_name is bound in result, and no capturing is possible when doing iterative substitution *)
          let final_result = List.fold_right2 (A.subst) operands new_param_name !result_ref in
          push_elem_on_input_acc (Expr (A.annotate_with_extent (final_result) (oper_ext))) 
      );
      shift_action = do_nothing_shift_action;
    }
  ) components in
  final_components


let get_operators_m (input : CS.t_string) (result : A.t) : binary_op list proc_state_m = 
  try 
    let operators = get_operators input result in
    return operators
  with
  | Failure s -> 
     pfail_with_ext s (A.get_extent_some result) 
  

