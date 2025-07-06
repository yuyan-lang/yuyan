
let rec remove_duplicates (l : 'a list) : 'a list = 
  match l with
  | [] -> []
  | ( x :: xs) -> if List.mem x xs then remove_duplicates xs else x :: remove_duplicates xs


let is_sublist (a : 'a list) (b : 'a list) : bool = 
  List.for_all (fun a -> List.mem a b) a

let minus (a : 'a list) (b : 'a list) : 'a list = 
  List.filter (fun a -> not (List.mem a b)) a

let intersect (a : 'a list) (b : 'a list) : 'a list = 
  List.filter (fun a -> List.mem a b) a


let equal_as_sets (a : 'a list) (b : 'a list) : bool = 
  is_sublist a b && is_sublist b a

let rec contains_duplicate (l : 'a list) : bool = 
  match l with
  | [] ->false
  | (h :: t) -> if List.mem h t then true else contains_duplicate t

let rec find_duplicates (l : 'a list) : 'a list = 
  match l with
  | [] -> []
  | (h :: t) -> if List.mem h t then h :: find_duplicates t else find_duplicates t
  
let union (a : 'a list) (b : 'a list) : 'a list = 
  remove_duplicates (a @b)


let rec drop_n n lst =
    if n <= 0 then
      lst
    else
      match lst with
      | [] -> []
      | _ :: tl -> drop_n (n - 1) tl
  ;;


let rec find_elem_by_key (lst : ('k * 'a) list) (key : 'k) : 'a option =
  match lst with
  | [] -> None
  | (s, v) :: tl ->
      if s = key then Some v
      else find_elem_by_key tl key

let lookup_elem_by_key (lst : (string * 'a) list) (key : string) : 'a =
  match find_elem_by_key lst key with
  | Some v -> v
  | None -> failwith ("lookup_elem_by_key: key not found: " ^ key ^ " in " ^ String.concat ", " (List.map fst lst))

let lookup_elem_by_key_generic (lst : ('k * 'a) list) (key : 'k) : 'a =
  match find_elem_by_key lst key with
  | Some v -> v
  | None -> failwith ("lookup_elem_by_key: key not found: ")

let lookup_index_of_elem_by_key (lst : (string * 'a) list) (key : string) : int =
  match List.find_index (fun (k, _) -> k = key) lst with
  | Some idx -> idx
  | None -> failwith ("lookup_index_of_elem_by_key: key not found: " ^ key)

let elem_exists_by_key (lst : (string * 'a) list) (key : string) : bool =
  match find_elem_by_key lst key with
  | Some _ -> true
  | None -> false

let update_elem_by_key (lst : (string * 'a) list) (key : string) (value : 'a) : (string * 'a) list =
  if elem_exists_by_key lst key then
    List.map (fun (k, v) -> if k = key then (k, value) else (k, v)) lst
  else
    (key, value) :: lst


  

(* Every Element of a list are equal *)
let all_equal lsts =
  match lsts with
  | [] | [_] -> true  (* Empty or single list are trivially equal *)
  | first :: rest -> List.for_all ((=) first) rest


let rec take n lst =
  match (n, lst) with
  | (0, _) | (_, []) -> []
  | (_, x :: xs) -> x :: take (n - 1) xs

let rec drop n lst =
  match (n, lst) with
  | (0, _) -> lst
  | (_, []) -> []
  | (_, _ :: xs) -> drop (n - 1) xs
(* 
let nth lst n =
  if List.length lst <= n then failwith "nth: index out of bounds"
  else List.nth lst n *)

let search_lookup (lst : (string * 'a) list) (key : string) : 'a =
  match find_elem_by_key lst key with
  | Some v -> v
  | None -> failwith ("search_lookup: key not found: " ^ key)

let tabulate f n =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (f i :: acc)
  in
  aux (n - 1) []
;;
let python_range start_idx end_idx =
  let rec aux current acc =
    if current >= end_idx then List.rev acc
    else aux (current + 1) (current :: acc)
  in
  aux start_idx []
;;
   

let repeat i n =
  let rec aux i acc =
    if i <= 0 then acc
    else aux (i - 1) (n :: acc)
  in
  aux i []


let range (begin_int : int) (end_int : int) : int list =
  (* [begin_int, end_int) *)
  List.init (end_int - begin_int) (fun i -> begin_int + i)

let filter_map_i (f : int -> 'a -> 'b option) (lst : 'a list) : 'b list =
  let rec aux i acc lst =
    match lst with
    | [] -> List.rev acc
    | x :: xs ->
        match f i x with
        | Some v -> aux (i + 1) (v :: acc) xs
        | None -> aux (i + 1) acc xs
  in
  aux 0 [] lst


let insert_at_index (l : 'a list) (idx : int) (new_input : 'a list) : 'a list =
  assert (idx >= 0 && idx <= List.length l);
  take idx l @ new_input @ drop idx l

let replace_idx (inputs : 'a list) (idx : int) (new_input : 'a list) : 'a list =
  assert (idx >= 0 && idx < List.length inputs);
  take idx inputs @ new_input @ drop (idx + 1) inputs
  (* List.concat (List.mapi (fun i x ->
    if i = idx then new_input else [x]
  ) inputs) *)

let insert_after_index (l : 'a list) (idx : int) (new_input : 'a) : 'a list =
  assert (idx >= 0 && idx < List.length l);
  insert_at_index l (idx + 1) [new_input]
    

let insert_after_key (lst : (string * 'a) list) (key_idx: string) ((key ,value) : string * 'a) : (string * 'a) list =
  assert (not (elem_exists_by_key lst key));
  match List.find_index (fun (k, _) -> k = key_idx) lst with
  | None -> failwith ("insert_after_key: key not found: " ^ key_idx)
  | Some idx ->
    replace_idx lst idx (List.nth lst idx :: [(key, value)])


let list_identical (l1 : 'a list) (l2 : 'a list) : bool = 
  List.for_all2 (fun x y -> x = y) l1 l2

 
let find_opt_i p x = 
  let rec aux i l =
    match l with
    | [] -> None
    | y :: ys -> if p y then Some (i, y) else aux (i + 1) ys
  in
  aux 0 x


let rec zip lst1 lst2 =
  match lst1, lst2 with
  | [], [] -> []
  | x::xs, y::ys -> (x, y) :: zip xs ys
  | _, _ -> invalid_arg "List.zip: lists have different lengths"
  
  

let show_list (lst : 'a list) (f : 'a -> string) : string =
  "[" ^ String.concat "; " (List.map f lst) ^ "]"

let forall_i (f : int -> 'a -> bool) (lst : 'a list) : bool =
  List.for_all2 f (List.init (List.length lst) Fun.id) lst
  






let find_index_i (p : int -> 'a -> bool) (lst : 'a list) : int option =
  let rec aux i lst =
    match lst with
    | [] -> None
    | x :: xs ->
        if p i x then Some i
        else aux (i + 1) xs
  in
  aux 0 lst

let lookup_index_of (elem : 'a) (lst : 'a list) : int =
  match List.find_index (fun x -> x = elem) lst with
  | Some idx -> idx
  | None -> failwith ("lookup_index: element not found: ")

let fold_left_non_empty f lst =
  match lst with
  | [] -> failwith "fold_left_non_empty: empty list"
  | x :: xs -> List.fold_left f x xs

let rec last lst =
  match lst with
  | [] -> failwith "last: empty list"
  | [x] -> x
  | _ :: xs -> last xs