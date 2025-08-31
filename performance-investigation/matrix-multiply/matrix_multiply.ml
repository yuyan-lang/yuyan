(* Matrix multiplication benchmark in OCaml using lists *)
(* This version uses lists to match YuYan's data structure *)

let upper_limit = 10000000

let matrix_size = 
  let args = Sys.argv in
  if Array.length args = 1 then
    2048
  else
    int_of_string args.(1)

(* Helper function to generate a list of n elements using a generator function *)
let rec tabulate n f =
  if n = 0 then []
  else f (n - 1) :: tabulate (n - 1) f

(* Generate list from 0 to n-1 *)
let rec range n =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (i :: acc)
  in
  aux (n - 1) []

(* Get nth element from a list - O(n) operation *)
let rec nth n lst =
  match lst with
  | [] -> failwith "Index out of bounds"
  | h :: t ->
    if n = 0 then h
    else nth (n - 1) t

(* Sum a list of floats *)
let sum_float_list lst =
  List.fold_left (+.) 0.0 lst

(* Generate a random matrix as a list of lists *)
let generate_random_matrix size =
  List.init size (fun _ ->
    List.init size (fun _ ->
      Random.float 1.0))

(* Matrix multiply using lists - matching YuYan's algorithm structure *)
let matrix_multiply matrix_a matrix_b =
  let size = List.length matrix_a in
  List.init size (fun row ->
    List.init size (fun col ->
      sum_float_list (
        List.init size (fun k ->
          (nth k (nth row matrix_a)) *. 
          (nth col (nth k matrix_b))))))

let main () =
  Random.self_init ();
  let matrix_a = generate_random_matrix matrix_size in
  let matrix_b = generate_random_matrix matrix_size in
  let start_time = Unix.gettimeofday () *. 1e9 in
  let matrix_c = matrix_multiply matrix_a matrix_b in
  let end_time = Unix.gettimeofday () *. 1e9 in
  let total_nanos = end_time -. start_time in
  let total_seconds = total_nanos *. 1e-9 in
  Printf.printf "%f\n" total_seconds

let () = main ()