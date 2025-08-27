(* Matrix multiplication benchmark in OCaml *)

let upper_limit = 10000000

let matrix_size = 
  let args = Sys.argv in
  if Array.length args = 1 then
    2048
  else
    int_of_string args.(1)

let generate_random_matrix size =
  Array.init size (fun _ ->
    Array.init size (fun _ ->
      Random.float 1.0))

let matrix_multiply matrix_a matrix_b =
  let size = Array.length matrix_a in
  Array.init size (fun row ->
    Array.init size (fun col ->
      let sum = ref 0.0 in
      for k = 0 to size - 1 do
        sum := !sum +. (matrix_a.(row).(k) *. matrix_b.(k).(col))
      done;
      !sum))

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