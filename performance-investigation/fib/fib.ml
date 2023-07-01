let rec fibonacci n =
  if n <= 1 then n
  else fibonacci (n - 1) + fibonacci (n - 2)

let () =
  match Array.length Sys.argv with
  | 2 ->
      let n = int_of_string Sys.argv.(1) in
      if n < 0 then
        print_endline "n must be a non-negative integer."
      else
        let result = fibonacci n in
        Printf.printf "The %dth number in the Fibonacci sequence is: %d\n" n result
  | _ ->
      Printf.printf "Usage: %s <n>\n" Sys.argv.(0)