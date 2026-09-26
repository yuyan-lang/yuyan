(* 文言：工取诸数组，循环仍以通值调之。汉语：与豫言同算法，运行时选择回调；保留编译器正常优化。 *)
external monotonic_seconds : unit -> float = "yy_monotonic_seconds"
let add_one n = n + 1
let multiply_three n = n * 3
let rec accumulate f n total =
  if n = 0 then total else accumulate f (n - 1) (total + f n)
let () =
  let n = int_of_string Sys.argv.(1) in
  let choice = int_of_string Sys.argv.(2) in
  let functions = Array.make 2 add_one in
  functions.(1) <- multiply_three;
  let f = functions.(choice) in
  for round = 0 to 5 do
    let size = Sys.opaque_identity n in
    let start = monotonic_seconds () in
    let result = accumulate f size 0 in
    let elapsed = monotonic_seconds () -. start in
    Printf.printf "高阶调用,%d,%.9f,%d\n%!" round elapsed result
  done
