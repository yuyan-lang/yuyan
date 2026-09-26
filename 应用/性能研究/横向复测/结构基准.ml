(* 文言：同树同数，遍之得和。汉语：与豫言基准同样建立满二叉树、求叶和，或填充数组后求和。OCaml 标识符使用其接受的拉丁字母。 *)
type tree = Leaf of int | Branch of tree * tree
let rec build d = if d = 0 then Leaf 1 else Branch (build (d-1), build (d-1))
let rec sum = function Leaf n -> n | Branch (a,b) -> sum a + sum b
let () =
  let n = int_of_string Sys.argv.(2) in
  let result = match Sys.argv.(1) with
  | "树基准" -> sum (build n)
  | "数组基准" ->
    let a = Array.make n 0 in
    for i = n downto 1 do a.(i-1) <- i done;
    let total = ref 0 in
    for i = n downto 1 do total := !total + a.(i-1) done;
    !total
  | _ -> failwith "未知基准" in
  Printf.printf "%d\n" result
