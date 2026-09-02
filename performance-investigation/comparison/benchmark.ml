let rec fibonacci n =
  if n < 2 then n else fibonacci (n - 1) + fibonacci (n - 2)

let sieve limit =
  let is_prime = Bytes.make (limit + 1) '\001' in
  Bytes.set is_prime 0 '\000';
  Bytes.set is_prime 1 '\000';
  let prime = ref 2 in
  while !prime * !prime <= limit do
    if Bytes.get is_prime !prime <> '\000' then begin
      let multiple = ref (!prime * !prime) in
      while !multiple <= limit do
        Bytes.set is_prime !multiple '\000';
        multiple := !multiple + !prime
      done
    end;
    incr prime
  done;
  let count = ref 0 in
  for value = 2 to limit do
    if Bytes.get is_prime value <> '\000' then incr count
  done;
  !count

let matrix_multiply size =
  let total = size * size in
  let a = Array.make total 0 in
  let b = Array.make total 0 in
  let c = Array.make total 0 in
  for index = 0 to total - 1 do
    let row = index / size in
    let col = index - (row * size) in
    a.(index) <- row + col + 1;
    b.(index) <- (row * 2) + col + 1
  done;
  for row = 0 to size - 1 do
    for col = 0 to size - 1 do
      let sum = ref 0 in
      for k = 0 to size - 1 do
        sum := !sum + (a.((row * size) + k) * b.((k * size) + col))
      done;
      c.((row * size) + col) <- !sum
    done
  done;
  c.(0) + c.(total - 1)

let swap values left right =
  let value = values.(left) in
  values.(left) <- values.(right);
  values.(right) <- value

let partition values low high =
  let middle = low + ((high - low) / 2) in
  swap values middle high;
  let pivot = values.(high) in
  let store = ref low in
  for index = low to high - 1 do
    if values.(index) < pivot then begin
      swap values index !store;
      incr store
    end
  done;
  swap values !store high;
  !store

let rec quicksort values low high =
  if low < high then begin
    let split = partition values low high in
    quicksort values low (split - 1);
    quicksort values (split + 1) high
  end

let quicksort_benchmark length =
  let values = Array.init length (fun index -> length - index) in
  quicksort values 0 (length - 1);
  values.(0) + values.(length / 2) + values.(length - 1)

let () =
  if Array.length Sys.argv <> 3 then
    failwith "usage: benchmark <fib|sieve|matrix|quicksort> <size>";
  let algorithm = Sys.argv.(1) in
  let size = int_of_string Sys.argv.(2) in
  let result =
    match algorithm with
    | "fib" -> fibonacci size
    | "sieve" -> sieve size
    | "matrix" -> matrix_multiply size
    | "quicksort" -> quicksort_benchmark size
    | _ -> failwith ("unknown algorithm: " ^ algorithm)
  in
  Printf.printf "%d\n" result
