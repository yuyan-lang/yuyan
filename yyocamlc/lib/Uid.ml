
let id = ref 1

let next () : int = 
  let _ = id := (!id) + 1 in
  !id

let nextu () : string = 
  let _ = id := (!id) + 1 in
  "u_" ^ string_of_int (!id)

let nextz () : string = 
  let _ = id := (!id) + 1 in
  "z_" ^ string_of_int (!id)

let nextr () : string = 
  let _ = id := (!id) + 1 in
  "r_" ^ string_of_int (!id)

let next_name (name: string) : string =
  let _ = id := (!id) + 1 in
  name ^ "_" ^ string_of_int (!id)