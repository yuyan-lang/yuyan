

let failwith (msg : string) : 'a = 
  print_endline msg;
  (* Print the backtrace *)
  (* Printexc.print_backtrace stderr; *)
  Printexc.print_raw_backtrace Stdlib.stdout (Printexc.get_callstack 20);
  (* Raise an exception *)
  failwith msg