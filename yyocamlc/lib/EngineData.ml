

(* A state is a charstream (for pending output )
a list of currently processed stack elements (and environment at that time (for backtracking?) )
and a current environment generator

[Actually it is a list of them, or a decision tree of past states]
*)

type scan_env = InString | InComment
type env = Expression | Scanning of scan_env


type ('a, 'b) map = ('a * 'b) list

(* processing state *)
type proc_state = {
  input_future : CharStream.t;
  env : env;
  input_acc : ProcessedElement.t list;
  store : Environment.t;
  registry : processor_registry;
}
and processor = {
  env : env; (*the environment of the processor should*)
  (* future_cond : CharStream.t -> bool; to decide whether it can process this state *)
  process : proc_state -> proc_state option; (* to process this state *)
}
and processor_registry = processor list

type 'a proc_state_m = proc_state -> ('a * proc_state) option
