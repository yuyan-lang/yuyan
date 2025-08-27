Printexc.record_backtrace true

let print_all_constants () =
  List.iter
    (fun (filepath, (_, constants)) ->
       print_endline ("Constants in " ^ filepath);
       List.iter
         (fun (id, value) -> print_endline ("  " ^ string_of_int id ^ " = " ^ EngineDataPrint.show_t_constant value))
         (List.rev constants))
    (List.rev !CompilationCache.compiled_files)
;;

let process_file (filename : string) (output_path : string option) (compile_only : bool) =
  let _ = CompilationManager.compile_or_retrieve_file_content (Unix.realpath filename) in
  (* let _ = print_all_constants () in *)
  let file_path = CompilationManager.output_ocaml () in
  print_endline ("[Done] Writing " ^ file_path);
  CompilationManager.compile_and_run_ocaml file_path output_path compile_only
;;

let input_files = ref []
let output_path = ref None
let compile_only = ref false
let show_help = ref false

let help_text = "
豫言编译器 (YuYan Compiler)

用法: yyocamlc [选项] <输入文件>

选项:
  -o <路径>    指定输出可执行文件路径
  -c           仅编译，不运行
  -h           显示此帮助信息
  -v           显示解析进度
  -vtc         显示类型检查进度
  -vcgen       显示代码生成进度
  -pt          显示解析追踪

示例:
  yyocamlc 程序.豫                    # 编译并运行程序.豫
  yyocamlc -o 输出程序 程序.豫        # 编译程序.豫为输出程序
  yyocamlc -c 程序.豫                 # 仅编译程序.豫，不运行
  yyocamlc -c -o myapp 程序.豫        # 编译为myapp，不运行
"

let rec process_args (args : string list) =
  match args with
  | [] -> ()
  | "-h" :: _ ->
    show_help := true;
    ()
  | "-o" :: path :: remaining_args ->
    output_path := Some path;
    process_args remaining_args
  | "-c" :: remaining_args ->
    compile_only := true;
    process_args remaining_args
  | "-v" :: remaining_args ->
    Flags.show_parse_progress := true;
    process_args remaining_args
  | "-vtc" :: remaining_args ->
    Flags.show_type_checking_progress := true;
    process_args remaining_args
  | "-vcgen" :: remaining_args ->
    Flags.show_codegen_progress := true;
    process_args remaining_args
  | "-pt" :: remaining_args ->
    Flags.show_parse_tracing := true;
    process_args remaining_args
  | file_name :: remaining_args ->
    input_files := file_name :: !input_files;
    process_args remaining_args
;;

(* Main function to process command line arguments *)
let main () =
  (* get the first arg*)
  let args = List.tl (Array.to_list Sys.argv) in
  process_args args;
  
  if !show_help then
    print_endline help_text
  else
    (* process the input files *)
    match !input_files with
    | [ filename ] -> process_file filename !output_path !compile_only
    | [] -> print_endline "错误：未提供输入文件。使用 -h 查看帮助。"
    | fs -> failwith ("S21: Expecting single filename, got" ^ String.concat ", " fs)
;;
