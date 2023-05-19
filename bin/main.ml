open Tlang

let files_dir = Fpath.v "examples"

let list_dir =
  let files = Sys.readdir (Fpath.to_string files_dir) in
  let files = Base.Array.filter ~f:(fun x -> Filename.extension x = ".t") files in
  Base.Array.map files ~f:(fun x -> Fpath.(add_seg files_dir x))
;;

let ir_dir filename =
  let parent = Fpath.parent filename in
  let ir_path = Fpath.add_seg parent "ir" in
  (try Sys.mkdir (Fpath.to_string ir_path) 0o777 with
   | Sys_error _ -> ());
  let ir_path = Fpath.add_seg ir_path (Fpath.filename filename) in
  Fpath.(ir_path -+ "ir")
;;

let compile_file filename =
  let in_ch = open_in (Fpath.to_string filename) in
  Error_msg.reset ();
  Error_msg.set_filename (Fpath.to_string filename);
  let content = really_input_string in_ch (in_channel_length in_ch) in
  let lexbuf = Lexing.from_string ~with_positions:true content in
  let err_str = ref "" in
  let parsed_exp =
    try Parser.prog Lexer.token lexbuf |> Option.some with
    | Lexer.Error (((sl, sr), (el, er)), msg) ->
      err_str := Printf.sprintf "Error at (%d:%d)-(%d:%d). %s" sl sr el er msg;
      None
    | Parser.Error ->
      err_str := "Parser Error occured";
      None
    | _ ->
      err_str := "Unexpected error";
      None
  in
  Option.iter
    (fun pe ->
      let Semant.{ ty; _ } = Semant.trans_prog pe in
      Printf.printf "Result type:%s\n" (Types.type2str ty))
    parsed_exp;
  Option.map
    (fun pe ->
      let ir_file = ir_dir filename in
      Ir_gen.gen_prog pe |> Ir_gen.dump (Fpath.to_string ir_file);
      ir_file)
    parsed_exp
;;

let backend_exec = "backend/build/tools/driver/tlang"

let build_dir dir_name =
  let llvm_ir_path = Fpath.add_seg files_dir dir_name in
  (try Sys.mkdir (Fpath.to_string llvm_ir_path) 0o777 with
   | Sys_error _ -> ());
  llvm_ir_path
;;

let llvm_dir = build_dir "llvm"
let exec_dir = build_dir "exec"
let out_dir = build_dir "out"

let clang_command llvm_file out_file =
  "clang -O3 " ^ Fpath.to_string llvm_file ^ " -o " ^ Fpath.to_string out_file
;;

let run_exec exec_file out_file =
  "./" ^ Fpath.to_string exec_file ^ " > " ^ Fpath.to_string out_file
;;

let print_cmd cmd = Printf.printf "%s\n" cmd

let ir_to_backend ir_file =
  let is_avail = Sys.file_exists backend_exec in
  let llvm_ir_file = Fpath.add_seg llvm_dir (Fpath.filename ir_file) in
  let llvm_ir_file = Fpath.(llvm_ir_file -+ ".ll") in
  let exec_file = Fpath.(add_seg exec_dir (filename ir_file) -+ ".exe") in
  let out_file = Fpath.(add_seg out_dir (filename ir_file) -+ ".out") in
  let cmd =
    backend_exec ^ " " ^ Fpath.to_string ir_file ^ " > " ^ Fpath.to_string llvm_ir_file
  in
  print_cmd cmd;
  let code = if is_avail then Sys.command cmd else -1 in
  let cmd = clang_command llvm_ir_file exec_file in
  print_cmd cmd;
  let code = if code = 0 then Sys.command cmd else -1 in
  let cmd = run_exec exec_file out_file in
  if code = 0 then Sys.command cmd else -1
;;

let () =
  let ir_files = Array.map compile_file list_dir in
  let ir_files = Base.Array.filter_opt ir_files in
  let _out_codes = Array.map ir_to_backend ir_files in
  ()
;;
