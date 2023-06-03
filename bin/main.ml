let files_dir = Fpath.v "examples"

let build_dir dir_name =
  let llvm_ir_path = Fpath.add_seg files_dir dir_name in
  (try Sys.mkdir (Fpath.to_string llvm_ir_path) 0o777 with
   | Sys_error _ -> ());
  llvm_ir_path
;;

let gen_new_file dir base_file ext = Fpath.(add_seg dir (filename base_file) -+ ext)
let llvm_dir = build_dir "llvm"
let exec_dir = build_dir "exec"
let out_dir = build_dir "out"
let ir_dir = build_dir "ir"
let ast_dir = build_dir "ast"

let list_dir =
  let files = Sys.readdir (Fpath.to_string files_dir) in
  let _files = Base.Array.filter ~f:(fun x -> Filename.extension x = ".t") files in
  let files = [| "op.t" |] in
  Base.Array.map files ~f:(fun x -> Fpath.(add_seg files_dir x))
;;

let log_err filename () =
  let err_file = gen_new_file out_dir filename ".err" in
  Tlang.Error_msg.log_err (Fpath.to_string err_file);
  None
;;

let check_types parsed_ast () =
  let open Tlang in
  let Semant.{ ty; _ } = Semant.trans_prog parsed_ast in
  Printf.printf "Result type:%s\n" (Types.type2str ty);
  Some parsed_ast
;;

let compile_file filename =
  let open Tlang in
  let in_ch = open_in (Fpath.to_string filename) in
  Error_msg.reset ();
  Error_msg.set_filename (Fpath.to_string filename);
  let content = really_input_string in_ch (in_channel_length in_ch) in
  let lexbuf = Lexing.from_string ~with_positions:true content in
  let err_str = ref "" in
  let parsed_exp =
    try Parser.prog Lexer.token lexbuf |> Option.some with
    | Lexer.Error (pos, msg) ->
      let (sl, sr), (el, er) = pos in
      err_str := Printf.sprintf "Error at (%d:%d)-(%d:%d). %s" sl sr el er msg;
      Error_msg.error pos !err_str;
      None
    | Parser.Error ->
      err_str := "Parser Error occured";
      Error_msg.error ((-1, -1), (-1, -1)) !err_str;
      None
    | _ ->
      err_str := "Unexpected error";
      Error_msg.error ((-1, -1), (-1, -1)) !err_str;
      None
  in
  (* semant phase *)
  let parsed_exp =
    if Error_msg.has_errors ()
    then log_err filename ()
    else Option.fold ~none:(log_err filename) ~some:check_types parsed_exp ()
  in
  Option.map
    (fun pe ->
      Sexplib0.Sexp.to_string_hum (Ast.sexp_of_comp_unit pe) |> Printf.printf "%s\n";
      let ir_file = gen_new_file ir_dir filename ".ir" in
      let ast_file = gen_new_file ast_dir filename ".ast" in
      (* Ast.sexp_of_comp_unit pe |> Sexplib0.Sexp.to_string |> Printf.printf "%s\n"; *)
      Ir_gen.gen_prog pe
      |> Ir_gen.dump (Fpath.to_string ast_file) (Fpath.to_string ir_file);
      ir_file)
    parsed_exp
;;

let backend_exec = "backend/build/tools/driver/tlang"

let clang_command llvm_file out_file =
  "clang -O3 " ^ Fpath.to_string llvm_file ^ " -o " ^ Fpath.to_string out_file
;;

let run_exec exec_file out_file =
  "./" ^ Fpath.to_string exec_file ^ " > " ^ Fpath.to_string out_file
;;

let print_cmd cmd = Printf.printf "%s\n" cmd

let ir_to_backend ir_file =
  let is_avail = Sys.file_exists backend_exec in
  let llvm_ir_file = gen_new_file llvm_dir ir_file ".ll" in
  let exec_file = gen_new_file exec_dir ir_file ".exe" in
  let out_file = gen_new_file out_dir ir_file ".out" in
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
