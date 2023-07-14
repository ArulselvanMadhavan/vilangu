open Cmdliner

exception NotAValidFileException of string
exception UnsupportedFileExtension
exception InvalidDirectory

let gen_new_file dir base_file ext = Fpath.(add_seg dir (filename base_file) -+ ext)

let log_err out_dir filename =
  let err_file = gen_new_file out_dir filename ".err" in
  Tlang.Error_msg.log_err (Fpath.to_string err_file)
;;

let to_opt out_dir filename a =
  let open Tlang in
  if Error_msg.has_errors ()
  then (
    log_err out_dir filename;
    None)
  else Some a
;;

let check_types parsed_ast =
  let open Tlang in
  Semant.trans_prog parsed_ast
;;

let compile_file out_dir filename =
  let open Tlang in
  let open Base.Option.Let_syntax in
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
      log_err out_dir filename;
      None
    | Parser.Error ->
      err_str := "Parser Error occured";
      Error_msg.error ((-1, -1), (-1, -1)) !err_str;
      log_err out_dir filename;
      None
    | _ ->
      err_str := "Unexpected error";
      Error_msg.error ((-1, -1), (-1, -1)) !err_str;
      log_err out_dir filename;
      None
  in
  let%bind parsed_ast = parsed_exp in
  let%map venv, tenv, parsed_ast = check_types parsed_ast |> to_opt out_dir filename in
  (* Sexplib0.Sexp.to_string_hum (Ast.sexp_of_comp_unit parsed_ast) |> Printf.printf "%s\n";   *)
  let ir_file = gen_new_file out_dir filename ".ir" in
  let ast_file = gen_new_file out_dir filename ".ast" in
  Ir_gen.gen_prog (venv, tenv, parsed_ast)
  |> Ir_gen.dump (Fpath.to_string ast_file) (Fpath.to_string ir_file);
  ir_file
;;

let clang_command llvm_file out_file =
  "clang -O3 " ^ Fpath.to_string llvm_file ^ " -o " ^ Fpath.to_string out_file
;;

let run_exec exec_file out_file =
  "./" ^ Fpath.to_string exec_file ^ " > " ^ Fpath.to_string out_file
;;

let ir_to_backend backend_exec out_dir ir_file =
  let llvm_ir_file = gen_new_file out_dir ir_file ".ll" in
  let exec_file = gen_new_file out_dir ir_file ".exe" in
  let out_file = gen_new_file out_dir ir_file ".out" in
  let cmd =
    Fpath.to_string backend_exec
    ^ " "
    ^ Fpath.to_string ir_file
    ^ " > "
    ^ Fpath.to_string llvm_ir_file
  in
  let code = Sys.command cmd in
  let cmd = clang_command llvm_ir_file exec_file in
  let code = if code = 0 then Sys.command cmd else -1 in
  let cmd = run_exec exec_file out_file in
  if code = 0 then Sys.command cmd else -1
;;

let source_file_t =
  let doc = ".t file to compile" in
  Arg.(value & pos 0 string "<file_to_compile>.t" & info [] ~docv:"SOURCE" ~doc)
;;

let out_dir_t =
  let doc = "directory for the outputs generated by the compiler" in
  Arg.(value & opt string "." & info [ "o"; "out_dir" ] ~docv:"OUTPUT_DIR" ~doc)
;;

let backend_exec_t =
  let doc = "backend executable path" in
  Arg.(
    value & pos 1 string "backend/build/tools/driver/tlang" & info [] ~docv:"BACKEND" ~doc)
;;

let validate_fpath fname =
  match Fpath.of_string fname with
  | Ok fpath -> fpath
  | Error (`Msg s) -> raise (NotAValidFileException s)
;;

let validate_source_file fname =
  let fpath = validate_fpath fname in
  let is_t = Fpath.has_ext ".t" fpath in
  if is_t then fpath else raise UnsupportedFileExtension
;;

let validate_dir fname =
  let fpath = validate_fpath fname in
  if Fpath.is_dir_path fpath then fpath else raise InvalidDirectory
;;

let compiler backend_exec out_dir source_file =
  let source_file = validate_source_file source_file in
  let out_dir = validate_dir out_dir in
  let backend_exec = validate_fpath backend_exec in
  let compile_file = compile_file out_dir in
  let ir_to_backend = ir_to_backend backend_exec out_dir in
  let ir_files = Array.map compile_file [| source_file |] in
  let ir_files = Base.Array.filter_opt ir_files in
  let _out_codes = Array.map ir_to_backend ir_files in
  ()
;;

let compiler_t = Term.(const compiler $ backend_exec_t $ out_dir_t $ source_file_t)

let cmd =
  let doc = "compiler for T-lang" in
  let man =
    [ `S Manpage.s_bugs; `P "Email bug reports to <arulselvan1234@gmail.com>." ]
  in
  let info = Cmd.info "vilangu" ~version:"%‌%VERSION%%" ~doc ~man in
  Cmd.v info compiler_t
;;

let () = exit (Cmd.eval cmd)
