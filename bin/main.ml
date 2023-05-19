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
  (try Sys.mkdir (Fpath.to_string ir_path) 0o777 with Sys_error _ -> ());
  let ir_path = Fpath.add_seg ir_path (Fpath.filename filename) in
  let ir_path = Fpath.(( -+ ) ir_path "ir") |> Fpath.to_string in
  ir_path
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
  Option.iter (fun pe -> Ir_gen.gen_prog pe |> Ir_gen.dump (ir_dir filename)) parsed_exp
;;

let () = Array.iter compile_file list_dir
