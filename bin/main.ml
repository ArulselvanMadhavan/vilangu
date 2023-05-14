let () =
  let open Tlang in
  let filename = "examples/simple.tlang" in
  let in_ch = open_in filename in
  Error_msg.set_filename filename;
  let content = really_input_string in_ch (in_channel_length in_ch) in
  let lexbuf = Lexing.from_string ~with_positions:true content in
  let default = Ast.{ main_decl = []; classdecs = [] } in
  let parsed_exp =
    try Parser.prog Lexer.token lexbuf with
    | Lexer.Error (_pos, msg) ->
      Printf.printf "Error at %d. %s" 0 msg;
      default
    | Parser.Error ->
      Printf.printf "Parser Error occured";
      default
    | _ ->
      Printf.printf "Unexpected error";
      default
  in
  Ast.sexp_of_comp_unit parsed_exp |> Sexplib0.Sexp.to_string_hum |> Stdlib.print_string
;;
