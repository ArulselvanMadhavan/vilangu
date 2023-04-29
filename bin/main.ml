let () =
  let open Tlang in
  let lexbuf = Lexing.from_string "1231 + 32 + 4" in
  let parsed_exp =
    try Parser.prog Lexer.token lexbuf with
    | Lexer.Error (_pos, msg) ->
      Printf.printf "Error at %d. %s" 0 msg;
      Ast.NilExp
    | Parser.Error ->
      Printf.printf "Parser Error occured";
      Ast.NilExp
    | _ ->
      Printf.printf "Unexpected error";
      Ast.NilExp
  in
  Ast.sexp_of_exp parsed_exp |> Sexplib0.Sexp.to_string_hum |> Stdlib.print_string
;;
