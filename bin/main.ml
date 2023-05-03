let () =
  let open Tlang in
  (* let lexbuf = Lexing.from_string "( 4124 + 321 * 2)" in *)
  let lexbuf = Lexing.from_string "int main(){ int i, j, k;}" in
  let default = Ast.MainFunc [] in
  (* let default = Ast.NilExp in *)
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
