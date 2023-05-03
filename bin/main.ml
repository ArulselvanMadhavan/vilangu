let tlang_prog =
  {| 
int main()
{
  int x[];
  int i;
  int sum;

  // create the array
  x = new int[10];

  // initialize x[i] = i
  i = 0;
  while (i < 10)
  {
    x[i] = i;
    i = i + 1;
  }

  // sum x[i] for i = 0 to 9
  i = 0;
  sum = 0;
  while (i < 10)
  {
    sum = sum + x[i];
    i = i + 1;
  }

  // output the sum
  out sum;
}
|}
;;

let () =
  let open Tlang in
  let lexbuf = Lexing.from_string tlang_prog in
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
