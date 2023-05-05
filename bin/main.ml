let tlang_prog =
  {| 
class Animal{
  Animal(int x, int[][] y[]) {
    this(4, 5);
    x = x + 1;
  }
    int[] x[],y,z;
  ~Animal(){
  }

  int result(int ff){
    ff = ff + 1;
    return ff;
  }
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
