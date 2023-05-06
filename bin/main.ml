let tlang_prog =
  {| 
class A {
  int f(int i) { return 1; }
  int f(A a) { return 5-2; }
}

class B extends A {
  int f(int i, int j) { return i + j; }
}

int main()
{
  A a;
  B b;

  a = new A();
  b = new B();

  out b.f(19) + 14;
  out b.f(a) + 13;
  out b.f(b) + 13;
  out b.f(7, 8);
}
|}
;;

let () =
  let open Tlang in
  let lexbuf = Lexing.from_string tlang_prog in
  let default = Ast.{ main_decl = Block []; classdecs = [] } in
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
