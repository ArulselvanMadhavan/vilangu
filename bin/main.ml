let tlang_prog =
  {| 
class Animal
{
  // all animals must speak
  int speak()
  {
    // abstract method: should not be called
    out -1;
    return 0;
  }
}

class Pig extends Animal
{
  int speak()
  {
    out 1999;
    return 1;
  }
}

class Rooster extends Animal
{
  int speak()
  {
    out 1066;
    return 1;
  }
}

class Dog extends Animal
{
  int speak()
  {
    out 1492;
    return 1;
  }
}

class Cow extends Animal
{
  int speak()
  {
    out 1980;
    return 1;
  }
}

int main()
{
  Animal zoo[];
  Animal animal;
  int i;

  // initialize an array and place some animals in it
  zoo = new Animal[10];
  zoo[0] = new Cow();
  zoo[1] = new Dog();
  zoo[2] = new Rooster();
  zoo[3] = new Pig();

  // now iterate over the array and ask each animal to speak 
  i = 0;
  while (!(zoo[i] == null))
  {
    animal = zoo[i];
    animal.speak();

    i = i + 1;
  }
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
