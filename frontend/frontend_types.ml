[@@@ocaml.warning "-27-30-39"]


type expr_p_function_app = {
  name : string;
  args : expr list;
}

and expr =
  | Integer of int64
  | Function_app of expr_p_function_app

type program = {
  main : expr list;
}

let rec default_expr_p_function_app 
  ?name:((name:string) = "")
  ?args:((args:expr list) = [])
  () : expr_p_function_app  = {
  name;
  args;
}

and default_expr () : expr = Integer (0L)

let rec default_program 
  ?main:((main:expr list) = [])
  () : program  = {
  main;
}
