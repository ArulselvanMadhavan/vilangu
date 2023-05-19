[@@@ocaml.warning "-27-30-39"]


type expr_p_function_app = {
  name : string;
  args : expr list;
}

and expr =
  | Integer of int32
  | Function_app of expr_p_function_app
  | Printf of expr_p_printf

and expr_p_printf = {
  format : string;
  f_args : expr list;
}

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

and default_expr () : expr = Integer (0l)

and default_expr_p_printf 
  ?format:((format:string) = "")
  ?f_args:((f_args:expr list) = [])
  () : expr_p_printf  = {
  format;
  f_args;
}

let rec default_program 
  ?main:((main:expr list) = [])
  () : program  = {
  main;
}
