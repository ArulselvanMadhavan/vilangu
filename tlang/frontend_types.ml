[@@@ocaml.warning "-27-30-39"]

type expr_p_function_app =
  { name : string
  ; block : expr list
  }

and expr =
  | Integer of int
  | Function_app of expr_p_function_app

type program = { main : expr list }

let rec default_expr_p_function_app ?(name : string = "") ?(block : expr list = []) ()
  : expr_p_function_app
  =
  { name; block }

and default_expr () : expr = Integer 0

let rec default_program ?(main : expr list = []) () : program = { main }
