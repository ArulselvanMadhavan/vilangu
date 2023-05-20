[@@@ocaml.warning "-27-30-39"]

type un_op =
  | Not
  | Neg

type expr_p_function_app =
  { name : string
  ; args : expr list
  }

and expr =
  | Integer of int32
  | Function_app of expr_p_function_app
  | Printf of expr_p_printf
  | Unop of expr_p_unop

and expr_p_printf =
  { format : string
  ; f_args : expr list
  }

and expr_p_unop =
  { op : un_op
  ; uexpr : expr
  }

type program = { main : expr list }

let rec default_un_op () : un_op = Not

let rec default_expr_p_function_app ?(name : string = "") ?(args : expr list = []) ()
  : expr_p_function_app
  =
  { name; args }

and default_expr () : expr = Integer 0l

and default_expr_p_printf ?(format : string = "") ?(f_args : expr list = []) ()
  : expr_p_printf
  =
  { format; f_args }

and default_expr_p_unop
  ?(op : un_op = default_un_op ())
  ?(uexpr : expr = default_expr ())
  ()
  : expr_p_unop
  =
  { op; uexpr }
;;

let rec default_program ?(main : expr list = []) () : program = { main }
