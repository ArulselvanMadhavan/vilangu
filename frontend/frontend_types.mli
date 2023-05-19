(** frontend.proto Types *)



(** {2 Types} *)

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


(** {2 Default values} *)

val default_expr_p_function_app : 
  ?name:string ->
  ?args:expr list ->
  unit ->
  expr_p_function_app
(** [default_expr_p_function_app ()] is the default value for type [expr_p_function_app] *)

val default_expr : unit -> expr
(** [default_expr ()] is the default value for type [expr] *)

val default_expr_p_printf : 
  ?format:string ->
  ?f_args:expr list ->
  unit ->
  expr_p_printf
(** [default_expr_p_printf ()] is the default value for type [expr_p_printf] *)

val default_program : 
  ?main:expr list ->
  unit ->
  program
(** [default_program ()] is the default value for type [program] *)
