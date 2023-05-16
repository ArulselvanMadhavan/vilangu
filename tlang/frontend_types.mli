(** frontend.proto Types *)

(** {2 Types} *)

type expr_p_function_app =
  { name : string
  ; block : expr list
  }

and expr =
  | Integer of int
  | Function_app of expr_p_function_app

type program = { main : expr list }

(** {2 Default values} *)

(** [default_expr_p_function_app ()] is the default value for type [expr_p_function_app] *)
val default_expr_p_function_app
  :  ?name:string
  -> ?block:expr list
  -> unit
  -> expr_p_function_app

(** [default_expr ()] is the default value for type [expr] *)
val default_expr : unit -> expr

(** [default_program ()] is the default value for type [program] *)
val default_program : ?main:expr list -> unit -> program
