(** frontend.proto Types *)

(** {2 Types} *)

type un_op =
  | Not
  | Neg

type bin_op = Plus

type expr_p_function_app =
  { name : string
  ; args : expr list
  }

and expr =
  | Integer of int32
  | Function_app of expr_p_function_app
  | Printf of expr_p_printf
  | Unop of expr_p_unop
  | Binop of expr_p_binop

and expr_p_printf =
  { format : string
  ; f_args : expr list
  }

and expr_p_unop =
  { op : un_op
  ; uexpr : expr
  }

and expr_p_binop =
  { bin_op : bin_op
  ; lexpr : expr
  ; rexpr : expr
  }

type program = { main : expr list }

(** {2 Default values} *)

(** [default_un_op ()] is the default value for type [un_op] *)
val default_un_op : unit -> un_op

(** [default_bin_op ()] is the default value for type [bin_op] *)
val default_bin_op : unit -> bin_op

(** [default_expr_p_function_app ()] is the default value for type [expr_p_function_app] *)
val default_expr_p_function_app
  :  ?name:string
  -> ?args:expr list
  -> unit
  -> expr_p_function_app

(** [default_expr ()] is the default value for type [expr] *)
val default_expr : unit -> expr

(** [default_expr_p_printf ()] is the default value for type [expr_p_printf] *)
val default_expr_p_printf : ?format:string -> ?f_args:expr list -> unit -> expr_p_printf

(** [default_expr_p_unop ()] is the default value for type [expr_p_unop] *)
val default_expr_p_unop : ?op:un_op -> ?uexpr:expr -> unit -> expr_p_unop

(** [default_expr_p_binop ()] is the default value for type [expr_p_binop] *)
val default_expr_p_binop
  :  ?bin_op:bin_op
  -> ?lexpr:expr
  -> ?rexpr:expr
  -> unit
  -> expr_p_binop

(** [default_program ()] is the default value for type [program] *)
val default_program : ?main:expr list -> unit -> program
