(** frontend.proto Types *)

(** {2 Types} *)

type un_op =
  | Not
  | Neg

type bin_op = Plus
type identifier_p_var = { var_name : string }
type identifier = Var of identifier_p_var
type expr_p_var_decl = { var_id : string }

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
  | Var_decl of expr_p_var_decl
  | Assign of expr_p_assign

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

and expr_p_assign =
  { lhs : identifier
  ; rhs : expr
  }

type program = { main : expr list }

(** {2 Default values} *)

(** [default_un_op ()] is the default value for type [un_op] *)
val default_un_op : unit -> un_op

(** [default_bin_op ()] is the default value for type [bin_op] *)
val default_bin_op : unit -> bin_op

(** [default_identifier_p_var ()] is the default value for type [identifier_p_var] *)
val default_identifier_p_var : ?var_name:string -> unit -> identifier_p_var

(** [default_identifier ()] is the default value for type [identifier] *)
val default_identifier : unit -> identifier

(** [default_expr_p_var_decl ()] is the default value for type [expr_p_var_decl] *)
val default_expr_p_var_decl : ?var_id:string -> unit -> expr_p_var_decl

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

(** [default_expr_p_assign ()] is the default value for type [expr_p_assign] *)
val default_expr_p_assign : ?lhs:identifier -> ?rhs:expr -> unit -> expr_p_assign

(** [default_program ()] is the default value for type [program] *)
val default_program : ?main:expr list -> unit -> program
