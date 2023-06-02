(** frontend.proto Types *)



(** {2 Types} *)

type un_op =
  | Not
  | Neg

type bin_op =
  | Plus
  | Equals
  | Lessthan

type identifier_p_var = {
  var_name : string;
}

type identifier =
  | Var of identifier_p_var

type type_expr_p_ranked_type = {
  rank : int32;
}

type type_expr =
  | Int32_ty of type_expr_p_ranked_type

type expr_p_var_decl = {
  var_id : string;
  texpr : type_expr;
}

type expr_p_function_app = {
  name : string;
  args : expr list;
}

and expr =
  | Integer of int32
  | Function_app of expr_p_function_app
  | Printf of expr_p_printf
  | Unop of expr_p_unop
  | Binop of expr_p_binop
  | Var_decl of expr_p_var_decl
  | Assign of expr_p_assign
  | Expr_id of identifier
  | If_expr of expr_p_if_expr
  | Block_expr of expr_p_block
  | While_expr of expr_p_while_expr

and expr_p_printf = {
  format : string;
  f_args : expr list;
}

and expr_p_unop = {
  op : un_op;
  uexpr : expr;
}

and expr_p_binop = {
  bin_op : bin_op;
  lexpr : expr;
  rexpr : expr;
}

and expr_p_assign = {
  lhs : identifier;
  rhs : expr;
}

and expr_p_if_expr = {
  eval : expr;
  if_expr : expr;
  else_expr : expr;
}

and expr_p_block = {
  expr_list : expr list;
}

and expr_p_while_expr = {
  while_cond : expr;
  while_block : expr;
}

type program = {
  main : expr list;
}


(** {2 Default values} *)

val default_un_op : unit -> un_op
(** [default_un_op ()] is the default value for type [un_op] *)

val default_bin_op : unit -> bin_op
(** [default_bin_op ()] is the default value for type [bin_op] *)

val default_identifier_p_var : 
  ?var_name:string ->
  unit ->
  identifier_p_var
(** [default_identifier_p_var ()] is the default value for type [identifier_p_var] *)

val default_identifier : unit -> identifier
(** [default_identifier ()] is the default value for type [identifier] *)

val default_type_expr_p_ranked_type : 
  ?rank:int32 ->
  unit ->
  type_expr_p_ranked_type
(** [default_type_expr_p_ranked_type ()] is the default value for type [type_expr_p_ranked_type] *)

val default_type_expr : unit -> type_expr
(** [default_type_expr ()] is the default value for type [type_expr] *)

val default_expr_p_var_decl : 
  ?var_id:string ->
  ?texpr:type_expr ->
  unit ->
  expr_p_var_decl
(** [default_expr_p_var_decl ()] is the default value for type [expr_p_var_decl] *)

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

val default_expr_p_unop : 
  ?op:un_op ->
  ?uexpr:expr ->
  unit ->
  expr_p_unop
(** [default_expr_p_unop ()] is the default value for type [expr_p_unop] *)

val default_expr_p_binop : 
  ?bin_op:bin_op ->
  ?lexpr:expr ->
  ?rexpr:expr ->
  unit ->
  expr_p_binop
(** [default_expr_p_binop ()] is the default value for type [expr_p_binop] *)

val default_expr_p_assign : 
  ?lhs:identifier ->
  ?rhs:expr ->
  unit ->
  expr_p_assign
(** [default_expr_p_assign ()] is the default value for type [expr_p_assign] *)

val default_expr_p_if_expr : 
  ?eval:expr ->
  ?if_expr:expr ->
  ?else_expr:expr ->
  unit ->
  expr_p_if_expr
(** [default_expr_p_if_expr ()] is the default value for type [expr_p_if_expr] *)

val default_expr_p_block : 
  ?expr_list:expr list ->
  unit ->
  expr_p_block
(** [default_expr_p_block ()] is the default value for type [expr_p_block] *)

val default_expr_p_while_expr : 
  ?while_cond:expr ->
  ?while_block:expr ->
  unit ->
  expr_p_while_expr
(** [default_expr_p_while_expr ()] is the default value for type [expr_p_while_expr] *)

val default_program : 
  ?main:expr list ->
  unit ->
  program
(** [default_program ()] is the default value for type [program] *)
