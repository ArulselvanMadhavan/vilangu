(** frontend.proto Pretty Printing *)

(** {2 Formatters} *)

(** [pp_un_op v] formats v *)
val pp_un_op : Format.formatter -> Frontend_types.un_op -> unit

(** [pp_bin_op v] formats v *)
val pp_bin_op : Format.formatter -> Frontend_types.bin_op -> unit

(** [pp_var_p_simple v] formats v *)
val pp_var_p_simple : Format.formatter -> Frontend_types.var_p_simple -> unit

(** [pp_type_expr_p_class v] formats v *)
val pp_type_expr_p_class : Format.formatter -> Frontend_types.type_expr_p_class -> unit

(** [pp_type_expr v] formats v *)
val pp_type_expr : Format.formatter -> Frontend_types.type_expr -> unit

(** [pp_type_expr_p_pointer v] formats v *)
val pp_type_expr_p_pointer
  :  Format.formatter
  -> Frontend_types.type_expr_p_pointer
  -> unit

(** [pp_expr_p_cast v] formats v *)
val pp_expr_p_cast : Format.formatter -> Frontend_types.expr_p_cast -> unit

(** [pp_var_p_subscript v] formats v *)
val pp_var_p_subscript : Format.formatter -> Frontend_types.var_p_subscript -> unit

(** [pp_var v] formats v *)
val pp_var : Format.formatter -> Frontend_types.var -> unit

(** [pp_var_p_field v] formats v *)
val pp_var_p_field : Format.formatter -> Frontend_types.var_p_field -> unit

(** [pp_expr v] formats v *)
val pp_expr : Format.formatter -> Frontend_types.expr -> unit

(** [pp_expr_p_function_app v] formats v *)
val pp_expr_p_function_app
  :  Format.formatter
  -> Frontend_types.expr_p_function_app
  -> unit

(** [pp_expr_p_unop v] formats v *)
val pp_expr_p_unop : Format.formatter -> Frontend_types.expr_p_unop -> unit

(** [pp_expr_p_binop v] formats v *)
val pp_expr_p_binop : Format.formatter -> Frontend_types.expr_p_binop -> unit

(** [pp_expr_p_assign v] formats v *)
val pp_expr_p_assign : Format.formatter -> Frontend_types.expr_p_assign -> unit

(** [pp_identifier v] formats v *)
val pp_identifier : Format.formatter -> Frontend_types.identifier -> unit

(** [pp_expr_p_array_creation v] formats v *)
val pp_expr_p_array_creation
  :  Format.formatter
  -> Frontend_types.expr_p_array_creation
  -> unit

(** [pp_expr_p_cast_expr v] formats v *)
val pp_expr_p_cast_expr : Format.formatter -> Frontend_types.expr_p_cast_expr -> unit

(** [pp_expr_p_class_creation v] formats v *)
val pp_expr_p_class_creation
  :  Format.formatter
  -> Frontend_types.expr_p_class_creation
  -> unit

(** [pp_expr_p_method_call v] formats v *)
val pp_expr_p_method_call : Format.formatter -> Frontend_types.expr_p_method_call -> unit

(** [pp_var_p_load v] formats v *)
val pp_var_p_load : Format.formatter -> Frontend_types.var_p_load -> unit

(** [pp_stmt_p_var_decl v] formats v *)
val pp_stmt_p_var_decl : Format.formatter -> Frontend_types.stmt_p_var_decl -> unit

(** [pp_stmt_p_printf v] formats v *)
val pp_stmt_p_printf : Format.formatter -> Frontend_types.stmt_p_printf -> unit

(** [pp_stmt_p_expr_stmt v] formats v *)
val pp_stmt_p_expr_stmt : Format.formatter -> Frontend_types.stmt_p_expr_stmt -> unit

(** [pp_stmt_p_delete v] formats v *)
val pp_stmt_p_delete : Format.formatter -> Frontend_types.stmt_p_delete -> unit

(** [pp_stmt_p_free v] formats v *)
val pp_stmt_p_free : Format.formatter -> Frontend_types.stmt_p_free -> unit

(** [pp_stmt_p_while v] formats v *)
val pp_stmt_p_while : Format.formatter -> Frontend_types.stmt_p_while -> unit

(** [pp_stmt v] formats v *)
val pp_stmt : Format.formatter -> Frontend_types.stmt -> unit

(** [pp_stmt_p_block v] formats v *)
val pp_stmt_p_block : Format.formatter -> Frontend_types.stmt_p_block -> unit

(** [pp_stmt_p_if_stmt v] formats v *)
val pp_stmt_p_if_stmt : Format.formatter -> Frontend_types.stmt_p_if_stmt -> unit

(** [pp_class_def v] formats v *)
val pp_class_def : Format.formatter -> Frontend_types.class_def -> unit

(** [pp_param v] formats v *)
val pp_param : Format.formatter -> Frontend_types.param -> unit

(** [pp_function_def v] formats v *)
val pp_function_def : Format.formatter -> Frontend_types.function_def -> unit

(** [pp_program v] formats v *)
val pp_program : Format.formatter -> Frontend_types.program -> unit
