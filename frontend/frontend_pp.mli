(** frontend.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_un_op : Format.formatter -> Frontend_types.un_op -> unit 
(** [pp_un_op v] formats v *)

val pp_bin_op : Format.formatter -> Frontend_types.bin_op -> unit 
(** [pp_bin_op v] formats v *)

val pp_identifier_p_var : Format.formatter -> Frontend_types.identifier_p_var -> unit 
(** [pp_identifier_p_var v] formats v *)

val pp_identifier : Format.formatter -> Frontend_types.identifier -> unit 
(** [pp_identifier v] formats v *)

val pp_expr_p_var_decl : Format.formatter -> Frontend_types.expr_p_var_decl -> unit 
(** [pp_expr_p_var_decl v] formats v *)

val pp_expr_p_function_app : Format.formatter -> Frontend_types.expr_p_function_app -> unit 
(** [pp_expr_p_function_app v] formats v *)

val pp_expr : Format.formatter -> Frontend_types.expr -> unit 
(** [pp_expr v] formats v *)

val pp_expr_p_printf : Format.formatter -> Frontend_types.expr_p_printf -> unit 
(** [pp_expr_p_printf v] formats v *)

val pp_expr_p_unop : Format.formatter -> Frontend_types.expr_p_unop -> unit 
(** [pp_expr_p_unop v] formats v *)

val pp_expr_p_binop : Format.formatter -> Frontend_types.expr_p_binop -> unit 
(** [pp_expr_p_binop v] formats v *)

val pp_program : Format.formatter -> Frontend_types.program -> unit 
(** [pp_program v] formats v *)
