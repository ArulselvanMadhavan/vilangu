(** frontend.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_un_op : Frontend_types.un_op -> Pbrt.Encoder.t -> unit
(** [encode_un_op v encoder] encodes [v] with the given [encoder] *)

val encode_bin_op : Frontend_types.bin_op -> Pbrt.Encoder.t -> unit
(** [encode_bin_op v encoder] encodes [v] with the given [encoder] *)

val encode_var_p_simple : Frontend_types.var_p_simple -> Pbrt.Encoder.t -> unit
(** [encode_var_p_simple v encoder] encodes [v] with the given [encoder] *)

val encode_type_expr_p_class : Frontend_types.type_expr_p_class -> Pbrt.Encoder.t -> unit
(** [encode_type_expr_p_class v encoder] encodes [v] with the given [encoder] *)

val encode_type_expr : Frontend_types.type_expr -> Pbrt.Encoder.t -> unit
(** [encode_type_expr v encoder] encodes [v] with the given [encoder] *)

val encode_type_expr_p_pointer : Frontend_types.type_expr_p_pointer -> Pbrt.Encoder.t -> unit
(** [encode_type_expr_p_pointer v encoder] encodes [v] with the given [encoder] *)

val encode_expr_p_cast : Frontend_types.expr_p_cast -> Pbrt.Encoder.t -> unit
(** [encode_expr_p_cast v encoder] encodes [v] with the given [encoder] *)

val encode_var_p_subscript : Frontend_types.var_p_subscript -> Pbrt.Encoder.t -> unit
(** [encode_var_p_subscript v encoder] encodes [v] with the given [encoder] *)

val encode_var : Frontend_types.var -> Pbrt.Encoder.t -> unit
(** [encode_var v encoder] encodes [v] with the given [encoder] *)

val encode_var_p_field : Frontend_types.var_p_field -> Pbrt.Encoder.t -> unit
(** [encode_var_p_field v encoder] encodes [v] with the given [encoder] *)

val encode_expr : Frontend_types.expr -> Pbrt.Encoder.t -> unit
(** [encode_expr v encoder] encodes [v] with the given [encoder] *)

val encode_expr_p_function_app : Frontend_types.expr_p_function_app -> Pbrt.Encoder.t -> unit
(** [encode_expr_p_function_app v encoder] encodes [v] with the given [encoder] *)

val encode_expr_p_unop : Frontend_types.expr_p_unop -> Pbrt.Encoder.t -> unit
(** [encode_expr_p_unop v encoder] encodes [v] with the given [encoder] *)

val encode_expr_p_binop : Frontend_types.expr_p_binop -> Pbrt.Encoder.t -> unit
(** [encode_expr_p_binop v encoder] encodes [v] with the given [encoder] *)

val encode_expr_p_assign : Frontend_types.expr_p_assign -> Pbrt.Encoder.t -> unit
(** [encode_expr_p_assign v encoder] encodes [v] with the given [encoder] *)

val encode_identifier : Frontend_types.identifier -> Pbrt.Encoder.t -> unit
(** [encode_identifier v encoder] encodes [v] with the given [encoder] *)

val encode_expr_p_array_creation : Frontend_types.expr_p_array_creation -> Pbrt.Encoder.t -> unit
(** [encode_expr_p_array_creation v encoder] encodes [v] with the given [encoder] *)

val encode_expr_p_cast_expr : Frontend_types.expr_p_cast_expr -> Pbrt.Encoder.t -> unit
(** [encode_expr_p_cast_expr v encoder] encodes [v] with the given [encoder] *)

val encode_expr_p_class_creation : Frontend_types.expr_p_class_creation -> Pbrt.Encoder.t -> unit
(** [encode_expr_p_class_creation v encoder] encodes [v] with the given [encoder] *)

val encode_var_p_load : Frontend_types.var_p_load -> Pbrt.Encoder.t -> unit
(** [encode_var_p_load v encoder] encodes [v] with the given [encoder] *)

val encode_stmt_p_var_decl : Frontend_types.stmt_p_var_decl -> Pbrt.Encoder.t -> unit
(** [encode_stmt_p_var_decl v encoder] encodes [v] with the given [encoder] *)

val encode_stmt_p_printf : Frontend_types.stmt_p_printf -> Pbrt.Encoder.t -> unit
(** [encode_stmt_p_printf v encoder] encodes [v] with the given [encoder] *)

val encode_stmt_p_expr_stmt : Frontend_types.stmt_p_expr_stmt -> Pbrt.Encoder.t -> unit
(** [encode_stmt_p_expr_stmt v encoder] encodes [v] with the given [encoder] *)

val encode_stmt_p_delete : Frontend_types.stmt_p_delete -> Pbrt.Encoder.t -> unit
(** [encode_stmt_p_delete v encoder] encodes [v] with the given [encoder] *)

val encode_stmt_p_while : Frontend_types.stmt_p_while -> Pbrt.Encoder.t -> unit
(** [encode_stmt_p_while v encoder] encodes [v] with the given [encoder] *)

val encode_stmt : Frontend_types.stmt -> Pbrt.Encoder.t -> unit
(** [encode_stmt v encoder] encodes [v] with the given [encoder] *)

val encode_stmt_p_block : Frontend_types.stmt_p_block -> Pbrt.Encoder.t -> unit
(** [encode_stmt_p_block v encoder] encodes [v] with the given [encoder] *)

val encode_stmt_p_if_stmt : Frontend_types.stmt_p_if_stmt -> Pbrt.Encoder.t -> unit
(** [encode_stmt_p_if_stmt v encoder] encodes [v] with the given [encoder] *)

val encode_class_def : Frontend_types.class_def -> Pbrt.Encoder.t -> unit
(** [encode_class_def v encoder] encodes [v] with the given [encoder] *)

val encode_param : Frontend_types.param -> Pbrt.Encoder.t -> unit
(** [encode_param v encoder] encodes [v] with the given [encoder] *)

val encode_function_def : Frontend_types.function_def -> Pbrt.Encoder.t -> unit
(** [encode_function_def v encoder] encodes [v] with the given [encoder] *)

val encode_program : Frontend_types.program -> Pbrt.Encoder.t -> unit
(** [encode_program v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_un_op : Pbrt.Decoder.t -> Frontend_types.un_op
(** [decode_un_op decoder] decodes a [un_op] value from [decoder] *)

val decode_bin_op : Pbrt.Decoder.t -> Frontend_types.bin_op
(** [decode_bin_op decoder] decodes a [bin_op] value from [decoder] *)

val decode_var_p_simple : Pbrt.Decoder.t -> Frontend_types.var_p_simple
(** [decode_var_p_simple decoder] decodes a [var_p_simple] value from [decoder] *)

val decode_type_expr_p_class : Pbrt.Decoder.t -> Frontend_types.type_expr_p_class
(** [decode_type_expr_p_class decoder] decodes a [type_expr_p_class] value from [decoder] *)

val decode_type_expr : Pbrt.Decoder.t -> Frontend_types.type_expr
(** [decode_type_expr decoder] decodes a [type_expr] value from [decoder] *)

val decode_type_expr_p_pointer : Pbrt.Decoder.t -> Frontend_types.type_expr_p_pointer
(** [decode_type_expr_p_pointer decoder] decodes a [type_expr_p_pointer] value from [decoder] *)

val decode_expr_p_cast : Pbrt.Decoder.t -> Frontend_types.expr_p_cast
(** [decode_expr_p_cast decoder] decodes a [expr_p_cast] value from [decoder] *)

val decode_var_p_subscript : Pbrt.Decoder.t -> Frontend_types.var_p_subscript
(** [decode_var_p_subscript decoder] decodes a [var_p_subscript] value from [decoder] *)

val decode_var : Pbrt.Decoder.t -> Frontend_types.var
(** [decode_var decoder] decodes a [var] value from [decoder] *)

val decode_var_p_field : Pbrt.Decoder.t -> Frontend_types.var_p_field
(** [decode_var_p_field decoder] decodes a [var_p_field] value from [decoder] *)

val decode_expr : Pbrt.Decoder.t -> Frontend_types.expr
(** [decode_expr decoder] decodes a [expr] value from [decoder] *)

val decode_expr_p_function_app : Pbrt.Decoder.t -> Frontend_types.expr_p_function_app
(** [decode_expr_p_function_app decoder] decodes a [expr_p_function_app] value from [decoder] *)

val decode_expr_p_unop : Pbrt.Decoder.t -> Frontend_types.expr_p_unop
(** [decode_expr_p_unop decoder] decodes a [expr_p_unop] value from [decoder] *)

val decode_expr_p_binop : Pbrt.Decoder.t -> Frontend_types.expr_p_binop
(** [decode_expr_p_binop decoder] decodes a [expr_p_binop] value from [decoder] *)

val decode_expr_p_assign : Pbrt.Decoder.t -> Frontend_types.expr_p_assign
(** [decode_expr_p_assign decoder] decodes a [expr_p_assign] value from [decoder] *)

val decode_identifier : Pbrt.Decoder.t -> Frontend_types.identifier
(** [decode_identifier decoder] decodes a [identifier] value from [decoder] *)

val decode_expr_p_array_creation : Pbrt.Decoder.t -> Frontend_types.expr_p_array_creation
(** [decode_expr_p_array_creation decoder] decodes a [expr_p_array_creation] value from [decoder] *)

val decode_expr_p_cast_expr : Pbrt.Decoder.t -> Frontend_types.expr_p_cast_expr
(** [decode_expr_p_cast_expr decoder] decodes a [expr_p_cast_expr] value from [decoder] *)

val decode_expr_p_class_creation : Pbrt.Decoder.t -> Frontend_types.expr_p_class_creation
(** [decode_expr_p_class_creation decoder] decodes a [expr_p_class_creation] value from [decoder] *)

val decode_var_p_load : Pbrt.Decoder.t -> Frontend_types.var_p_load
(** [decode_var_p_load decoder] decodes a [var_p_load] value from [decoder] *)

val decode_stmt_p_var_decl : Pbrt.Decoder.t -> Frontend_types.stmt_p_var_decl
(** [decode_stmt_p_var_decl decoder] decodes a [stmt_p_var_decl] value from [decoder] *)

val decode_stmt_p_printf : Pbrt.Decoder.t -> Frontend_types.stmt_p_printf
(** [decode_stmt_p_printf decoder] decodes a [stmt_p_printf] value from [decoder] *)

val decode_stmt_p_expr_stmt : Pbrt.Decoder.t -> Frontend_types.stmt_p_expr_stmt
(** [decode_stmt_p_expr_stmt decoder] decodes a [stmt_p_expr_stmt] value from [decoder] *)

val decode_stmt_p_delete : Pbrt.Decoder.t -> Frontend_types.stmt_p_delete
(** [decode_stmt_p_delete decoder] decodes a [stmt_p_delete] value from [decoder] *)

val decode_stmt_p_while : Pbrt.Decoder.t -> Frontend_types.stmt_p_while
(** [decode_stmt_p_while decoder] decodes a [stmt_p_while] value from [decoder] *)

val decode_stmt : Pbrt.Decoder.t -> Frontend_types.stmt
(** [decode_stmt decoder] decodes a [stmt] value from [decoder] *)

val decode_stmt_p_block : Pbrt.Decoder.t -> Frontend_types.stmt_p_block
(** [decode_stmt_p_block decoder] decodes a [stmt_p_block] value from [decoder] *)

val decode_stmt_p_if_stmt : Pbrt.Decoder.t -> Frontend_types.stmt_p_if_stmt
(** [decode_stmt_p_if_stmt decoder] decodes a [stmt_p_if_stmt] value from [decoder] *)

val decode_class_def : Pbrt.Decoder.t -> Frontend_types.class_def
(** [decode_class_def decoder] decodes a [class_def] value from [decoder] *)

val decode_param : Pbrt.Decoder.t -> Frontend_types.param
(** [decode_param decoder] decodes a [param] value from [decoder] *)

val decode_function_def : Pbrt.Decoder.t -> Frontend_types.function_def
(** [decode_function_def decoder] decodes a [function_def] value from [decoder] *)

val decode_program : Pbrt.Decoder.t -> Frontend_types.program
(** [decode_program decoder] decodes a [program] value from [decoder] *)
