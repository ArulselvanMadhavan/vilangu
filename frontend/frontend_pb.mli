(** frontend.proto Binary Encoding *)

(** {2 Protobuf Encoding} *)

(** [encode_un_op v encoder] encodes [v] with the given [encoder] *)
val encode_un_op : Frontend_types.un_op -> Pbrt.Encoder.t -> unit

(** [encode_bin_op v encoder] encodes [v] with the given [encoder] *)
val encode_bin_op : Frontend_types.bin_op -> Pbrt.Encoder.t -> unit

(** [encode_expr_p_function_app v encoder] encodes [v] with the given [encoder] *)
val encode_expr_p_function_app
  :  Frontend_types.expr_p_function_app
  -> Pbrt.Encoder.t
  -> unit

(** [encode_expr v encoder] encodes [v] with the given [encoder] *)
val encode_expr : Frontend_types.expr -> Pbrt.Encoder.t -> unit

(** [encode_expr_p_printf v encoder] encodes [v] with the given [encoder] *)
val encode_expr_p_printf : Frontend_types.expr_p_printf -> Pbrt.Encoder.t -> unit

(** [encode_expr_p_unop v encoder] encodes [v] with the given [encoder] *)
val encode_expr_p_unop : Frontend_types.expr_p_unop -> Pbrt.Encoder.t -> unit

(** [encode_expr_p_binop v encoder] encodes [v] with the given [encoder] *)
val encode_expr_p_binop : Frontend_types.expr_p_binop -> Pbrt.Encoder.t -> unit

(** [encode_program v encoder] encodes [v] with the given [encoder] *)
val encode_program : Frontend_types.program -> Pbrt.Encoder.t -> unit

(** {2 Protobuf Decoding} *)

(** [decode_un_op decoder] decodes a [un_op] value from [decoder] *)
val decode_un_op : Pbrt.Decoder.t -> Frontend_types.un_op

(** [decode_bin_op decoder] decodes a [bin_op] value from [decoder] *)
val decode_bin_op : Pbrt.Decoder.t -> Frontend_types.bin_op

(** [decode_expr_p_function_app decoder] decodes a [expr_p_function_app] value from [decoder] *)
val decode_expr_p_function_app : Pbrt.Decoder.t -> Frontend_types.expr_p_function_app

(** [decode_expr decoder] decodes a [expr] value from [decoder] *)
val decode_expr : Pbrt.Decoder.t -> Frontend_types.expr

(** [decode_expr_p_printf decoder] decodes a [expr_p_printf] value from [decoder] *)
val decode_expr_p_printf : Pbrt.Decoder.t -> Frontend_types.expr_p_printf

(** [decode_expr_p_unop decoder] decodes a [expr_p_unop] value from [decoder] *)
val decode_expr_p_unop : Pbrt.Decoder.t -> Frontend_types.expr_p_unop

(** [decode_expr_p_binop decoder] decodes a [expr_p_binop] value from [decoder] *)
val decode_expr_p_binop : Pbrt.Decoder.t -> Frontend_types.expr_p_binop

(** [decode_program decoder] decodes a [program] value from [decoder] *)
val decode_program : Pbrt.Decoder.t -> Frontend_types.program
