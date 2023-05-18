(** frontend.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_expr_p_function_app : Frontend_types.expr_p_function_app -> Pbrt.Encoder.t -> unit
(** [encode_expr_p_function_app v encoder] encodes [v] with the given [encoder] *)

val encode_expr : Frontend_types.expr -> Pbrt.Encoder.t -> unit
(** [encode_expr v encoder] encodes [v] with the given [encoder] *)

val encode_program : Frontend_types.program -> Pbrt.Encoder.t -> unit
(** [encode_program v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_expr_p_function_app : Pbrt.Decoder.t -> Frontend_types.expr_p_function_app
(** [decode_expr_p_function_app decoder] decodes a [expr_p_function_app] value from [decoder] *)

val decode_expr : Pbrt.Decoder.t -> Frontend_types.expr
(** [decode_expr decoder] decodes a [expr] value from [decoder] *)

val decode_program : Pbrt.Decoder.t -> Frontend_types.program
(** [decode_program decoder] decodes a [program] value from [decoder] *)
