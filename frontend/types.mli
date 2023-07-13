open Base
module FT = Frontend_types

type ty =
  | INT
  | VOID
  | NULL
  | NAME of
      Symbol.symbol
      * (Symbol.symbol * ty) list
      * Symbol.symbol option (* class_name, field_names list *)
  | ARRAY of int * ty
[@@deriving sexp]

val type2str : ty -> string
val type_match : ty -> ty -> bool
val gen_type_expr : ty -> FT.type_expr
val is_int : ty -> bool
val is_array : ty -> bool
val is_ref : ty -> bool  
