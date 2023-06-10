open Base
module FT = Frontend_types

type unique = unit ref

type ty =
  | INT
  | VOID
  | NULL
  | NAME of Symbol.symbol * ty option ref
  | ARRAY of int * ty
[@@deriving sexp]

val type2str : ty -> string
val type_match : ty -> ty -> bool
val gen_type_expr : ty -> FT.type_expr
val is_int : ty -> bool
