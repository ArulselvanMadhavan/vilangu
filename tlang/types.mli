open Base

type unique = unit ref

type ty =
  | INT
  | UNIT
  | NULL
  | NAME of Symbol.symbol * ty option ref
[@@deriving sexp]

val type2str : ty -> string
