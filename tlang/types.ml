open Base

type unique = unit ref

type ty =
  | INT
  | UNIT
  | NULL
  | NAME of Symbol.symbol * ty option ref
[@@deriving sexp]

let type2str = function
  | NULL -> "NULL"
  | INT -> "int"
  | NAME (id, _) -> Symbol.name id
  | UNIT -> "unit"
;;
