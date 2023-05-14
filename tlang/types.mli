type unique = unit ref

type ty =
  | INT
  | UNIT
  | NULL
  | NAME of Symbol.symbol * ty option ref

val type2str : ty -> string
