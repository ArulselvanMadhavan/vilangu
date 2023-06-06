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

let type_match t1 t2 =
  match t1, t2 with
  | INT, INT -> true
  | UNIT, UNIT -> true
  | NULL, NULL -> true
  | NAME ((_, id1), _), NAME ((_, id2), _) -> Int.(id1 = id2)
  | _, _ -> false
;;
