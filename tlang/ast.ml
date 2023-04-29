open Base

type loc = int * int
and span = loc * loc [@@deriving sexp]

type pos = span [@@deriving sexp]

type exp =
  | IntExp of int
  | OpExp of
      { left : exp
      ; oper : oper
      ; right : exp
      ; pos : pos
      }
  | NilExp (* Is this needed? *)
[@@deriving sexp]

and oper =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
[@@deriving sexp]
