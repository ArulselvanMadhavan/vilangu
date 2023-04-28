(* AST inspired from https://github.com/ksrky/tiger/blob/master/lib/absyn.ml *)
open Base
type loc = int * int
and span = loc * loc [@@deriving sexp]
    
type pos = span [@@deriving sexp]

type exp =
  | IntExp of int
  | OpExp of {left:exp; oper: oper; right: exp; pos: pos}
  | NilExp
[@@deriving sexp]

and oper = PlusOp
