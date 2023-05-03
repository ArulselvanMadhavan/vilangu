open Base

type loc = int * int
and span = loc * loc [@@deriving sexp]

type pos = span [@@deriving sexp]
type symbol = Symbol.symbol [@@deriving sexp]

type comp_unit = MainFunc of stmt list

and stmt =
  | VariableDecl of
      { type_ : type_
      ; ids : symbol list
      }
(* | MultiVarDecl of { type_ : type_; ids: symbol list } *)

and type_ = IntType [@@deriving sexp]
