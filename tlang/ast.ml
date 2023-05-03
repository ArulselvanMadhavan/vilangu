open Base

type loc = int * int
and span = loc * loc [@@deriving sexp]

type pos = span [@@deriving sexp]
type symbol = Symbol.symbol [@@deriving sexp]

type comp_unit = MainFunc of stmt list

and variable =
  { type_ : type_
  ; id : symbol
  ; rank : int
  }

and stmt =
  | VariableDecl of variable list
  | Assignment of
      { lhs : symbol
      ; exp : exp
      }
  | While of
      { exp : exp
      ; block : stmt list
      }
  | Output of exp

and exp =
  | Identifier of symbol
  | IntLit of int
  | OpExp of
      { left : exp
      ; oper : oper
      ; right : exp
      }
  | NullLit

and oper =
  | LT
  | GT
  | PLUS
  | MULT

and type_ = IntType [@@deriving sexp]
