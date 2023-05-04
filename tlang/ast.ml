open Base

type loc = int * int
and span = loc * loc [@@deriving sexp]

type pos = span [@@deriving sexp]
type symbol = Symbol.symbol [@@deriving sexp]

type comp_unit =
  | ClassDecs of (classdec list)
  | MainFunc of stmt list

and classdec = { name: symbol; base: symbol option}
  
and var =
  | SimpleVar of symbol
  | SubscriptVar of var * exp

and variable =
  { type_ : type_
  ; id : symbol
  ; rank : int
  }

and stmt =
  | VariableDecl of variable list
  | Assignment of
      { lhs : var
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
  | ArrayExp of
      { type_ : type_
      ; exprs : exp list
      }
  | VarExp of var
  | NullLit

and oper =
  | LT
  | GT
  | PLUS
  | MULT
  | EQUALS

and type_ = IntType [@@deriving sexp]
