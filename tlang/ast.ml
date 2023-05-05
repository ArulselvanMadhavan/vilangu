open Base

type loc = int * int
and span = loc * loc [@@deriving sexp]

type pos = span [@@deriving sexp]
type symbol = Symbol.symbol [@@deriving sexp]

type comp_unit =
  | ClassDecs of classdec list
  | MainFunc of stmt list

and classdec =
  | ClassDec of
      { name : symbol
      ; base : symbol option
      ; class_body : class_body list
      }

and class_field =
  | Field of
      { name : symbol
      ; type_ : type_
      ; rank : int
      }

and return_t =
  | Return of
      { type_ : type_
      ; rank : int
      }

and class_body =
  | Constructor of
      { name : symbol
      ; fparams : param list
      ; body : stmt list
      }
  | FieldDec of class_field list
  | Method of
      { name : symbol
      ; return_t : return_t
      ; fparams : param list
      ; body : stmt list
      }
  | Destructor of
      { name : symbol
      ; body : stmt list
      }

and param =
  | Param of
      { name : symbol
      ; typ : type_
      ; rank : int
      }

and var =
  | SimpleVar of symbol
  | SubscriptVar of var * exp
  | FieldVar of var * symbol

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
  | MethodCall of
      { var : var (* ; ty : symbol . How to encode self/this *)
      ; args : exp list
      }
  | ReturnStmt of exp option

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

and type_ =
  | IntType
  | NameTy of symbol
[@@deriving sexp]
