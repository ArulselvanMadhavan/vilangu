open Base

type loc = int * int
and span = loc * loc [@@deriving sexp]

type pos = span [@@deriving sexp]
type symbol = Symbol.symbol [@@deriving sexp]

type comp_unit =
  | ClassDecs of classdec list
  | MainFunc of stmt

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
      ; body : stmt
      }
  | FieldDec of class_field list
  | Method of
      { name : symbol
      ; return_t : return_t
      ; fparams : param list
      ; body : stmt
      }
  | Destructor of
      { name : symbol
      ; body : stmt
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
  | Block of stmt list
  | Assignment of
      { lhs : var
      ; exp : exp
      }
  | While of
      { exp : exp
      ; block : stmt
      }
  | Output of exp
  | ReturnStmt of exp option
  | Empty
  | Break
  | Continue
  | ExprStmt of exp
  | Delete of exp
  | IfElse of
      { exp : exp
      ; istmt : stmt
      ; estmt : stmt
      }

and exp =
  | Identifier of symbol
  | IntLit of int
  | OpExp of
      { left : exp
      ; oper : oper
      ; right : exp
      }
  | ArrayCreationExp of
      { type_ : type_
      ; exprs : exp list
      ; empty_dims : int
      }
  | ClassCreationExp of
      { type_ : type_
      ; args : exp list
      }
  | VarExp of var
  | NullLit
  | This
  | Super
  | MethodCall of
      { base : exp
      ; field : exp option
      ; args : exp list
      }
  | FieldAccess of exp * symbol

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
