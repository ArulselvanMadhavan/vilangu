open Base

type loc = int * int
and span = loc * loc [@@deriving sexp]

type pos = span [@@deriving sexp]
type symbol = Symbol.symbol [@@deriving sexp]

type comp_unit =
  { main_decl : main list
  ; class_decs : classdec list
  }

and classdec =
  | ClassDec of
      { name : symbol
      ; base : symbol option
      ; class_body : class_body list
      }

and class_field =
  | Field of
      { name : symbol
      ; type_ : type_ (* ; rank : int *)
      }

and return_t = Return of { type_ : type_ (* ; rank : int *) }

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
      ; type_ : type_ (* ; rank : int *)
      }

and var =
  | SimpleVar of symbol * pos
  | SubscriptVar of var * exp * pos
  | FieldVar of exp * symbol * pos
  | LoadVar of var (* used only on lhs *)

and variable =
  { type_ : type_
  ; id : symbol (* ; rank : int *)
  }

and main =
  | VariableDecl of variable list
  | MainStmt of stmt

and stmt =
  | Block of stmt list
  | While of
      { exp : exp
      ; block : stmt
      }
  | Output of exp * pos
  | ReturnStmt of exp option
  | Empty
  | Break of pos
  | Continue of pos
  | ExprStmt of exp
  | Delete of exp
  | IfElse of
      { exp : exp
      ; istmt : stmt
      ; estmt : stmt
      ; pos : pos
      }

and exp =
  | Identifier of symbol * pos
  | IntLit of int32 * pos
  | OpExp of operator * pos
  | ArrayCreationExp of
      { type_ : type_
      ; exprs : exp list
      ; pos : pos
      }
  | ClassCreationExp of
      { type_ : type_
      ; args : exp list
      ; pos : pos
      }
  | VarExp of var * pos
  | NullLit of pos
  | This of pos
  | Super of pos
  | MethodCall of
      { base : exp
      ; field : exp option
      ; args : exp list
      ; pos : pos
      }
  | CastEvalExp of
      { to_ : exp
      ; from_ : exp
      }
  | CastType of
      { type_ : type_
      ; exp : exp
      ; cast_type : cast_type option
      }
  | Assignment of
      { lhs : var
      ; exp : exp
      ; pos : pos
      }

and operator =
  | UnaryOp of
      { oper : uoper
      ; exp : exp
      }
  | BinaryOp of
      { left : exp
      ; oper : bioper
      ; right : exp
      }

and bioper =
  | LessThanOp
  | GreaterThanOp
  | PlusOp
  | MultOp
  | DivideOp
  | EqualsOp
  | MinusOp

and uoper =
  | NotOp
  | NegateOp

and type_ =
  | Primitive of symbol * pos
  | Reference of ref_type

and ref_type =
  | ArrayType of int * type_
  | ClassType of symbol * pos

and cast_type =
  | Wide
  | Narrow
  | Identity
[@@deriving sexp]

let append_rank_to_type rank1 type_ =
  if rank1 > 0
  then (
    match type_ with
    | Reference (ArrayType (rank2, type_)) -> Reference (ArrayType (rank1 + rank2, type_))
    | _ -> Reference (ArrayType (rank1, type_)))
  else type_
;;
