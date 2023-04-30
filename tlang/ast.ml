open Base

type loc = int * int
and span = loc * loc [@@deriving sexp]

type pos = span [@@deriving sexp]
type symbol = Symbol.symbol [@@deriving sexp]

type comp_unit = MainFunc of stmt list
and stmt = Expr of { pos : pos } [@@deriving sexp]

(* type exp = *)
(*   | IntExp of int *)
(*   | OpExp of *)
(*       { left : exp *)
(*       ; oper : oper *)
(*       ; right : exp *)
(*       ; pos : pos *)
(*       } *)
(*   | LetExp of *)
(*       { decs : dec list *)
(*       ; body : exp *)
(*       ; pos : pos *)
(*       } *)
(*   | NilExp (\* Is this needed? *\) *)
(* [@@deriving sexp] *)

(* and field = *)
(*   { name : symbol *)
(*   ; escape : bool ref *)
(*   ; typ : symbol *)
(*   ; pos : pos *)
(*   } *)

(* and fundec = *)
(*   { name : symbol *)
(*   ; params : field list *)
(*   ; result : (symbol * pos) option *)
(*   ; body : exp *)
(*   ; pos : pos *)
(*   } *)

(* and dec = *)
(*   | FunctionDec of fundec list *)
(*     (\* | VarDec of {name: symbol; escape: bool ref; typ: (symbol * pos) option; init: exp; pos: pos} (\\*  *\\) *\) *)
(* [@@deriving sexp] *)

(* and oper = *)
(*   | PlusOp *)
(*   | MinusOp *)
(*   | TimesOp *)
(*   | DivideOp *)
(* [@@deriving sexp] *)
