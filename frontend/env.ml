open Base
module T = Types
module S = Symbol

type ty = T.ty [@@deriving sexp]

type enventry =
  | VarEntry of { ty : ty }
  | FunEntry of
      { label : Temp.label
      ; formals : ty list
      ; result : ty
      }
[@@deriving sexp]

let int_symbol = S.symbol "int"
let obj_symbol = S.symbol "Object"
let base_tenv = S.init [ int_symbol, T.INT; obj_symbol, T.NAME (obj_symbol, ref None) ]

let base_venv =
  let base_v = [ "out", [ T.INT ], T.VOID ] in
  let make_sym (name, formals, result) =
    let label = Temp.named_label name in
    S.symbol name, FunEntry { label; formals; result }
  in
  Stdlib.List.map make_sym base_v |> S.init
;;
