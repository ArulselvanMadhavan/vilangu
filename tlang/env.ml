module T = Types
module S = Symbol

type ty = T.ty

type enventry =
  | VarEntry of { ty : ty }
  | FunEntry of
      { label : Temp.label
      ; formals : ty list
      ; result : ty
      }

let base_tenv = S.init [ S.symbol "int", T.INT ]

let base_venv =
  let base_v = [ "out", [ T.INT ], T.UNIT ] in
  let make_sym (name, formals, result) =
    let label = Temp.named_label name in
    S.symbol name, FunEntry { label; formals; result }
  in
  List.map make_sym base_v |> S.init
;;
