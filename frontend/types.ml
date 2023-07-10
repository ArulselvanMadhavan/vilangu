open Base
module FT = Frontend_types

type unique = unit ref

type ty =
  | INT
  | VOID
  | NULL
    (* Used to represent error values. Internal use. Programmer can’t construct a NULL type. A null literal is an instance of object type *)
  | NAME of Symbol.symbol * ty option ref
  | ARRAY of int * ty
[@@deriving sexp]

let rec type2str = function
  | NULL -> "NULL"
  | INT -> "i32"
  | NAME (id, _) -> Symbol.name id
  | VOID -> "void"
  | ARRAY (rank, ty) -> type2str ty ^ String.concat (List.init rank ~f:(fun _ -> "arr"))
;;

let rec type_match t1 t2 =
  match t1, t2 with
  | INT, INT -> true
  | VOID, VOID -> true
  | NULL, NULL -> true
  | NAME ((_, id1), _), NAME ((_, id2), _) -> Int.(id1 = id2)
  | ARRAY (rank1, ty1), ARRAY (rank2, ty2) ->
    if rank1 = rank2 then type_match ty1 ty2 else false
  | _, _ -> false
;;

(* let get_array_type ty rank = *)
(*   let ty_str = type2str ty in *)
(*   ty_str ^ Base.String.concat (List.init rank ~f:(fun _ -> "arr")) *)
let gen_type_expr = function
  | INT -> FT.Int32
  | ARRAY (_, _) as arr_type ->
    let name = type2str arr_type in
    FT.Pointer { data = FT.Class { name } }
  | NAME ((name, _), _) -> FT.Pointer { data = FT.Class { name } }
  | _ -> FT.Int32
;;

let is_int = function
  | INT -> true
  | _ -> false
;;

let is_array = function
  | ARRAY _ -> true
  | _ -> false
;;
