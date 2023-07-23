open Base
module FT = Frontend_types

type ty =
  | INT
  | VOID
    (* Used to represent error values. Internal use. Programmer can’t construct a NULL type. A null literal is an instance of object type *)
  | NULL
  (* field names -> types -> index *)
  (* Find index based on field name -> We can use List.find - not efficient *)
  (* Hashtbl of name -> ty and index *)
  (* Sort hashtbl *)
  | NAME of Symbol.symbol * (Symbol.symbol * ty) list * Symbol.symbol option * (string * ty) list
  (* class_name, field_names list, base_class *)
  | NAMEREF of Symbol.symbol
  | ARRAY of int * ty
[@@deriving sexp]

let rec type2str = function
  | NULL -> "NULL"
  | INT -> "i32"
  | NAME (id, _, _, _) -> Symbol.name id
  | NAMEREF id -> Symbol.name id
  | VOID -> "void"
  | ARRAY (rank, ty) -> type2str ty ^ String.concat (List.init rank ~f:(fun _ -> "arr"))
;;

let rec type_match t1 t2 =
  match t1, t2 with
  | INT, INT -> true
  | VOID, VOID -> true
  | NULL, NULL -> true
  | NAME ((_, id1), _, _, _), NAME ((_, id2), _, _, _) -> Int.(id1 = id2)
  | ARRAY (rank1, ty1), ARRAY (rank2, ty2) ->
    if rank1 = rank2 then type_match ty1 ty2 else false
  | _, _ -> false
;;

let gen_type_expr = function
  | INT -> FT.Int32
  | ARRAY (_, _) as arr_type ->
    let name = type2str arr_type in
    FT.Pointer { data = FT.Class { name } }
  | NAME ((name, _), _, _, _) -> FT.Pointer { data = FT.Class { name } }
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

let is_ref = function
  | ARRAY _ | NAME _ | NAMEREF _ -> true
  | _ -> false
;;
