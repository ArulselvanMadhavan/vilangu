module A = Ast
module T = Types

exception VtableEntryNotFound

let get_class_defn class_name class_defns =
  let matching_class_defns =
    List.filter (fun (A.ClassDec { name; _ }) -> class_name = name) class_defns
  in
  List.hd matching_class_defns
;;

let rec get_class_fields class_name class_defns =
  get_class_defn class_name class_defns
  |> fun (A.ClassDec { base; class_body; _ }) ->
  Option.fold
    ~none:[]
    ~some:(fun super_class -> get_class_fields super_class class_defns)
    base
  |> fun superclass_fields ->
  let field_defns =
    Base.List.filter_map class_body ~f:(fun body ->
      match body with
      | A.FieldDec fields -> Some fields
      | _ -> None)
  in
  let field_defns = List.concat field_defns in
  List.concat [ superclass_fields; field_defns ]
;;

let vtable_method_name method_name arg_types =
  Base.String.concat ~sep:"_" (method_name :: arg_types)
;;

let vtable_offset = 2

let find_vtable_index ty method_name =
  match ty with
  | T.NAME (_, _, _, vtable) ->
    let vtable = List.map (fun (v, _) -> v) vtable in
    let vtbl_idx = Base.List.findi vtable ~f:(fun _ -> String.equal method_name) in
    Option.map (fun (v, _) -> v + vtable_offset) vtbl_idx
  | _ -> raise VtableEntryNotFound
;;

let gen_this_variants tenv ty =
  let open Base.Option.Let_syntax in
  let rec gen ty =
    match ty with
    | T.NAME ((class_name, _), _, base, _) ->
      let base_xs = Option.value (handle_base base) ~default:[] in
      class_name :: base_xs
    | x -> [ T.type2str x ]
  and handle_base base =
    let%bind base = base in
    let%map ty = Symbol.look (tenv, base) in
    gen ty
  in
  gen ty
;;

let lookup_method_index tenv ty method_name args_ty =
  let args_list = ty :: args_ty in
  let is_found = ref false in
  let vtbl_idx = ref None in
  let args = Base.List.to_array args_list in
  let args_len = Array.length args in
  let args_var =
    Array.map (fun a -> Base.List.to_array (gen_this_variants tenv a)) args
  in
  let rec append_variants arg_idx acc vars =
    for i = 0 to Array.length vars - 1 do
      if !is_found = false
      then (
        let acc = vars.(i) :: acc in
        if arg_idx < args_len - 1
        then (
          let next_idx = arg_idx + 1 in
          append_variants next_idx acc args_var.(next_idx))
        else (
          let method_name = vtable_method_name method_name (List.rev acc) in
          vtbl_idx := find_vtable_index ty method_name;
          if Option.is_some !vtbl_idx then is_found := true else ()))
    done
  in
  append_variants 0 [] args_var.(0);
  !vtbl_idx
;;
