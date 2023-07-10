module A = Ast
module T = Types

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
