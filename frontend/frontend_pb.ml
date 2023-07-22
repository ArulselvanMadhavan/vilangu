[@@@ocaml.warning "-27-30-39"]

type var_p_simple_mutable = {
  mutable var_name : string;
}

let default_var_p_simple_mutable () : var_p_simple_mutable = {
  var_name = "";
}

type type_expr_p_class_mutable = {
  mutable name : string;
}

let default_type_expr_p_class_mutable () : type_expr_p_class_mutable = {
  name = "";
}

type type_expr_p_pointer_mutable = {
  mutable data : Frontend_types.type_expr;
}

let default_type_expr_p_pointer_mutable () : type_expr_p_pointer_mutable = {
  data = Frontend_types.default_type_expr ();
}

type var_p_subscript_mutable = {
  mutable base_var : Frontend_types.var;
  mutable var_exp : Frontend_types.expr;
  mutable len_var : Frontend_types.var;
  mutable line_no : int32;
}

let default_var_p_subscript_mutable () : var_p_subscript_mutable = {
  base_var = Frontend_types.default_var ();
  var_exp = Frontend_types.default_expr ();
  len_var = Frontend_types.default_var ();
  line_no = 0l;
}

type var_p_field_mutable = {
  mutable base_expr : Frontend_types.expr;
  mutable field_index : int32;
  mutable field_line_no : int32;
}

let default_var_p_field_mutable () : var_p_field_mutable = {
  base_expr = Frontend_types.default_expr ();
  field_index = 0l;
  field_line_no = 0l;
}

type expr_p_function_app_mutable = {
  mutable name : string;
  mutable args : Frontend_types.expr list;
}

let default_expr_p_function_app_mutable () : expr_p_function_app_mutable = {
  name = "";
  args = [];
}

type expr_p_unop_mutable = {
  mutable op : Frontend_types.un_op;
  mutable uexpr : Frontend_types.expr;
}

let default_expr_p_unop_mutable () : expr_p_unop_mutable = {
  op = Frontend_types.default_un_op ();
  uexpr = Frontend_types.default_expr ();
}

type expr_p_binop_mutable = {
  mutable bin_op : Frontend_types.bin_op;
  mutable lexpr : Frontend_types.expr;
  mutable rexpr : Frontend_types.expr;
  mutable op_line_no : int32;
}

let default_expr_p_binop_mutable () : expr_p_binop_mutable = {
  bin_op = Frontend_types.default_bin_op ();
  lexpr = Frontend_types.default_expr ();
  rexpr = Frontend_types.default_expr ();
  op_line_no = 0l;
}

type expr_p_assign_mutable = {
  mutable lhs : Frontend_types.var;
  mutable rhs : Frontend_types.expr;
}

let default_expr_p_assign_mutable () : expr_p_assign_mutable = {
  lhs = Frontend_types.default_var ();
  rhs = Frontend_types.default_expr ();
}

type identifier_mutable = {
  mutable id : Frontend_types.var;
}

let default_identifier_mutable () : identifier_mutable = {
  id = Frontend_types.default_var ();
}

type expr_p_array_creation_mutable = {
  mutable creation_exprs : Frontend_types.expr list;
  mutable texpr : Frontend_types.type_expr;
  mutable make_line_no : int32;
}

let default_expr_p_array_creation_mutable () : expr_p_array_creation_mutable = {
  creation_exprs = [];
  texpr = Frontend_types.default_type_expr ();
  make_line_no = 0l;
}

type expr_p_cast_expr_mutable = {
  mutable cast_to : Frontend_types.type_expr;
  mutable expr : Frontend_types.expr;
  mutable cast_type : Frontend_types.expr_p_cast;
  mutable cast_line_no : int32;
}

let default_expr_p_cast_expr_mutable () : expr_p_cast_expr_mutable = {
  cast_to = Frontend_types.default_type_expr ();
  expr = Frontend_types.default_expr ();
  cast_type = Frontend_types.default_expr_p_cast ();
  cast_line_no = 0l;
}

type expr_p_class_creation_mutable = {
  mutable con_texpr : Frontend_types.type_expr;
  mutable con_args : Frontend_types.expr list;
  mutable vtable_index : int32;
}

let default_expr_p_class_creation_mutable () : expr_p_class_creation_mutable = {
  con_texpr = Frontend_types.default_type_expr ();
  con_args = [];
  vtable_index = 0l;
}

type var_p_load_mutable = {
  mutable var : Frontend_types.var;
}

let default_var_p_load_mutable () : var_p_load_mutable = {
  var = Frontend_types.default_var ();
}

type stmt_p_var_decl_mutable = {
  mutable var_id : string;
  mutable texpr : Frontend_types.type_expr;
}

let default_stmt_p_var_decl_mutable () : stmt_p_var_decl_mutable = {
  var_id = "";
  texpr = Frontend_types.default_type_expr ();
}

type stmt_p_printf_mutable = {
  mutable format : string;
  mutable f_args : Frontend_types.expr list;
}

let default_stmt_p_printf_mutable () : stmt_p_printf_mutable = {
  format = "";
  f_args = [];
}

type stmt_p_expr_stmt_mutable = {
  mutable expr_stmt : Frontend_types.expr;
}

let default_stmt_p_expr_stmt_mutable () : stmt_p_expr_stmt_mutable = {
  expr_stmt = Frontend_types.default_expr ();
}

type stmt_p_delete_mutable = {
  mutable del_expr : Frontend_types.expr;
}

let default_stmt_p_delete_mutable () : stmt_p_delete_mutable = {
  del_expr = Frontend_types.default_expr ();
}

type stmt_p_free_mutable = {
  mutable free_expr : Frontend_types.expr;
}

let default_stmt_p_free_mutable () : stmt_p_free_mutable = {
  free_expr = Frontend_types.default_expr ();
}

type stmt_p_while_mutable = {
  mutable while_cond : Frontend_types.expr;
  mutable while_block : Frontend_types.stmt;
}

let default_stmt_p_while_mutable () : stmt_p_while_mutable = {
  while_cond = Frontend_types.default_expr ();
  while_block = Frontend_types.default_stmt ();
}

type stmt_p_block_mutable = {
  mutable stmt_list : Frontend_types.stmt list;
}

let default_stmt_p_block_mutable () : stmt_p_block_mutable = {
  stmt_list = [];
}

type stmt_p_if_stmt_mutable = {
  mutable eval : Frontend_types.expr;
  mutable if_stmt : Frontend_types.stmt;
  mutable else_stmt : Frontend_types.stmt;
}

let default_stmt_p_if_stmt_mutable () : stmt_p_if_stmt_mutable = {
  eval = Frontend_types.default_expr ();
  if_stmt = Frontend_types.default_stmt ();
  else_stmt = Frontend_types.default_stmt ();
}

type class_def_mutable = {
  mutable name : string;
  mutable fields : Frontend_types.type_expr list;
  mutable base_class_name : string;
  mutable vtable : string list;
}

let default_class_def_mutable () : class_def_mutable = {
  name = "";
  fields = [];
  base_class_name = "";
  vtable = [];
}

type param_mutable = {
  mutable param_type : Frontend_types.type_expr;
  mutable param_name : string;
}

let default_param_mutable () : param_mutable = {
  param_type = Frontend_types.default_type_expr ();
  param_name = "";
}

type function_def_mutable = {
  mutable name : string;
  mutable return_t : Frontend_types.type_expr;
  mutable params : Frontend_types.param list;
  mutable body : Frontend_types.stmt;
}

let default_function_def_mutable () : function_def_mutable = {
  name = "";
  return_t = Frontend_types.default_type_expr ();
  params = [];
  body = Frontend_types.default_stmt ();
}

type program_mutable = {
  mutable main : Frontend_types.stmt list;
  mutable classdefs : Frontend_types.class_def list;
  mutable function_defs : Frontend_types.function_def list;
}

let default_program_mutable () : program_mutable = {
  main = [];
  classdefs = [];
  function_defs = [];
}


let rec decode_un_op d = 
  let rec loop () = 
    let ret:Frontend_types.un_op = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "un_op"
      | Some (1, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Not : Frontend_types.un_op)
      end
      | Some (2, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Neg : Frontend_types.un_op)
      end
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_bin_op d = 
  let rec loop () = 
    let ret:Frontend_types.bin_op = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "bin_op"
      | Some (1, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Plus : Frontend_types.bin_op)
      end
      | Some (2, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Equals : Frontend_types.bin_op)
      end
      | Some (3, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Less_than : Frontend_types.bin_op)
      end
      | Some (4, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Greater_than : Frontend_types.bin_op)
      end
      | Some (5, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Mult_op : Frontend_types.bin_op)
      end
      | Some (6, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Divide_op : Frontend_types.bin_op)
      end
      | Some (7, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Subtract_op : Frontend_types.bin_op)
      end
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_var_p_simple d =
  let v = default_var_p_simple_mutable () in
  let continue__= ref true in
  let var_name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.var_name <- Pbrt.Decoder.string d; var_name_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(var_p_simple), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !var_name_is_set then Pbrt.Decoder.missing_field "var_name" end;
  ({
    Frontend_types.var_name = v.var_name;
  } : Frontend_types.var_p_simple)

let rec decode_type_expr_p_class d =
  let v = default_type_expr_p_class_mutable () in
  let continue__= ref true in
  let name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d; name_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(type_expr_p_class), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  ({
    Frontend_types.name = v.name;
  } : Frontend_types.type_expr_p_class)

let rec decode_type_expr d = 
  let rec loop () = 
    let ret:Frontend_types.type_expr = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "type_expr"
      | Some (1, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Int32 : Frontend_types.type_expr)
      end
      | Some (2, _) -> (Frontend_types.Class (decode_type_expr_p_class (Pbrt.Decoder.nested d)) : Frontend_types.type_expr) 
      | Some (3, _) -> (Frontend_types.Pointer (decode_type_expr_p_pointer (Pbrt.Decoder.nested d)) : Frontend_types.type_expr) 
      | Some (4, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Void : Frontend_types.type_expr)
      end
      | Some (5, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Bool : Frontend_types.type_expr)
      end
      | Some (6, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Int8 : Frontend_types.type_expr)
      end
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_type_expr_p_pointer d =
  let v = default_type_expr_p_pointer_mutable () in
  let continue__= ref true in
  let data_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.data <- decode_type_expr (Pbrt.Decoder.nested d); data_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(type_expr_p_pointer), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !data_is_set then Pbrt.Decoder.missing_field "data" end;
  ({
    Frontend_types.data = v.data;
  } : Frontend_types.type_expr_p_pointer)

let rec decode_expr_p_cast d = 
  let rec loop () = 
    let ret:Frontend_types.expr_p_cast = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "expr_p_cast"
      | Some (1, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.No_cast : Frontend_types.expr_p_cast)
      end
      | Some (2, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Wide_cast : Frontend_types.expr_p_cast)
      end
      | Some (3, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Narrow_cast : Frontend_types.expr_p_cast)
      end
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_var_p_subscript d =
  let v = default_var_p_subscript_mutable () in
  let continue__= ref true in
  let line_no_is_set = ref false in
  let len_var_is_set = ref false in
  let var_exp_is_set = ref false in
  let base_var_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.base_var <- decode_var (Pbrt.Decoder.nested d); base_var_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(var_p_subscript), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.var_exp <- decode_expr (Pbrt.Decoder.nested d); var_exp_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(var_p_subscript), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.len_var <- decode_var (Pbrt.Decoder.nested d); len_var_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(var_p_subscript), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      v.line_no <- Pbrt.Decoder.int32_as_varint d; line_no_is_set := true;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(var_p_subscript), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !line_no_is_set then Pbrt.Decoder.missing_field "line_no" end;
  begin if not !len_var_is_set then Pbrt.Decoder.missing_field "len_var" end;
  begin if not !var_exp_is_set then Pbrt.Decoder.missing_field "var_exp" end;
  begin if not !base_var_is_set then Pbrt.Decoder.missing_field "base_var" end;
  ({
    Frontend_types.base_var = v.base_var;
    Frontend_types.var_exp = v.var_exp;
    Frontend_types.len_var = v.len_var;
    Frontend_types.line_no = v.line_no;
  } : Frontend_types.var_p_subscript)

and decode_var d = 
  let rec loop () = 
    let ret:Frontend_types.var = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "var"
      | Some (1, _) -> (Frontend_types.Simple (decode_var_p_simple (Pbrt.Decoder.nested d)) : Frontend_types.var) 
      | Some (2, _) -> (Frontend_types.Subscript (decode_var_p_subscript (Pbrt.Decoder.nested d)) : Frontend_types.var) 
      | Some (3, _) -> (Frontend_types.Field (decode_var_p_field (Pbrt.Decoder.nested d)) : Frontend_types.var) 
      | Some (4, _) -> (Frontend_types.Load_var (decode_var_p_load (Pbrt.Decoder.nested d)) : Frontend_types.var) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_var_p_field d =
  let v = default_var_p_field_mutable () in
  let continue__= ref true in
  let field_line_no_is_set = ref false in
  let field_index_is_set = ref false in
  let base_expr_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.base_expr <- decode_expr (Pbrt.Decoder.nested d); base_expr_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(var_p_field), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.field_index <- Pbrt.Decoder.int32_as_varint d; field_index_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(var_p_field), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.field_line_no <- Pbrt.Decoder.int32_as_varint d; field_line_no_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(var_p_field), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !field_line_no_is_set then Pbrt.Decoder.missing_field "field_line_no" end;
  begin if not !field_index_is_set then Pbrt.Decoder.missing_field "field_index" end;
  begin if not !base_expr_is_set then Pbrt.Decoder.missing_field "base_expr" end;
  ({
    Frontend_types.base_expr = v.base_expr;
    Frontend_types.field_index = v.field_index;
    Frontend_types.field_line_no = v.field_line_no;
  } : Frontend_types.var_p_field)

and decode_expr d = 
  let rec loop () = 
    let ret:Frontend_types.expr = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "expr"
      | Some (1, _) -> (Frontend_types.Integer (Pbrt.Decoder.int32_as_varint d) : Frontend_types.expr) 
      | Some (2, _) -> (Frontend_types.Function_app (decode_expr_p_function_app (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (3, _) -> (Frontend_types.Unop (decode_expr_p_unop (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (4, _) -> (Frontend_types.Binop (decode_expr_p_binop (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (5, _) -> (Frontend_types.Assign (decode_expr_p_assign (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (6, _) -> (Frontend_types.Expr_id (decode_identifier (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (7, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Empty : Frontend_types.expr)
      end
      | Some (8, _) -> (Frontend_types.Array_creation (decode_expr_p_array_creation (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (9, _) -> (Frontend_types.Var_exp (decode_var (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (10, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Null_lit : Frontend_types.expr)
      end
      | Some (11, _) -> (Frontend_types.Cast_expr (decode_expr_p_cast_expr (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (12, _) -> (Frontend_types.Class_creation (decode_expr_p_class_creation (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_expr_p_function_app d =
  let v = default_expr_p_function_app_mutable () in
  let continue__= ref true in
  let name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.args <- List.rev v.args;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d; name_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_function_app), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.args <- (decode_expr (Pbrt.Decoder.nested d)) :: v.args;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_function_app), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  ({
    Frontend_types.name = v.name;
    Frontend_types.args = v.args;
  } : Frontend_types.expr_p_function_app)

and decode_expr_p_unop d =
  let v = default_expr_p_unop_mutable () in
  let continue__= ref true in
  let uexpr_is_set = ref false in
  let op_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.op <- decode_un_op (Pbrt.Decoder.nested d); op_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_unop), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.uexpr <- decode_expr (Pbrt.Decoder.nested d); uexpr_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_unop), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !uexpr_is_set then Pbrt.Decoder.missing_field "uexpr" end;
  begin if not !op_is_set then Pbrt.Decoder.missing_field "op" end;
  ({
    Frontend_types.op = v.op;
    Frontend_types.uexpr = v.uexpr;
  } : Frontend_types.expr_p_unop)

and decode_expr_p_binop d =
  let v = default_expr_p_binop_mutable () in
  let continue__= ref true in
  let op_line_no_is_set = ref false in
  let rexpr_is_set = ref false in
  let lexpr_is_set = ref false in
  let bin_op_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.bin_op <- decode_bin_op (Pbrt.Decoder.nested d); bin_op_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_binop), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.lexpr <- decode_expr (Pbrt.Decoder.nested d); lexpr_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_binop), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.rexpr <- decode_expr (Pbrt.Decoder.nested d); rexpr_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_binop), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      v.op_line_no <- Pbrt.Decoder.int32_as_varint d; op_line_no_is_set := true;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_binop), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !op_line_no_is_set then Pbrt.Decoder.missing_field "op_line_no" end;
  begin if not !rexpr_is_set then Pbrt.Decoder.missing_field "rexpr" end;
  begin if not !lexpr_is_set then Pbrt.Decoder.missing_field "lexpr" end;
  begin if not !bin_op_is_set then Pbrt.Decoder.missing_field "bin_op" end;
  ({
    Frontend_types.bin_op = v.bin_op;
    Frontend_types.lexpr = v.lexpr;
    Frontend_types.rexpr = v.rexpr;
    Frontend_types.op_line_no = v.op_line_no;
  } : Frontend_types.expr_p_binop)

and decode_expr_p_assign d =
  let v = default_expr_p_assign_mutable () in
  let continue__= ref true in
  let rhs_is_set = ref false in
  let lhs_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.lhs <- decode_var (Pbrt.Decoder.nested d); lhs_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_assign), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.rhs <- decode_expr (Pbrt.Decoder.nested d); rhs_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_assign), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !rhs_is_set then Pbrt.Decoder.missing_field "rhs" end;
  begin if not !lhs_is_set then Pbrt.Decoder.missing_field "lhs" end;
  ({
    Frontend_types.lhs = v.lhs;
    Frontend_types.rhs = v.rhs;
  } : Frontend_types.expr_p_assign)

and decode_identifier d =
  let v = default_identifier_mutable () in
  let continue__= ref true in
  let id_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.id <- decode_var (Pbrt.Decoder.nested d); id_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(identifier), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  ({
    Frontend_types.id = v.id;
  } : Frontend_types.identifier)

and decode_expr_p_array_creation d =
  let v = default_expr_p_array_creation_mutable () in
  let continue__= ref true in
  let make_line_no_is_set = ref false in
  let texpr_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.creation_exprs <- List.rev v.creation_exprs;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.creation_exprs <- (decode_expr (Pbrt.Decoder.nested d)) :: v.creation_exprs;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_array_creation), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.texpr <- decode_type_expr (Pbrt.Decoder.nested d); texpr_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_array_creation), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.make_line_no <- Pbrt.Decoder.int32_as_varint d; make_line_no_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_array_creation), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !make_line_no_is_set then Pbrt.Decoder.missing_field "make_line_no" end;
  begin if not !texpr_is_set then Pbrt.Decoder.missing_field "texpr" end;
  ({
    Frontend_types.creation_exprs = v.creation_exprs;
    Frontend_types.texpr = v.texpr;
    Frontend_types.make_line_no = v.make_line_no;
  } : Frontend_types.expr_p_array_creation)

and decode_expr_p_cast_expr d =
  let v = default_expr_p_cast_expr_mutable () in
  let continue__= ref true in
  let cast_line_no_is_set = ref false in
  let cast_type_is_set = ref false in
  let expr_is_set = ref false in
  let cast_to_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.cast_to <- decode_type_expr (Pbrt.Decoder.nested d); cast_to_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_cast_expr), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.expr <- decode_expr (Pbrt.Decoder.nested d); expr_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_cast_expr), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.cast_type <- decode_expr_p_cast (Pbrt.Decoder.nested d); cast_type_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_cast_expr), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      v.cast_line_no <- Pbrt.Decoder.int32_as_varint d; cast_line_no_is_set := true;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_cast_expr), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !cast_line_no_is_set then Pbrt.Decoder.missing_field "cast_line_no" end;
  begin if not !cast_type_is_set then Pbrt.Decoder.missing_field "cast_type" end;
  begin if not !expr_is_set then Pbrt.Decoder.missing_field "expr" end;
  begin if not !cast_to_is_set then Pbrt.Decoder.missing_field "cast_to" end;
  ({
    Frontend_types.cast_to = v.cast_to;
    Frontend_types.expr = v.expr;
    Frontend_types.cast_type = v.cast_type;
    Frontend_types.cast_line_no = v.cast_line_no;
  } : Frontend_types.expr_p_cast_expr)

and decode_expr_p_class_creation d =
  let v = default_expr_p_class_creation_mutable () in
  let continue__= ref true in
  let vtable_index_is_set = ref false in
  let con_texpr_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.con_args <- List.rev v.con_args;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.con_texpr <- decode_type_expr (Pbrt.Decoder.nested d); con_texpr_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_class_creation), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.con_args <- (decode_expr (Pbrt.Decoder.nested d)) :: v.con_args;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_class_creation), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.vtable_index <- Pbrt.Decoder.int32_as_varint d; vtable_index_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_class_creation), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !vtable_index_is_set then Pbrt.Decoder.missing_field "vtable_index" end;
  begin if not !con_texpr_is_set then Pbrt.Decoder.missing_field "con_texpr" end;
  ({
    Frontend_types.con_texpr = v.con_texpr;
    Frontend_types.con_args = v.con_args;
    Frontend_types.vtable_index = v.vtable_index;
  } : Frontend_types.expr_p_class_creation)

and decode_var_p_load d =
  let v = default_var_p_load_mutable () in
  let continue__= ref true in
  let var_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.var <- decode_var (Pbrt.Decoder.nested d); var_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(var_p_load), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !var_is_set then Pbrt.Decoder.missing_field "var" end;
  ({
    Frontend_types.var = v.var;
  } : Frontend_types.var_p_load)

let rec decode_stmt_p_var_decl d =
  let v = default_stmt_p_var_decl_mutable () in
  let continue__= ref true in
  let texpr_is_set = ref false in
  let var_id_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.var_id <- Pbrt.Decoder.string d; var_id_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(stmt_p_var_decl), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.texpr <- decode_type_expr (Pbrt.Decoder.nested d); texpr_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(stmt_p_var_decl), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !texpr_is_set then Pbrt.Decoder.missing_field "texpr" end;
  begin if not !var_id_is_set then Pbrt.Decoder.missing_field "var_id" end;
  ({
    Frontend_types.var_id = v.var_id;
    Frontend_types.texpr = v.texpr;
  } : Frontend_types.stmt_p_var_decl)

let rec decode_stmt_p_printf d =
  let v = default_stmt_p_printf_mutable () in
  let continue__= ref true in
  let format_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.f_args <- List.rev v.f_args;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.format <- Pbrt.Decoder.string d; format_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(stmt_p_printf), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.f_args <- (decode_expr (Pbrt.Decoder.nested d)) :: v.f_args;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(stmt_p_printf), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !format_is_set then Pbrt.Decoder.missing_field "format" end;
  ({
    Frontend_types.format = v.format;
    Frontend_types.f_args = v.f_args;
  } : Frontend_types.stmt_p_printf)

let rec decode_stmt_p_expr_stmt d =
  let v = default_stmt_p_expr_stmt_mutable () in
  let continue__= ref true in
  let expr_stmt_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.expr_stmt <- decode_expr (Pbrt.Decoder.nested d); expr_stmt_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(stmt_p_expr_stmt), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !expr_stmt_is_set then Pbrt.Decoder.missing_field "expr_stmt" end;
  ({
    Frontend_types.expr_stmt = v.expr_stmt;
  } : Frontend_types.stmt_p_expr_stmt)

let rec decode_stmt_p_delete d =
  let v = default_stmt_p_delete_mutable () in
  let continue__= ref true in
  let del_expr_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.del_expr <- decode_expr (Pbrt.Decoder.nested d); del_expr_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(stmt_p_delete), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !del_expr_is_set then Pbrt.Decoder.missing_field "del_expr" end;
  ({
    Frontend_types.del_expr = v.del_expr;
  } : Frontend_types.stmt_p_delete)

let rec decode_stmt_p_free d =
  let v = default_stmt_p_free_mutable () in
  let continue__= ref true in
  let free_expr_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.free_expr <- decode_expr (Pbrt.Decoder.nested d); free_expr_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(stmt_p_free), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !free_expr_is_set then Pbrt.Decoder.missing_field "free_expr" end;
  ({
    Frontend_types.free_expr = v.free_expr;
  } : Frontend_types.stmt_p_free)

let rec decode_stmt_p_while d =
  let v = default_stmt_p_while_mutable () in
  let continue__= ref true in
  let while_block_is_set = ref false in
  let while_cond_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.while_cond <- decode_expr (Pbrt.Decoder.nested d); while_cond_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(stmt_p_while), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.while_block <- decode_stmt (Pbrt.Decoder.nested d); while_block_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(stmt_p_while), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !while_block_is_set then Pbrt.Decoder.missing_field "while_block" end;
  begin if not !while_cond_is_set then Pbrt.Decoder.missing_field "while_cond" end;
  ({
    Frontend_types.while_cond = v.while_cond;
    Frontend_types.while_block = v.while_block;
  } : Frontend_types.stmt_p_while)

and decode_stmt d = 
  let rec loop () = 
    let ret:Frontend_types.stmt = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "stmt"
      | Some (1, _) -> (Frontend_types.Var_decl (decode_stmt_p_var_decl (Pbrt.Decoder.nested d)) : Frontend_types.stmt) 
      | Some (2, _) -> (Frontend_types.Expr_stmt (decode_stmt_p_expr_stmt (Pbrt.Decoder.nested d)) : Frontend_types.stmt) 
      | Some (3, _) -> (Frontend_types.Printf (decode_stmt_p_printf (Pbrt.Decoder.nested d)) : Frontend_types.stmt) 
      | Some (4, _) -> (Frontend_types.While (decode_stmt_p_while (Pbrt.Decoder.nested d)) : Frontend_types.stmt) 
      | Some (5, _) -> (Frontend_types.Block (decode_stmt_p_block (Pbrt.Decoder.nested d)) : Frontend_types.stmt) 
      | Some (6, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Break : Frontend_types.stmt)
      end
      | Some (7, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Continue : Frontend_types.stmt)
      end
      | Some (8, _) -> (Frontend_types.If_stmt (decode_stmt_p_if_stmt (Pbrt.Decoder.nested d)) : Frontend_types.stmt) 
      | Some (9, _) -> (Frontend_types.Delete (decode_stmt_p_delete (Pbrt.Decoder.nested d)) : Frontend_types.stmt) 
      | Some (10, _) -> (Frontend_types.Free (decode_stmt_p_free (Pbrt.Decoder.nested d)) : Frontend_types.stmt) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_stmt_p_block d =
  let v = default_stmt_p_block_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.stmt_list <- List.rev v.stmt_list;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.stmt_list <- (decode_stmt (Pbrt.Decoder.nested d)) :: v.stmt_list;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(stmt_p_block), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Frontend_types.stmt_list = v.stmt_list;
  } : Frontend_types.stmt_p_block)

and decode_stmt_p_if_stmt d =
  let v = default_stmt_p_if_stmt_mutable () in
  let continue__= ref true in
  let else_stmt_is_set = ref false in
  let if_stmt_is_set = ref false in
  let eval_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.eval <- decode_expr (Pbrt.Decoder.nested d); eval_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(stmt_p_if_stmt), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.if_stmt <- decode_stmt (Pbrt.Decoder.nested d); if_stmt_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(stmt_p_if_stmt), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.else_stmt <- decode_stmt (Pbrt.Decoder.nested d); else_stmt_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(stmt_p_if_stmt), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !else_stmt_is_set then Pbrt.Decoder.missing_field "else_stmt" end;
  begin if not !if_stmt_is_set then Pbrt.Decoder.missing_field "if_stmt" end;
  begin if not !eval_is_set then Pbrt.Decoder.missing_field "eval" end;
  ({
    Frontend_types.eval = v.eval;
    Frontend_types.if_stmt = v.if_stmt;
    Frontend_types.else_stmt = v.else_stmt;
  } : Frontend_types.stmt_p_if_stmt)

let rec decode_class_def d =
  let v = default_class_def_mutable () in
  let continue__= ref true in
  let base_class_name_is_set = ref false in
  let name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.vtable <- List.rev v.vtable;
      v.fields <- List.rev v.fields;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d; name_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_def), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.fields <- (decode_type_expr (Pbrt.Decoder.nested d)) :: v.fields;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_def), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.base_class_name <- Pbrt.Decoder.string d; base_class_name_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_def), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.vtable <- (Pbrt.Decoder.string d) :: v.vtable;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(class_def), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !base_class_name_is_set then Pbrt.Decoder.missing_field "base_class_name" end;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  ({
    Frontend_types.name = v.name;
    Frontend_types.fields = v.fields;
    Frontend_types.base_class_name = v.base_class_name;
    Frontend_types.vtable = v.vtable;
  } : Frontend_types.class_def)

let rec decode_param d =
  let v = default_param_mutable () in
  let continue__= ref true in
  let param_name_is_set = ref false in
  let param_type_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.param_type <- decode_type_expr (Pbrt.Decoder.nested d); param_type_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(param), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.param_name <- Pbrt.Decoder.string d; param_name_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(param), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !param_name_is_set then Pbrt.Decoder.missing_field "param_name" end;
  begin if not !param_type_is_set then Pbrt.Decoder.missing_field "param_type" end;
  ({
    Frontend_types.param_type = v.param_type;
    Frontend_types.param_name = v.param_name;
  } : Frontend_types.param)

let rec decode_function_def d =
  let v = default_function_def_mutable () in
  let continue__= ref true in
  let body_is_set = ref false in
  let return_t_is_set = ref false in
  let name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.params <- List.rev v.params;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d; name_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(function_def), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.return_t <- decode_type_expr (Pbrt.Decoder.nested d); return_t_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(function_def), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.params <- (decode_param (Pbrt.Decoder.nested d)) :: v.params;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(function_def), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.body <- decode_stmt (Pbrt.Decoder.nested d); body_is_set := true;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(function_def), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !body_is_set then Pbrt.Decoder.missing_field "body" end;
  begin if not !return_t_is_set then Pbrt.Decoder.missing_field "return_t" end;
  begin if not !name_is_set then Pbrt.Decoder.missing_field "name" end;
  ({
    Frontend_types.name = v.name;
    Frontend_types.return_t = v.return_t;
    Frontend_types.params = v.params;
    Frontend_types.body = v.body;
  } : Frontend_types.function_def)

let rec decode_program d =
  let v = default_program_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.function_defs <- List.rev v.function_defs;
      v.classdefs <- List.rev v.classdefs;
      v.main <- List.rev v.main;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.main <- (decode_stmt (Pbrt.Decoder.nested d)) :: v.main;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(program), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.classdefs <- (decode_class_def (Pbrt.Decoder.nested d)) :: v.classdefs;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(program), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.function_defs <- (decode_function_def (Pbrt.Decoder.nested d)) :: v.function_defs;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(program), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Frontend_types.main = v.main;
    Frontend_types.classdefs = v.classdefs;
    Frontend_types.function_defs = v.function_defs;
  } : Frontend_types.program)

let rec encode_un_op (v:Frontend_types.un_op) encoder = 
  begin match v with
  | Frontend_types.Not ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Neg ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

let rec encode_bin_op (v:Frontend_types.bin_op) encoder = 
  begin match v with
  | Frontend_types.Plus ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Equals ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Less_than ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Greater_than ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Mult_op ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Divide_op ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Subtract_op ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

let rec encode_var_p_simple (v:Frontend_types.var_p_simple) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Frontend_types.var_name encoder;
  ()

let rec encode_type_expr_p_class (v:Frontend_types.type_expr_p_class) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Frontend_types.name encoder;
  ()

let rec encode_type_expr (v:Frontend_types.type_expr) encoder = 
  begin match v with
  | Frontend_types.Int32 ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Class x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_expr_p_class x) encoder;
  | Frontend_types.Pointer x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_expr_p_pointer x) encoder;
  | Frontend_types.Void ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Bool ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Int8 ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

and encode_type_expr_p_pointer (v:Frontend_types.type_expr_p_pointer) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_expr v.Frontend_types.data) encoder;
  ()

let rec encode_expr_p_cast (v:Frontend_types.expr_p_cast) encoder = 
  begin match v with
  | Frontend_types.No_cast ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Wide_cast ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Narrow_cast ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

let rec encode_var_p_subscript (v:Frontend_types.var_p_subscript) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_var v.Frontend_types.base_var) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.var_exp) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_var v.Frontend_types.len_var) encoder;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Frontend_types.line_no encoder;
  ()

and encode_var (v:Frontend_types.var) encoder = 
  begin match v with
  | Frontend_types.Simple x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_var_p_simple x) encoder;
  | Frontend_types.Subscript x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_var_p_subscript x) encoder;
  | Frontend_types.Field x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_var_p_field x) encoder;
  | Frontend_types.Load_var x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_var_p_load x) encoder;
  end

and encode_var_p_field (v:Frontend_types.var_p_field) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.base_expr) encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Frontend_types.field_index encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Frontend_types.field_line_no encoder;
  ()

and encode_expr (v:Frontend_types.expr) encoder = 
  begin match v with
  | Frontend_types.Integer x ->
    Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int32_as_varint x encoder;
  | Frontend_types.Function_app x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_function_app x) encoder;
  | Frontend_types.Unop x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_unop x) encoder;
  | Frontend_types.Binop x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_binop x) encoder;
  | Frontend_types.Assign x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_assign x) encoder;
  | Frontend_types.Expr_id x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_identifier x) encoder;
  | Frontend_types.Empty ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Array_creation x ->
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_array_creation x) encoder;
  | Frontend_types.Var_exp x ->
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_var x) encoder;
  | Frontend_types.Null_lit ->
    Pbrt.Encoder.key (10, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Cast_expr x ->
    Pbrt.Encoder.key (11, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_cast_expr x) encoder;
  | Frontend_types.Class_creation x ->
    Pbrt.Encoder.key (12, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_class_creation x) encoder;
  end

and encode_expr_p_function_app (v:Frontend_types.expr_p_function_app) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Frontend_types.name encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr x) encoder;
  ) v.Frontend_types.args;
  ()

and encode_expr_p_unop (v:Frontend_types.expr_p_unop) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_un_op v.Frontend_types.op) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.uexpr) encoder;
  ()

and encode_expr_p_binop (v:Frontend_types.expr_p_binop) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_bin_op v.Frontend_types.bin_op) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.lexpr) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.rexpr) encoder;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Frontend_types.op_line_no encoder;
  ()

and encode_expr_p_assign (v:Frontend_types.expr_p_assign) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_var v.Frontend_types.lhs) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.rhs) encoder;
  ()

and encode_identifier (v:Frontend_types.identifier) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_var v.Frontend_types.id) encoder;
  ()

and encode_expr_p_array_creation (v:Frontend_types.expr_p_array_creation) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr x) encoder;
  ) v.Frontend_types.creation_exprs;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_expr v.Frontend_types.texpr) encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Frontend_types.make_line_no encoder;
  ()

and encode_expr_p_cast_expr (v:Frontend_types.expr_p_cast_expr) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_expr v.Frontend_types.cast_to) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.expr) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr_p_cast v.Frontend_types.cast_type) encoder;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Frontend_types.cast_line_no encoder;
  ()

and encode_expr_p_class_creation (v:Frontend_types.expr_p_class_creation) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_expr v.Frontend_types.con_texpr) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr x) encoder;
  ) v.Frontend_types.con_args;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Frontend_types.vtable_index encoder;
  ()

and encode_var_p_load (v:Frontend_types.var_p_load) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_var v.Frontend_types.var) encoder;
  ()

let rec encode_stmt_p_var_decl (v:Frontend_types.stmt_p_var_decl) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Frontend_types.var_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_expr v.Frontend_types.texpr) encoder;
  ()

let rec encode_stmt_p_printf (v:Frontend_types.stmt_p_printf) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Frontend_types.format encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr x) encoder;
  ) v.Frontend_types.f_args;
  ()

let rec encode_stmt_p_expr_stmt (v:Frontend_types.stmt_p_expr_stmt) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.expr_stmt) encoder;
  ()

let rec encode_stmt_p_delete (v:Frontend_types.stmt_p_delete) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.del_expr) encoder;
  ()

let rec encode_stmt_p_free (v:Frontend_types.stmt_p_free) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.free_expr) encoder;
  ()

let rec encode_stmt_p_while (v:Frontend_types.stmt_p_while) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.while_cond) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_stmt v.Frontend_types.while_block) encoder;
  ()

and encode_stmt (v:Frontend_types.stmt) encoder = 
  begin match v with
  | Frontend_types.Var_decl x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_stmt_p_var_decl x) encoder;
  | Frontend_types.Expr_stmt x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_stmt_p_expr_stmt x) encoder;
  | Frontend_types.Printf x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_stmt_p_printf x) encoder;
  | Frontend_types.While x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_stmt_p_while x) encoder;
  | Frontend_types.Block x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_stmt_p_block x) encoder;
  | Frontend_types.Break ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Continue ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.If_stmt x ->
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_stmt_p_if_stmt x) encoder;
  | Frontend_types.Delete x ->
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_stmt_p_delete x) encoder;
  | Frontend_types.Free x ->
    Pbrt.Encoder.key (10, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_stmt_p_free x) encoder;
  end

and encode_stmt_p_block (v:Frontend_types.stmt_p_block) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_stmt x) encoder;
  ) v.Frontend_types.stmt_list;
  ()

and encode_stmt_p_if_stmt (v:Frontend_types.stmt_p_if_stmt) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.eval) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_stmt v.Frontend_types.if_stmt) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_stmt v.Frontend_types.else_stmt) encoder;
  ()

let rec encode_class_def (v:Frontend_types.class_def) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Frontend_types.name encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_expr x) encoder;
  ) v.Frontend_types.fields;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Frontend_types.base_class_name encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  ) v.Frontend_types.vtable;
  ()

let rec encode_param (v:Frontend_types.param) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_expr v.Frontend_types.param_type) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Frontend_types.param_name encoder;
  ()

let rec encode_function_def (v:Frontend_types.function_def) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Frontend_types.name encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_expr v.Frontend_types.return_t) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_param x) encoder;
  ) v.Frontend_types.params;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_stmt v.Frontend_types.body) encoder;
  ()

let rec encode_program (v:Frontend_types.program) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_stmt x) encoder;
  ) v.Frontend_types.main;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_class_def x) encoder;
  ) v.Frontend_types.classdefs;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_function_def x) encoder;
  ) v.Frontend_types.function_defs;
  ()
