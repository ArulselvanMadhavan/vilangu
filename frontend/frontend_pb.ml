[@@@ocaml.warning "-27-30-39"]

type identifier_p_var_mutable = {
  mutable var_name : string;
}

let default_identifier_p_var_mutable () : identifier_p_var_mutable = {
  var_name = "";
}

type type_expr_p_ranked_type_mutable = {
  mutable rank : int32;
}

let default_type_expr_p_ranked_type_mutable () : type_expr_p_ranked_type_mutable = {
  rank = 0l;
}

type expr_p_var_decl_mutable = {
  mutable var_id : string;
  mutable texpr : Frontend_types.type_expr;
}

let default_expr_p_var_decl_mutable () : expr_p_var_decl_mutable = {
  var_id = "";
  texpr = Frontend_types.default_type_expr ();
}

type expr_p_function_app_mutable = {
  mutable name : string;
  mutable args : Frontend_types.expr list;
}

let default_expr_p_function_app_mutable () : expr_p_function_app_mutable = {
  name = "";
  args = [];
}

type expr_p_printf_mutable = {
  mutable format : string;
  mutable f_args : Frontend_types.expr list;
}

let default_expr_p_printf_mutable () : expr_p_printf_mutable = {
  format = "";
  f_args = [];
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
}

let default_expr_p_binop_mutable () : expr_p_binop_mutable = {
  bin_op = Frontend_types.default_bin_op ();
  lexpr = Frontend_types.default_expr ();
  rexpr = Frontend_types.default_expr ();
}

type expr_p_assign_mutable = {
  mutable lhs : Frontend_types.identifier;
  mutable rhs : Frontend_types.expr;
}

let default_expr_p_assign_mutable () : expr_p_assign_mutable = {
  lhs = Frontend_types.default_identifier ();
  rhs = Frontend_types.default_expr ();
}

type expr_p_if_expr_mutable = {
  mutable eval : Frontend_types.expr;
  mutable if_expr : Frontend_types.expr;
  mutable else_expr : Frontend_types.expr;
}

let default_expr_p_if_expr_mutable () : expr_p_if_expr_mutable = {
  eval = Frontend_types.default_expr ();
  if_expr = Frontend_types.default_expr ();
  else_expr = Frontend_types.default_expr ();
}

type expr_p_block_mutable = {
  mutable expr_list : Frontend_types.expr list;
}

let default_expr_p_block_mutable () : expr_p_block_mutable = {
  expr_list = [];
}

type expr_p_while_expr_mutable = {
  mutable while_cond : Frontend_types.expr;
  mutable while_block : Frontend_types.expr;
}

let default_expr_p_while_expr_mutable () : expr_p_while_expr_mutable = {
  while_cond = Frontend_types.default_expr ();
  while_block = Frontend_types.default_expr ();
}

type program_mutable = {
  mutable main : Frontend_types.expr list;
}

let default_program_mutable () : program_mutable = {
  main = [];
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

let rec decode_identifier_p_var d =
  let v = default_identifier_p_var_mutable () in
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
      Pbrt.Decoder.unexpected_payload "Message(identifier_p_var), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !var_name_is_set then Pbrt.Decoder.missing_field "var_name" end;
  ({
    Frontend_types.var_name = v.var_name;
  } : Frontend_types.identifier_p_var)

let rec decode_identifier d = 
  let rec loop () = 
    let ret:Frontend_types.identifier = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "identifier"
      | Some (1, _) -> (Frontend_types.Var (decode_identifier_p_var (Pbrt.Decoder.nested d)) : Frontend_types.identifier) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_type_expr_p_ranked_type d =
  let v = default_type_expr_p_ranked_type_mutable () in
  let continue__= ref true in
  let rank_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.rank <- Pbrt.Decoder.int32_as_varint d; rank_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(type_expr_p_ranked_type), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !rank_is_set then Pbrt.Decoder.missing_field "rank" end;
  ({
    Frontend_types.rank = v.rank;
  } : Frontend_types.type_expr_p_ranked_type)

let rec decode_type_expr d = 
  let rec loop () = 
    let ret:Frontend_types.type_expr = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "type_expr"
      | Some (1, _) -> (Frontend_types.Int32_ty (decode_type_expr_p_ranked_type (Pbrt.Decoder.nested d)) : Frontend_types.type_expr) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_expr_p_var_decl d =
  let v = default_expr_p_var_decl_mutable () in
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
      Pbrt.Decoder.unexpected_payload "Message(expr_p_var_decl), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.texpr <- decode_type_expr (Pbrt.Decoder.nested d); texpr_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_var_decl), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !texpr_is_set then Pbrt.Decoder.missing_field "texpr" end;
  begin if not !var_id_is_set then Pbrt.Decoder.missing_field "var_id" end;
  ({
    Frontend_types.var_id = v.var_id;
    Frontend_types.texpr = v.texpr;
  } : Frontend_types.expr_p_var_decl)

let rec decode_expr_p_function_app d =
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

and decode_expr d = 
  let rec loop () = 
    let ret:Frontend_types.expr = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "expr"
      | Some (1, _) -> (Frontend_types.Integer (Pbrt.Decoder.int32_as_varint d) : Frontend_types.expr) 
      | Some (2, _) -> (Frontend_types.Function_app (decode_expr_p_function_app (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (3, _) -> (Frontend_types.Printf (decode_expr_p_printf (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (4, _) -> (Frontend_types.Unop (decode_expr_p_unop (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (5, _) -> (Frontend_types.Binop (decode_expr_p_binop (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (6, _) -> (Frontend_types.Var_decl (decode_expr_p_var_decl (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (7, _) -> (Frontend_types.Assign (decode_expr_p_assign (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (8, _) -> (Frontend_types.Expr_id (decode_identifier (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (9, _) -> (Frontend_types.If_expr (decode_expr_p_if_expr (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (10, _) -> (Frontend_types.Block_expr (decode_expr_p_block (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (11, _) -> (Frontend_types.While_expr (decode_expr_p_while_expr (Pbrt.Decoder.nested d)) : Frontend_types.expr) 
      | Some (12, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Break : Frontend_types.expr)
      end
      | Some (13, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Continue : Frontend_types.expr)
      end
      | Some (14, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Frontend_types.Empty : Frontend_types.expr)
      end
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_expr_p_printf d =
  let v = default_expr_p_printf_mutable () in
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
      Pbrt.Decoder.unexpected_payload "Message(expr_p_printf), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.f_args <- (decode_expr (Pbrt.Decoder.nested d)) :: v.f_args;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_printf), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !format_is_set then Pbrt.Decoder.missing_field "format" end;
  ({
    Frontend_types.format = v.format;
    Frontend_types.f_args = v.f_args;
  } : Frontend_types.expr_p_printf)

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
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !rexpr_is_set then Pbrt.Decoder.missing_field "rexpr" end;
  begin if not !lexpr_is_set then Pbrt.Decoder.missing_field "lexpr" end;
  begin if not !bin_op_is_set then Pbrt.Decoder.missing_field "bin_op" end;
  ({
    Frontend_types.bin_op = v.bin_op;
    Frontend_types.lexpr = v.lexpr;
    Frontend_types.rexpr = v.rexpr;
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
      v.lhs <- decode_identifier (Pbrt.Decoder.nested d); lhs_is_set := true;
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

and decode_expr_p_if_expr d =
  let v = default_expr_p_if_expr_mutable () in
  let continue__= ref true in
  let else_expr_is_set = ref false in
  let if_expr_is_set = ref false in
  let eval_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.eval <- decode_expr (Pbrt.Decoder.nested d); eval_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_if_expr), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.if_expr <- decode_expr (Pbrt.Decoder.nested d); if_expr_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_if_expr), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.else_expr <- decode_expr (Pbrt.Decoder.nested d); else_expr_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_if_expr), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !else_expr_is_set then Pbrt.Decoder.missing_field "else_expr" end;
  begin if not !if_expr_is_set then Pbrt.Decoder.missing_field "if_expr" end;
  begin if not !eval_is_set then Pbrt.Decoder.missing_field "eval" end;
  ({
    Frontend_types.eval = v.eval;
    Frontend_types.if_expr = v.if_expr;
    Frontend_types.else_expr = v.else_expr;
  } : Frontend_types.expr_p_if_expr)

and decode_expr_p_block d =
  let v = default_expr_p_block_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.expr_list <- List.rev v.expr_list;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.expr_list <- (decode_expr (Pbrt.Decoder.nested d)) :: v.expr_list;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_block), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Frontend_types.expr_list = v.expr_list;
  } : Frontend_types.expr_p_block)

and decode_expr_p_while_expr d =
  let v = default_expr_p_while_expr_mutable () in
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
      Pbrt.Decoder.unexpected_payload "Message(expr_p_while_expr), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.while_block <- decode_expr (Pbrt.Decoder.nested d); while_block_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(expr_p_while_expr), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !while_block_is_set then Pbrt.Decoder.missing_field "while_block" end;
  begin if not !while_cond_is_set then Pbrt.Decoder.missing_field "while_cond" end;
  ({
    Frontend_types.while_cond = v.while_cond;
    Frontend_types.while_block = v.while_block;
  } : Frontend_types.expr_p_while_expr)

let rec decode_program d =
  let v = default_program_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.main <- List.rev v.main;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.main <- (decode_expr (Pbrt.Decoder.nested d)) :: v.main;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(program), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Frontend_types.main = v.main;
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

let rec encode_identifier_p_var (v:Frontend_types.identifier_p_var) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Frontend_types.var_name encoder;
  ()

let rec encode_identifier (v:Frontend_types.identifier) encoder = 
  begin match v with
  | Frontend_types.Var x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_identifier_p_var x) encoder;
  end

let rec encode_type_expr_p_ranked_type (v:Frontend_types.type_expr_p_ranked_type) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Frontend_types.rank encoder;
  ()

let rec encode_type_expr (v:Frontend_types.type_expr) encoder = 
  begin match v with
  | Frontend_types.Int32_ty x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_expr_p_ranked_type x) encoder;
  end

let rec encode_expr_p_var_decl (v:Frontend_types.expr_p_var_decl) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Frontend_types.var_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_type_expr v.Frontend_types.texpr) encoder;
  ()

let rec encode_expr_p_function_app (v:Frontend_types.expr_p_function_app) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Frontend_types.name encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr x) encoder;
  ) v.Frontend_types.args;
  ()

and encode_expr (v:Frontend_types.expr) encoder = 
  begin match v with
  | Frontend_types.Integer x ->
    Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int32_as_varint x encoder;
  | Frontend_types.Function_app x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_function_app x) encoder;
  | Frontend_types.Printf x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_printf x) encoder;
  | Frontend_types.Unop x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_unop x) encoder;
  | Frontend_types.Binop x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_binop x) encoder;
  | Frontend_types.Var_decl x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_var_decl x) encoder;
  | Frontend_types.Assign x ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_assign x) encoder;
  | Frontend_types.Expr_id x ->
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_identifier x) encoder;
  | Frontend_types.If_expr x ->
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_if_expr x) encoder;
  | Frontend_types.Block_expr x ->
    Pbrt.Encoder.key (10, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_block x) encoder;
  | Frontend_types.While_expr x ->
    Pbrt.Encoder.key (11, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr_p_while_expr x) encoder;
  | Frontend_types.Break ->
    Pbrt.Encoder.key (12, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Continue ->
    Pbrt.Encoder.key (13, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Empty ->
    Pbrt.Encoder.key (14, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

and encode_expr_p_printf (v:Frontend_types.expr_p_printf) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Frontend_types.format encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr x) encoder;
  ) v.Frontend_types.f_args;
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
  ()

and encode_expr_p_assign (v:Frontend_types.expr_p_assign) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_identifier v.Frontend_types.lhs) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.rhs) encoder;
  ()

and encode_expr_p_if_expr (v:Frontend_types.expr_p_if_expr) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.eval) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.if_expr) encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.else_expr) encoder;
  ()

and encode_expr_p_block (v:Frontend_types.expr_p_block) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr x) encoder;
  ) v.Frontend_types.expr_list;
  ()

and encode_expr_p_while_expr (v:Frontend_types.expr_p_while_expr) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.while_cond) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.while_block) encoder;
  ()

let rec encode_program (v:Frontend_types.program) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr x) encoder;
  ) v.Frontend_types.main;
  ()
