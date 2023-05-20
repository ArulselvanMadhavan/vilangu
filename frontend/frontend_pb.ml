[@@@ocaml.warning "-27-30-39"]

type expr_p_function_app_mutable =
  { mutable name : string
  ; mutable args : Frontend_types.expr list
  }

let default_expr_p_function_app_mutable () : expr_p_function_app_mutable =
  { name = ""; args = [] }
;;

type expr_p_printf_mutable =
  { mutable format : string
  ; mutable f_args : Frontend_types.expr list
  }

let default_expr_p_printf_mutable () : expr_p_printf_mutable =
  { format = ""; f_args = [] }
;;

type expr_p_unop_mutable =
  { mutable op : Frontend_types.un_op
  ; mutable uexpr : Frontend_types.expr
  }

let default_expr_p_unop_mutable () : expr_p_unop_mutable =
  { op = Frontend_types.default_un_op (); uexpr = Frontend_types.default_expr () }
;;

type program_mutable = { mutable main : Frontend_types.expr list }

let default_program_mutable () : program_mutable = { main = [] }

let rec decode_un_op d =
  let rec loop () =
    let ret : Frontend_types.un_op =
      match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "un_op"
      | Some (1, _) ->
        Pbrt.Decoder.empty_nested d;
        (Frontend_types.Not : Frontend_types.un_op)
      | Some (2, _) ->
        Pbrt.Decoder.empty_nested d;
        (Frontend_types.Neg : Frontend_types.un_op)
      | Some (n, payload_kind) ->
        Pbrt.Decoder.skip d payload_kind;
        loop ()
    in
    ret
  in
  loop ()
;;

let rec decode_expr_p_function_app d =
  let v = default_expr_p_function_app_mutable () in
  let continue__ = ref true in
  let name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      v.args <- List.rev v.args;
      continue__ := false
    | Some (1, Pbrt.Bytes) ->
      v.name <- Pbrt.Decoder.string d;
      name_is_set := true
    | Some (1, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(expr_p_function_app), field(1)" pk
    | Some (2, Pbrt.Bytes) -> v.args <- decode_expr (Pbrt.Decoder.nested d) :: v.args
    | Some (2, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(expr_p_function_app), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  if not !name_is_set then Pbrt.Decoder.missing_field "name";
  ({ Frontend_types.name = v.name; Frontend_types.args = v.args }
    : Frontend_types.expr_p_function_app)

and decode_expr d =
  let rec loop () =
    let ret : Frontend_types.expr =
      match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "expr"
      | Some (1, _) ->
        (Frontend_types.Integer (Pbrt.Decoder.int32_as_varint d) : Frontend_types.expr)
      | Some (2, _) ->
        (Frontend_types.Function_app (decode_expr_p_function_app (Pbrt.Decoder.nested d))
          : Frontend_types.expr)
      | Some (3, _) ->
        (Frontend_types.Printf (decode_expr_p_printf (Pbrt.Decoder.nested d))
          : Frontend_types.expr)
      | Some (4, _) ->
        (Frontend_types.Unop (decode_expr_p_unop (Pbrt.Decoder.nested d))
          : Frontend_types.expr)
      | Some (n, payload_kind) ->
        Pbrt.Decoder.skip d payload_kind;
        loop ()
    in
    ret
  in
  loop ()

and decode_expr_p_printf d =
  let v = default_expr_p_printf_mutable () in
  let continue__ = ref true in
  let format_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      v.f_args <- List.rev v.f_args;
      continue__ := false
    | Some (1, Pbrt.Bytes) ->
      v.format <- Pbrt.Decoder.string d;
      format_is_set := true
    | Some (1, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(expr_p_printf), field(1)" pk
    | Some (2, Pbrt.Bytes) -> v.f_args <- decode_expr (Pbrt.Decoder.nested d) :: v.f_args
    | Some (2, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(expr_p_printf), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  if not !format_is_set then Pbrt.Decoder.missing_field "format";
  ({ Frontend_types.format = v.format; Frontend_types.f_args = v.f_args }
    : Frontend_types.expr_p_printf)

and decode_expr_p_unop d =
  let v = default_expr_p_unop_mutable () in
  let continue__ = ref true in
  let uexpr_is_set = ref false in
  let op_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      ();
      continue__ := false
    | Some (1, Pbrt.Bytes) ->
      v.op <- decode_un_op (Pbrt.Decoder.nested d);
      op_is_set := true
    | Some (1, pk) -> Pbrt.Decoder.unexpected_payload "Message(expr_p_unop), field(1)" pk
    | Some (2, Pbrt.Bytes) ->
      v.uexpr <- decode_expr (Pbrt.Decoder.nested d);
      uexpr_is_set := true
    | Some (2, pk) -> Pbrt.Decoder.unexpected_payload "Message(expr_p_unop), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  if not !uexpr_is_set then Pbrt.Decoder.missing_field "uexpr";
  if not !op_is_set then Pbrt.Decoder.missing_field "op";
  ({ Frontend_types.op = v.op; Frontend_types.uexpr = v.uexpr }
    : Frontend_types.expr_p_unop)
;;

let rec decode_program d =
  let v = default_program_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      v.main <- List.rev v.main;
      continue__ := false
    | Some (1, Pbrt.Bytes) -> v.main <- decode_expr (Pbrt.Decoder.nested d) :: v.main
    | Some (1, pk) -> Pbrt.Decoder.unexpected_payload "Message(program), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({ Frontend_types.main = v.main } : Frontend_types.program)
;;

let rec encode_un_op (v : Frontend_types.un_op) encoder =
  match v with
  | Frontend_types.Not ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
    Pbrt.Encoder.empty_nested encoder
  | Frontend_types.Neg ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
    Pbrt.Encoder.empty_nested encoder
;;

let rec encode_expr_p_function_app (v : Frontend_types.expr_p_function_app) encoder =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Frontend_types.name encoder;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_expr x) encoder)
    v.Frontend_types.args;
  ()

and encode_expr (v : Frontend_types.expr) encoder =
  match v with
  | Frontend_types.Integer x ->
    Pbrt.Encoder.key (1, Pbrt.Varint) encoder;
    Pbrt.Encoder.int32_as_varint x encoder
  | Frontend_types.Function_app x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
    Pbrt.Encoder.nested (encode_expr_p_function_app x) encoder
  | Frontend_types.Printf x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
    Pbrt.Encoder.nested (encode_expr_p_printf x) encoder
  | Frontend_types.Unop x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder;
    Pbrt.Encoder.nested (encode_expr_p_unop x) encoder

and encode_expr_p_printf (v : Frontend_types.expr_p_printf) encoder =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Frontend_types.format encoder;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_expr x) encoder)
    v.Frontend_types.f_args;
  ()

and encode_expr_p_unop (v : Frontend_types.expr_p_unop) encoder =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.nested (encode_un_op v.Frontend_types.op) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
  Pbrt.Encoder.nested (encode_expr v.Frontend_types.uexpr) encoder;
  ()
;;

let rec encode_program (v : Frontend_types.program) encoder =
  List.iter
    (fun x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_expr x) encoder)
    v.Frontend_types.main;
  ()
;;
