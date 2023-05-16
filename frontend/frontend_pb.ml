[@@@ocaml.warning "-27-30-39"]

type expr_p_function_app_mutable =
  { mutable name : string
  ; mutable block : Frontend_types.expr list
  }

let default_expr_p_function_app_mutable () : expr_p_function_app_mutable =
  { name = ""; block = [] }
;;

type program_mutable = { mutable main : Frontend_types.expr list }

let default_program_mutable () : program_mutable = { main = [] }

let rec decode_expr_p_function_app d =
  let v = default_expr_p_function_app_mutable () in
  let continue__ = ref true in
  let name_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      v.block <- List.rev v.block;
      continue__ := false
    | Some (1, Pbrt.Bytes) ->
      v.name <- Pbrt.Decoder.string d;
      name_is_set := true
    | Some (1, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(expr_p_function_app), field(1)" pk
    | Some (2, Pbrt.Bytes) -> v.block <- decode_expr (Pbrt.Decoder.nested d) :: v.block
    | Some (2, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(expr_p_function_app), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  if not !name_is_set then Pbrt.Decoder.missing_field "name";
  ({ Frontend_types.name = v.name; Frontend_types.block = v.block }
    : Frontend_types.expr_p_function_app)

and decode_expr d =
  let rec loop () =
    let ret : Frontend_types.expr =
      match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "expr"
      | Some (1, _) ->
        (Frontend_types.Integer (Pbrt.Decoder.int_as_varint d) : Frontend_types.expr)
      | Some (2, _) ->
        (Frontend_types.Function_app (decode_expr_p_function_app (Pbrt.Decoder.nested d))
          : Frontend_types.expr)
      | Some (n, payload_kind) ->
        Pbrt.Decoder.skip d payload_kind;
        loop ()
    in
    ret
  in
  loop ()
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

let rec encode_expr_p_function_app (v : Frontend_types.expr_p_function_app) encoder =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Frontend_types.name encoder;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_expr x) encoder)
    v.Frontend_types.block;
  ()

and encode_expr (v : Frontend_types.expr) encoder =
  match v with
  | Frontend_types.Integer x ->
    Pbrt.Encoder.key (1, Pbrt.Varint) encoder;
    Pbrt.Encoder.int_as_varint x encoder
  | Frontend_types.Function_app x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
    Pbrt.Encoder.nested (encode_expr_p_function_app x) encoder
;;

let rec encode_program (v : Frontend_types.program) encoder =
  List.iter
    (fun x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_expr x) encoder)
    v.Frontend_types.main;
  ()
;;
