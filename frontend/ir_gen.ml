module A = Ast
module FT = Frontend_types
module S = Symbol

let gen_binop = function
  | A.PlusOp -> FT.Plus
  | A.EqualsOp -> FT.Equals
  | A.LessThanOp -> FT.Lessthan
  | _ -> FT.Plus
;;

let gen_expr e =
  let rec gexpr = function
    | A.IntLit (i, _) -> FT.Integer i
    | A.OpExp (A.UnaryOp { oper = A.NegateOp; exp }, _) ->
      FT.Unop { op = FT.Neg; uexpr = gexpr exp }
    | A.OpExp (A.BinaryOp { oper; left; right }, _) ->
      FT.Binop { bin_op = gen_binop oper; lexpr = gexpr left; rexpr = gexpr right }
    | A.Assignment { lhs = SimpleVar ((sym_name, _), _); exp; _ } ->
      FT.Assign { lhs = FT.Var { var_name = sym_name }; rhs = gexpr exp }
    | A.Identifier ((sym_name, _), _) -> FT.Expr_id (FT.Var { var_name = sym_name })
    | _ -> FT.Integer (Int32.of_int (-1))
  in
  gexpr e
;;

let gen_stmt s =
  let rec gstmt = function
    | A.Output o -> FT.Printf { format = "%d"; f_args = [ gen_expr o ] }
    | A.ExprStmt e -> gen_expr e
    | A.IfElse { exp; istmt; estmt } ->
      FT.If_expr { eval = gen_expr exp; if_expr = gstmt istmt; else_expr = gstmt estmt }
    | A.Block xs -> FT.Block_expr { expr_list = List.map gstmt xs }
    | A.While { exp; block } ->
      FT.While_expr { while_cond = gen_expr exp; while_block = gstmt block }
    | A.Break -> FT.Break
    | A.Continue -> FT.Continue
    | _ -> FT.Integer (Int32.of_int (-1))
  in
  gstmt s
;;

exception NoMatchingTypeExpr

let gen_texpr rank = function
  | A.NameTy (typ, _) ->
    (match S.look (Env.base_tenv, typ) with
     | Some Types.INT -> FT.Int32_ty { rank }
     | _ -> raise NoMatchingTypeExpr)
;;

let gen_decls xs =
  let rec gen_dec = function
    | [] -> []
    | A.{ id = sym_name, _sym_id; type_; rank } :: xs ->
      FT.Var_decl { var_id = sym_name; texpr = gen_texpr (Int32.of_int rank) type_ }
      :: gen_dec xs
  in
  gen_dec xs
;;

let gen_prog A.{ main_decl; _ } =
  let rec gen_main = function
    | [] -> []
    | A.VariableDecl decls :: xs -> gen_decls decls @ gen_main xs
    | A.MainStmt s :: xs -> gen_stmt s :: gen_main xs
  in
  FT.{ main = gen_main main_decl }
;;

let dump ast_fname ir_fname prog =
  let oc = open_out ast_fname in
  let out_fmt = Format.str_formatter in
  Frontend_pp.pp_program out_fmt prog;
  let out_str = Format.flush_str_formatter () in
  output_string oc out_str;
  close_out oc;
  let encoder = Pbrt.Encoder.create () in
  Frontend_pb.encode_program prog encoder;
  let oc = open_out ir_fname in
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc
;;
