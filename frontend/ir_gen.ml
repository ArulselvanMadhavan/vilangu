module A = Ast
module FT = Frontend_types

let gen_expr e =
  let rec gexpr = function
    | A.IntLit (i, _) -> FT.Integer i
    | A.OpExp (A.UnaryOp { oper = A.NegateOp; exp }, _) ->
      FT.Unop { op = FT.Neg; uexpr = gexpr exp }
    | A.OpExp (A.BinaryOp { oper = A.PlusOp; left; right }, _) ->
      FT.Binop { bin_op = FT.Plus; lexpr = gexpr left; rexpr = gexpr right }
    | A.Assignment { lhs = SimpleVar ((sym_name, _), _); exp; _ } ->
      FT.Assign { lhs = FT.Var { var_name = sym_name }; rhs = gexpr exp }
    | A.Identifier ((sym_name, _), _) -> FT.Expr_id (FT.Var { var_name = sym_name })
    | _ -> FT.Integer (Int32.of_int (-1))
  in
  gexpr e
;;

let gen_stmt s =
  let gstmt = function
    | A.Output o -> FT.Printf { format = "%d"; f_args = [ gen_expr o ] }
    | A.ExprStmt e -> gen_expr e
    | _ -> FT.Integer (Int32.of_int (-1))
  in
  gstmt s
;;

let gen_decls xs =
  let rec gen_dec = function
    | [] -> []
    | A.{ id = sym_name, _sym_id; _ } :: xs ->
      FT.Var_decl { var_id = sym_name } :: gen_dec xs
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
