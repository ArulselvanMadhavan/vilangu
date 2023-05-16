module A = Ast
module FT = Frontend_types

let gen_expr e =
  let gexpr = function
    | A.IntLit (i, _) -> FT.Integer i
    | _ -> FT.Integer (-1)
  in
  gexpr e
;;

let gen_stmt s =
  let gstmt = function
    | A.Output o -> FT.Function_app { name = "out"; block = [ gen_expr o ] }
    | _ -> FT.Integer (-1)
  in
  gstmt s
;;

let gen_prog A.{ main_decl; _ } =
  let rec gen_main = function
    | [] -> []
    | A.VariableDecl _ :: xs -> FT.Integer (-1) :: gen_main xs
    | A.MainStmt s :: xs -> gen_stmt s :: gen_main xs
  in
  let prog = FT.{ main = gen_main main_decl } in
  let encoder = Pbrt.Encoder.create () in
  Frontend_pb.encode_program prog encoder
;;
