module A = Ast
module FT = Frontend_types

let gen_expr e =
  let gexpr = function
    | A.IntLit (i, _) -> FT.Integer i
    | A.OpExp (op, _) ->
      Printf.printf "%s\n" (A.sexp_of_operator op |> Sexplib0.Sexp.to_string);
      FT.Integer (Int32.of_int (-1))
    | _ -> FT.Integer (Int32.of_int (-1))
  in
  gexpr e
;;

let gen_stmt s =
  let gstmt = function
    | A.Output o -> FT.Printf { format = "%d"; f_args = [ gen_expr o ] }
    | _ -> FT.Integer (Int32.of_int (-1))
  in
  gstmt s
;;

let gen_prog A.{ main_decl; _ } =
  let rec gen_main = function
    | [] -> []
    | A.VariableDecl _ :: xs -> FT.Integer (Int32.of_int (-1)) :: gen_main xs
    | A.MainStmt s :: xs -> gen_stmt s :: gen_main xs
  in
  FT.{ main = gen_main main_decl }
;;

let dump fname prog =
  let encoder = Pbrt.Encoder.create () in
  Frontend_pb.encode_program prog encoder;
  let oc = open_out fname in
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc
;;
