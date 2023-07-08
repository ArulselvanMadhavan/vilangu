module A = Ast
module E = Env
module S = Symbol
module T = Types
module Tr = Translate

type venv = E.enventry S.table
type tenv = T.ty S.table

type 'a stmty =
  { stmt : 'a
  ; ty : T.ty
  }

let error pos msg ret =
  Error_msg.error pos msg;
  ret
;;

let loop_nest_count = ref 0
let err_stmty stmt = { stmt; ty = T.NULL }
let err_stmty_unit = { stmt = (); ty = T.NULL }

(* Possible options
   1. Alter the ast cast exprs to include information about the cast; Widening/Narrow ->
   2. Do we widening/narrow information? saves a call to Object_IsA
   3. Can we do the assignment cast check at IR_gen step.
   4. Run assignment cast check once again - 
*)

exception AstTypeNotFound

let get_ast_type pos = function
  | T.INT -> A.Primitive (Env.int_symbol, pos)
  | _ -> raise AstTypeNotFound
;;

let widen_array lty =
  match lty with
  | Some (A.Reference (A.ArrayType (rank, lty))) -> Some (A.Reference (A.ArrayType (rank + 1, lty)))
  | Some (A.Reference (A.ClassType _) as lty) -> Some (A.Reference (A.ArrayType (1, lty)))
  | Some _ as x -> x            (* No widening of non-primitive type *)
  | None -> None
;;

let adjust_lty cast_ty lty =
  match cast_ty with
  | Some A.Wide -> Some A.Wide, widen_array lty
  | _ -> cast_ty, lty
;;

(* Check lhs type with rhs type. If possible, find cast type. For wide casts. Find the recommended lhs cast *)
let rec compare_and_cast pos lhs_ty rhs_ty =
  match lhs_ty, rhs_ty with
  | T.ARRAY (lrank, lty), T.ARRAY (rrank, rty) ->
    let get_ty rank ty =
      let rank = rank - 1 in
      if rank > 0 then T.ARRAY (rank, ty) else ty
    in
    let cast_ty, lty = compare_and_cast pos (get_ty lrank lty) (get_ty rrank rty) in
    adjust_lty cast_ty lty
  | T.NAME (sym, _), T.ARRAY _ when sym = Env.obj_symbol ->
    Some A.Wide, Some (A.Reference (A.ClassType (sym, pos)))
  | T.NAME _, T.ARRAY _ ->
    None, None (* No cast when rhs is Array and lhs is not object *)
  | T.INT, T.INT ->
    (* Possible via recursive element type checks *)
    Some A.Identity, None
  | T.ARRAY _, T.NAME (sym, _) when sym = Env.obj_symbol -> Some A.Narrow, None
  | _, _ -> None, None
;;

let assignment_cast (lhs_ty, rhs_ty, pos, rhs_exp) =
  let check_cast lhs_ty rhs_ty =
    match compare_and_cast pos lhs_ty rhs_ty with
    | Some A.Wide, Some lhs_ty ->
      A.CastType { type_ = lhs_ty; exp = rhs_exp; cast_type = Some A.Wide; pos }
    (* | T.ARRAY (rank, ty), T.NAME (sym, _) when sym = Env.obj_symbol -> *)
    (*     A.CastType *)
    (*     { type_ = A.Reference (A.ArrayType (rank, get_ast_type pos ty)) *)
    (*     ; exp = rhs_exp *)
    (*     ; cast_type = Some A.Narrow *)
    (*     } *)
    | _ ->
      error
        pos
        ("type mismatch. Assignment cast not possible. expecting "
         ^ T.type2str lhs_ty
         ^ ", but got "
         ^ T.type2str rhs_ty)
        rhs_exp
  in
  if T.type_match lhs_ty rhs_ty then rhs_exp else check_cast lhs_ty rhs_ty
;;

let check_type (exp_ty, got_ty, pos) =
  if T.type_match exp_ty got_ty
  then ()
  else
    error
      pos
      ("type mismatch. "
       ^ "expecting "
       ^ T.type2str exp_ty
       ^ ", but got "
       ^ T.type2str got_ty)
      ()
;;

let type_name_lookup tenv typ pos =
  match S.look (tenv, typ) with
  | None -> error pos ("unknown type." ^ S.name typ) T.NULL
  | Some res_ty -> res_ty
;;

let rec trans_type tenv = function
  | A.Primitive (typ, pos) -> type_name_lookup tenv typ pos
  | A.Reference (A.ArrayType (rank, typ)) ->
    let res = trans_type tenv typ in
    T.ARRAY (rank, res)
  | A.Reference (A.ClassType (typ, pos)) -> type_name_lookup tenv typ pos
;;

let trans_dec (venv, tenv, A.{ type_; id }) =
  let ty = trans_type tenv type_ in
  (* If the base type exists, then ranks > 0 also exist *)
  let venv' = S.enter (venv, id, E.VarEntry { ty }) in
  venv', tenv, { stmt = (); ty = T.VOID }
;;

let rec trans_var (venv, tenv, var) =
  match var with
  | A.SimpleVar (id, pos) ->
    (match S.look (venv, id) with
     | Some (E.VarEntry { ty }) -> { stmt = (); ty }
     | Some _ -> error pos "expecting a variable, not a function" err_stmty_unit
     | None -> error pos ("undefined variable " ^ S.name id) err_stmty_unit)
  | A.SubscriptVar (var, exp, pos) ->
    let { ty = var_ty; _ } = trans_var (venv, tenv, var) in
    let { ty = exp_ty; _ } = trans_exp (venv, tenv, exp) in
    let res_ty =
      match var_ty with
      | T.ARRAY (rank, low_ty) when rank > 1 ->
        { ty = T.ARRAY (rank - 1, low_ty); stmt = () }
      | T.ARRAY (rank, low_ty) when rank > 0 -> { ty = low_ty; stmt = () }
      | _ -> error pos "Subscript access on non-array type" err_stmty_unit
    in
    if T.is_int exp_ty
    then res_ty
    else error pos "subscript access with non-int dim" err_stmty_unit
  | A.FieldVar (exp, (sym_name, _), pos) ->
    let { ty = exp_ty; _ } = trans_exp (venv, tenv, exp) in
    if T.is_int exp_ty
    then error pos "field access on non-object type" err_stmty_unit
    else if not (String.equal sym_name "length")
    then error pos ("unknown field " ^ sym_name) err_stmty_unit
    else { (* This has to be length *)
           ty = T.INT; stmt = () }
  | A.LoadVar var -> trans_var (venv, tenv, var)

and trans_exp (venv, tenv, exp) =
  match exp with
  | A.Assignment { lhs; exp; pos } ->
    let { ty = var_ty; _ } = trans_var (venv, tenv, lhs) in
    let { ty = exp_ty; stmt = rhs_exp } = trans_exp (venv, tenv, exp) in
    let rhs_exp = assignment_cast (var_ty, exp_ty, pos, rhs_exp) in
    { stmt = A.Assignment { lhs; exp = rhs_exp; pos }; ty = T.VOID }
  | A.Identifier (id, pos) as exp ->
    let { ty; _ } = trans_var (venv, tenv, A.SimpleVar (id, pos)) in
    { stmt = exp; ty }
  | A.OpExp (A.BinaryOp { left; oper; right }, pos) as exp ->
    let lexp = trans_exp (venv, tenv, left) in
    let rexp = trans_exp (venv, tenv, right) in
    if T.type_match lexp.ty rexp.ty
    then { ty = lexp.ty; stmt = exp }
    else (
      let oper_str = A.sexp_of_bioper oper |> Sexplib0.Sexp.to_string_hum in
      error pos (oper_str ^ " operand types don't match") (err_stmty exp))
  | A.OpExp (A.UnaryOp { oper; exp = unary_exp }, pos) as exp ->
    let { stmt = unary_exp; ty } = trans_exp (venv, tenv, unary_exp) in
    if T.is_int ty
    then { stmt = A.OpExp (A.UnaryOp { oper; exp = unary_exp }, pos); ty }
    else (
      let oper_str = A.sexp_of_uoper oper |> Sexplib0.Sexp.to_string_hum in
      error
        pos
        (oper_str ^ " is applied on incompatible type: " ^ T.type2str ty)
        (err_stmty exp))
  | A.IntLit _ as exp -> { ty = T.INT; stmt = exp }
  | A.ArrayCreationExp { type_; exprs; pos } as exp ->
    let expr_rank = List.length exprs in
    let is_int acc exp =
      let { ty = res_ty; _ } = trans_exp (venv, tenv, exp) in
      T.is_int res_ty && acc
    in
    let is_int_exprs = List.fold_left is_int true exprs in
    if is_int_exprs
    then (
      let type_ = A.append_rank_to_type expr_rank type_ in
      let ty = trans_type tenv type_ in
      { stmt = exp; ty })
    else (error pos "Array Creation Expr has non int dim") (err_stmty exp)
  | A.VarExp (v, _) as exp ->
    let { ty; _ } = trans_var (venv, tenv, v) in
    { stmt = exp; ty }
  | A.CastType { type_; exp; pos; _ } as cexp ->
    let { stmt; ty = exp_ty } = trans_exp (venv, tenv, exp) in
    let cast_ty = trans_type tenv type_ in
    (* obj | i32arr *)
    let dummy_pos = (-1, -1), (-1, -1) in
    let stmt =
      match compare_and_cast dummy_pos cast_ty exp_ty with
      | Some A.Wide, _ -> A.CastType { type_; exp = stmt; cast_type = Some A.Wide; pos }
      | Some A.Narrow, _ ->
        A.CastType { type_; exp = stmt; cast_type = Some A.Narrow; pos }
      | Some A.Identity, _ ->
        A.CastType { type_; exp = stmt; cast_type = Some A.Identity; pos }
      | None, _ -> error pos "cast not possible" cexp
    in
    { stmt; ty = cast_ty }
  | _ as exp -> err_stmty exp
;;

let inside_loop_check stmt pos =
  let stmt_str = A.sexp_of_stmt stmt |> Sexplib0.Sexp.to_string_hum in
  if !loop_nest_count = 0
  then error pos (stmt_str ^ "statement not directly inside a loop") (err_stmty stmt)
  else { stmt; ty = T.VOID }
;;

let rec trans_stmt (venv, tenv, stmt) : 'a stmty =
  match stmt with
  | A.ExprStmt e ->
    let { stmt; ty } = trans_exp (venv, tenv, e) in
    { stmt = A.ExprStmt stmt; ty }
  | A.While { exp; block } ->
    let { stmt = tr_exp; _ } = trans_exp (venv, tenv, exp) in
    loop_nest_count := !loop_nest_count + 1;
    let { stmt = block; ty } = trans_stmt (venv, tenv, block) in
    loop_nest_count := !loop_nest_count - 1;
    { stmt = A.While { exp = tr_exp; block }; ty }
  | A.Output (e, pos) as stmt ->
    let { stmt = exp; ty } = trans_exp (venv, tenv, e) in
    if T.is_int ty
    then { stmt = A.Output (exp, pos); ty }
    else error pos "Output statement doesn't have an int expression" (err_stmty stmt)
  | A.Continue pos as stmt -> inside_loop_check stmt pos
  | A.Break pos as stmt -> inside_loop_check stmt pos
  | A.Block xs as stmt ->
    let _ = trans_blk (venv, tenv) xs in
    { stmt; ty = T.VOID }
  | A.IfElse { exp; istmt; estmt; pos } ->
    let { stmt = tr_exp; _ } = trans_exp (venv, tenv, exp) in
    let ires = trans_stmt (venv, tenv, istmt) in
    let eres = trans_stmt (venv, tenv, estmt) in
    if T.type_match ires.ty eres.ty
    then { stmt = A.IfElse { exp = tr_exp; istmt; estmt; pos }; ty = ires.ty }
    else error pos "If and else branch types don't match" (err_stmty stmt)
  | _ -> err_stmty stmt

and trans_blk (venv, tenv) = function
  | [] -> []
  | x :: xs -> trans_stmt (venv, tenv, x) :: trans_blk (venv, tenv) xs
;;

let trans_vars (venv, tenv, vars) : venv * tenv * 'a stmty =
  let venv, tenv, stmtys =
    Base.List.fold
      ~init:(venv, tenv, [])
      ~f:(fun (venv, tenv, xs) dec ->
        let venv, tenv, stmty = trans_dec (venv, tenv, dec) in
        venv, tenv, stmty :: xs)
      vars
  in
  venv, tenv, Base.List.last_exn stmtys
;;

let trans_main (venv, tenv, main_stmts) =
  let tr_main (venv, tenv, stmt) =
    match stmt with
    | A.VariableDecl vars ->
      let venv, tenv, { ty; _ } = trans_vars (venv, tenv, vars) in
      venv, tenv, { stmt = A.VariableDecl vars; ty }
    | A.MainStmt stmt ->
      let { stmt; ty } = trans_stmt (venv, tenv, stmt) in
      venv, tenv, { stmt = A.MainStmt stmt; ty }
  in
  let venv, tenv, stmtys =
    Base.List.fold
      ~init:(venv, tenv, [])
      ~f:(fun (venv, tenv, xs) main_stmt ->
        let venv, tenv, stmty = tr_main (venv, tenv, main_stmt) in
        venv, tenv, stmty :: xs)
      main_stmts
  in
  let stmtys = List.rev stmtys in
  venv, tenv, List.map (fun { stmt; _ } -> stmt) stmtys
;;

let trans_class (tenv, class_decs) =
  let tr_class (tenv, A.ClassDec { name; _ }) =
    let ctype = T.NAME (name, ref None) in
    let tenv = S.enter (tenv, name, ctype) in
    tenv, { stmt = (); ty = ctype }
  in
  Base.List.fold class_decs ~init:(tenv, []) ~f:(fun (tenv, xs) cdec ->
    let tenv, x = tr_class (tenv, cdec) in
    tenv, x :: xs)
;;

let trans_prog comp_unit =
  let A.{ main_decl; class_decs } = comp_unit in
  let tenv, _ = trans_class (E.base_tenv, class_decs) in
  (* Add class defs to module *)
  let venv, tenv, tr_main = trans_main (E.base_venv, tenv, main_decl) in
  (* Add array types to module *)
  (* let arr_classes = array_class_decs venv in *)
  venv, tenv, A.{ main_decl = tr_main; class_decs }
;;
