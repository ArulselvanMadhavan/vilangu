module A = Ast
module E = Env
module S = Symbol
module T = Types
module Tr = Translate

type venv = E.enventry S.table
type tenv = T.ty S.table

type stmty =
  { stmt : Tr.stmt
  ; ty : T.ty
  ; rank : int
  }

let error pos msg ret =
  Error_msg.error pos msg;
  ret
;;

let err_stmty = { stmt = (); ty = T.NULL; rank = 0 }

let check_rank (ty, exp_rank, got_rank, pos) =
  if Int.equal exp_rank got_rank
  then ()
  else
    error
      pos
      ("rank mismatch for type"
       ^ T.type2str ty
       ^ ". expecting "
       ^ Int.to_string exp_rank
       ^ " , but got "
       ^ Int.to_string got_rank)
      ()
;;

let rec check_type (exp_ty, got_ty, exp_rank, got_rank, pos) =
  let exp_ty' = actual_ty (exp_ty, pos) in
  let got_ty' = actual_ty (got_ty, pos) in
  (* note: checking physical equality *)
  if exp_ty' == got_ty' || exp_ty' == T.NULL || T.NULL == got_ty'
  then check_rank (exp_ty', exp_rank, got_rank, pos)
  else
    error
      pos
      ("type mismatch. "
       ^ "expecting "
       ^ T.type2str exp_ty
       ^ ", but got "
       ^ T.type2str got_ty)
      ()

and actual_ty (ty, pos) =
  let rec walk = function
    | T.NAME (id, ref) ->
      (match !ref with
       | Some ty -> walk ty
       | None -> error pos ("unknown type. " ^ S.name id) T.NULL)
    | ty -> ty
  in
  walk ty
;;

let trans_dec (venv, tenv, A.{ type_ = NameTy (typ, pos); id; rank }) =
  match S.look (tenv, typ) with
  | None -> error pos ("unknown type." ^ S.name typ) (venv, tenv, err_stmty)
  | Some res_ty ->
    let venv' = S.enter (venv, id, E.VarEntry { ty = res_ty; rank }) in
    venv', tenv, { stmt = (); ty = T.UNIT; rank = 0 }
;;

let trans_var (venv, var) =
  match var with
  | A.SimpleVar (id, pos) ->
    (match S.look (venv, id) with
     | Some (E.VarEntry { ty; rank }) -> { stmt = (); ty; rank }
     | Some _ -> error pos "expecting a variable, not a function" err_stmty
     | None -> error pos ("undefined variable " ^ S.name id) err_stmty)
  | _ -> err_stmty
;;

let rec trans_exp (venv, tenv, exp) =
  match exp with
  | A.Assignment { lhs; exp; pos } ->
    let { ty = var_ty; rank = vrank; _ } = trans_var (venv, lhs) in
    let { ty = exp_ty; rank = erank; _ } = trans_exp (venv, tenv, exp) in
    check_type (var_ty, exp_ty, vrank, erank, pos);
    (* TODO: check rank *)
    { stmt = (); ty = T.UNIT; rank = 0 }
  | A.Identifier (id, pos) -> trans_var (venv, A.SimpleVar (id, pos))
  | _ -> err_stmty
;;

let trans_stmt (venv, tenv, stmt) : stmty =
  match stmt with
  | A.ExprStmt e -> trans_exp (venv, tenv, e)
  | _ -> err_stmty
;;

let trans_vars (venv, tenv, vars) : venv * tenv * stmty =
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

let trans_main (venv, tenv, main_stmts) : stmty =
  let tr_main (venv, tenv, stmt) =
    match stmt with
    | A.VariableDecl vars -> trans_vars (venv, tenv, vars)
    | A.MainStmt stmt -> venv, tenv, trans_stmt (venv, tenv, stmt)
  in
  let _, _, stmty =
    Base.List.fold
      ~init:(venv, tenv, [])
      ~f:(fun (venv, tenv, xs) main_stmt ->
        let venv, tenv, stmty = tr_main (venv, tenv, main_stmt) in
        venv, tenv, stmty :: xs)
      main_stmts
  in
  Base.List.last_exn stmty
;;

let trans_class (tenv, class_decs) =
  let tr_class (tenv, A.ClassDec { name; _ }) =
    let ctype = T.NAME (name, ref None) in
    let tenv = S.enter (tenv, name, ctype) in
    tenv, { stmt = (); ty = ctype; rank = 0 }
  in
  Base.List.fold class_decs ~init:(tenv, []) ~f:(fun (tenv, xs) cdec ->
    let tenv, x = tr_class (tenv, cdec) in
    tenv, x :: xs)
;;

let trans_prog comp_unit =
  let A.{ main_decl; classdecs } = comp_unit in
  let tenv, _ = trans_class (E.base_tenv, classdecs) in
  trans_main (E.base_venv, tenv, main_decl)
;;