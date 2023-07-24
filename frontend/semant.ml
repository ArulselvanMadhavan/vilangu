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
exception ThisTypeNotFound
exception SuperTypeNotFound
exception NameRefToNameException
exception ExpectedClassType

let get_ast_type pos = function
  | T.INT -> A.Primitive (Env.int_symbol, pos)
  | _ -> raise AstTypeNotFound
;;

let widen_array lty =
  match lty with
  | Some (A.Reference (A.ArrayType (rank, lty))) ->
    Some (A.Reference (A.ArrayType (rank + 1, lty)))
  | Some (A.Reference (A.ClassType _) as lty) -> Some (A.Reference (A.ArrayType (1, lty)))
  | Some _ as x -> x (* No widening of non-primitive type *)
  | None -> None
;;

let adjust_lty cast_ty lty =
  match cast_ty with
  | Some A.Wide -> Some A.Wide, widen_array lty
  | _ -> cast_ty, lty
;;

let rec is_subclass tenv lhs_ty rhs_ty =
  match rhs_ty with
  | T.NAME _ when T.type_match lhs_ty rhs_ty -> true
  | T.NAME (_, _, Some base, _) ->
    Base.Option.fold
      ~init:false
      ~f:(fun _ -> is_subclass tenv lhs_ty)
      (S.look (tenv, base))
  | _ -> false
;;

(* Check lhs type with rhs type. If possible, find cast type. For wide casts. Find the recommended lhs cast *)
let rec compare_and_cast tenv pos lhs_ty rhs_ty =
  match lhs_ty, rhs_ty with
  | T.ARRAY (lrank, lty), T.ARRAY (rrank, rty) ->
    let get_ty rank ty =
      let rank = rank - 1 in
      if rank > 0 then T.ARRAY (rank, ty) else ty
    in
    let cast_ty, lty = compare_and_cast tenv pos (get_ty lrank lty) (get_ty rrank rty) in
    adjust_lty cast_ty lty
  | T.NAME (sym, _, _, _), T.ARRAY _ when sym = Env.obj_symbol ->
    Some A.Wide, Some (A.Reference (A.ClassType (sym, pos)))
  | T.NAME _, T.ARRAY _ ->
    None, None (* No cast when rhs is Array and lhs is not object *)
  | T.ARRAY _, T.NAME (sym, _, _, _) when sym = Env.obj_symbol -> Some A.Narrow, None
  | T.NAME (sym1, _, _, _), T.NAME (sym2, _, _, _) when sym1 = sym2 ->
    Some A.Identity, None
  | T.NAME (lhs_name, _, _, _), T.NAME _ when is_subclass tenv lhs_ty rhs_ty ->
    Some A.Wide, Some (A.Reference (A.ClassType (lhs_name, pos)))
  | T.NAME _, T.NAME _ when is_subclass tenv rhs_ty lhs_ty -> Some A.Narrow, None
  | T.NAME (lhs_name, _, _, _), T.NULL ->
    Some A.Wide, Some (A.Reference (A.ClassType (lhs_name, pos)))
  | T.INT, T.INT ->
    (* Possible via recursive element type checks *)
    Some A.Identity, None
  | _, _ -> None, None
;;

let assignment_cast tenv (lhs_ty, rhs_ty, pos, rhs_exp) =
  let check_cast lhs_ty rhs_ty =
    match compare_and_cast tenv pos lhs_ty rhs_ty with
    | Some A.Wide, Some lhs_ty ->
      A.CastType { type_ = lhs_ty; exp = rhs_exp; cast_type = Some A.Wide; pos }
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
  let is_null = if T.is_int ty || T.is_array ty then false else true in
  let venv' = S.enter (venv, id, E.VarEntry { ty; is_null }) in
  venv', tenv, { stmt = (); ty = T.VOID }
;;

let is_init = function
  | A.ArrayCreationExp _ -> true
  | A.ClassCreationExp _ -> true
  | _ -> false
;;

let rec trans_var ?(lhs = false) ?(is_init = false) (venv, tenv, var) =
  match var with
  | A.SimpleVar (id, pos) as subvar ->
    let venv' = if is_init then S.update id E.update_null venv else venv in
    (match S.look (venv', id) with
     | Some (E.VarEntry { ty; is_null }) ->
       if lhs || not is_null
       then venv', { stmt = subvar; ty }
       else venv', { stmt = subvar; ty = T.NULL }
     | Some _ -> venv, error pos "expecting a variable, not a function" (err_stmty subvar)
     | None -> venv, error pos ("undefined variable " ^ S.name id) (err_stmty subvar))
  | A.SubscriptVar (var, exp, pos) as subvar ->
    let venv, { ty = exp_ty; stmt = exp } = trans_exp (venv, tenv, exp) in
    let venv, { ty = var_ty; stmt = var } = trans_var (venv, tenv, var) in
    let res_ty =
      match var_ty with
      | T.ARRAY (rank, low_ty) when rank > 1 ->
        venv, { ty = T.ARRAY (rank - 1, low_ty); stmt = A.SubscriptVar (var, exp, pos) }
      | T.ARRAY (rank, low_ty) when rank > 0 ->
        venv, { ty = low_ty; stmt = A.SubscriptVar (var, exp, pos) }
      | _ -> venv, error pos "Subscript access on non-array type" (err_stmty subvar)
    in
    if T.is_int exp_ty
    then res_ty
    else venv, error pos "subscript access with non-int dim" (err_stmty subvar)
  | A.FieldVar (exp, (sym_name, sym_id), _, pos) as var ->
    let venv, { ty = exp_ty; stmt = exp } = trans_exp (venv, tenv, exp) in
    let check_field class_name fields =
      let len = List.length fields in
      let map_field idx ((name, _), ty) =
        if String.equal name sym_name then Some (len - idx, ty) else None
      in
      let field_opt = Base.List.find_mapi ~f:map_field (List.rev fields) in
      match field_opt with
      | Some (idx, ty) ->
        venv, { ty; stmt = A.FieldVar (exp, (sym_name, sym_id), idx, pos) }
      | None ->
        ( venv
        , error
            pos
            ("unknown field " ^ sym_name ^ " in class " ^ class_name)
            (err_stmty var) )
    in
    (match exp_ty with
     | T.ARRAY _ ->
       if not (String.equal sym_name "length")
       then venv, error pos ("unknown field " ^ sym_name) (err_stmty var)
       else venv, { ty = T.INT; stmt = A.FieldVar (exp, (sym_name, sym_id), 2, pos) }
     | T.NAME ((class_name, _), fields, _, _) -> check_field class_name fields
     | _ ->
       ( venv
       , error
           pos
           ("field access on non-object type " ^ T.type2str exp_ty)
           (err_stmty var) ))
  | A.LoadVar var -> trans_var (venv, tenv, var)

and trans_exp (venv, tenv, exp) =
  match exp with
  | A.Assignment { lhs; exp; pos } when is_init exp ->
    let venv, { ty = exp_ty; stmt = rhs_exp } = trans_exp (venv, tenv, exp) in
    let venv, { ty = var_ty; stmt = lhs_var } =
      trans_var ~lhs:true ~is_init:true (venv, tenv, lhs)
    in
    let rhs_exp = assignment_cast tenv (var_ty, exp_ty, pos, rhs_exp) in
    venv, { stmt = A.Assignment { lhs = lhs_var; exp = rhs_exp; pos }; ty = T.VOID }
  | A.Assignment { lhs; exp; pos } ->
    let venv, { ty = var_ty; stmt = lhs_var } =
      trans_var ~lhs:true ~is_init:true (venv, tenv, lhs)
    in
    let venv, { ty = exp_ty; stmt = rhs_exp } = trans_exp (venv, tenv, exp) in
    let rhs_exp = assignment_cast tenv (var_ty, exp_ty, pos, rhs_exp) in
    venv, { stmt = A.Assignment { lhs = lhs_var; exp = rhs_exp; pos }; ty = T.VOID }
  (* | A.Identifier (id, pos) as exp -> *)
  (*   let venv, { ty; _ } = trans_var (venv, tenv, A.SimpleVar (id, pos)) in *)
  (*   venv, { stmt = exp; ty } *)
  | A.OpExp (A.BinaryOp { left; oper; right }, pos) ->
    let venv, { stmt = lexp; ty = lty } = trans_exp (venv, tenv, left) in
    let venv, { stmt = rexp; ty = rty } = trans_exp (venv, tenv, right) in
    if T.type_match lty rty
    then
      ( venv
      , { ty = lty; stmt = A.OpExp (A.BinaryOp { left = lexp; oper; right = rexp }, pos) }
      )
    else (
      let oper_str = A.sexp_of_bioper oper |> Sexplib0.Sexp.to_string_hum in
      venv, error pos (oper_str ^ " operand types don't match") (err_stmty exp))
  | A.OpExp (A.UnaryOp { oper; exp = unary_exp }, pos) ->
    let venv, { stmt = unary_exp; ty } = trans_exp (venv, tenv, unary_exp) in
    if T.is_int ty
    then venv, { stmt = A.OpExp (A.UnaryOp { oper; exp = unary_exp }, pos); ty }
    else (
      let oper_str = A.sexp_of_uoper oper |> Sexplib0.Sexp.to_string_hum in
      ( venv
      , error
          pos
          (oper_str ^ " is applied on incompatible type: " ^ T.type2str ty)
          (err_stmty exp) ))
  | A.IntLit _ as exp -> venv, { ty = T.INT; stmt = exp }
  | A.ArrayCreationExp { type_; exprs; pos; _ } as acexp ->
    (* let expr_rank = List.length exprs in *)
    let is_int acc exp =
      let _venv, { ty = res_ty; _ } = trans_exp (venv, tenv, exp) in
      T.is_int res_ty && acc
    in
    let is_int_exprs = List.fold_left is_int true exprs in
    if is_int_exprs
    then (
      let ty = trans_type tenv type_ in
      (* Constructor is at vtable index 5 *)
      venv, { stmt = A.ArrayCreationExp { type_; exprs; pos; vtbl_idx = Some 5 }; ty })
    else venv, (error pos "Array Creation Expr has non int dim") (err_stmty acexp)
  | A.VarExp (v, pos) ->
    let venv, { ty; stmt } = trans_var (venv, tenv, v) in
    venv, { stmt = A.VarExp (stmt, pos); ty }
  | A.CastType { type_; exp; pos; _ } as cexp ->
    let venv, { stmt; ty = exp_ty } = trans_exp (venv, tenv, exp) in
    let cast_ty = trans_type tenv type_ in
    (* obj | i32arr *)
    let stmt =
      match compare_and_cast tenv A.default_pos cast_ty exp_ty with
      | Some A.Wide, _ -> A.CastType { type_; exp = stmt; cast_type = Some A.Wide; pos }
      | Some A.Narrow, _ ->
        A.CastType { type_; exp = stmt; cast_type = Some A.Narrow; pos }
      | Some A.Identity, _ ->
        A.CastType { type_; exp = stmt; cast_type = Some A.Identity; pos }
      | None, _ -> error pos "cast not possible" cexp
    in
    venv, { stmt; ty = cast_ty }
  | A.ClassCreationExp { type_; args; pos; _ } as orig ->
    let ty = trans_type tenv type_ in
    let _, args_stmty =
      Base.List.fold
        ~init:(venv, [])
        ~f:(fun (venv, acc) e ->
          let venv, res = trans_exp (venv, tenv, e) in
          venv, res :: acc)
        args
    in
    let args = List.map (fun { stmt; _ } -> stmt) args_stmty in
    let args_ty = List.map (fun { ty; _ } -> T.type2str ty) args_stmty in
    let args_ty = T.type2str ty :: args_ty in
    (* add this as first arg *)
    let method_name = Ir_gen_env.vtable_method_name (T.type2str ty) args_ty in
    let vtbl_idx = Ir_gen_env.find_vtable_index ty method_name in
    let on_success v =
      { ty; stmt = A.ClassCreationExp { type_; args; pos; vtbl_idx = Some v } }
    in
    let on_error m () = error pos (m ^ "method not found in vtbl.") (err_stmty orig) in
    (* offset vtbl idx by 2 to account for RTTI *)
    ( venv
    , Base.Option.fold
        vtbl_idx
        ~init:(on_error method_name)
        ~f:(fun _ v () -> on_success v)
        () )
  | A.This pos ->
    let venv, { ty; stmt } = trans_var (venv, tenv, A.SimpleVar (E.this_symbol, pos)) in
    venv, { ty; stmt = A.VarExp (stmt, pos) }
  | A.MethodCall { base; field; args; pos; _ } as exp ->
    let venv, { stmt = base; ty } = trans_exp (venv, tenv, base) in
    (match ty with
     | T.NAME (_, _, _, _vtable) ->
       let method_name =
         Option.fold ~none:"" ~some:(fun (A.Identifier ((name, _), _)) -> name) field
       in
       let venv, args =
         Base.List.fold
           ~init:(venv, [])
           ~f:(fun (venv, acc) e ->
             let venv, stm_ty = trans_exp (venv, tenv, e) in
             venv, stm_ty :: acc)
           args
       in
       let this_ty = T.type2str ty in
       let args_ty = List.map (fun { ty; _ } -> T.type2str ty) args in
       let args = List.map (fun { stmt; _ } -> stmt) args in
       let args_ty = this_ty :: args_ty in
       let vname = Ir_gen_env.vtable_method_name method_name args_ty in
       let vtbl_idx = Ir_gen_env.find_vtable_index ty vname in
       let on_success v =
         let stmt = A.MethodCall { base; field; args; pos; vtbl_idx = Some v } in
         { ty; stmt }
       in
       let on_error m () =
         error pos (m ^ " method not found in vtbl of ty:" ^ this_ty) (err_stmty exp)
       in
       (* offset vtbl idx by 2 to account for RTTI *)
       ( venv
       , Base.Option.fold
           vtbl_idx
           ~init:(on_error method_name)
           ~f:(fun _ v () -> on_success v)
           () )
       (* venv, { stmt; ty = T.NULL } *)
     | _ -> venv, error pos ("method not avail on type:" ^ T.type2str ty) (err_stmty exp))
  | _ as exp -> venv, err_stmty exp
;;

let inside_loop_check stmt pos =
  let stmt_str = A.sexp_of_stmt stmt |> Sexplib0.Sexp.to_string_hum in
  if !loop_nest_count = 0
  then error pos (stmt_str ^ "statement not directly inside a loop") (err_stmty stmt)
  else { stmt; ty = T.VOID }
;;

let rec trans_stmt (venv, tenv, stmt) =
  match stmt with
  | A.ExprStmt e ->
    let venv, { stmt; ty } = trans_exp (venv, tenv, e) in
    venv, { stmt = A.ExprStmt stmt; ty }
  | A.While { exp; block } ->
    let venv, { stmt = tr_exp; _ } = trans_exp (venv, tenv, exp) in
    loop_nest_count := !loop_nest_count + 1;
    let venv, { stmt = block; ty } = trans_stmt (venv, tenv, block) in
    loop_nest_count := !loop_nest_count - 1;
    venv, { stmt = A.While { exp = tr_exp; block }; ty }
  | A.Output (e, pos) as stmt ->
    let venv, { stmt = exp; ty } = trans_exp (venv, tenv, e) in
    if T.is_int ty
    then venv, { stmt = A.Output (exp, pos); ty }
    else
      venv, error pos "Output statement doesn't have an int expression" (err_stmty stmt)
  | A.Continue pos as stmt -> venv, inside_loop_check stmt pos
  | A.Break pos as stmt -> venv, inside_loop_check stmt pos
  | A.Block xs ->
    let venv, stmts = trans_blk (venv, tenv) xs in
    venv, { stmt = A.Block stmts; ty = T.VOID }
  | A.IfElse { exp; istmt; estmt; pos } ->
    let venv, { stmt = tr_exp; _ } = trans_exp (venv, tenv, exp) in
    let venv, ires = trans_stmt (venv, tenv, istmt) in
    let venv, eres = trans_stmt (venv, tenv, estmt) in
    if T.type_match ires.ty eres.ty
    then venv, { stmt = A.IfElse { exp = tr_exp; istmt; estmt; pos }; ty = ires.ty }
    else venv, error pos "If and else branch types don't match" (err_stmty stmt)
  | A.Delete (exp, pos) as orig ->
    let venv, { ty; stmt } = trans_exp (venv, tenv, exp) in
    let method_name = "~" ^ T.type2str ty in
    let args = [ T.type2str ty ] in
    let method_name = Ir_gen_env.vtable_method_name method_name args in
    let field = Some (A.Identifier (S.symbol method_name, pos)) in
    (match ty with
     | T.ARRAY _ ->
       let stmt =
         A.ExprStmt
           (A.MethodCall { base = stmt; field; args = []; pos; vtbl_idx = Some 4 })
       in
       venv, { stmt; ty = T.VOID }
     | T.NAME _ ->
       let vtbl_idx = Ir_gen_env.find_vtable_index ty method_name in
       let on_success v =
         let stmt =
           A.ExprStmt
             (A.MethodCall { base = stmt; field; args = []; pos; vtbl_idx = Some v })
         in
         { ty = T.VOID; stmt }
       in
       let on_error m () =
         error pos (m ^ " method not found in vtbl.") (err_stmty orig)
       in
       (* offset vtbl idx by 2 to account for RTTI *)
       ( venv
       , Base.Option.fold
           vtbl_idx
           ~init:(on_error method_name)
           ~f:(fun _ v () -> on_success v)
           () )
     | _ ->
       ( venv
       , error
           pos
           ("delete applied to a value with non-ref type " ^ T.type2str ty)
           (err_stmty orig) ))
  | _ -> venv, err_stmty stmt

and trans_blk (venv, tenv) xs =
  let venv, stmts =
    Base.List.fold xs ~init:(venv, []) ~f:(fun (venv, stmts) x ->
      let venv, s = trans_stmt (venv, tenv, x) in
      venv, s :: stmts)
  in
  let stmts = List.map (fun { stmt; _ } -> stmt) stmts in
  venv, List.rev stmts
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
      let venv, { stmt; ty } = trans_stmt (venv, tenv, stmt) in
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

let replace_namerefs tenv =
  let result = ref tenv in
  let nameref_to_name id =
    match S.look (!result, id) with
    | Some (T.NAME (id, fields, base, vtable)) -> T.NAME (id, fields, base, vtable)
    | _ -> raise NameRefToNameException
  in
  let map_ref (field_name, field_ty) =
    match field_ty with
    | T.NAMEREF id -> field_name, nameref_to_name id
    | x -> field_name, x
  in
  let update_type = function
    | Some (T.NAMEREF id) -> Some (nameref_to_name id)
    | Some (T.NAME (id, fields, base, vtable)) ->
      let fields = List.map map_ref fields in
      Some (T.NAME (id, fields, base, vtable))
    | x -> x
  in
  let iter_type sym _ty = result := S.update sym update_type !result in
  S.iter iter_type tenv;
  !result
;;

let build_this_param name =
  let loc = -1, -1 in
  let pos = loc, loc in
  A.Param { name = E.this_symbol; type_ = A.Reference (A.ClassType (name, pos)) }
;;

let find_super_ty tenv ty =
  (* let handle_super = function *)
  (*   | Some sty -> sty *)
  (*   | None -> raise SuperTypeNotFound *)
  (* in *)
  let handle_base = function
    | Some basety -> S.look (tenv, basety)
    | None -> None
  in
  match ty with
  | T.NAME (_, _, basety, _) -> handle_base basety
  | _ -> raise ExpectedClassType
;;

let handle_method tenv name fparams body =
  let fparams = build_this_param name :: fparams in
  let init_scope venv (A.Param { type_; name }) =
    let var = A.{ type_; id = name } in
    let venv, _, _ = trans_dec (venv, tenv, var) in
    venv
  in
  let ty =
    match S.look (tenv, name) with
    | Some ty -> ty
    | _ -> raise ThisTypeNotFound
  in
  let init_this = function
    | Some (E.VarEntry _) -> Some (E.VarEntry { ty; is_null = false })
    | _ -> None
  in
  let super_ty = find_super_ty tenv ty in
  let init_super _ =
    Base.Option.map super_ty ~f:(fun super_ty ->
      E.VarEntry { ty = super_ty; is_null = false })
  in
  let venv = Base.List.fold fparams ~init:E.base_venv ~f:init_scope in
  let venv = S.update E.this_symbol init_this venv in
  let venv = S.update E.super_symbol init_super venv in
  let _, { stmt = body; _ } = trans_stmt (venv, tenv, body) in
  fparams, body
;;

let super_call acc base =
  let field = Some (A.Identifier (base, A.default_pos)) in
  let base = A.VarExp (A.SimpleVar (E.super_symbol, A.default_pos), A.default_pos) in
  let super_call =
    A.MethodCall { base; field; args = []; pos = A.default_pos; vtbl_idx = None }
  in
  A.ExprStmt super_call :: acc
;;

let handle_const_body base stmt =
  (* let on_error pos () = *)
  (*   error pos "explicit constructor invocation not found as the first statement." () *)
  (* in *)
  let check_and_append stmt =
    let h_exp = function
      | A.MethodCall _ -> true
      | _ -> false
    in
    let rec h_stmt = function
      | A.ExprStmt e -> h_exp e
      | A.ReturnStmt s -> Base.Option.fold ~init:false ~f:(fun _ -> h_exp) s
      | A.Block stmts ->
        Base.Option.fold ~init:false ~f:(fun _ -> h_stmt) (Base.List.hd stmts)
      | _ -> false
    in
    let mk_super () =
      let open Base.Option.Let_syntax in
      let%map base = base in
      super_call [] base |> List.hd
    in
    let p_stmt _ stmt () = if h_stmt stmt then None else mk_super () in
    Base.Option.fold stmt ~init:mk_super ~f:p_stmt ()
  in
  let handle_blk = function
    | A.Block stmts ->
      let stmt1 = Base.List.hd stmts in
      let super_opt = check_and_append stmt1 in
      let stmts = Base.Option.fold super_opt ~init:stmts ~f:(fun acc s ->
          s :: acc
        ) in
      A.Block stmts
    | x -> x
  in
  handle_blk stmt
;;

let trans_method base class_name tenv = function
  | A.Constructor { name; fparams; body; pos } ->
        let body = handle_const_body base body in
    let fparams, body = handle_method tenv name fparams body in
    A.Constructor { name; fparams; body; pos }
  | A.Method { name; fparams; body; return_t } ->
    let fparams, body = handle_method tenv class_name fparams body in
    A.Method { name; fparams; body; return_t }
  | A.Destructor { name; fparams; body } ->
    let fparams, body = handle_method tenv class_name fparams body in
    A.Destructor { name; fparams; body }
  | x -> x
;;

let class_fields tenv = function
  | A.FieldDec xs ->
    List.map (fun (A.Field { name; type_; pos }) -> name, trans_type tenv type_, pos) xs
  | _ -> []
;;

let find_depth h class_decs =
  let open Base in
  let rec helper (A.ClassDec { base; name; pos; _ }) =
    let class_name, _ = name in
    let lookup _acc (name, _) =
      let cdec = Hashtbl.find h name in
      let err () =
        error pos ("class " ^ class_name ^ " doesn't extend from a valid base " ^ name) 0
      in
      let handle_base _acc cd () = 1 + helper cd in
      Option.fold cdec ~init:err ~f:handle_base ()
    in
    Option.fold base ~init:0 ~f:lookup
  in
  List.map ~f:(fun cd -> helper cd, cd) class_decs
;;

let class_decs_tbl class_decs =
  let open Base in
  let h = Hashtbl.create (module String) in
  let add_to_tbl (A.ClassDec { name; _ } as data) =
    let name, _ = name in
    let _ = Hashtbl.add h ~key:name ~data in
    ()
  in
  List.iter ~f:add_to_tbl class_decs;
  h
;;

let dedup_class_fields tenv class_body class_name =
  let open Base in
  let fields = List.map ~f:(class_fields tenv) class_body |> List.concat in
  let h = Hashtbl.create (module String) in
  (* duplicate field name check. This should only throw error for current class fields. When fields match with base class, they should be treated as overrides. *)
  let remove_duplicate_fields (((field_name, _) as sym), ty, pos) =
    match Base.Hashtbl.add h ~key:field_name ~data:ty with
    | `Ok -> Some (sym, ty)
    | _ ->
      error
        pos
        ("field " ^ field_name ^ " is already present in class " ^ class_name)
        None
  in
  List.filter_map ~f:remove_duplicate_fields fields
;;

let rec dedup_classdec_fields h tenv class_name =
  let open Base in
  let open Base.Option.Let_syntax in
  let handle_base base =
    let%map base = base in
    let base, _ = base in
    dedup_classdec_fields h tenv base
  in
  let handle_class =
    let class_dec = Hashtbl.find h class_name in
    let%map (A.ClassDec { base; class_body; _ }) = class_dec in
    let base_fields = Option.value (handle_base base) ~default:[] in
    let mem_fields = dedup_class_fields tenv class_body class_name in
    List.append base_fields mem_fields
  in
  Option.value handle_class ~default:[]
;;

let param_to_type tenv (A.Param { type_; _ }) = trans_type tenv type_ |> Types.type2str

let rec get_annot_methods h tenv class_name =
  let open Base in
  let gen_method_name name fparams =
    let name, _ = name in
    let fparams = List.map fparams ~f:(param_to_type tenv) in
    let fparams = class_name :: fparams in
    (* Add this to fparams *)
    Ir_gen_env.vtable_method_name name fparams
  in
  let get_return_type (A.Return { type_ }) = trans_type tenv type_ in
  let is_method = function
    | A.Method { name; fparams; return_t; _ } ->
      let ret_ty = get_return_type return_t in
      Some (gen_method_name name fparams, ret_ty)
    | A.Constructor { name; fparams; _ } ->
      let type_ = A.Reference (A.ClassType (name, A.default_pos)) in
      Some (gen_method_name name fparams, trans_type tenv type_)
    | A.Destructor { name; _ } -> Some (gen_method_name name [], Types.VOID)
    | _ -> None
  in
  let get_methods (A.ClassDec { class_body; base; _ }) =
    let super_methods =
      Option.fold base ~init:[] ~f:(fun _ (base, _) -> get_annot_methods h tenv base)
    in
    let methods = List.filter_map class_body ~f:is_method in
    List.append super_methods methods
  in
  let class_def = Hashtbl.find h class_name in
  Option.fold ~init:[] ~f:(fun _ cd -> get_methods cd) class_def
;;

let attach_vtable h tenv =
  let result = ref tenv in
  let update_type = function
    | Some (T.NAME (id, fields, base, _)) ->
      let class_name, _ = id in
      let vtable = get_annot_methods h tenv class_name in
      Some (T.NAME (id, fields, base, vtable))
    | x -> x
  in
  let iter_type sym _ty = result := S.update sym update_type !result in
  S.iter iter_type tenv;
  !result
;;

let add_default_con base name class_body =
  let body = [] in
  let body = Base.Option.fold ~init:body ~f:super_call base in
  let body = A.Block body in
  let con = A.Constructor { name; fparams = []; body; pos = A.default_pos } in
  con :: class_body
;;

let add_default_des base name class_body =
  let body = [] in
  let dest_name name =
    let des_name, _ = name in
    Symbol.symbol ("~" ^ des_name)
  in
  let body =
    Base.Option.fold ~init:body ~f:(fun acc b -> dest_name b |> super_call acc) base
  in
  let body = A.Block body in
  let name = dest_name name in
  let des = A.Destructor { name; body; fparams = [] } in
  des :: class_body
;;

let is_constructor = function
  | A.Constructor _ -> true
  | _ -> false
;;

let is_destructor = function
  | A.Destructor _ -> true
  | _ -> false
;;

let add_default_method class_decs ~exists_f ~default_gen =
  let update_body base name class_body =
    let has_con = Base.List.exists class_body ~f:exists_f in
    if has_con then class_body else default_gen base name class_body
  in
  let handle_class (A.ClassDec { name; class_body; base; pos }) =
    let class_body = update_body base name class_body in
    A.ClassDec { name; class_body; base; pos }
  in
  List.map handle_class class_decs
;;

let object_class_dec =
  let name = E.obj_symbol in
  let class_body = add_default_con None name [] in
  let class_body = add_default_des None name class_body in
  A.ClassDec { name; class_body; base = None; pos = A.default_pos }
;;

let add_extend name base =
  if E.obj_symbol = name
  then base
  else Option.value base ~default:E.obj_symbol |> Option.some
;;

let trans_class (tenv, class_decs) =
  let open Base in
  (* Add this param to methods and constructors *)
  let h = class_decs_tbl class_decs in
  let tr_class (tenv, class_dec) =
    let (A.ClassDec { name; base; class_body; pos }) = class_dec in
    let class_name, _ = name in
    let fields = dedup_classdec_fields h tenv class_name in
    let base = add_extend name base in
    let ctype = Types.NAME (name, fields, base, []) in
    let update_fn = Option.map ~f:(fun _ -> ctype) in
    let tenv = S.update name update_fn tenv in
    tenv, { stmt = A.ClassDec { name; base; pos; class_body }; ty = ctype }
  in
  let class_depth = find_depth h class_decs in
  let class_decs =
    List.sort class_depth ~compare:(fun (d1, _) (d2, _) -> Int.compare d1 d2)
  in
  let class_decs = List.map ~f:(fun (_, cd) -> cd) class_decs in
  (* Enter class names *)
  let tenv =
    List.fold class_decs ~init:tenv ~f:(fun tenv (A.ClassDec { name; _ }) ->
      S.enter (tenv, name, Types.NAMEREF name))
  in
  let tenv, class_decs =
    List.fold class_decs ~init:(tenv, []) ~f:(fun (tenv, xs) cdec ->
      let tenv, { stmt; _ } = tr_class (tenv, cdec) in
      (* This should update fields *)
      tenv, stmt :: xs)
  in
  (* All type defns are complete. *)
  (* Now update placeholder definitions. For typedef with a T.NAME name, use tenv *)
  let tenv = replace_namerefs tenv in
  (* typedefs are complete.*)
  (* add object class *)
  let class_decs = object_class_dec :: class_decs in
  (* check and add default constructors *)
  let class_decs =
    add_default_method class_decs ~exists_f:is_constructor ~default_gen:add_default_con
  in
  let class_decs =
    add_default_method class_decs ~exists_f:is_destructor ~default_gen:add_default_des
  in
  (* No more additional methods need to be added. *)
  (* methods will have this inserted in scope. Now generate vtable *)
  List.iter class_decs ~f:(fun (A.ClassDec { name; _ } as cdec) ->
    let name, _ = name in
    Hashtbl.update h name ~f:(fun _ -> cdec));
  let tenv = attach_vtable h tenv in
  (* Post vtable creation checks. *)
  (* 1. check if all constructor have a valid super init. *)
  let tr_class_body tenv (A.ClassDec { name; base; class_body; pos }) =
    let class_body = List.map ~f:(trans_method base name tenv) class_body in
    let cdec = A.ClassDec { name; base; class_body; pos } in
    let name, _ = name in
    Hashtbl.update h name ~f:(fun _ -> cdec);
    cdec
  in
  let class_decs = List.map ~f:(tr_class_body tenv) class_decs in
  (* check explicit constr invoc *)
  (* List.iter class_decs ~f:check_constr_invoc; *)
  tenv, class_decs (* sorted by depth *)
;;

let trans_prog comp_unit =
  let A.{ main_decl; class_decs } = comp_unit in
  let tenv, class_decs = trans_class (E.base_tenv, class_decs) in
  (* Add class defs to module *)
  let venv, tenv, tr_main = trans_main (E.base_venv, tenv, main_decl) in
  (* Add array types to module *)
  (* let arr_classes = array_class_decs venv in *)
  venv, tenv, A.{ main_decl = tr_main; class_decs }
;;
