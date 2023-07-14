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
  | T.NAME (_, _, Some base) ->
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
  | T.NAME (sym, _, _), T.ARRAY _ when sym = Env.obj_symbol ->
    Some A.Wide, Some (A.Reference (A.ClassType (sym, pos)))
  | T.NAME _, T.ARRAY _ ->
    None, None (* No cast when rhs is Array and lhs is not object *)
  | T.ARRAY _, T.NAME (sym, _, _) when sym = Env.obj_symbol -> Some A.Narrow, None
  | T.NAME (sym1, _, _), T.NAME (sym2, _, _) when sym1 = sym2 -> Some A.Identity, None
  | T.NAME (lhs_name, _, _), T.NAME _ when is_subclass tenv lhs_ty rhs_ty ->
     Some A.Wide, Some (A.Reference (A.ClassType (lhs_name, pos)))
  | T.NAME _, T.NAME _ when is_subclass tenv rhs_ty lhs_ty ->
     Some A.Narrow, None
  | T.NAME (lhs_name, _, _), T.NULL ->
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
      let map_field idx ((name, _), ty) =
        if String.equal name sym_name then Some (idx + 1, ty) else None
      in
      let field_opt = Base.List.find_mapi ~f:map_field fields in
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
     | T.NAME ((class_name, _), fields, _) -> check_field class_name fields
     | _ -> venv, error pos "field access on non-object type" (err_stmty var))
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
  | A.Identifier (id, pos) as exp ->
    let venv, { ty; _ } = trans_var (venv, tenv, A.SimpleVar (id, pos)) in
    venv, { stmt = exp; ty }
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
  | A.ArrayCreationExp { type_; exprs; pos } as exp ->
    (* let expr_rank = List.length exprs in *)
    let is_int acc exp =
      let _venv, { ty = res_ty; _ } = trans_exp (venv, tenv, exp) in
      T.is_int res_ty && acc
    in
    let is_int_exprs = List.fold_left is_int true exprs in
    if is_int_exprs
    then (
      (* let type_ = A.append_rank_to_type expr_rank type_ in *)
      let ty = trans_type tenv type_ in
      venv, { stmt = exp; ty })
    else venv, (error pos "Array Creation Expr has non int dim") (err_stmty exp)
  | A.VarExp (v, pos) ->
    let venv, { ty; stmt } = trans_var (venv, tenv, v) in
    venv, { stmt = A.VarExp (stmt, pos); ty }
  | A.CastType { type_; exp; pos; _ } as cexp ->
    let venv, { stmt; ty = exp_ty } = trans_exp (venv, tenv, exp) in
    let cast_ty = trans_type tenv type_ in
    (* obj | i32arr *)
    let dummy_pos = (-1, -1), (-1, -1) in
    let stmt =
      match compare_and_cast tenv dummy_pos cast_ty exp_ty with
      | Some A.Wide, _ -> A.CastType { type_; exp = stmt; cast_type = Some A.Wide; pos }
      | Some A.Narrow, _ ->
        A.CastType { type_; exp = stmt; cast_type = Some A.Narrow; pos }
      | Some A.Identity, _ ->
        A.CastType { type_; exp = stmt; cast_type = Some A.Identity; pos }
      | None, _ -> error pos "cast not possible" cexp
    in
    venv, { stmt; ty = cast_ty }
  | A.ClassCreationExp { type_; _ } as exp ->
    let ty = trans_type tenv type_ in
    venv, { ty; stmt = exp }
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
  | A.Block xs as stmt ->
    let venv = trans_blk (venv, tenv) xs in
    venv, { stmt; ty = T.VOID }
  | A.IfElse { exp; istmt; estmt; pos } ->
    let venv, { stmt = tr_exp; _ } = trans_exp (venv, tenv, exp) in
    let venv, ires = trans_stmt (venv, tenv, istmt) in
    let venv, eres = trans_stmt (venv, tenv, estmt) in
    if T.type_match ires.ty eres.ty
    then venv, { stmt = A.IfElse { exp = tr_exp; istmt; estmt; pos }; ty = ires.ty }
    else venv, error pos "If and else branch types don't match" (err_stmty stmt)
  | _ -> venv, err_stmty stmt

and trans_blk (venv, tenv) xs =
  Base.List.fold xs ~init:venv ~f:(fun venv x ->
    let venv, _ = trans_stmt (venv, tenv, x) in
    venv)
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
  (* field_index = 0 is reserved for vtable *)
  let field_index = ref 1 in
  let remove_duplicate_fields (((field_name, _) as sym), ty, pos) =
    match Base.Hashtbl.add h ~key:field_name ~data:(ty, !field_index) with
    | `Ok ->
      field_index := !field_index + 1;
      Some (sym, ty)
    | _ ->
      error
        pos
        ("field " ^ field_name ^ " is already present in class " ^ class_name)
        None
  in
  List.filter_map ~f:remove_duplicate_fields fields
;;

let rec dedup_classdec_fields h tenv (A.ClassDec { name; class_body; base; _ }) =
  let open Base in
  let name, _ = name in
  let handle_base =
    let open Base.Option.Let_syntax in
    let%bind base = base in
    let base, _ = base in
    let%map base = Hashtbl.find h base in
    dedup_classdec_fields h tenv base
  in
  let base_fields = Option.value handle_base ~default:[] in
  let mem_fields = dedup_class_fields tenv class_body name in
  List.append base_fields mem_fields
;;

let trans_class (tenv, class_decs) =
  let open Base in
  let h = class_decs_tbl class_decs in
  let tr_class (tenv, class_dec) =
    let fields = dedup_classdec_fields h tenv class_dec in
    let (A.ClassDec { name; base; _ }) = class_dec in
    let ctype = Types.NAME (name, fields, base) in
    let tenv = S.enter (tenv, name, ctype) in
    tenv, { stmt = (); ty = ctype }
  in
  let class_depth = find_depth h class_decs in
  let class_decs =
    List.sort class_depth ~compare:(fun (d1, _) (d2, _) -> Int.compare d1 d2)
  in
  let class_decs = List.map ~f:(fun (_, cd) -> cd) class_decs in
  let tenv, _ =
    Base.List.fold class_decs ~init:(tenv, []) ~f:(fun (tenv, xs) cdec ->
      let tenv, x = tr_class (tenv, cdec) in
      tenv, x :: xs)
  in
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
