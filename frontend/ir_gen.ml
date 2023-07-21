module A = Ast
module FT = Frontend_types
module S = Symbol
module T = Types
module E = Env

let gen_binop = function
  | A.PlusOp -> FT.Plus
  | A.EqualsOp -> FT.Equals
  | A.LessThanOp -> FT.Less_than
  | A.GreaterThanOp -> FT.Greater_than
  | A.MultOp -> FT.Mult_op
  | A.DivideOp -> FT.Divide_op
  | A.MinusOp -> FT.Subtract_op
;;

exception NoMatchingTypeExpr
exception SubscriptAccessException
exception NonArrayTypeException
exception ClassNotFoundException

let gen_texpr tenv type_ =
  let ty = Semant.trans_type tenv type_ in
  T.gen_type_expr ty
;;

(* A.type_ -> T.Ty -> FT.type_expr *)
(* let ty = gen_texpr rank typ in *)
(* FT.Ref (FT.Array_type { rank = Int32.of_int rank; typ = ty }) *)

let gen_cast_type = function
  | A.Identity -> FT.No_cast
  | A.Wide -> FT.Wide_cast
  | A.Narrow -> FT.Narrow_cast
;;

let gen_expr tenv e =
  let rec gexpr = function
    | A.IntLit (i, _) -> FT.Integer i
    | A.OpExp (A.UnaryOp { oper = A.NegateOp; exp }, _) ->
      FT.Unop { op = FT.Neg; uexpr = gexpr exp }
    | A.OpExp (A.BinaryOp { oper; left; right }, pos) ->
      FT.Binop
        { bin_op = gen_binop oper
        ; lexpr = gexpr left
        ; rexpr = gexpr right
        ; op_line_no = A.line_no pos
        }
    | A.Assignment { lhs; exp; _ } -> FT.Assign { lhs = gen_var lhs; rhs = gexpr exp }
    | A.Identifier ((sym_name, _), _) ->
      FT.Expr_id { id = FT.Simple { var_name = sym_name } }
    | A.ArrayCreationExp { type_; exprs; pos } ->
      let ty = Semant.trans_type tenv type_ in
      let texpr = T.gen_type_expr ty in
      let make_line_no = A.line_no pos in
      FT.Array_creation { creation_exprs = List.map gexpr exprs; texpr; make_line_no }
    | A.VarExp (v, _) -> FT.Var_exp (gen_var v)
    | A.CastType { type_; exp; cast_type; pos } ->
      let cast_type = Option.fold cast_type ~none:FT.No_cast ~some:gen_cast_type in
      FT.Cast_expr
        { cast_to = gen_texpr tenv type_
        ; expr = gexpr exp
        ; cast_type
        ; cast_line_no = A.line_no pos
        }
    | A.ClassCreationExp { type_; args; vtbl_idx; _ } ->
      let con_args = List.map gexpr args in
      let vtable_index = Option.value vtbl_idx ~default:(-1) |> Int32.of_int in
      FT.Class_creation { con_texpr = gen_texpr tenv type_; con_args; vtable_index }
    | _ -> FT.Integer (Int32.of_int (-1))
  and gen_var = function
    | A.SimpleVar ((sym_name, _), _) -> FT.Simple { var_name = sym_name }
    (* | A.SubscriptVar ((A.FieldVar _ as v), exp, pos) -> *)
    (*   (match gen_var v with *)
    (*    | FT.Field _ as base_var -> *)
    (*      let line_no = A.line_no pos in *)
    (*      let len_var = *)
    (*        FT.Field *)
    (*          { base_expr = FT.Var_exp base_var (\* This will walk to the array field *\) *)
    (*          ; field_index = Int32.of_int 2    (\* This will access the length field of the array *\) *)
    (*          ; field_line_no = line_no *)
    (*          } *)
    (*      in *)
    (*      FT.Subscript { base_var; var_exp = gexpr exp; len_var; line_no } *)
    (*    | _ -> raise SubscriptAccessException) *)
    | A.SubscriptVar (v, exp, pos) ->
      let line_no = A.line_no pos in
      FT.Subscript
        { base_var =
            FT.Field
              { base_expr = FT.Var_exp (gen_var v)
              ; field_index = Int32.one
              ; field_line_no = line_no
              }
        ; len_var =
            FT.Field
              { base_expr = FT.Var_exp (gen_var v)
              ; field_index = Int32.of_int 2
              ; field_line_no = line_no
              }
        ; var_exp = gexpr exp
        ; line_no
        }
    | A.FieldVar (base_exp, _, id, pos) ->
      FT.Field
        { base_expr = gexpr base_exp
        ; field_index = Int32.of_int id
        ; field_line_no = A.line_no pos
        }
      (* Always defaults to length field *)
    | A.LoadVar v -> FT.Load_var { var = gen_var v }
  in
  gexpr e
;;

let gen_stmt tenv s =
  let rec gstmt = function
    | A.Output (o, _) -> FT.Printf { format = "%d"; f_args = [ gen_expr tenv o ] }
    | A.ExprStmt e -> FT.Expr_stmt { expr_stmt = gen_expr tenv e }
    (* | A.IfElse { exp; istmt; estmt; _ } -> *)
    (*   FT.If_expr { eval = gen_expr exp; if_expr = gstmt istmt; else_expr = gstmt estmt } *)
    | A.Block xs -> FT.Block { stmt_list = List.map gstmt xs }
    | A.While { exp; block } ->
      FT.While { while_cond = gen_expr tenv exp; while_block = gstmt block }
    | A.Break _ -> FT.Break
    | A.Continue _ -> FT.Continue
    | A.Delete (e, _) -> FT.Delete { del_expr = gen_expr tenv e }
    (* | A.Empty -> FT.Empty *)
    (* | _ -> FT.Integer (Int32.of_int (-1)) *)
    | _ -> FT.Printf { format = "%d"; f_args = [ FT.Integer (Int32.of_int (-1)) ] }
  in
  gstmt s
;;

let gen_decls tenv xs =
  let rec gen_dec = function
    | [] -> []
    | A.{ id = sym_name, _sym_id; type_; _ } :: xs ->
      FT.Var_decl { var_id = sym_name; texpr = gen_texpr tenv type_ } :: gen_dec xs
  in
  gen_dec xs
;;

let ir_gen_field_defn tenv (A.Field { type_; _ }) = gen_texpr tenv type_

let gen_params tenv fparams =
  let gparam (A.Param { name; type_ }) =
    let param_name, _ = name in
    let param_type = gen_texpr tenv type_ in
    FT.{ param_name; param_type }
  in
  List.map gparam fparams
;;

let gen_fun_def tenv = function
  | A.Constructor { name; fparams; body } ->
    let name, _ = name in
    let args = List.map (Semant.param_to_type tenv) fparams in
    let name = Ir_gen_env.vtable_method_name name args in
    List.length fparams |> Printf.printf "len:%s|%d\n" name;
    let params = gen_params tenv fparams in
    let body = gen_stmt tenv body in
    FT.{ name; return_t = FT.Void; params; body } |> Option.some
  | _ -> None
;;

(* let is_constructor = function *)
(*   | A.Constructor _ -> true *)
(*   | _ -> false *)
(* ;; *)

let add_default_con name class_body =
  let con =
    A.Constructor { name; fparams = [ Semant.build_this_param name ]; body = A.Block [] }
  in
  con :: class_body
;;

(* let add_default_const name class_body = *)
(*   let has_con = Base.List.exists class_body ~f:is_constructor in *)
(*   if has_con then class_body else add_default_con name class_body *)
(* ;; *)

let ir_gen_class_defn tenv (A.ClassDec { name; base; class_body; _ }) =
  let class_defn =
    match S.look (tenv, name) with
    | Some (T.NAME (_, fields, _, vtable)) ->
      let ir_fields = List.map (fun (_, ty) -> T.gen_type_expr ty) fields in
      let name, _ = name in
      let base_class_name, _ = Option.value base ~default:Env.obj_symbol in
      FT.{ name; fields = ir_fields; base_class_name; vtable }
    | _ -> raise ClassNotFoundException
  in
  (* gen fun_def for const *)
  (* use tenv to get vtable *)
  (* for all constructor defs in class_body find an entry in the vtable *)
  (* don’t worry about default const for now. *)
  let fun_defs = List.filter_map (gen_fun_def tenv) class_body in
  class_defn, fun_defs
;;

let ir_gen_class_defns tenv class_defns = List.map (ir_gen_class_defn tenv) class_defns

let mk_arr_inner_type lower_rank type_ =
  if lower_rank > 0
  then T.gen_type_expr @@ T.ARRAY (lower_rank, type_)
  else FT.Pointer { data = T.gen_type_expr type_ }
;;

let make_array_class name rank type_ =
  let lower_rank = rank - 1 in
  let lower_type = mk_arr_inner_type lower_rank type_ in
  let base_class_name, _ = Env.obj_symbol in
  (* vtable - empty list *)
  FT.{ name; fields = [ lower_type; FT.Int32 ]; base_class_name; vtable = [] }
;;

let mk_arr_constr name rank type_ =
  let cur_type = T.gen_type_expr (ARRAY (rank, type_)) in
  let lower_rank = rank - 1 in
  let lower_type = mk_arr_inner_type lower_rank type_ in
  let this_field field_index =
    FT.Field
      { base_expr = FT.Var_exp (FT.Simple { var_name = "this" })
      ; field_index
      ; field_line_no = Int32.zero
      }
  in
  let this_assign field_index field_name =
    FT.Expr_stmt
      { expr_stmt =
          FT.Assign
            { lhs = this_field field_index
            ; rhs = FT.Var_exp (FT.Simple { var_name = field_name })
            }
      }
  in
  let var_exp s = FT.Var_exp (FT.Simple { var_name = s }) in
  let _while_stmt =
    FT.While
      { while_cond =
          FT.Binop
            { bin_op = FT.Less_than
            ; lexpr = FT.Var_exp (FT.Simple { var_name = "i" })
            ; rexpr = FT.Var_exp (FT.Simple { var_name = "length" })
            ; op_line_no = Int32.zero
            }
      ; while_block =
          FT.Block
            { stmt_list =
                [ FT.Expr_stmt
                    { expr_stmt =
                        FT.Assign
                          { lhs =
                              FT.Subscript
                                { base_var = this_field Int32.one
                                ; var_exp = var_exp "i"
                                ; len_var = this_field (Int32.of_int 2)
                                ; line_no =
                                    Int32.zero
                                    (* There is no line no for compiler generated code *)
                                }
                          ; rhs = var_exp "zeroInit"
                          }
                    }
                ; FT.Expr_stmt
                    { expr_stmt =
                        FT.Assign
                          { lhs = FT.Simple { var_name = "i" }
                          ; rhs =
                              FT.Binop
                                { bin_op = FT.Plus
                                ; lexpr = var_exp "i"
                                ; rexpr = FT.Integer (Int32.of_int 2)
                                ; op_line_no = Int32.zero
                                }
                          }
                    }
                ]
            }
      }
  in
  let body =
    FT.Block
      { stmt_list =
          [ this_assign Int32.zero "vtable"
          ; this_assign Int32.one "data"
          ; this_assign (Int32.of_int 2) "length"
            (* ; FT.Var_decl { var_id = "i"; texpr = FT.Int32 } *)
            (* ; FT.Var_decl { var_id = "zeroInit"; texpr = zero_val_type } *)
            (* ; while_stmt *)
          ]
      }
  in
  FT.
    { name
    ; return_t = FT.Void
    ; params =
        [ FT.{ param_name = "this"; param_type = cur_type }
        ; FT.
            { param_name = "vtable"
            ; param_type =
                FT.Pointer
                  { data =
                      FT.Class
                        { name = T.type2str (ARRAY (rank, type_)) ^ "_Vtable_type" }
                  }
            }
        ; FT.{ param_name = "data"; param_type = lower_type }
        ; FT.{ param_name = "length"; param_type = FT.Int32 }
        ]
    ; body
    }
;;

let filter_arr_creation_exp tenv main_decl =
  let arr_types = ref [] in
  let rec h_exp = function
    | A.ArrayCreationExp { type_; _ } ->
      let ty = Semant.trans_type tenv type_ in
      arr_types := ty :: !arr_types
      (* Printf.printf "%s|%d\n" (T.type2str ty) (List.length exprs) *)
    | A.ClassCreationExp { args; _ } -> List.iter h_exp args
    | A.MethodCall { base; field; args; _ } ->
      List.iter h_exp args;
      h_exp base;
      Option.fold ~none:() ~some:h_exp field
    | A.CastEvalExp { to_; from_ } ->
      h_exp to_;
      h_exp from_
    | A.CastType { exp; _ } -> h_exp exp
    | A.Assignment { exp; _ } -> h_exp exp
    | Ast.Identifier (_, _)
    | Ast.IntLit (_, _)
    | Ast.OpExp (_, _)
    | Ast.VarExp (_, _)
    | Ast.NullLit _ | Ast.This _ | Ast.Super _ -> ()
  in
  let rec h_stmt = function
    | A.Block stmts -> List.iter h_stmt stmts
    | A.While { exp; block } ->
      h_exp exp;
      h_stmt block
    | A.Output (exp, _) -> h_exp exp
    | A.ReturnStmt (Some exp) -> h_exp exp
    | A.ExprStmt exp -> h_exp exp
    | A.Delete (exp, _) -> h_exp exp
    | A.IfElse { exp; istmt; estmt; _ } ->
      h_exp exp;
      h_stmt istmt;
      h_stmt estmt
    | Ast.ReturnStmt None -> ()
    | Ast.Empty | Ast.Break _ | Ast.Continue _ -> ()
  in
  let filter_main = function
    | A.MainStmt stmt -> Some stmt
    | _ -> None
  in
  let compare t1 t2 =
    match t1, t2 with
    | T.ARRAY (r1, _), T.ARRAY (r2, _) -> Int.compare r1 r2
    | _, _ -> raise NonArrayTypeException
  in
  List.iter h_stmt (Base.List.filter_map ~f:filter_main main_decl);
  Base.List.sort !arr_types ~compare
;;

let arr_class_defns venv tenv main_decl =
  let h = Base.Hash_set.create (module Base.String) in
  let class_results = ref [] in
  let func_results = ref [] in
  let add_class type_ rank =
    let name = T.type2str (ARRAY (rank, type_)) in
    let is_present = Base.Hash_set.exists h ~f:(String.equal name) in
    if is_present
    then ()
    else (
      Base.Hash_set.add h name;
      let array_class = make_array_class name rank type_ in
      class_results := array_class :: !class_results)
  in
  let add_constructor type_ rank =
    let name = T.type2str (ARRAY (rank, type_)) ^ "_Constructor" in
    let is_present = Base.Hash_set.exists h ~f:(String.equal name) in
    if is_present
    then ()
    else (
      Base.Hash_set.add h name;
      let const_func = mk_arr_constr name rank type_ in
      func_results := const_func :: !func_results)
  in
  let arr_types = filter_arr_creation_exp tenv main_decl in
  let handle_arr_type = function
    | T.ARRAY (rank, type_) ->
      let ranks = List.init rank (fun rank -> rank + 1) in
      List.iter (add_class type_) ranks;
      List.iter (add_constructor type_) ranks
    | _ -> ()
  in
  let handle_ventry _symbol = function
    | E.VarEntry { ty; _ } -> handle_arr_type ty
    | _ -> ()
  in
  let rec handle_tentry _sym = function
    | T.NAME (_ty_name, fields, _, _) ->
      List.iter (fun (sym, ty) -> handle_tentry sym ty) fields
    | T.ARRAY _ as arr -> handle_arr_type arr
    | _ -> ()
  in
  List.iter handle_arr_type arr_types;
  S.iter handle_ventry venv;
  S.iter handle_tentry tenv;
  List.rev !class_results, List.rev !func_results
;;

let obj_class_defn =
  let name, _ = E.obj_symbol in
  let obj_methods =
    let obj_cons = add_default_con E.obj_symbol [] in
    List.filter_map (gen_fun_def E.base_tenv) obj_cons
  in
  (* FIXME: Check vtable *)
  FT.{ name; fields = []; base_class_name = name; vtable = [] }, obj_methods
;;

let ir_gen_function_defns tenv class_defns =
  let method_defns (A.ClassDec { name; class_body; _ }) =
    let method_defn _class_name (name, return_t, _, _) =
      let name, _ = name in
      let (A.Return { type_ = return_t }) = return_t in
      let return_t = Semant.trans_type tenv return_t |> T.gen_type_expr in
      let body = FT.Block { stmt_list = [] } in
      FT.{ name; return_t; params = []; body }
    in
    let filter_method b =
      match b with
      | A.Method { name; return_t; fparams; body } -> Some (name, return_t, fparams, body)
      | _ -> None
    in
    let methods = List.filter_map filter_method class_body in
    List.map (method_defn name) methods
  in
  List.map method_defns class_defns |> List.concat
;;

let obj_is_a_fun =
  (* obj is a reference type *)
  (* need a local variable? *)
  (* need support for a null literal *)
  (* vtbl - vtbl_ty_ptr *)
  (* object - obj_ty_ptr *)
  let name = "Object_IsA" in
  let return_t = FT.Bool in
  let obj_ty_name, _ = Env.obj_symbol in
  let obj_param_name = String.lowercase_ascii obj_ty_name in
  let param1_ty = FT.Pointer { data = FT.Class { name = obj_ty_name } } in
  let param2_ty =
    FT.Pointer { data = FT.Class { name = obj_ty_name ^ "_Vtable_type" } }
  in
  let param3_ty = FT.Pointer { data = FT.Int8 } in
  let param1 = FT.{ param_type = param1_ty; param_name = obj_param_name } in
  let param2 = FT.{ param_type = param2_ty; param_name = "vtbl" } in
  let param3 = FT.{ param_type = param3_ty; param_name = "name" } in
  let params = [ param1; param2; param3 ] in
  let check_equal lexpr rexpr =
    FT.Binop { bin_op = FT.Equals; lexpr; rexpr; op_line_no = Int32.zero }
  in
  let make_id var_name = FT.Var_exp (FT.Simple { var_name }) in
  let expr_stmt expr = FT.Expr_stmt { expr_stmt = expr } in
  let obj_is_not_null =
    FT.Unop { op = FT.Not; uexpr = check_equal (make_id obj_param_name) FT.Null_lit }
  in
  (* if object is not null
     while(vtbl != null){
       if(vtbl.1 == name){
          break;
       } else {
        vtbl = vtbl.0;
       }
     }
     return vtbl == null;
  *)
  let while_vtbl_not_null =
    let vtbl = make_id param2.param_name in
    let while_cond = FT.Unop { op = FT.Not; uexpr = check_equal vtbl FT.Null_lit } in
    let vtbl_base =
      FT.Var_exp
        (FT.Field
           { base_expr = vtbl; field_index = Int32.zero; field_line_no = Int32.zero })
    in
    let vtbl_name =
      FT.Var_exp
        (FT.Field
           { base_expr = vtbl; field_index = Int32.one; field_line_no = Int32.zero })
    in
    let assign_base =
      FT.Assign { lhs = FT.Simple { var_name = param2.param_name }; rhs = vtbl_base }
    in
    let while_block =
      FT.Block
        { stmt_list =
            [ FT.If_stmt
                { eval = check_equal vtbl_name (make_id param3.param_name)
                ; if_stmt = FT.Break
                ; else_stmt = expr_stmt assign_base
                }
            ]
        }
    in
    FT.While { while_cond; while_block }
  in
  let obj_vtbl =
    FT.Field
      { base_expr = make_id obj_param_name
      ; field_index = Int32.zero
      ; field_line_no = Int32.zero
      }
  in
  let if_obj_not_null =
    FT.If_stmt
      { eval = obj_is_not_null
      ; if_stmt =
          FT.Block
            { stmt_list =
                [ expr_stmt
                    (FT.Assign
                       { lhs = FT.Simple { var_name = param2.param_name }
                       ; rhs = FT.Var_exp obj_vtbl
                       })
                ; while_vtbl_not_null
                ; expr_stmt
                    (FT.Unop
                       { op = FT.Not
                       ; uexpr = check_equal (make_id param2.param_name) FT.Null_lit
                       })
                ]
            }
      ; else_stmt = expr_stmt (check_equal (FT.Integer Int32.zero) (FT.Integer Int32.one))
      }
  in
  let body = FT.Block { stmt_list = [ if_obj_not_null ] } in
  FT.{ name; return_t; params; body }
;;

let gen_prog (venv, tenv, A.{ main_decl; class_decs }) =
  let rec gen_main = function
    | [] -> []
    | A.VariableDecl decls :: xs -> gen_decls tenv decls @ gen_main xs
    | A.MainStmt s :: xs -> gen_stmt tenv s :: gen_main xs
  in
  let arr_classdefs, arr_funcdefs = arr_class_defns venv tenv main_decl in
  let classdefs, method_defs = ir_gen_class_defns tenv class_decs |> Base.List.unzip in
  let method_defs = List.concat method_defs in
  let obj_class_def, obj_method_defs = obj_class_defn in
  let obj_and_arr_classdefs = List.cons obj_class_def arr_classdefs in
  let classdefs = List.append obj_and_arr_classdefs classdefs in
  (* let funcdefs = ir_gen_function_defns tenv class_decs in *)
  let obj_funcs = obj_is_a_fun :: obj_method_defs in
  let obj_and_arr_funcs = List.append obj_funcs arr_funcdefs in
  let function_defs = List.append obj_and_arr_funcs method_defs in
  FT.{ main = gen_main main_decl; classdefs; function_defs }
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
