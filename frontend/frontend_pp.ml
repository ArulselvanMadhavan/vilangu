[@@@ocaml.warning "-27-30-39"]

let rec pp_un_op fmt (v : Frontend_types.un_op) =
  match v with
  | Frontend_types.Not -> Format.fprintf fmt "Not"
  | Frontend_types.Neg -> Format.fprintf fmt "Neg"
;;

let rec pp_bin_op fmt (v : Frontend_types.bin_op) =
  match v with
  | Frontend_types.Plus -> Format.fprintf fmt "Plus"
  | Frontend_types.Equals -> Format.fprintf fmt "Equals"
  | Frontend_types.Less_than -> Format.fprintf fmt "Less_than"
  | Frontend_types.Greater_than -> Format.fprintf fmt "Greater_than"
  | Frontend_types.Mult_op -> Format.fprintf fmt "Mult_op"
  | Frontend_types.Divide_op -> Format.fprintf fmt "Divide_op"
  | Frontend_types.Subtract_op -> Format.fprintf fmt "Subtract_op"
;;

let rec pp_var_p_simple fmt (v : Frontend_types.var_p_simple) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~first:true
      "var_name"
      Pbrt.Pp.pp_string
      fmt
      v.Frontend_types.var_name
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
;;

let rec pp_type_expr_p_class fmt (v : Frontend_types.type_expr_p_class) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "name" Pbrt.Pp.pp_string fmt v.Frontend_types.name
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
;;

let rec pp_type_expr fmt (v : Frontend_types.type_expr) =
  match v with
  | Frontend_types.Int32 -> Format.fprintf fmt "Int32"
  | Frontend_types.Class x ->
    Format.fprintf fmt "@[<hv2>Class(@,%a)@]" pp_type_expr_p_class x
  | Frontend_types.Pointer x ->
    Format.fprintf fmt "@[<hv2>Pointer(@,%a)@]" pp_type_expr_p_pointer x
  | Frontend_types.Void -> Format.fprintf fmt "Void"
  | Frontend_types.Bool -> Format.fprintf fmt "Bool"
  | Frontend_types.Int8 -> Format.fprintf fmt "Int8"

and pp_type_expr_p_pointer fmt (v : Frontend_types.type_expr_p_pointer) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data" pp_type_expr fmt v.Frontend_types.data
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
;;

let rec pp_expr_p_cast fmt (v : Frontend_types.expr_p_cast) =
  match v with
  | Frontend_types.No_cast -> Format.fprintf fmt "No_cast"
  | Frontend_types.Wide_cast -> Format.fprintf fmt "Wide_cast"
  | Frontend_types.Narrow_cast -> Format.fprintf fmt "Narrow_cast"
;;

let rec pp_var_p_subscript fmt (v : Frontend_types.var_p_subscript) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "base_var" pp_var fmt v.Frontend_types.base_var;
    Pbrt.Pp.pp_record_field ~first:false "var_exp" pp_expr fmt v.Frontend_types.var_exp;
    Pbrt.Pp.pp_record_field ~first:false "len_var" pp_var fmt v.Frontend_types.len_var;
    Pbrt.Pp.pp_record_field
      ~first:false
      "line_no"
      Pbrt.Pp.pp_int32
      fmt
      v.Frontend_types.line_no
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_var fmt (v : Frontend_types.var) =
  match v with
  | Frontend_types.Simple x ->
    Format.fprintf fmt "@[<hv2>Simple(@,%a)@]" pp_var_p_simple x
  | Frontend_types.Subscript x ->
    Format.fprintf fmt "@[<hv2>Subscript(@,%a)@]" pp_var_p_subscript x
  | Frontend_types.Field x -> Format.fprintf fmt "@[<hv2>Field(@,%a)@]" pp_var_p_field x
  | Frontend_types.Load_var x ->
    Format.fprintf fmt "@[<hv2>Load_var(@,%a)@]" pp_var_p_load x

and pp_var_p_field fmt (v : Frontend_types.var_p_field) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "base_expr" pp_expr fmt v.Frontend_types.base_expr;
    Pbrt.Pp.pp_record_field
      ~first:false
      "field_index"
      Pbrt.Pp.pp_int32
      fmt
      v.Frontend_types.field_index;
    Pbrt.Pp.pp_record_field
      ~first:false
      "field_line_no"
      Pbrt.Pp.pp_int32
      fmt
      v.Frontend_types.field_line_no
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expr fmt (v : Frontend_types.expr) =
  match v with
  | Frontend_types.Integer x ->
    Format.fprintf fmt "@[<hv2>Integer(@,%a)@]" Pbrt.Pp.pp_int32 x
  | Frontend_types.Function_app x ->
    Format.fprintf fmt "@[<hv2>Function_app(@,%a)@]" pp_expr_p_function_app x
  | Frontend_types.Unop x -> Format.fprintf fmt "@[<hv2>Unop(@,%a)@]" pp_expr_p_unop x
  | Frontend_types.Binop x -> Format.fprintf fmt "@[<hv2>Binop(@,%a)@]" pp_expr_p_binop x
  | Frontend_types.Assign x ->
    Format.fprintf fmt "@[<hv2>Assign(@,%a)@]" pp_expr_p_assign x
  | Frontend_types.Expr_id x ->
    Format.fprintf fmt "@[<hv2>Expr_id(@,%a)@]" pp_identifier x
  | Frontend_types.Empty -> Format.fprintf fmt "Empty"
  | Frontend_types.Array_creation x ->
    Format.fprintf fmt "@[<hv2>Array_creation(@,%a)@]" pp_expr_p_array_creation x
  | Frontend_types.Var_exp x -> Format.fprintf fmt "@[<hv2>Var_exp(@,%a)@]" pp_var x
  | Frontend_types.Null_lit -> Format.fprintf fmt "Null_lit"
  | Frontend_types.Cast_expr x ->
    Format.fprintf fmt "@[<hv2>Cast_expr(@,%a)@]" pp_expr_p_cast_expr x

and pp_expr_p_function_app fmt (v : Frontend_types.expr_p_function_app) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "name" Pbrt.Pp.pp_string fmt v.Frontend_types.name;
    Pbrt.Pp.pp_record_field
      ~first:false
      "args"
      (Pbrt.Pp.pp_list pp_expr)
      fmt
      v.Frontend_types.args
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expr_p_unop fmt (v : Frontend_types.expr_p_unop) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "op" pp_un_op fmt v.Frontend_types.op;
    Pbrt.Pp.pp_record_field ~first:false "uexpr" pp_expr fmt v.Frontend_types.uexpr
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expr_p_binop fmt (v : Frontend_types.expr_p_binop) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "bin_op" pp_bin_op fmt v.Frontend_types.bin_op;
    Pbrt.Pp.pp_record_field ~first:false "lexpr" pp_expr fmt v.Frontend_types.lexpr;
    Pbrt.Pp.pp_record_field ~first:false "rexpr" pp_expr fmt v.Frontend_types.rexpr;
    Pbrt.Pp.pp_record_field
      ~first:false
      "op_line_no"
      Pbrt.Pp.pp_int32
      fmt
      v.Frontend_types.op_line_no
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expr_p_assign fmt (v : Frontend_types.expr_p_assign) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "lhs" pp_var fmt v.Frontend_types.lhs;
    Pbrt.Pp.pp_record_field ~first:false "rhs" pp_expr fmt v.Frontend_types.rhs
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_identifier fmt (v : Frontend_types.identifier) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "id" pp_var fmt v.Frontend_types.id
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expr_p_array_creation fmt (v : Frontend_types.expr_p_array_creation) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~first:true
      "creation_exprs"
      (Pbrt.Pp.pp_list pp_expr)
      fmt
      v.Frontend_types.creation_exprs;
    Pbrt.Pp.pp_record_field ~first:false "texpr" pp_type_expr fmt v.Frontend_types.texpr;
    Pbrt.Pp.pp_record_field
      ~first:false
      "make_line_no"
      Pbrt.Pp.pp_int32
      fmt
      v.Frontend_types.make_line_no
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expr_p_cast_expr fmt (v : Frontend_types.expr_p_cast_expr) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~first:true
      "cast_to"
      pp_type_expr
      fmt
      v.Frontend_types.cast_to;
    Pbrt.Pp.pp_record_field ~first:false "expr" pp_expr fmt v.Frontend_types.expr;
    Pbrt.Pp.pp_record_field
      ~first:false
      "cast_type"
      pp_expr_p_cast
      fmt
      v.Frontend_types.cast_type
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_var_p_load fmt (v : Frontend_types.var_p_load) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "var" pp_var fmt v.Frontend_types.var
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
;;

let rec pp_stmt_p_var_decl fmt (v : Frontend_types.stmt_p_var_decl) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~first:true
      "var_id"
      Pbrt.Pp.pp_string
      fmt
      v.Frontend_types.var_id;
    Pbrt.Pp.pp_record_field ~first:false "texpr" pp_type_expr fmt v.Frontend_types.texpr
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
;;

let rec pp_stmt_p_printf fmt (v : Frontend_types.stmt_p_printf) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~first:true
      "format"
      Pbrt.Pp.pp_string
      fmt
      v.Frontend_types.format;
    Pbrt.Pp.pp_record_field
      ~first:false
      "f_args"
      (Pbrt.Pp.pp_list pp_expr)
      fmt
      v.Frontend_types.f_args
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
;;

let rec pp_stmt_p_expr_stmt fmt (v : Frontend_types.stmt_p_expr_stmt) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "expr_stmt" pp_expr fmt v.Frontend_types.expr_stmt
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
;;

let rec pp_stmt_p_while fmt (v : Frontend_types.stmt_p_while) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~first:true
      "while_cond"
      pp_expr
      fmt
      v.Frontend_types.while_cond;
    Pbrt.Pp.pp_record_field
      ~first:false
      "while_block"
      pp_stmt
      fmt
      v.Frontend_types.while_block
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_stmt fmt (v : Frontend_types.stmt) =
  match v with
  | Frontend_types.Var_decl x ->
    Format.fprintf fmt "@[<hv2>Var_decl(@,%a)@]" pp_stmt_p_var_decl x
  | Frontend_types.Expr_stmt x ->
    Format.fprintf fmt "@[<hv2>Expr_stmt(@,%a)@]" pp_stmt_p_expr_stmt x
  | Frontend_types.Printf x ->
    Format.fprintf fmt "@[<hv2>Printf(@,%a)@]" pp_stmt_p_printf x
  | Frontend_types.While x -> Format.fprintf fmt "@[<hv2>While(@,%a)@]" pp_stmt_p_while x
  | Frontend_types.Block x -> Format.fprintf fmt "@[<hv2>Block(@,%a)@]" pp_stmt_p_block x
  | Frontend_types.Break -> Format.fprintf fmt "Break"
  | Frontend_types.Continue -> Format.fprintf fmt "Continue"
  | Frontend_types.If_stmt x ->
    Format.fprintf fmt "@[<hv2>If_stmt(@,%a)@]" pp_stmt_p_if_stmt x

and pp_stmt_p_block fmt (v : Frontend_types.stmt_p_block) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~first:true
      "stmt_list"
      (Pbrt.Pp.pp_list pp_stmt)
      fmt
      v.Frontend_types.stmt_list
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_stmt_p_if_stmt fmt (v : Frontend_types.stmt_p_if_stmt) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "eval" pp_expr fmt v.Frontend_types.eval;
    Pbrt.Pp.pp_record_field ~first:false "if_stmt" pp_stmt fmt v.Frontend_types.if_stmt;
    Pbrt.Pp.pp_record_field
      ~first:false
      "else_stmt"
      pp_stmt
      fmt
      v.Frontend_types.else_stmt
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
;;

let rec pp_class_def fmt (v : Frontend_types.class_def) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "name" Pbrt.Pp.pp_string fmt v.Frontend_types.name;
    Pbrt.Pp.pp_record_field
      ~first:false
      "fields"
      (Pbrt.Pp.pp_list pp_type_expr)
      fmt
      v.Frontend_types.fields;
    Pbrt.Pp.pp_record_field
      ~first:false
      "base_class_name"
      Pbrt.Pp.pp_string
      fmt
      v.Frontend_types.base_class_name
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
;;

let rec pp_param fmt (v : Frontend_types.param) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~first:true
      "param_type"
      pp_type_expr
      fmt
      v.Frontend_types.param_type;
    Pbrt.Pp.pp_record_field
      ~first:false
      "param_name"
      Pbrt.Pp.pp_string
      fmt
      v.Frontend_types.param_name
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
;;

let rec pp_function_def fmt (v : Frontend_types.function_def) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "name" Pbrt.Pp.pp_string fmt v.Frontend_types.name;
    Pbrt.Pp.pp_record_field
      ~first:false
      "return_t"
      pp_type_expr
      fmt
      v.Frontend_types.return_t;
    Pbrt.Pp.pp_record_field
      ~first:false
      "params"
      (Pbrt.Pp.pp_list pp_param)
      fmt
      v.Frontend_types.params;
    Pbrt.Pp.pp_record_field ~first:false "body" pp_stmt fmt v.Frontend_types.body
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
;;

let rec pp_program fmt (v : Frontend_types.program) =
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field
      ~first:true
      "main"
      (Pbrt.Pp.pp_list pp_stmt)
      fmt
      v.Frontend_types.main;
    Pbrt.Pp.pp_record_field
      ~first:false
      "classdefs"
      (Pbrt.Pp.pp_list pp_class_def)
      fmt
      v.Frontend_types.classdefs;
    Pbrt.Pp.pp_record_field
      ~first:false
      "function_defs"
      (Pbrt.Pp.pp_list pp_function_def)
      fmt
      v.Frontend_types.function_defs
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
;;
