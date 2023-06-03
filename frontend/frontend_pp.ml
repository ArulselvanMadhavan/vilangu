[@@@ocaml.warning "-27-30-39"]

let rec pp_un_op fmt (v:Frontend_types.un_op) =
  match v with
  | Frontend_types.Not  -> Format.fprintf fmt "Not"
  | Frontend_types.Neg  -> Format.fprintf fmt "Neg"

let rec pp_bin_op fmt (v:Frontend_types.bin_op) =
  match v with
  | Frontend_types.Plus  -> Format.fprintf fmt "Plus"
  | Frontend_types.Equals  -> Format.fprintf fmt "Equals"
  | Frontend_types.Lessthan  -> Format.fprintf fmt "Lessthan"

let rec pp_identifier_p_var fmt (v:Frontend_types.identifier_p_var) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "var_name" Pbrt.Pp.pp_string fmt v.Frontend_types.var_name;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_identifier fmt (v:Frontend_types.identifier) =
  match v with
  | Frontend_types.Var x -> Format.fprintf fmt "@[<hv2>Var(@,%a)@]" pp_identifier_p_var x

let rec pp_type_expr_p_ranked_type fmt (v:Frontend_types.type_expr_p_ranked_type) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "rank" Pbrt.Pp.pp_int32 fmt v.Frontend_types.rank;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_type_expr fmt (v:Frontend_types.type_expr) =
  match v with
  | Frontend_types.Int32_ty x -> Format.fprintf fmt "@[<hv2>Int32_ty(@,%a)@]" pp_type_expr_p_ranked_type x

let rec pp_expr_p_var_decl fmt (v:Frontend_types.expr_p_var_decl) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "var_id" Pbrt.Pp.pp_string fmt v.Frontend_types.var_id;
    Pbrt.Pp.pp_record_field ~first:false "texpr" pp_type_expr fmt v.Frontend_types.texpr;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_expr_p_function_app fmt (v:Frontend_types.expr_p_function_app) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "name" Pbrt.Pp.pp_string fmt v.Frontend_types.name;
    Pbrt.Pp.pp_record_field ~first:false "args" (Pbrt.Pp.pp_list pp_expr) fmt v.Frontend_types.args;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expr fmt (v:Frontend_types.expr) =
  match v with
  | Frontend_types.Integer x -> Format.fprintf fmt "@[<hv2>Integer(@,%a)@]" Pbrt.Pp.pp_int32 x
  | Frontend_types.Function_app x -> Format.fprintf fmt "@[<hv2>Function_app(@,%a)@]" pp_expr_p_function_app x
  | Frontend_types.Printf x -> Format.fprintf fmt "@[<hv2>Printf(@,%a)@]" pp_expr_p_printf x
  | Frontend_types.Unop x -> Format.fprintf fmt "@[<hv2>Unop(@,%a)@]" pp_expr_p_unop x
  | Frontend_types.Binop x -> Format.fprintf fmt "@[<hv2>Binop(@,%a)@]" pp_expr_p_binop x
  | Frontend_types.Var_decl x -> Format.fprintf fmt "@[<hv2>Var_decl(@,%a)@]" pp_expr_p_var_decl x
  | Frontend_types.Assign x -> Format.fprintf fmt "@[<hv2>Assign(@,%a)@]" pp_expr_p_assign x
  | Frontend_types.Expr_id x -> Format.fprintf fmt "@[<hv2>Expr_id(@,%a)@]" pp_identifier x
  | Frontend_types.If_expr x -> Format.fprintf fmt "@[<hv2>If_expr(@,%a)@]" pp_expr_p_if_expr x
  | Frontend_types.Block_expr x -> Format.fprintf fmt "@[<hv2>Block_expr(@,%a)@]" pp_expr_p_block x
  | Frontend_types.While_expr x -> Format.fprintf fmt "@[<hv2>While_expr(@,%a)@]" pp_expr_p_while_expr x
  | Frontend_types.Break  -> Format.fprintf fmt "Break"
  | Frontend_types.Continue  -> Format.fprintf fmt "Continue"
  | Frontend_types.Empty  -> Format.fprintf fmt "Empty"

and pp_expr_p_printf fmt (v:Frontend_types.expr_p_printf) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "format" Pbrt.Pp.pp_string fmt v.Frontend_types.format;
    Pbrt.Pp.pp_record_field ~first:false "f_args" (Pbrt.Pp.pp_list pp_expr) fmt v.Frontend_types.f_args;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expr_p_unop fmt (v:Frontend_types.expr_p_unop) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "op" pp_un_op fmt v.Frontend_types.op;
    Pbrt.Pp.pp_record_field ~first:false "uexpr" pp_expr fmt v.Frontend_types.uexpr;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expr_p_binop fmt (v:Frontend_types.expr_p_binop) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "bin_op" pp_bin_op fmt v.Frontend_types.bin_op;
    Pbrt.Pp.pp_record_field ~first:false "lexpr" pp_expr fmt v.Frontend_types.lexpr;
    Pbrt.Pp.pp_record_field ~first:false "rexpr" pp_expr fmt v.Frontend_types.rexpr;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expr_p_assign fmt (v:Frontend_types.expr_p_assign) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "lhs" pp_identifier fmt v.Frontend_types.lhs;
    Pbrt.Pp.pp_record_field ~first:false "rhs" pp_expr fmt v.Frontend_types.rhs;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expr_p_if_expr fmt (v:Frontend_types.expr_p_if_expr) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "eval" pp_expr fmt v.Frontend_types.eval;
    Pbrt.Pp.pp_record_field ~first:false "if_expr" pp_expr fmt v.Frontend_types.if_expr;
    Pbrt.Pp.pp_record_field ~first:false "else_expr" pp_expr fmt v.Frontend_types.else_expr;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expr_p_block fmt (v:Frontend_types.expr_p_block) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "expr_list" (Pbrt.Pp.pp_list pp_expr) fmt v.Frontend_types.expr_list;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_expr_p_while_expr fmt (v:Frontend_types.expr_p_while_expr) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "while_cond" pp_expr fmt v.Frontend_types.while_cond;
    Pbrt.Pp.pp_record_field ~first:false "while_block" pp_expr fmt v.Frontend_types.while_block;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_program fmt (v:Frontend_types.program) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "main" (Pbrt.Pp.pp_list pp_expr) fmt v.Frontend_types.main;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
