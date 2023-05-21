[@@@ocaml.warning "-27-30-39"]

let rec pp_un_op fmt (v:Frontend_types.un_op) =
  match v with
  | Frontend_types.Not  -> Format.fprintf fmt "Not"
  | Frontend_types.Neg  -> Format.fprintf fmt "Neg"

let rec pp_bin_op fmt (v:Frontend_types.bin_op) =
  match v with
  | Frontend_types.Plus  -> Format.fprintf fmt "Plus"

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

let rec pp_program fmt (v:Frontend_types.program) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "main" (Pbrt.Pp.pp_list pp_expr) fmt v.Frontend_types.main;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()