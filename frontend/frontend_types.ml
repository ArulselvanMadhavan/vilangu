[@@@ocaml.warning "-27-30-39"]

type un_op =
  | Not
  | Neg

type bin_op = Plus
type identifier_p_var = { var_name : string }
type identifier = Var of identifier_p_var
type expr_p_var_decl = { var_id : string }

type expr_p_function_app =
  { name : string
  ; args : expr list
  }

and expr =
  | Integer of int32
  | Function_app of expr_p_function_app
  | Printf of expr_p_printf
  | Unop of expr_p_unop
  | Binop of expr_p_binop
  | Var_decl of expr_p_var_decl
  | Assign of expr_p_assign
  | Expr_id of identifier

and expr_p_printf =
  { format : string
  ; f_args : expr list
  }

and expr_p_unop =
  { op : un_op
  ; uexpr : expr
  }

and expr_p_binop =
  { bin_op : bin_op
  ; lexpr : expr
  ; rexpr : expr
  }

and expr_p_assign =
  { lhs : identifier
  ; rhs : expr
  }

type program = { main : expr list }

let rec default_un_op () : un_op = Not
let rec default_bin_op () : bin_op = Plus

let rec default_identifier_p_var ?(var_name : string = "") () : identifier_p_var =
  { var_name }
;;

let rec default_identifier () : identifier = Var (default_identifier_p_var ())
let rec default_expr_p_var_decl ?(var_id : string = "") () : expr_p_var_decl = { var_id }

let rec default_expr_p_function_app ?(name : string = "") ?(args : expr list = []) ()
  : expr_p_function_app
  =
  { name; args }

and default_expr () : expr = Integer 0l

and default_expr_p_printf ?(format : string = "") ?(f_args : expr list = []) ()
  : expr_p_printf
  =
  { format; f_args }

and default_expr_p_unop
  ?(op : un_op = default_un_op ())
  ?(uexpr : expr = default_expr ())
  ()
  : expr_p_unop
  =
  { op; uexpr }

and default_expr_p_binop
  ?(bin_op : bin_op = default_bin_op ())
  ?(lexpr : expr = default_expr ())
  ?(rexpr : expr = default_expr ())
  ()
  : expr_p_binop
  =
  { bin_op; lexpr; rexpr }

and default_expr_p_assign
  ?(lhs : identifier = default_identifier ())
  ?(rhs : expr = default_expr ())
  ()
  : expr_p_assign
  =
  { lhs; rhs }
;;

let rec default_program ?(main : expr list = []) () : program = { main }
