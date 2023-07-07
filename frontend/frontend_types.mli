(** frontend.proto Types *)



(** {2 Types} *)

type un_op =
  | Not
  | Neg

type bin_op =
  | Plus
  | Equals
  | Less_than
  | Greater_than
  | Mult_op
  | Divide_op
  | Subtract_op

type var_p_simple = {
  var_name : string;
}

type type_expr_p_class = {
  name : string;
}

type type_expr =
  | Int32
  | Class of type_expr_p_class
  | Pointer of type_expr_p_pointer
  | Void
  | Bool
  | Int8

and type_expr_p_pointer = {
  data : type_expr;
}

type expr_p_cast =
  | No_cast
  | Wide_cast
  | Narrow_cast

type var_p_subscript = {
  base_var : var;
  var_exp : expr;
  len_var : var;
  line_no : int32;
}

and var =
  | Simple of var_p_simple
  | Subscript of var_p_subscript
  | Field of var_p_field
  | Load_var of var_p_load

and var_p_field = {
  base_expr : expr;
  field_index : int32;
}

and expr =
  | Integer of int32
  | Function_app of expr_p_function_app
  | Unop of expr_p_unop
  | Binop of expr_p_binop
  | Assign of expr_p_assign
  | Expr_id of identifier
  | Empty
  | Array_creation of expr_p_array_creation
  | Var_exp of var
  | Null_lit
  | Cast_expr of expr_p_cast_expr

and expr_p_function_app = {
  name : string;
  args : expr list;
}

and expr_p_unop = {
  op : un_op;
  uexpr : expr;
}

and expr_p_binop = {
  bin_op : bin_op;
  lexpr : expr;
  rexpr : expr;
}

and expr_p_assign = {
  lhs : var;
  rhs : expr;
}

and identifier = {
  id : var;
}

and expr_p_array_creation = {
  creation_exprs : expr list;
  texpr : type_expr;
  make_line_no : int32;
}

and expr_p_cast_expr = {
  cast_to : type_expr;
  expr : expr;
  cast_type : expr_p_cast;
}

and var_p_load = {
  var : var;
}

type stmt_p_var_decl = {
  var_id : string;
  texpr : type_expr;
}

type stmt_p_printf = {
  format : string;
  f_args : expr list;
}

type stmt_p_expr_stmt = {
  expr_stmt : expr;
}

type stmt_p_while = {
  while_cond : expr;
  while_block : stmt;
}

and stmt =
  | Var_decl of stmt_p_var_decl
  | Expr_stmt of stmt_p_expr_stmt
  | Printf of stmt_p_printf
  | While of stmt_p_while
  | Block of stmt_p_block
  | Break
  | Continue
  | If_stmt of stmt_p_if_stmt

and stmt_p_block = {
  stmt_list : stmt list;
}

and stmt_p_if_stmt = {
  eval : expr;
  if_stmt : stmt;
  else_stmt : stmt;
}

type class_def = {
  name : string;
  fields : type_expr list;
  base_class_name : string;
}

type param = {
  param_type : type_expr;
  param_name : string;
}

type function_def = {
  name : string;
  return_t : type_expr;
  params : param list;
  body : stmt;
}

type program = {
  main : stmt list;
  classdefs : class_def list;
  function_defs : function_def list;
}


(** {2 Default values} *)

val default_un_op : unit -> un_op
(** [default_un_op ()] is the default value for type [un_op] *)

val default_bin_op : unit -> bin_op
(** [default_bin_op ()] is the default value for type [bin_op] *)

val default_var_p_simple : 
  ?var_name:string ->
  unit ->
  var_p_simple
(** [default_var_p_simple ()] is the default value for type [var_p_simple] *)

val default_type_expr_p_class : 
  ?name:string ->
  unit ->
  type_expr_p_class
(** [default_type_expr_p_class ()] is the default value for type [type_expr_p_class] *)

val default_type_expr : unit -> type_expr
(** [default_type_expr ()] is the default value for type [type_expr] *)

val default_type_expr_p_pointer : 
  ?data:type_expr ->
  unit ->
  type_expr_p_pointer
(** [default_type_expr_p_pointer ()] is the default value for type [type_expr_p_pointer] *)

val default_expr_p_cast : unit -> expr_p_cast
(** [default_expr_p_cast ()] is the default value for type [expr_p_cast] *)

val default_var_p_subscript : 
  ?base_var:var ->
  ?var_exp:expr ->
  ?len_var:var ->
  ?line_no:int32 ->
  unit ->
  var_p_subscript
(** [default_var_p_subscript ()] is the default value for type [var_p_subscript] *)

val default_var : unit -> var
(** [default_var ()] is the default value for type [var] *)

val default_var_p_field : 
  ?base_expr:expr ->
  ?field_index:int32 ->
  unit ->
  var_p_field
(** [default_var_p_field ()] is the default value for type [var_p_field] *)

val default_expr : unit -> expr
(** [default_expr ()] is the default value for type [expr] *)

val default_expr_p_function_app : 
  ?name:string ->
  ?args:expr list ->
  unit ->
  expr_p_function_app
(** [default_expr_p_function_app ()] is the default value for type [expr_p_function_app] *)

val default_expr_p_unop : 
  ?op:un_op ->
  ?uexpr:expr ->
  unit ->
  expr_p_unop
(** [default_expr_p_unop ()] is the default value for type [expr_p_unop] *)

val default_expr_p_binop : 
  ?bin_op:bin_op ->
  ?lexpr:expr ->
  ?rexpr:expr ->
  unit ->
  expr_p_binop
(** [default_expr_p_binop ()] is the default value for type [expr_p_binop] *)

val default_expr_p_assign : 
  ?lhs:var ->
  ?rhs:expr ->
  unit ->
  expr_p_assign
(** [default_expr_p_assign ()] is the default value for type [expr_p_assign] *)

val default_identifier : 
  ?id:var ->
  unit ->
  identifier
(** [default_identifier ()] is the default value for type [identifier] *)

val default_expr_p_array_creation : 
  ?creation_exprs:expr list ->
  ?texpr:type_expr ->
  ?make_line_no:int32 ->
  unit ->
  expr_p_array_creation
(** [default_expr_p_array_creation ()] is the default value for type [expr_p_array_creation] *)

val default_expr_p_cast_expr : 
  ?cast_to:type_expr ->
  ?expr:expr ->
  ?cast_type:expr_p_cast ->
  unit ->
  expr_p_cast_expr
(** [default_expr_p_cast_expr ()] is the default value for type [expr_p_cast_expr] *)

val default_var_p_load : 
  ?var:var ->
  unit ->
  var_p_load
(** [default_var_p_load ()] is the default value for type [var_p_load] *)

val default_stmt_p_var_decl : 
  ?var_id:string ->
  ?texpr:type_expr ->
  unit ->
  stmt_p_var_decl
(** [default_stmt_p_var_decl ()] is the default value for type [stmt_p_var_decl] *)

val default_stmt_p_printf : 
  ?format:string ->
  ?f_args:expr list ->
  unit ->
  stmt_p_printf
(** [default_stmt_p_printf ()] is the default value for type [stmt_p_printf] *)

val default_stmt_p_expr_stmt : 
  ?expr_stmt:expr ->
  unit ->
  stmt_p_expr_stmt
(** [default_stmt_p_expr_stmt ()] is the default value for type [stmt_p_expr_stmt] *)

val default_stmt_p_while : 
  ?while_cond:expr ->
  ?while_block:stmt ->
  unit ->
  stmt_p_while
(** [default_stmt_p_while ()] is the default value for type [stmt_p_while] *)

val default_stmt : unit -> stmt
(** [default_stmt ()] is the default value for type [stmt] *)

val default_stmt_p_block : 
  ?stmt_list:stmt list ->
  unit ->
  stmt_p_block
(** [default_stmt_p_block ()] is the default value for type [stmt_p_block] *)

val default_stmt_p_if_stmt : 
  ?eval:expr ->
  ?if_stmt:stmt ->
  ?else_stmt:stmt ->
  unit ->
  stmt_p_if_stmt
(** [default_stmt_p_if_stmt ()] is the default value for type [stmt_p_if_stmt] *)

val default_class_def : 
  ?name:string ->
  ?fields:type_expr list ->
  ?base_class_name:string ->
  unit ->
  class_def
(** [default_class_def ()] is the default value for type [class_def] *)

val default_param : 
  ?param_type:type_expr ->
  ?param_name:string ->
  unit ->
  param
(** [default_param ()] is the default value for type [param] *)

val default_function_def : 
  ?name:string ->
  ?return_t:type_expr ->
  ?params:param list ->
  ?body:stmt ->
  unit ->
  function_def
(** [default_function_def ()] is the default value for type [function_def] *)

val default_program : 
  ?main:stmt list ->
  ?classdefs:class_def list ->
  ?function_defs:function_def list ->
  unit ->
  program
(** [default_program ()] is the default value for type [program] *)
