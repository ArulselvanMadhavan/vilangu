[@@@ocaml.warning "-27-30-39"]


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
  field_line_no : int32;
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

let rec default_un_op (): un_op = Not

let rec default_bin_op (): bin_op = Plus

let rec default_var_p_simple 
  ?var_name:((var_name:string) = "")
  () : var_p_simple  = {
  var_name;
}

let rec default_type_expr_p_class 
  ?name:((name:string) = "")
  () : type_expr_p_class  = {
  name;
}

let rec default_type_expr (): type_expr = Int32

and default_type_expr_p_pointer 
  ?data:((data:type_expr) = default_type_expr ())
  () : type_expr_p_pointer  = {
  data;
}

let rec default_expr_p_cast (): expr_p_cast = No_cast

let rec default_var_p_subscript 
  ?base_var:((base_var:var) = default_var ())
  ?var_exp:((var_exp:expr) = default_expr ())
  ?len_var:((len_var:var) = default_var ())
  ?line_no:((line_no:int32) = 0l)
  () : var_p_subscript  = {
  base_var;
  var_exp;
  len_var;
  line_no;
}

and default_var () : var = Simple (default_var_p_simple ())

and default_var_p_field 
  ?base_expr:((base_expr:expr) = default_expr ())
  ?field_index:((field_index:int32) = 0l)
  ?field_line_no:((field_line_no:int32) = 0l)
  () : var_p_field  = {
  base_expr;
  field_index;
  field_line_no;
}

and default_expr () : expr = Integer (0l)

and default_expr_p_function_app 
  ?name:((name:string) = "")
  ?args:((args:expr list) = [])
  () : expr_p_function_app  = {
  name;
  args;
}

and default_expr_p_unop 
  ?op:((op:un_op) = default_un_op ())
  ?uexpr:((uexpr:expr) = default_expr ())
  () : expr_p_unop  = {
  op;
  uexpr;
}

and default_expr_p_binop 
  ?bin_op:((bin_op:bin_op) = default_bin_op ())
  ?lexpr:((lexpr:expr) = default_expr ())
  ?rexpr:((rexpr:expr) = default_expr ())
  () : expr_p_binop  = {
  bin_op;
  lexpr;
  rexpr;
}

and default_expr_p_assign 
  ?lhs:((lhs:var) = default_var ())
  ?rhs:((rhs:expr) = default_expr ())
  () : expr_p_assign  = {
  lhs;
  rhs;
}

and default_identifier 
  ?id:((id:var) = default_var ())
  () : identifier  = {
  id;
}

and default_expr_p_array_creation 
  ?creation_exprs:((creation_exprs:expr list) = [])
  ?texpr:((texpr:type_expr) = default_type_expr ())
  ?make_line_no:((make_line_no:int32) = 0l)
  () : expr_p_array_creation  = {
  creation_exprs;
  texpr;
  make_line_no;
}

and default_expr_p_cast_expr 
  ?cast_to:((cast_to:type_expr) = default_type_expr ())
  ?expr:((expr:expr) = default_expr ())
  ?cast_type:((cast_type:expr_p_cast) = default_expr_p_cast ())
  () : expr_p_cast_expr  = {
  cast_to;
  expr;
  cast_type;
}

and default_var_p_load 
  ?var:((var:var) = default_var ())
  () : var_p_load  = {
  var;
}

let rec default_stmt_p_var_decl 
  ?var_id:((var_id:string) = "")
  ?texpr:((texpr:type_expr) = default_type_expr ())
  () : stmt_p_var_decl  = {
  var_id;
  texpr;
}

let rec default_stmt_p_printf 
  ?format:((format:string) = "")
  ?f_args:((f_args:expr list) = [])
  () : stmt_p_printf  = {
  format;
  f_args;
}

let rec default_stmt_p_expr_stmt 
  ?expr_stmt:((expr_stmt:expr) = default_expr ())
  () : stmt_p_expr_stmt  = {
  expr_stmt;
}

let rec default_stmt_p_while 
  ?while_cond:((while_cond:expr) = default_expr ())
  ?while_block:((while_block:stmt) = default_stmt ())
  () : stmt_p_while  = {
  while_cond;
  while_block;
}

and default_stmt () : stmt = Var_decl (default_stmt_p_var_decl ())

and default_stmt_p_block 
  ?stmt_list:((stmt_list:stmt list) = [])
  () : stmt_p_block  = {
  stmt_list;
}

and default_stmt_p_if_stmt 
  ?eval:((eval:expr) = default_expr ())
  ?if_stmt:((if_stmt:stmt) = default_stmt ())
  ?else_stmt:((else_stmt:stmt) = default_stmt ())
  () : stmt_p_if_stmt  = {
  eval;
  if_stmt;
  else_stmt;
}

let rec default_class_def 
  ?name:((name:string) = "")
  ?fields:((fields:type_expr list) = [])
  ?base_class_name:((base_class_name:string) = "")
  () : class_def  = {
  name;
  fields;
  base_class_name;
}

let rec default_param 
  ?param_type:((param_type:type_expr) = default_type_expr ())
  ?param_name:((param_name:string) = "")
  () : param  = {
  param_type;
  param_name;
}

let rec default_function_def 
  ?name:((name:string) = "")
  ?return_t:((return_t:type_expr) = default_type_expr ())
  ?params:((params:param list) = [])
  ?body:((body:stmt) = default_stmt ())
  () : function_def  = {
  name;
  return_t;
  params;
  body;
}

let rec default_program 
  ?main:((main:stmt list) = [])
  ?classdefs:((classdefs:class_def list) = [])
  ?function_defs:((function_defs:function_def list) = [])
  () : program  = {
  main;
  classdefs;
  function_defs;
}
