%{
open Ast
%}
%token EOF
%token TILDE
%token INT
%token MAIN CLASS EXTENDS
%token THIS SUPER
%token WHILE NEW RETURN DELETE IF ELSE
%token OUT
%token BREAK CONTINUE
%token NULL
%token <string> ID
%token <int32> NUM
%token ASSIGN_OP
%token LT GT EQUALS
%token PLUS MINUS
%token MULT DIV
%token LPAREN RPAREN LBRACE RBRACE LSQB RSQB
%token SEMICOLON COMMA DOT
%token NOT
(* %left LT GT *)
(* %nonassoc UMINUS *)
%start <comp_unit> prog

%{
let lp((sp, ep) : (Lexing.position * Lexing.position)) : pos
  = ((sp.pos_lnum, sp.pos_cnum - sp.pos_bol), (ep.pos_lnum, ep.pos_cnum - sp.pos_bol))

exception NotAValidCastExp
let get_type = function
  | Ast.VarExp (Ast.SimpleVar (sym, pos), _) -> Ast.ClassType (sym, pos)
  | _ -> raise NotAValidCastExp
%}

%%

let prog :=
  ~=comp_unit; EOF; <>         (* <> is identity *)

let comp_unit :=                       
  | ~=main_decl; { {main_decl ; class_decs = []}}
  | ~=main_decl; ~=class_decls; { {main_decl; class_decs = class_decls} }
  | ~=class_decls; ~=main_decl; { {main_decl; class_decs = class_decls} }
  | cs1=class_decls; ~=main_decl; cs2=class_decls; { {main_decl; class_decs= cs1 @ cs2}}

let main_decl :=
  | INT; MAIN; LPAREN; RPAREN; ~=main_block; { main_block }

let main_block :=
  | LBRACE; ~=main_blk_stmts; RBRACE; { List.rev main_blk_stmts }
  | LBRACE; RBRACE; { [] }

let class_decls :=
  | ~=class_decls; ~=class_decl; { class_decl :: class_decls }
  | ~=class_decl; { [ class_decl ] }

let class_decl :=
  | CLASS; ~=id; ~=class_body; { ClassDec {name = id; base=None; class_body; pos=lp($loc)} }
  | CLASS; name=id; EXTENDS; class_type=id; ~=class_body; { ClassDec {name; base=Some class_type; class_body; pos=lp($loc)} }

let class_body :=
  | LBRACE; ~=class_body_decls; RBRACE; { List.rev class_body_decls }
  | LBRACE; RBRACE; { [] }

let class_body_decls :=
  | ~=class_body_decls; ~=class_body_decl; { class_body_decl :: class_body_decls }
  | ~=class_body_decl; { [ class_body_decl ] }

let class_body_decl :=
  | ~=const_decl; { const_decl }
  | ~=dest_decl; { dest_decl }
  | ~=class_mem; { class_mem }

let class_mem :=
  | ~=field_decl; { field_decl }
  | ~=mthd_decl; { mthd_decl }

let field_decl :=
  | type_=typ; ~=decls; SEMICOLON; { FieldDec (List.map (fun (rank, id) -> Field { type_ = append_rank_to_type rank type_; name = id; pos = lp($loc)}) (decls)) }

let decls ==
  separated_list(COMMA, decl)

let decl :=
  | (rank, id)=decl; _e=dimension; { (rank + 1, id) }
  | ~=id; { (0, id) }

let mthd_decl :=
  | type_=typ; (id, fparams)=mthd_desc; ~=mth_body; { Method {return_t = Return { type_ }; name=id; fparams ; body = mth_body} }

let mthd_desc :=
  | ~=id; ~=formal_params; { (id, formal_params) }
  (* method_decl dimension - Is this true?*)

let mth_body :=
  | ~=block; { block }

let const_decl :=
  | (id, fparams)=const_desc; ~=const_body; { Constructor { name=id; fparams; body=const_body } }

let const_desc :=
  | ~=id; ~=formal_params; { (id,formal_params) }

let const_body :=
  | LBRACE; ~=const_invoc; ~=block_stmts; RBRACE; { Block (ExprStmt const_invoc :: block_stmts) }
  | LBRACE; ~=const_invoc; RBRACE; { Block [ ExprStmt const_invoc ] }
  | ~=block; { block }

let const_invoc :=
  | THIS; ~=arguments; SEMICOLON; { MethodCall { base = (This (lp($loc))); field = None; args = arguments; pos = lp($loc) ; vtbl_idx = None } }
  | SUPER; ~=arguments; SEMICOLON; { MethodCall { base = (Super (lp($loc))); field = None; args = arguments; pos = lp($loc) ; vtbl_idx = None} }

let dest_decl :=
  | ~=dest_desc; ~=dest_body; { Destructor {name = dest_desc; body = dest_body } }

let dest_desc :=
  | TILDE; ~=id; LPAREN; RPAREN; { let name, i = id in ("~" ^ name, i)}

let dest_body :=
  | ~=block; { block }

let formal_params ==
  | LPAREN; ~=fplist; RPAREN; { fplist }
  | LPAREN; RPAREN; {[]}

let fplist :=
  | ~=fplist; COMMA; ~=formal_param; { formal_param :: fplist }
  | ~=formal_param; { [ formal_param] }

let formal_param :=
  | type_=typ; (rank1, id)=decl; { Param { type_ = append_rank_to_type rank1 type_; name = id} }

(* VariableDeclarationID substituted with decl *)
let block :=
  | LBRACE; ~=block_stmts; RBRACE; { Block (List.rev block_stmts) }
  | LBRACE; RBRACE; { Block [] }

let block_stmts :=
  | ~=block_stmts; ~=block_stmt; { block_stmt :: block_stmts }
  | ~=block_stmt; { [ block_stmt ] }

let block_stmt :=
  | ~=stmt; { stmt }

(* Main block substituted *)
let main_blk_stmts :=
  | ~=main_blk_stmts; ~=main_blk_stmt; { main_blk_stmt :: main_blk_stmts }
  | ~=main_blk_stmt; { [main_blk_stmt] }

let main_blk_stmt :=
  | ~=main_var_dec; { VariableDecl main_var_dec }
  | ~=block_stmt; { MainStmt block_stmt }

let main_var_dec :=
  | ~=main_var_desc; SEMICOLON; { main_var_desc }

let main_var_desc :=
  | type_=typ; ~=decls; { (List.map (fun (rank, id) -> {type_ = append_rank_to_type rank type_; id}) decls)}

let stmt :=
  | ~=while_stmt; <>
  | ~=return_stmt; <>
  | ~=empty_stmt; <>
  | ~=continue; <>                                  
  | ~=break; <>
  | ~=expr_stmt; <>
  | ~=delete_stmt; <>
  | ~=if_stmt; <>
  | ~=out_stmt; <>
  | ~=block; <>

let if_stmt :=
  | IF; exp=paren_exp; istmt=stmt; ELSE; estmt=stmt; { IfElse {exp;istmt;estmt;pos=lp($loc)} }
               
let while_stmt :=
  | WHILE; ~=paren_exp; ~=stmt; { While { exp=paren_exp; block=stmt} }

let return_stmt :=                     
  | RETURN; SEMICOLON; { ReturnStmt None}
  | RETURN; ~=exp; SEMICOLON; {ReturnStmt (Some exp)}

let delete_stmt :=
  | DELETE; ~=exp; SEMICOLON; { Delete (exp, lp($loc)) }

let out_stmt :=                
  | OUT; ~=exp; SEMICOLON; { Output (exp, lp($loc)) }

let break :=
  | BREAK; SEMICOLON; { Break (lp($loc)) }

let continue :=
  | CONTINUE; SEMICOLON; { Continue (lp($loc)) }

let empty_stmt :=                    
  | SEMICOLON; {Empty}

let paren_exp :=
  | LPAREN; ~=exp; RPAREN; { exp }

let expr_stmt :=
  | ~=stmt_expr; SEMICOLON; { stmt_expr }

let stmt_expr :=
  | ~=assignment; { ExprStmt assignment }
  | ~=mth_invoc; { ExprStmt mth_invoc}

let exp :=
  | left=exp; EQUALS; right=relexp; { OpExp (BinaryOp {left; right; oper=EqualsOp}, lp($loc)) }
  | ~=relexp; {relexp}

let assignment :=
  | lhs=lhs; ASSIGN_OP; ~=exp; { Assignment {lhs; exp; pos=lp($loc)} }

let lhs :=
  | ~=id; { (SimpleVar (id, lp($loc))) }
  | ~=field_access; { LoadVar field_access }
  | ~=array_access; { array_access }

let relexp :=
  | left=relexp; LT; right=addexp; { OpExp (BinaryOp {left; right; oper=LessThanOp}, lp($loc)) }
  | left=relexp; GT; right=addexp; { OpExp (BinaryOp {left; right; oper=GreaterThanOp}, lp($loc)) }
  | ~=addexp; { addexp }

let addexp :=
  | left=addexp; PLUS; right=mulexp; { OpExp (BinaryOp {left;right; oper=PlusOp}, lp($loc))}
  | left=addexp; MINUS; right=mulexp; {OpExp (BinaryOp {left; right; oper=MinusOp}, lp($loc))}
  | ~=mulexp; { mulexp }

let mulexp :=
  | left=mulexp; MULT; right=unaryexp; { OpExp (BinaryOp {left; right; oper=MultOp}, lp($loc))}
  | left=mulexp; DIV; right=unaryexp; {OpExp (BinaryOp {left; right; oper=DivideOp}, lp($loc))}
  | ~=unaryexp; { unaryexp }

let unaryexp :=
  | MINUS; exp=castexp; { OpExp (UnaryOp{oper= NegateOp; exp}, lp($loc)) }
  | NOT; exp=castexp; { OpExp (UnaryOp {oper=NotOp; exp}, lp($loc)) }
  | ~=castexp; <>

let castexp :=
  | ~=paren_exp; exp=castexp; { CastType { type_ = Reference (get_type paren_exp); exp; cast_type = None; pos = lp($loc) } }
  | LPAREN; type_=arr_type;RPAREN; exp=castexp; { CastType { type_ = Reference type_; exp; cast_type = None; pos = lp($loc)}}
  | ~=primary; { primary }

let primary :=
  | ~=id; { VarExp (SimpleVar (id, lp($loc)), lp($loc)) } (* VarExp SimpleVar *)
  | ~=arrayexpr; { arrayexpr }
  | ~=primlit; { primlit }

let primlit :=
  | ~=paren_exp; { paren_exp }
  | THIS; { This (lp($loc)) }
  | ~=field_access; { VarExp (field_access, lp($loc)) }
  | ~=mth_invoc; { mth_invoc }
  | ~=literal; { literal }
  | ~=array_access; { VarExp (array_access, lp($loc)) }
  | ~=class_inst_creation; { class_inst_creation }

let class_inst_creation :=
  | NEW; type_=class_type; ~=arguments; { ClassCreationExp {type_ = Reference type_; args=arguments; pos = (lp($loc)); vtbl_idx = None} }

let arrayexpr :=
  | NEW; ~=prim_type; ~=dimexprs; empty_dims=dimensions; {
     ArrayCreationExp {type_=append_rank_to_type (empty_dims + (List.length dimexprs)) prim_type; exprs=dimexprs; pos = lp($loc); vtbl_idx = None}}
  | NEW; ~=prim_type; ~=dimexprs; { ArrayCreationExp {type_ = append_rank_to_type (List.length dimexprs) prim_type; exprs=dimexprs; pos= lp($loc); vtbl_idx = None} }
  | NEW; type_=class_type; ~=dimexprs; empty_dims=dimensions; { ArrayCreationExp {type_ = append_rank_to_type (empty_dims + (List.length dimexprs)) (Reference type_); exprs=dimexprs; pos = lp($loc); vtbl_idx = None}}
  | NEW; type_=class_type; ~=dimexprs; { ArrayCreationExp {type_ = append_rank_to_type (List.length dimexprs) (Reference type_); exprs=dimexprs; pos = lp($loc); vtbl_idx = None}}

let dimexprs :=
  | ~=dimexprs; ~=dimexpr; { dimexpr :: dimexprs }
  | ~=dimexpr; { [dimexpr] }

let dimexpr :=
  | LSQB; ~=exp; RSQB; { exp }

let dimensions :=
  | rank=dimensions; _d=dimension; { rank + 1}
  | _d=dimension; { 1 }

let dimension :=
  | LSQB; RSQB; {}

let field_access :=
  | ~=primary; DOT; ~=id; { FieldVar (primary, id, (-1), lp($loc)) }
  | SUPER; DOT; ~=id; { FieldVar ((Super (lp($loc))), id, (-1), lp($loc)) }

let mth_invoc :=
  | ~=id; ~=arguments; { MethodCall {base = VarExp (SimpleVar (id, lp($loc)), lp($loc)); field = None; args = arguments; pos = lp($loc); vtbl_idx = None} }
  | ~=primary;  DOT; ~=id; ~=arguments; { MethodCall {base = primary; field = Some (Identifier (id, lp($loc))); args = arguments; pos = lp($loc); vtbl_idx = None} }
  | SUPER; DOT; ~=id; ~=arguments; { MethodCall {base=(Super (lp($loc))); field=Some (Identifier (id, lp($loc))); args=arguments; pos = lp($loc); vtbl_idx = None} }

let primary_no_new_array :=
  | ~=array_access; { array_access }
  | ~=field_access; { field_access }

let array_access :=
  | ~=id; ~=dimexpr; { SubscriptVar (SimpleVar (id, lp($loc)), dimexpr, lp($loc)) }
  | ~=primary_no_new_array; ~=dimexpr; { SubscriptVar (primary_no_new_array, dimexpr, lp($loc)) }

let arguments :=
  | LPAREN; ~=args_list; RPAREN; { args_list }
  | LPAREN; RPAREN; {[]}

let args_list :=
  | ~=args_list; COMMA; ~=exp; { exp :: args_list }
  | ~=exp; { [exp]}

let typ :=
  | ~=prim_type; { prim_type }
  | ~=ref_type; { ref_type }

let prim_type :=
  | ~=num_type; { num_type }

let num_type :=
  | ~=int_type; { int_type }

let int_type :=
  | INT; { Primitive (Env.int_symbol, lp($loc)) }

let ref_type :=
  | ~=class_type; { Reference class_type }
  | ~=arr_type; { Reference arr_type }

let class_type :=
  | ~=id; { (ClassType (id, lp($loc))) }

let arr_type :=
  | ~=prim_type; dimension; { ArrayType (1, prim_type) }
  | ~=id; dimension; { ArrayType (1, (Reference (ClassType (id, lp($loc))))) }
  | prev_arr=arr_type; dimension; {
      match prev_arr with
      | ArrayType (rank, ty) -> ArrayType (rank + 1, ty)
      | ty -> ty
  }

let literal :=
  | intval=NUM; { IntLit (intval, lp($loc))
                  }
  | NULL; { NullLit (lp($loc)) }

let id :=
  | ~=ID;                                 <Symbol.symbol>                                            
