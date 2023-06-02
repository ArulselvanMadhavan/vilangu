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
%token MULT
%token LPAREN RPAREN LBRACE RBRACE LSQB RSQB
%token SEMICOLON COMMA DOT
%token NOT
(* %left LT GT *)
(* %nonassoc UMINUS *)
%start <comp_unit> prog

%{
let lp((sp, ep) : (Lexing.position * Lexing.position)) : pos
      = ((sp.pos_lnum, sp.pos_cnum - sp.pos_bol), (ep.pos_lnum, ep.pos_cnum - sp.pos_bol))
%}

%%

let prog :=
  ~=comp_unit; EOF; <>         (* <> is identity *)

let comp_unit :=                       
  | ~=main_decl; { {main_decl ; classdecs = []}}
  | ~=main_decl; ~=class_decls; { {main_decl; classdecs = class_decls} }
  | ~=class_decls; ~=main_decl; { {main_decl; classdecs = class_decls} }
  | cs1=class_decls; ~=main_decl; cs2=class_decls; { {main_decl; classdecs= cs1 @ cs2}}

let main_decl :=
  | INT; MAIN; LPAREN; RPAREN; ~=main_block; { main_block }

let main_block :=
  | LBRACE; ~=main_blk_stmts; RBRACE; { List.rev main_blk_stmts }
  | LBRACE; RBRACE; { [] }

let class_decls :=
  | ~=class_decls; ~=class_decl; { class_decl :: class_decls }
  | ~=class_decl; { [ class_decl ] }

let class_decl :=
  | CLASS; ~=id; ~=class_body; { ClassDec {name = id; base=None; class_body} }
  | CLASS; name=id; EXTENDS; class_type=id; ~=class_body; { ClassDec {name; base=Some class_type; class_body} }

let class_body :=
  | LBRACE; ~=class_body_decls; RBRACE; { class_body_decls }
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
  | (rank1, type_)=typ; ~=decls; SEMICOLON; { FieldDec (List.map (fun (rank2, id) -> Field { type_; rank = rank1 + rank2; name = id}) decls) }

let decls ==
  separated_list(COMMA, decl)

let decl :=
  | (rank, id)=decl; _e=dimension; { (rank + 1, id) }
  | ~=id; { (0, id) }

let mthd_decl :=
  | (rank, type_)=typ; (id, fparams)=mthd_desc; ~=mth_body; { Method {return_t = Return { type_; rank}; name=id; fparams ; body = mth_body} }

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
  | THIS; ~=arguments; SEMICOLON; { MethodCall { base = (This (lp($loc))); field = None; args = arguments; pos = lp($loc) } }
  | SUPER; ~=arguments; SEMICOLON; { MethodCall { base = (Super (lp($loc))); field = None; args = arguments; pos = lp($loc) } }

let dest_decl :=
  | ~=dest_desc; ~=dest_body; { Destructor {name = dest_desc; body = dest_body } }

let dest_desc :=
  | TILDE; ~=id; LPAREN; RPAREN; { id }

let dest_body :=
  | ~=block; { block }

let formal_params ==
  | LPAREN; ~=fplist; RPAREN; { fplist }
  | LPAREN; RPAREN; {[]}

let fplist :=
  | ~=fplist; COMMA; ~=formal_param; { formal_param :: fplist }
  | ~=formal_param; { [ formal_param] }

let formal_param :=
 | (rank1, typ)=typ; (rank2, id)=decl; { Param { typ; name = id; rank = rank1 + rank2 } }

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
  | (rank1, type_)=typ; ~=decls; { (List.map (fun (rank2, id) -> {type_; id; rank=rank1 + rank2}) decls)}

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
  | IF; exp=paren_exp; istmt=stmt; ELSE; estmt=stmt; { IfElse {exp;istmt;estmt} }
               
let while_stmt :=
  | WHILE; ~=paren_exp; ~=stmt; { While { exp=paren_exp; block=stmt} }

let return_stmt :=                     
  | RETURN; SEMICOLON; { ReturnStmt None}
  | RETURN; ~=exp; SEMICOLON; {ReturnStmt (Some exp)}

let delete_stmt :=
  | DELETE; ~=exp; SEMICOLON; { Delete exp }

let out_stmt :=                
  | OUT; ~=exp; SEMICOLON; { Output exp }

let break :=
  | BREAK; SEMICOLON; { Break }

let continue :=
  | CONTINUE; { Continue }

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
  | ~=id; { SimpleVar (id, lp($loc)) }
  | ~=field_access; { field_access }
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
  | ~=unaryexp; { unaryexp }

let unaryexp :=
  | MINUS; exp=castexp; { OpExp (UnaryOp{oper= NegateOp; exp}, lp($loc)) }
  | NOT; exp=castexp; { OpExp (UnaryOp {oper=NotOp; exp}, lp($loc)) }
  | ~=castexp; <>

let castexp :=
  | ~=paren_exp; ~=castexp; { CastEvalExp {to_=paren_exp; from_=castexp}}
  | LPAREN; (rank, type_)=arr_type;RPAREN; exp=castexp; { CastType { rank; type_; exp}}
  | ~=primary; { primary }

let primary :=
  | ~=id; { Identifier (id, lp($loc)) }
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
  | NEW; (_, type_)=class_type; ~=arguments; { ClassCreationExp {type_; args=arguments; pos = (lp($loc))} }

let arrayexpr :=
  | NEW; ~=prim_type; ~=dimexprs; empty_dims=dimensions; { ArrayCreationExp {type_=prim_type; exprs=dimexprs; empty_dims; pos = lp($loc)}}
  | NEW; ~=prim_type; ~=dimexprs; { ArrayCreationExp {type_ = prim_type; exprs=dimexprs; empty_dims=0; pos= lp($loc)} }
  | NEW; (_, type_)=class_type; ~=dimexprs; empty_dims=dimensions; { ArrayCreationExp {type_; exprs=dimexprs; empty_dims; pos = lp($loc)}}
  | NEW; (_, type_)=class_type; ~=dimexprs; { ArrayCreationExp {type_; exprs=dimexprs; empty_dims=0; pos = lp($loc)}}

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
  | ~=primary; DOT; ~=id; { FieldVar (primary, id, lp($loc)) }
  | SUPER; DOT; ~=id; { FieldVar ((Super (lp($loc))), id, lp($loc)) }

let mth_invoc :=
  | ~=id; ~=arguments; { MethodCall {base = Identifier (id, lp($loc)); field = None; args = arguments; pos = lp($loc)} }
  | ~=primary;  DOT; ~=id; ~=arguments; { MethodCall {base = primary; field = Some (Identifier (id, lp($loc))); args = arguments; pos = lp($loc)} }
  | SUPER; DOT; ~=id; ~=arguments; { MethodCall {base=(Super (lp($loc))); field=Some (Identifier (id, lp($loc))); args=arguments; pos = lp($loc)} }

let array_access :=
  | ~=id; ~=dimexpr; { (SubscriptVar (SimpleVar (id, lp($loc)), dimexpr, lp($loc))) }

let arguments :=
  | LPAREN; ~=args_list; RPAREN; { args_list }
  | LPAREN; RPAREN; {[]}

let args_list :=
  | ~=args_list; COMMA; ~=exp; { exp :: args_list }
  | ~=exp; { [exp]}

let typ :=
  | ~=prim_type; { (0, prim_type) }
  | ~=ref_type; { ref_type }

let prim_type :=
  | ~=num_type; { num_type }

let num_type :=
  | ~=int_type; { int_type }

let int_type :=
  | INT; { NameTy (Env.int_symbol, lp($loc)) }

let ref_type :=
  | ~=class_type; { class_type }
  | ~=arr_type; { arr_type }

let class_type :=
  | ~=id; { (0, NameTy (id, lp($loc))) }

let arr_type :=
  | ~=prim_type; dimension; { (1, prim_type) }
  | ~=id; dimension; { (1, NameTy (id, lp($loc))) }
  | (rank, dim)=arr_type; dimension; {(rank + 1, dim)}

let literal :=
  | intval=NUM; { IntLit (intval, lp($loc))
                  }
  | NULL; { NullLit (lp($loc)) }

let id :=
  | ~=ID;                                 <Symbol.symbol>                                            
