%{
open Ast
%}
%token EOF
%token INT
%token MAIN CLASS EXTENDS
%token THIS
%token WHILE NEW
%token OUT
%token NULL
%token <string> ID
%token <int> NUM
%token ASSIGN_OP
%token LT GT EQUALS
%token PLUS
%token MULT
%token LPAREN RPAREN LBRACE RBRACE LSQB RSQB
%token SEMICOLON
%token COMMA
(* %left LT GT *)
%start <comp_unit> prog

%{
(* let lp((sp, ep) : (Lexing.position * Lexing.position)) : pos *)
    (*   = ((sp.pos_lnum, sp.pos_cnum - sp.pos_bol + 1), (ep.pos_lnum, ep.pos_cnum - sp.pos_bol + 1)) *)
let var (rank, id) = {type_=IntType; id; rank}
%}

%%

let prog :=
  ~=comp_unit; EOF; <>         (* <> is identity *)

let comp_unit :=                       
  | ~=class_decls; { ClassDecs class_decls }
  | INT; MAIN; LPAREN; RPAREN; ~=block; { MainFunc block }

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

let const_decl :=
  | (id, fparams)=const_desc; ~=const_body; { Constructor { name=id; fparams; body=const_body } }

let const_body :=
  | LBRACE; ~=const_invoc; RBRACE; { [ const_invoc ] }

let const_invoc :=
  | THIS; ~=arguments; SEMICOLON; { MethodCall { field = []; args = arguments} }

let arguments :=
  | LPAREN; ~=args_list; RPAREN; { args_list }

let args_list :=
  | ~=args_list; COMMA; ~=exp; { exp :: args_list }
  | ~=exp; { [exp]}

let const_desc :=
  | ~=id; ~=formal_params; { (id,formal_params) }

let formal_params ==
  | LPAREN; ~=fplist; RPAREN; { fplist }

let fplist == separated_list(COMMA, formal_param)

let formal_param :=
 | (rank1, typ)=typ; (rank2, id)=decl; { Field { typ; name = id; rank = rank1 + rank2 } }

let typ :=
  | ~=prim_type; { (0, prim_type) }
  | ~=ref_type; { ref_type }

let ref_type :=
  | ~=class_type; { class_type }
  | ~=arr_type; { arr_type }

let class_type :=
  | ~=id; { (0, NameTy id) }

let dimension :=
  | LSQB; RSQB; {}

let arr_type :=
  | ~=prim_type; dimension; { (1, prim_type) }
  | ~=id; dimension; { (1, NameTy id) }
  | (rank, dim)=arr_type; dimension; {(rank + 1, dim)}

let prim_type :=
  | ~=num_type; { num_type }

let num_type :=
  | ~=int_type; { int_type }

let int_type :=
  | INT; { IntType }

let block :=
  | LBRACE; ~=stmts; RBRACE; { stmts }
(* == is %inline non-terminal *)
let stmts ==
  list(stmt)
        
let stmt :=
  | INT; ~=decls; SEMICOLON; { VariableDecl (List.map var decls) }
  | lhs=lhs; ASSIGN_OP; ~=exp; SEMICOLON; { Assignment {lhs; exp} }
  | WHILE; ~=exp; ~=block; { While { exp; block} }
  | OUT; ~=exp; SEMICOLON; { Output exp }

let lhs :=
  | ~=id; { SimpleVar id }
  | ~=array_access; { array_access }

let decls ==
  separated_list(COMMA, decl)

let exp :=
  | left=exp; EQUALS; right=relexp; { OpExp {left; right; oper=EQUALS}}
  | ~=relexp; {relexp}

let relexp :=
  | left=relexp; LT; right=addexp; { OpExp {left; right; oper=LT} }
  | left=relexp; GT; right=addexp; { OpExp {left; right; oper=GT} }
  | ~=addexp; { addexp }

let addexp :=
  | left=addexp; PLUS; right=mulexp; { OpExp {left;right; oper=PLUS}}
  | ~=mulexp; { mulexp }

let mulexp :=
  | left=mulexp; MULT; right=castexp; { OpExp {left; right; oper=MULT}}
  | ~=castexp; { castexp }

let castexp :=
  | ~=primary; { primary }

let primary :=
  | ~=id; { Identifier id }
  | ~=arrayexpr; { arrayexpr }
  | ~=primlit; { primlit }

let primlit :=
  | LPAREN; ~=exp; RPAREN; { exp }
  | ~=literal; { literal }
  | ~=array_access; { VarExp array_access }

let array_access :=
  | ~=id; ~=dimexpr; { (SubscriptVar (SimpleVar id, dimexpr)) }

let arrayexpr :=
  | NEW; INT; ~=dimexprs; { ArrayExp {type_ = IntType; exprs=dimexprs} }

let dimexprs :=
  | ~=dimexprs; ~=dimexpr; { dimexpr :: dimexprs }
  | ~=dimexpr; { [dimexpr] }

let dimexpr :=
  | LSQB; ~=exp; RSQB; { exp }

let literal :=
  | int=NUM; { IntLit int}
  | NULL; { NullLit }

let decl :=
  | (rank, id)=decl; LSQB; RSQB; { (rank + 1, id) }
  | ~=id; { (0, id) }

let id :=
  | ~=ID;                                 <Symbol.symbol>      
