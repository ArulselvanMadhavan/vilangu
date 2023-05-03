%{
open Ast
%}
%token EOF
%token INT
%token MAIN
%token WHILE
%token OUT
%token NULL
%token <string> ID
%token <int> NUM
%token ASSIGN_OP
%token LT GT
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
let var (is_array, id) = {type_=IntType; id; is_array}
%}

%%

let prog :=
  ~=comp_unit; EOF; <>         (* <> is identity *)

let comp_unit :=                       
  | INT; MAIN; LPAREN; RPAREN; ~=block; { MainFunc block }

let block :=
  | LBRACE; ~=stmts; RBRACE; { stmts }
(* == is %inline non-terminal *)
let stmts ==
  list(stmt)
        
let stmt :=
  | INT; ~=decls; SEMICOLON; { VariableDecl (List.map var decls) }
  | lhs=id; ASSIGN_OP; ~=exp; SEMICOLON; { Assignment {lhs; exp} }
  | WHILE; ~=exp; ~=block; { While { exp; block} }
  | OUT; ~=exp; SEMICOLON; { Output exp }

let decls ==
  separated_list(COMMA, decl)

let exp :=
  | left=exp; LT; right=relexp; { OpExp {left; right; oper=LT} }
  | left=exp; GT; right=relexp; { OpExp {left; right; oper=GT} }
  | ~=relexp; { relexp }

let relexp :=
  | left=relexp; PLUS; right=addexp; { OpExp {left;right; oper=PLUS}}
  | ~=addexp; { addexp }

let addexp :=
  | left=addexp; MULT; right=mulexp; { OpExp {left; right; oper=MULT}}
  | ~=mulexp; { mulexp }

let mulexp :=
  | LPAREN; ~=exp; RPAREN; { exp }
  | ~=primary; { primary }

let primary :=
  | ~=id; { Identifier id }
  | ~=literal; { literal }

let literal :=
  | int=NUM; { IntLit int}
  | NULL; { NullLit }

let decl :=
  | ~=id; LSQB; RSQB; { (true, id) }
  | ~=id; { (false, id) }

let id :=
  | ~=ID;                                 <Symbol.symbol>      
