%{
open Ast
%}
%token EOF
%token INT
%token MAIN
%token WHILE
%token NULL
%token <string> ID
%token <int> NUM
%token ASSIGN_OP
%token LT GT
%token LPAREN RPAREN LBRACE RBRACE
%token SEMICOLON
%token COMMA
(* %left LT GT *)
%start <comp_unit> prog

%{
(* let lp((sp, ep) : (Lexing.position * Lexing.position)) : pos *)
(*   = ((sp.pos_lnum, sp.pos_cnum - sp.pos_bol + 1), (ep.pos_lnum, ep.pos_cnum - sp.pos_bol + 1)) *)
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
  | INT; ~=ids; SEMICOLON; { VariableDecl {type_=IntType; ids=ids } }
  | lhs=id; ASSIGN_OP; ~=exp; SEMICOLON; { Assignment {lhs; exp} }
  | WHILE; ~=exp; ~=block; { While { exp; block} }
let ids ==
  separated_list(COMMA, id)

let exp :=
  | left=exp; LT; right=relexp; { OpExp {left; right; oper=LT} }
  | left=exp; GT; right=relexp; { OpExp {left; right; oper=GT} }
  | ~=relexp; { relexp }

let relexp :=
  | LPAREN; ~=exp; RPAREN; { exp }
  | ~=primary; { primary }

let primary :=
  | ~=id; { Identifier id }
  | ~=literal; { literal }

let literal :=
  | int=NUM; { IntLit int}
  | NULL; { NullLit }

let id :=
  | ~=ID;                                 <Symbol.symbol>      
