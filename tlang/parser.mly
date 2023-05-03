%{
open Ast
%}
%token EOF
%token INT
%token MAIN
%token <string> ID
%token LPAREN RPAREN LBRACE RBRACE
%token SEMICOLON
%token COMMA
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
  (* | INT; ~=id; SEMICOLON; { VariableDecl { type_=IntType; id=id } } *)
  | INT; ~=ids; SEMICOLON; { VariableDecl {type_=IntType; ids=ids } }

let ids ==
  separated_list(COMMA, id)

let id :=
  | ~=ID;                                 <Symbol.symbol>      
