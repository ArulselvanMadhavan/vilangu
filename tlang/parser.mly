%{
open Ast
%}
%token EOF
%token INT
%token MAIN CLASS
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
  (* | ~=class_decl; { } *)
  | INT; MAIN; LPAREN; RPAREN; ~=block; { MainFunc block }

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
