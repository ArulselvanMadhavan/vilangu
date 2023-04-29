%{
open Ast
%}

%token EOF
%token <string> ID
%token <int> INT
%token PLUS MINUS TIMES DIVIDE

%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS
%start <exp> prog

%{
let lp((sp, ep) : (Lexing.position * Lexing.position)) : pos
  = ((sp.pos_lnum, sp.pos_cnum - sp.pos_bol + 1), (ep.pos_lnum, ep.pos_cnum - sp.pos_bol + 1))
%}

%%

let prog :=
  | ~=exp; EOF; <>

let exp :=
  | int=INT; {IntExp int}
  | MINUS; right=exp; %prec UMINUS {OpExp{left=IntExp 0; oper=MinusOp; right; pos=lp($loc)}}
  | left=exp; PLUS; right=exp;            { OpExp{left; oper=PlusOp; right; pos=lp($loc)} }
  | left=exp; MINUS; right=exp;           { OpExp{left; oper=MinusOp; right; pos=lp($loc)} }
  | left=exp; TIMES; right=exp;           { OpExp{left; oper=TimesOp; right; pos=lp($loc)} }
  | left=exp; DIVIDE; right=exp;          { OpExp{left; oper=DivideOp; right; pos=lp($loc)} }
