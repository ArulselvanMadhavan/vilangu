%{
open Ast
%}

%token EOF
%token <int> INT
%token PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE
%nonassoc EQ NEQ GT LT GE LE
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
      | left=exp; PLUS; right=exp;            { OpExp{left; oper=PlusOp; right; pos=lp($loc)} }

                  
