%{
open Ast
%}

%token EOF
%token <string> ID
%token SEMICOLON
%token COMMA
%token INT
%token MAIN
%token LPAREN RPAREN LBRACE RBRACE
%start <comp_unit> prog

%{
(* let lp((sp, ep) : (Lexing.position * Lexing.position)) : pos *)
(*   = ((sp.pos_lnum, sp.pos_cnum - sp.pos_bol + 1), (ep.pos_lnum, ep.pos_cnum - sp.pos_bol + 1)) *)
%}

%%

let prog :=
  | ~=comp_unit; EOF; <>

let comp_unit :=
  | INT; MAIN; LPAREN; RPAREN; block=block; {MainFunc block}

let block :=
  | LBRACE; main_blk_stmts=list(main_blk_stmt); RBRACE; { main_blk_stmts }
  | LBRACE; RBRACE; { [] }

let main_blk_stmt :=
      | ~=main_var_decl_stmt; {main_var_decl_stmt}

let main_var_decl_stmt :=
      | main_var_dec=main_var_dec; SEMICOLON; { main_var_dec }

let main_var_dec :=
  | type_=type_; id=var_decls; { VariableDecl { type_; id}}

let type_ :=
  (* | ref_type=ref_type; { ref_type } *)
  | prim_type=prim_type; { prim_type }

let prim_type :=
  | num_type=num_type; { num_type }

let num_type :=
  | INT; { IntType }

let var_decls :=
  (* | decls=separated_list(COMMA, var_decl); {decls} *)
  | ~=var_decl; { var_decl }

let var_decl :=
  | s=ID; { Symbol.symbol s }
      
(* let ref_type := *)
(*   | class_type=class_type { class_type } *)
(*   | array_type=arra *)
(* let exp := *)
(*   | int=INT; {IntExp int} *)
(*   | MINUS; right=exp; %prec UMINUS {OpExp{left=IntExp 0; oper=MinusOp; right; pos=lp($loc)}} *)
(*   | left=exp; PLUS; right=exp;            { OpExp{left; oper=PlusOp; right; pos=lp($loc)} } *)
(*   | left=exp; MINUS; right=exp;           { OpExp{left; oper=MinusOp; right; pos=lp($loc)} } *)
(*   | left=exp; TIMES; right=exp;           { OpExp{left; oper=TimesOp; right; pos=lp($loc)} } *)
(*   | left=exp; DIVIDE; right=exp;          { OpExp{left; oper=DivideOp; right; pos=lp($loc)} } *)
