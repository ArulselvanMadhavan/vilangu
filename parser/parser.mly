%token <int> INT
%token EOF
%start <Json.value option> prog
%%
                             

prog:
| v = value { Some v }
| EOF { None };

value:
  | i = INT { `Int i };
