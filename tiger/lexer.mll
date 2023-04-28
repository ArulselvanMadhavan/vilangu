{
open Parser
type pos = Ast.pos    
exception Error of pos * string
}

let digit = ['0'-'9']

let alpha = ['a'-'z' 'A'-'Z']

let space = ['\t' '\r' ' ']

let newline = '\n'

let decimal = digit+

let id = alpha (alpha | digit | '_')*

let string = '\"' [^ '"']* '\"'

rule token =
  parse
  | space+ { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | "+" {PLUS}
  | decimal as i { INT (int_of_string i) }
  | eof { EOF }
            
