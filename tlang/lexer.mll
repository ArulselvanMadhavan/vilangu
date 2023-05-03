{
open Parser

type pos = Ast.pos

exception Error of pos * string

let get_loc (pos : Lexing.position) = pos.pos_lnum, (pos.pos_cnum - pos.pos_bol + 1)

let get_pos lexbuf =
  get_loc (Lexing.lexeme_start_p lexbuf), get_loc (Lexing.lexeme_end_p lexbuf)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let space = ['\t' '\r' ' ']
(* let newline = '\n' *)
let decimal = digit+
let id = alpha (alpha | digit | '_')*

rule token = parse
  | space+ { token lexbuf }
  (* | "+" { PLUS } *)
  (* | "-" { MINUS } *)
  (* | "*" { MULT } *)
  | ";" { SEMICOLON }
  | "int" { INT }
  | "main" { MAIN }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "," { COMMA }
  | id as s       { ID s }
  (* | decimal as num {NUM (int_of_string num)} *)
  | _             { Error_msg.error (get_pos lexbuf) "illegal character" ; token lexbuf }
  | eof           { EOF }  
      
