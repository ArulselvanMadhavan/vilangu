{
open Parser

type pos = Ast.pos

exception Error of pos * string

(* let get_loc (pos : Lexing.position) = pos.pos_lnum, (pos.pos_cnum - pos.pos_bol + 1) *)

(* let get_pos lexbuf = *)
(*   get_loc (Lexing.lexeme_start_p lexbuf), get_loc (Lexing.lexeme_end_p lexbuf) *)
}

(* let digit = ['0'-'9'] *)
(* let alpha = ['a'-'z' 'A'-'Z'] *)
let space = ['\t' '\r' ' ']
(* let newline = '\n' *)
(* let decimal = digit+ *)
(* let id = alpha (alpha | digit | '_')*   *)

(* rule token = parse *)
(* | space+ { token lexbuf} *)
(* | newline { Lexing.new_line lexbuf; token lexbuf} *)
(* | "+"           { PLUS } *)
(* | "-"           { MINUS } *)
(* | "*"           { TIMES } *)
(* | "/"           { DIVIDE } *)
(* | decimal as i  { INT (int_of_string i) } *)
(* | id as s       { ID s } *)
(* | _             { Error_msg.error (get_pos lexbuf) "illegal charcter" ; token lexbuf } *)

        (* end of a file *)
rule token = parse
  | space+ { token lexbuf }
  | "int" { INT }
  | "main" { MAIN }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | eof           { EOF }
  
