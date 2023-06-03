{
open Parser

type pos = Ast.pos

exception Error of pos * string

let get_loc (pos : Lexing.position) = pos.pos_lnum, (pos.pos_cnum - pos.pos_bol + 1)

let get_pos lexbuf =
  get_loc (Lexing.lexeme_start_p lexbuf), get_loc (Lexing.lexeme_end_p lexbuf)

let get_int32 lexbuf num =
  let i32 = Int32.of_string_opt num in
  let handle_i32 i32 () = i32 in
  let handle_err () =
    let pos = get_pos lexbuf in
    Error_msg.error pos "illegal integer literal";
    Int32.min_int
  in
  Option.fold ~none:handle_err ~some:handle_i32 i32 ()  
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let space = ['\t' '\r' ' ']
let newline = '\n'
let decimal = digit+
let negative_num = '-' (digit)+
let id = alpha (alpha | digit | '_')*

rule token = parse
  | space+ { token lexbuf }
  | newline       { Lexing.new_line lexbuf; token lexbuf }
  | "//"          { line_comment lexbuf }
  | ";" { SEMICOLON }
  | "int" { INT }
  | "main" { MAIN }
  | "class" { CLASS }
  | "extends" { EXTENDS }
  | "this" { THIS }
  | "super" { SUPER }
  | "new" { NEW }
  | "delete" { DELETE}
  | "if" { IF }
  | "else" { ELSE }
  | "return" { RETURN }
  | "while" { WHILE }
  | "out" { OUT }
  | "break" { BREAK }
  | "continue" { CONTINUE }
  | "null" { NULL }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "," { COMMA }
  | "." { DOT }
  | "==" { EQUALS }
  | "=" { ASSIGN_OP }
  | "<" { LT }
  | ">" { GT }
  | "+" { PLUS }
  | "/" { DIV }
  | "*" { MULT }
  | "!" { NOT }
  | "[" { LSQB }
  | "]" { RSQB }
  | id as s       { ID s }
  | decimal as num { NUM (get_int32 lexbuf num) }
  | negative_num as num { NUM (get_int32 lexbuf num) }
  | "-" { MINUS }
  | "~" { TILDE }
  | eof           { EOF }  
  | _             { Error_msg.error (get_pos lexbuf) "illegal character" ; token lexbuf }
      
and line_comment = parse
| ('\n' | eof)  { Lexing.new_line lexbuf; token lexbuf }
| _             { line_comment lexbuf }
