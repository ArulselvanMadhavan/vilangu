open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf
    outx
    "%s|%d|%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_with_error lexbuf =
  let open Tiger_parser in
  try Parser.prog Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
    Printf.fprintf Stdlib.stderr "%a: %s" print_position lexbuf msg;
    None
  | Parser.Error ->
    Printf.fprintf Stdlib.stderr "%a: syntax error:\n" print_position lexbuf;
    Stdlib.exit (-1)
;;

let rec parse_and_print lexbuf =
  let res = parse_with_error lexbuf in
  match res with
  | Some (`Int i) ->
    Printf.printf "%d\n" i;
    parse_and_print lexbuf
  | None -> ()
;;

let () =
  let lexbuf = Lexing.from_string "1231" in
  parse_and_print lexbuf;
;;
