let error (((sl, sr), (el, er)) : Ast.pos) (msg : string) =
  print_string
    (string_of_int sl
     ^ ":"
     ^ string_of_int sr
     ^ "-"
     ^ string_of_int el
     ^ ":"
     ^ string_of_int er);
  List.iter print_string [ ": "; msg; "\n" ]
;;
