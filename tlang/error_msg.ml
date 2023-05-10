type t = {
  any_errors : bool [@default false];
  file_name : string; [@default ""]
  line_num : int; [@default 1]
  line_pos : int list;
}
[@@deriving make]

let error_state = ref (make ())
    
exception Error
  
let reset () =
  error_state := make ()

let set_filename file_name =
  error_state := { !error_state with file_name }

let error (((sl, sr), (el, er)) : Ast.pos) (msg : string) =
  error_state := { !error_state with any_errors = true;};
  (* let error_state = !error_state in *)
  print_string (!error_state.file_name ^ ":");
  print_string
    (string_of_int sl ^ ":" ^ string_of_int sr ^ "-" ^ string_of_int el ^ ":" ^ string_of_int er);
  List.iter print_string [": "; msg; "\n"]  
