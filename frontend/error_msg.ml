type t =
  { any_errors : bool [@default false]
  ; file_name : string [@default ""]
  ; line_num : int [@default 1]
  ; line_pos : int list
  ; errors : string list
  }
[@@deriving make]

let error_state = ref (make ())

exception Error

let reset () = error_state := make ()
let set_filename file_name = error_state := { !error_state with file_name }

let error (((sl, sr), (el, er)) : Ast.pos) (msg : string) =
  error_state := { !error_state with any_errors = true };
  (* let error_state = !error_state in *)
  let err_str = !error_state.file_name ^ ":" in
  let err_str =
    err_str
    ^ string_of_int sl
    ^ ":"
    ^ string_of_int sr
    ^ " - "
    ^ string_of_int el
    ^ ":"
    ^ string_of_int er
  in
  let err_str = err_str ^ ": " ^ msg ^ "\n" in
  error_state := { !error_state with errors = err_str :: !error_state.errors }
;;

let log_err filename =
  let out_ch = open_out filename in
  let pipe_str = output_string out_ch in
  let pipe_trm = output_string stdout in
  List.iter (fun s -> pipe_str s; pipe_trm s) !error_state.errors
;;

let has_errors () = !error_state.any_errors
