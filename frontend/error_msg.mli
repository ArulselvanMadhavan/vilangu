type t =
  { any_errors : bool [@default false]
  ; file_name : string [@default ""]
  ; line_num : int [@default 1]
  ; line_pos : int list
  ; errors : string list
  }

exception Error

val error_state : t ref
val reset : unit -> unit
val error : Ast.pos -> string -> unit
val set_filename : string -> unit
val log_err : string -> unit
val has_errors : unit -> bool
