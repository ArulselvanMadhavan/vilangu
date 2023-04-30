open Base

type symbol = string * int [@@deriving sexp]

let nextsym = ref (-1)
let size_hint = 128
let hashtable = Hashtbl.create ~growth_allowed:true ~size:size_hint (module String)

let symbol (name : string) : symbol =
  match Hashtbl.find hashtable name with
  | Some i -> name, i
  | None ->
    Int.incr nextsym;
    Hashtbl.add_exn hashtable ~key:name ~data:!nextsym;
    name, !nextsym
;;

let name (s, _) = s
