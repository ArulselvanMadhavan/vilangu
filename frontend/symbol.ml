open Base

type symbol = string * int [@@deriving sexp]

let nextsym = ref (-1)
let size_hint = 128
let hashtable = Stdlib.Hashtbl.create ~random:true size_hint

let symbol (name : string) : symbol =
  match Stdlib.Hashtbl.find_opt hashtable name with
  | Some i -> name, i
  | None ->
    Int.incr nextsym;
    Stdlib.Hashtbl.add hashtable name !nextsym;
    name, !nextsym
;;

let name (s, _) = s

module Table = Stdlib.Map.Make (struct
  type t = symbol

  let compare = Stdlib.compare
end)

type 'a table = 'a Table.t

let empty = Table.empty
let enter (env, k, v) = Table.add k v env
let look (env, k) = Table.find_opt k env
let init l = Table.of_seq (Stdlib.List.to_seq l)
