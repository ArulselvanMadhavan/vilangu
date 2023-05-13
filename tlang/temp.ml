type label = Symbol.symbol

let labs = ref 0

let new_label () =
  let i = !labs in
  labs := i + 1;
  Symbol.symbol ("L" ^ string_of_int i)
;;

let named_label = Symbol.symbol
