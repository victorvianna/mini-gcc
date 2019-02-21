
open Format

let rec print_list sep print fmt = function
  | [] -> ()
  | [x] -> print fmt x
  | x :: r -> print fmt x; sep fmt (); print_list sep print fmt r

let comma fmt () = fprintf fmt ",@ "
let semi fmt () = fprintf fmt ";@ "
let space fmt () = fprintf fmt "@ "
let newline fmt () = fprintf fmt "@\n"

