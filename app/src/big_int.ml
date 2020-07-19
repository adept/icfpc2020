open! Core
include Big_int_Z

type t = big_int

let zero = zero_big_int
let one = big_int_of_int 1
let two = big_int_of_int 2
let to_string = string_of_big_int
let of_string = big_int_of_string
let of_int = big_int_of_int
let ( + ) = add_big_int
let ( - ) = sub_big_int
let ( * ) = mult_big_int

let ( / ) a b =
  (* Round towards zero *)
  let res = div_big_int a b in
  let multiply_up = mult_big_int res b in
  if gt_big_int (abs_big_int multiply_up) (abs_big_int a)
  then if gt_big_int res zero then sub_big_int res one else add_big_int res one
  else res
;;

let ( < ) = lt_big_int
let ( = ) = eq_big_int
let ( >= ) = ge_big_int
let equal = eq_big_int
let is_zero t = eq_big_int zero t
let sexp_of_t t = Sexp.of_string (string_of_big_int t)
