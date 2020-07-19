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
let ( / ) = div_big_int
let ( < ) = lt_big_int
let ( = ) = eq_big_int
let ( >= ) = ge_big_int
let equal = eq_big_int
let is_zero t = eq_big_int zero t
let sexp_of_t t = Sexp.of_string (string_of_big_int t)
