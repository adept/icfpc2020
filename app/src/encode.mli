open! Core

type t =
  | Number of int
  | Cons of (t * t)
  | Nil
[@@deriving sexp]

val encode : int -> string
val decode : string -> (t * string) Or_error.t
