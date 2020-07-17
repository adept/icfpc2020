open! Core

type t =
  | Number of int
  | Cons of (t * t)
  | Nil
[@@deriving sexp]

val encode : t -> string
val decode : string -> t Or_error.t
