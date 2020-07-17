open! Core

type t =
  | Number of int
  | List of t list
[@@deriving sexp]

val encode : int -> string
val decode : string -> t Or_error.t
