open! Core

type t =
  | Number of int
  | List of t list
[@@deriving sexp]

val demod : string -> t Or_error.t
