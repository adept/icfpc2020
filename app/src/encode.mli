open! Core

type t =
  | Number of int
  | Cons of (t * t)
  | Nil
[@@deriving sexp]

val encode : t -> string
val decode : string -> (t * string) Or_error.t

(** Outputs somethings like [ap ap cons 1 ap ap cons 2 nil]  *)
val to_string_mach : t -> string
