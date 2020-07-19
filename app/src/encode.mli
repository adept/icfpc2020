open! Core

type t =
  | Number of Big_int.t
  | Cons of (t * t)
  | Nil
[@@deriving sexp_of]

val encode : t -> string
val decode : string -> (t * string) Or_error.t

(** Outputs somethings like [ap ap cons 1 ap ap cons 2 nil]  *)
val to_string_mach : t -> string

(** Convert to/from Eval lists. *)

val to_eval : t -> Eval.t
val of_eval_exn : Eval.t -> t
