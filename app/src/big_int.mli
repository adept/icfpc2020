open! Core
open! Big_int_Z

type t = Z.t [@@deriving sexp_of]

val to_string : t -> string
val of_string : string -> t

(** Ops  *)

val ( + ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( < ) : t -> t -> bool
val equal : t -> t -> bool
val mult_int_big_int : int -> t -> t
val zero : t
val is_zero : t -> bool
