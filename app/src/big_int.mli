open! Core
open! Big_int_Z

type t = Z.t [@@deriving sexp_of]

val zero : t
val one : t
val two : t
val to_string : t -> string
val of_string : string -> t
val of_int : int -> t

(** Ops  *)

val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val mod_big_int : t -> t -> t
val ( < ) : t -> t -> bool
val ( = ) : t -> t -> bool
val ( >= ) : t -> t -> bool
val equal : t -> t -> bool
val mult_int_big_int : int -> t -> t
val is_zero : t -> bool
