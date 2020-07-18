open! Core
module Id : Unique_id.Id

module Big_int : sig
  open! Big_int_Z

  type t = Z.t [@@deriving sexp_of]

  val zero : t
  val is_zero : t -> bool
end

type t =
  | Nil
  | True
  | False
  | Var of string
  | Num of Big_int.t
  | App of t * t
[@@deriving equal, sexp]

val to_string_hum : t -> string
val load_defs_exn : filename:string -> t String.Map.t

(** List-like access *)
val car : t -> t

val cdr : t -> t
val decode_vector : t -> (Big_int.t * Big_int.t) list list

(** Parsing *)

val parse : string list -> (t * string list) Or_error.t
val parse_exn : string list -> t
val parse_def_exn : string -> string * t

(** Evaluating *)

val base_defs : t String.Map.t
val eval_custom : verbose:bool -> defs:t String.Map.t -> (t -> t) Staged.t
