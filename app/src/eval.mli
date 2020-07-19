open! Core

type t =
  { mutable evaluated : t option
  ; u : u
  }

and u =
  | Var of string
  | App of t * t
[@@deriving sexp_of]

val app : t -> t -> t
val var : string -> t
val to_string_hum : t -> string
val load_defs_exn : filename:string -> t String.Map.t

(** List-like access *)
val car : t -> t

val cdr : t -> t
val decode_vector : t -> (int * int) list list

(** Parsing *)

val parse : string list -> (t * string list) Or_error.t
val parse_exn : string list -> t
val parse_def_exn : string -> string * t

(** Evaluating *)

val base_defs : t String.Map.t
val eval_custom : t -> verbose:bool -> defs:t String.Map.t -> t
