open! Core

type t = Lambda.L.term =
  | Var of string
  | Abs of (string * t)
  | App of t * t
[@@deriving equal, sexp]

val to_string_hum : t -> string
val load_defs_exn : filename:string -> t String.Map.t

(** Parsing *)

val parse : string list -> (t * string list) Or_error.t
val parse_exn : string list -> t
val parse_def_exn : string -> string * t

(** Evaluating *)

val base_defs : t String.Map.t
val eval : t -> verbose:bool -> defs:t String.Map.t -> t
