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
val to_string_mach : t -> string
val load_defs_exn : filename:string -> t String.Map.t
val to_int_exn : t -> Big_int.t

(** List-like access *)
val car : t -> t

val cdr : t -> t
val decode_vector : t -> (int * int) list list
val encode_int_list : int list -> t
val encode_list : t list -> t
val decode_list : t -> t list
val id : t -> t
val tuple2 : (t -> 'a) -> (t -> 'b) -> t -> 'a * 'b
val tuple3 : (t -> 'a) -> (t -> 'b) -> (t -> 'c) -> t -> 'a * 'b * 'c
val tuple4 : (t -> 'a) -> (t -> 'b) -> (t -> 'c) -> (t -> 'd) -> t -> 'a * 'b * 'c * 'd

val tuple5
  :  (t -> 'a)
  -> (t -> 'b)
  -> (t -> 'c)
  -> (t -> 'd)
  -> (t -> 'e)
  -> t
  -> 'a * 'b * 'c * 'd * 'e

val tuple8
  :  (t -> 'a)
  -> (t -> 'b)
  -> (t -> 'c)
  -> (t -> 'd)
  -> (t -> 'e)
  -> (t -> 'f)
  -> (t -> 'g)
  -> (t -> 'h)
  -> t
  -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h

(** Parsing *)

val parse : string list -> (t * string list) Or_error.t
val parse_exn : string list -> t
val parse_def_exn : string -> string * t

(** Evaluating *)

val base_defs : t String.Map.t
val eval_custom : t -> verbose:bool -> defs:t String.Map.t -> t
