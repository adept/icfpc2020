open! Core

(* Bits 0..1 define a positive or negative number (and signal width) via a
   high/low or low/high signal change:

01: positive number

10: negative number

Bits 2..(n+2) define the width of the following binary-encoded number via a
   unary-encoded number of length n composed of high signals ending with a low
   signal. The number width (in bits) is four times the unary encoding (i.e. 4 *
   n):

0: 0 [i.e. the number zero]

10: 4-bit number [i.e. 1-7]

110: 8-bit number [i.e. 1-255]

1110: 12-bit number [i.e. 1-4095]

…

The remaining bits, i.e. (n + 3)..(n + 3 + 4*n - 1), determine the number
   itself, in most-significant-bit first binary notation. Using the examples
   from this message:

0001: 1

00010000: 16

000100000000: 256

…

With this encoding, the number zero only requires three bits (i.e. 010), but
   arbitrarily large numbers can also be represented. *)

type t =
  | Number of Big_int.t
  | Cons of (t * t)
  | Nil
[@@deriving sexp_of]

let encode_int n =
  let open Big_int in
  let pos_neg = if n >= zero then "01" else "10" in
  let rec bits x acc =
    if x = zero
    then (
      let extra_zeros =
        let open Int in
        let rem = List.length acc mod 4 in
        if rem = zero then zero else 4 - rem
      in
      String.make extra_zeros '0' ^ String.of_char_list acc)
    else (
      let bit = if Big_int.mod_big_int x two = zero then '0' else '1' in
      bits (x / of_int 2) (bit :: acc))
  in
  let bits = bits n [] in
  let four_bit_blocks = Int.( / ) (String.length bits) 4 in
  let len = if n = zero then "0" else String.make four_bit_blocks '1' ^ "0" in
  pos_neg ^ len ^ bits
;;

let rec encode = function
  | Number x -> encode_int x
  | Cons (a, b) -> "11" ^ encode a ^ encode b
  | Nil -> "00"
;;

let rec decode = function
  | str when String.is_prefix str ~prefix:"00" -> Ok (Nil, String.slice str 2 0)
  | str when String.is_prefix str ~prefix:"11" ->
    let open Or_error.Let_syntax in
    let%bind car, leftover = decode (String.slice str 2 0) in
    let%bind cdr, leftover = decode leftover in
    Ok (Cons (car, cdr), leftover)
  | str when String.is_prefix str ~prefix:"010" ->
    Ok (Number Big_int.zero, String.slice str 3 0)
  | str ->
    let open Or_error.Let_syntax in
    let%bind () =
      if String.length str < 3
      then Or_error.errorf "String too short: '%s'" str
      else Ok ()
    in
    let%bind () =
      if String.for_all str ~f:(function
             | '0' | '1' -> true
             | _ -> false)
      then Ok ()
      else Or_error.errorf "String not binary: '%s'" str
    in
    let%bind sign =
      if String.is_prefix str ~prefix:"01"
      then Ok 1
      else if String.is_prefix str ~prefix:"10"
      then Ok ~-1
      else Or_error.errorf "Unknown sign: '%s'" str
    in
    let str = String.slice str 2 0 in
    let%bind quads, str =
      match String.lsplit2 str ~on:'0' with
      | None -> Or_error.errorf "Invalid quad spec: '%s'" str
      | Some (quads, rest) -> Ok (String.length quads, rest)
    in
    let value_str = String.slice str 0 (quads * 4) in
    let leftover = String.slice str (quads * 4) 0 in
    let value =
      let open Big_int in
      let acc = ref Big_int.zero in
      let pow = ref Big_int.one in
      for i = Int.( - ) (String.length value_str) 1 downto 0 do
        if Char.equal value_str.[i] '1' then acc := !acc + !pow;
        pow := !pow * two
      done;
      !acc
    in
    Or_error.try_with (fun () -> Number (Big_int.mult_int_big_int sign value), leftover)
;;

let rec to_string_mach = function
  | Number n -> Big_int.to_string n
  | Nil -> "nil"
  | Cons (x, y) -> "ap ap cons " ^ to_string_mach x ^ " " ^ to_string_mach y
;;

let rec to_eval = function
  | Nil -> Eval.var "nil"
  | Number n -> Eval.var (Big_int.to_string n)
  | Cons (x, y) -> Eval.app (Eval.app (Eval.var "cons") (to_eval x)) (to_eval y)
;;

let rec of_eval_exn ev =
  match (ev : Eval.t) with
  | { u = Var "nil"; _ } -> Nil
  | { u = Var n; _ } -> Number (Big_int.of_string n)
  | { u = App ({ u = App ({ u = Var "cons"; _ }, x); _ }, y); _ } ->
    Cons (of_eval_exn x, of_eval_exn y)
  | _ ->
    raise_s [%sexp "Encode.of_eval_exn expected list", (Eval.to_string_hum ev : string)]
;;
