open! Core

type t =
  | Number of int
  | List of t list
[@@deriving sexp]

let demod = function
  | "010" -> Ok (Number 0)
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
    let%bind () =
      if quads * 4 <> String.length str
      then
        Or_error.errorf
          "Wrong length for quads section: '%s' (expected %d)"
          str
          (quads * 4)
      else Ok ()
    in
    Or_error.try_with (fun () -> Number (sign * int_of_string ("0b" ^ str)))
;;
