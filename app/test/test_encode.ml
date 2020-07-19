open! Core
open! Icfpc2020

let%expect_test "encode numbers" =
  let open Encode in
  print_endline (encode (Number Big_int.zero));
  [%expect {|010|}];
  print_endline (encode (Number Big_int.one));
  [%expect {|01100001|}];
  print_endline (encode (Number (Big_int.of_int 16)));
  [%expect {|0111000010000|}];
  print_endline (encode (Number (Big_int.of_int 256)));
  [%expect {|011110000100000000|}]
;;

let%expect_test "encode lists" =
  let open Encode in
  print_endline (encode Nil);
  [%expect {|00|}];
  print_endline (encode (Cons (Nil, Nil)));
  [%expect {|110000|}];
  print_endline (encode (Cons (Number Big_int.zero, Nil)));
  [%expect {|1101000|}];
  print_endline (encode (Cons (Number Big_int.one, Number Big_int.two)));
  [%expect {|110110000101100010|}]
;;

let test str =
  match Encode.decode str with
  | Error err -> print_endline (Error.to_string_hum err)
  | Ok (enc, leftover) ->
    print_s (Encode.sexp_of_t enc);
    if not (String.is_empty leftover) then printf "Leftover: '%s'" leftover
;;

let%expect_test "single numbers" =
  test "010";
  [%expect {| (Number 0) |}];
  test "01100001";
  [%expect {| (Number 1) |}];
  test "10100001";
  [%expect {| (Number -1) |}];
  test "01100010";
  [%expect {| (Number 2) |}];
  test "10100010";
  [%expect {| (Number -2) |}];
  test "0111000010000";
  [%expect {| (Number 16) |}];
  test "1011000010000";
  [%expect {| (Number -16) |}];
  test "0111011111111";
  [%expect {| (Number 255) |}];
  test "1011011111111";
  [%expect {| (Number -255) |}];
  test "011110000100000000";
  [%expect {| (Number 256) |}];
  test "101110000100000000";
  [%expect {| (Number -256) |}]
;;

let%expect_test "lists" =
  test "00";
  [%expect {| Nil |}];
  test "110000";
  [%expect {| (Cons (Nil Nil))|}];
  test "1101000";
  [%expect {| (Cons ((Number 0) Nil))|}];
  test "110110000101100010";
  [%expect {| (Cons ((Number 1) (Number 2)))|}];
  test "1101100001110110001000";
  [%expect {| (Cons ((Number 1) (Cons ((Number 2) Nil))))|}];
  test "1101100001110110001000";
  [%expect {| (Cons ((Number 1) (Cons ((Number 2) Nil))))|}];
  test "110110000101100010";
  [%expect {| (Cons ((Number 1) (Number 2))) |}];
  test "1101100001111101100010110110001100110110010000";
  [%expect
    {|
    (Cons
     ((Number 1)
      (Cons
       ((Cons ((Number 2) (Cons ((Number 3) Nil)))) (Cons ((Number 4) Nil))))))|}]
;;
