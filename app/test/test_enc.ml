open! Core
open! Icfpc2020

let test str =
  match Enc.demod str with
  | Error err -> print_endline (Error.to_string_hum err)
  | Ok enc -> print_s (Enc.sexp_of_t enc)
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
