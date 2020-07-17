open! Core

let%test _ = String.equal Icfpc2020.Commands.something_to_test "it works!"

let%expect_test _ =
  printf "%s\n" Icfpc2020.Commands.something_to_test;
  [%expect {| it works! |}]
;;
