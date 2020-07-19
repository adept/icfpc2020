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

let%expect_test "server response debugging" =
  test "111010110100";
  [%expect {| (Cons ((Number -13) Nil)) |}]
;;

let%expect_test "server response" =
  let response =
    In_channel.read_all "../../../../app/test/big_response.txt" |> String.strip
  in
  test response;
  [%expect
    {|
    (Cons
     ((Number 1)
      (Cons
       ((Number 0)
        (Cons
         ((Number 3)
          (Cons
           ((Number 31)
            (Cons
             ((Cons
               ((Cons ((Number 1) (Cons ((Number 0) (Cons ((Number 4) Nil))))))
                (Cons
                 ((Cons ((Number 0) (Cons ((Number 1) (Cons ((Number 3) Nil))))))
                  Nil))))
              (Cons
               ((Cons
                 ((Cons ((Number 16) (Cons ((Number 128) Nil))))
                  (Cons
                   ((Cons
                     ((Cons
                       ((Number 0)
                        (Cons
                         ((Cons
                           ((Cons
                             ((Cons
                               ((Number 1)
                                (Cons
                                 ((Number 0)
                                  (Cons
                                   ((Cons ((Number 48) (Number 24)))
                                    (Cons
                                     ((Cons ((Number 0) (Number 0)))
                                      (Cons
                                       ((Cons
                                         ((Number 206)
                                          (Cons
                                           ((Number 30)
                                            (Cons
                                             ((Number 10)
                                              (Cons ((Number 1) Nil))))))))
                                        (Cons
                                         ((Number 0)
                                          (Cons
                                           ((Number 64) (Cons ((Number 1) Nil))))))))))))))))
                              (Cons (Nil Nil))))
                            (Cons
                             ((Cons
                               ((Cons
                                 ((Number 0)
                                  (Cons
                                   ((Number 1)
                                    (Cons
                                     ((Cons ((Number -48) (Number -24)))
                                      (Cons
                                       ((Cons ((Number 0) (Number 0)))
                                        (Cons
                                         ((Cons
                                           ((Number 110)
                                            (Cons
                                             ((Number 40)
                                              (Cons
                                               ((Number 20)
                                                (Cons ((Number 1) Nil))))))))
                                          (Cons
                                           ((Number 0)
                                            (Cons
                                             ((Number 128)
                                              (Cons ((Number 2) Nil))))))))))))))))
                                (Cons (Nil Nil))))
                              Nil))))
                          Nil))))
                      (Cons
                       ((Cons
                         ((Number 1)
                          (Cons
                           ((Cons
                             ((Cons
                               ((Cons
                                 ((Number 1)
                                  (Cons
                                   ((Number 0)
                                    (Cons
                                     ((Cons ((Number 46) (Number 25)))
                                      (Cons
                                       ((Cons ((Number -2) (Number 1)))
                                        (Cons
                                         ((Cons
                                           ((Number 205)
                                            (Cons
                                             ((Number 30)
                                              (Cons
                                               ((Number 10)
                                                (Cons ((Number 1) Nil))))))))
                                          (Cons
                                           ((Number 0)
                                            (Cons
                                             ((Number 64)
                                              (Cons ((Number 1) Nil))))))))))))))))
                                (Cons
                                 ((Cons
                                   ((Cons
                                     ((Number 0)
                                      (Cons
                                       ((Cons ((Number 1) (Number -1))) Nil))))
                                    Nil))
                                  Nil))))
                              (Cons
                               ((Cons
                                 ((Cons
                                   ((Number 0)
                                    (Cons
                                     ((Number 1)
                                      (Cons
                                       ((Cons ((Number -46) (Number -24)))
                                        (Cons
                                         ((Cons ((Number 2) (Number 0)))
                                          (Cons
                                           ((Cons
                                             ((Number 109)
                                              (Cons
                                               ((Number 40)
                                                (Cons
                                                 ((Number 20)
                                                  (Cons ((Number 1) Nil))))))))
                                            (Cons
                                             ((Number 0)
                                              (Cons
                                               ((Number 128)
                                                (Cons ((Number 2) Nil))))))))))))))))
                                  (Cons
                                   ((Cons
                                     ((Cons
                                       ((Number 0)
                                        (Cons
                                         ((Cons ((Number -1) (Number 0))) Nil))))
                                      Nil))
                                    Nil))))
                                Nil))))
                            Nil))))
                        (Cons
                         ((Cons
                           ((Number 2)
                            (Cons
                             ((Cons
                               ((Cons
                                 ((Cons
                                   ((Number 1)
                                    (Cons
                                     ((Number 0)
                                      (Cons
                                       ((Cons ((Number 42) (Number 27)))
                                        (Cons
                                         ((Cons ((Number -4) (Number 2)))
                                          (Cons
                                           ((Cons
                                             ((Number 204)
                                              (Cons
                                               ((Number 30)
                                                (Cons
                                                 ((Number 10)
                                                  (Cons ((Number 1) Nil))))))))
                                            (Cons
                                             ((Number 0)
                                              (Cons
                                               ((Number 64)
                                                (Cons ((Number 1) Nil))))))))))))))))
                                  (Cons
                                   ((Cons
                                     ((Cons
                                       ((Number 0)
                                        (Cons
                                         ((Cons ((Number 1) (Number -1))) Nil))))
                                      Nil))
                                    Nil))))
                                (Cons
                                 ((Cons
                                   ((Cons
                                     ((Number 0)
                                      (Cons
                                       ((Number 1)
                                        (Cons
                                         ((Cons ((Number -42) (Number -24)))
                                          (Cons
                                           ((Cons ((Number 4) (Number 0)))
                                            (Cons
                                             ((Cons
                                               ((Number 108)
                                                (Cons
                                                 ((Number 40)
                                                  (Cons
                                                   ((Number 20)
                                                    (Cons ((Number 1) Nil))))))))
                                              (Cons
                                               ((Number 0)
                                                (Cons
                                                 ((Number 128)
                                                  (Cons ((Number 2) Nil))))))))))))))))
                                    (Cons
                                     ((Cons
                                       ((Cons
                                         ((Number 0)
                                          (Cons
                                           ((Cons ((Number -1) (Number 0))) Nil))))
                                        Nil))
                                      Nil))))
                                  Nil))))
                              Nil))))
                          (Cons
                           ((Cons
                             ((Number 3)
                              (Cons
                               ((Cons
                                 ((Cons
                                   ((Cons
                                     ((Number 1)
                                      (Cons
                                       ((Number 0)
                                        (Cons
                                         ((Cons ((Number 37) (Number 29)))
                                          (Cons
                                           ((Cons ((Number -5) (Number 2)))
                                            (Cons
                                             ((Cons
                                               ((Number 204)
                                                (Cons
                                                 ((Number 30)
                                                  (Cons
                                                   ((Number 10)
                                                    (Cons ((Number 1) Nil))))))))
                                              (Cons
                                               ((Number 0)
                                                (Cons
                                                 ((Number 64)
                                                  (Cons ((Number 1) Nil))))))))))))))))
                                    (Cons (Nil Nil))))
                                  (Cons
                                   ((Cons
                                     ((Cons
                                       ((Number 0)
                                        (Cons
                                         ((Number 1)
                                          (Cons
                                           ((Cons ((Number -37) (Number -25)))
                                            (Cons
                                             ((Cons ((Number 5) (Number -1)))
                                              (Cons
                                               ((Cons
                                                 ((Number 107)
                                                  (Cons
                                                   ((Number 40)
                                                    (Cons
                                                     ((Number 20)
                                                      (Cons ((Number 1) Nil))))))))
                                                (Cons
                                                 ((Number 0)
                                                  (Cons
                                                   ((Number 128)
                                                    (Cons ((Number 2) Nil))))))))))))))))
                                      (Cons
                                       ((Cons
                                         ((Cons
                                           ((Number 0)
                                            (Cons
                                             ((Cons ((Number 0) (Number 1))) Nil))))
                                          Nil))
                                        Nil))))
                                    Nil))))
                                Nil))))
                            (Cons
                             ((Cons
                               ((Number 4)
                                (Cons
                                 ((Cons
                                   ((Cons
                                     ((Cons
                                       ((Number 1)
                                        (Cons
                                         ((Number 0)
                                          (Cons
                                           ((Cons ((Number 32) (Number 32)))
                                            (Cons
                                             ((Cons ((Number -5) (Number 3)))
                                              (Cons
                                               ((Cons
                                                 ((Number 203)
                                                  (Cons
                                                   ((Number 30)
                                                    (Cons
                                                     ((Number 10)
                                                      (Cons ((Number 1) Nil))))))))
                                                (Cons
                                                 ((Number 7)
                                                  (Cons
                                                   ((Number 64)
                                                    (Cons ((Number 1) Nil))))))))))))))))
                                      (Cons
                                       ((Cons
                                         ((Cons
                                           ((Number 0)
                                            (Cons
                                             ((Cons ((Number -1) (Number -1)))
                                              Nil))))
                                          Nil))
                                        Nil))))
                                    (Cons
                                     ((Cons
                                       ((Cons
                                         ((Number 0)
                                          (Cons
                                           ((Number 1)
                                            (Cons
                                             ((Cons ((Number -32) (Number -27)))
                                              (Cons
                                               ((Cons ((Number 5) (Number -2)))
                                                (Cons
                                                 ((Cons
                                                   ((Number 106)
                                                    (Cons
                                                     ((Number 40)
                                                      (Cons
                                                       ((Number 20)
                                                        (Cons ((Number 1) Nil))))))))
                                                  (Cons
                                                   ((Number 28)
                                                    (Cons
                                                     ((Number 128)
                                                      (Cons ((Number 2) Nil))))))))))))))))
                                        (Cons
                                         ((Cons
                                           ((Cons
                                             ((Number 2)
                                              (Cons
                                               ((Cons ((Number 31) (Number 31)))
                                                (Cons
                                                 ((Number 40)
                                                  (Cons
                                                   ((Number 39)
                                                    (Cons ((Number 4) Nil))))))))))
                                            (Cons
                                             ((Cons
                                               ((Number 0)
                                                (Cons
                                                 ((Cons ((Number 1) (Number 1)))
                                                  Nil))))
                                              Nil))))
                                          Nil))))
                                      Nil))))
                                  Nil))))
                              (Cons
                               ((Cons
                                 ((Number 5)
                                  (Cons
                                   ((Cons
                                     ((Cons
                                       ((Cons
                                         ((Number 1)
                                          (Cons
                                           ((Number 0)
                                            (Cons
                                             ((Cons ((Number 27) (Number 35)))
                                              (Cons
                                               ((Cons ((Number -5) (Number 3)))
                                                (Cons
                                                 ((Cons
                                                   ((Number 202)
                                                    (Cons
                                                     ((Number 30)
                                                      (Cons
                                                       ((Number 10)
                                                        (Cons ((Number 1) Nil))))))))
                                                  (Cons
                                                   ((Number 5)
                                                    (Cons
                                                     ((Number 64)
                                                      (Cons ((Number 1) Nil))))))))))))))))
                                        (Cons
                                         ((Cons
                                           ((Cons
                                             ((Number 0)
                                              (Cons
                                               ((Cons ((Number -1) (Number -1)))
                                                Nil))))
                                            Nil))
                                          Nil))))
                                      (Cons
                                       ((Cons
                                         ((Cons
                                           ((Number 0)
                                            (Cons
                                             ((Number 1)
                                              (Cons
                                               ((Cons
                                                 ((Number -27) (Number -30)))
                                                (Cons
                                                 ((Cons ((Number 5) (Number -3)))
                                                  (Cons
                                                   ((Cons
                                                     ((Number 105)
                                                      (Cons
                                                       ((Number 40)
                                                        (Cons
                                                         ((Number 20)
                                                          (Cons ((Number 1) Nil))))))))
                                                    (Cons
                                                     ((Number 16)
                                                      (Cons
                                                       ((Number 128)
                                                        (Cons ((Number 2) Nil))))))))))))))))
                                          (Cons
                                           ((Cons
                                             ((Cons
                                               ((Number 0)
                                                (Cons
                                                 ((Cons ((Number 1) (Number 1)))
                                                  Nil))))
                                              Nil))
                                            Nil))))
                                        Nil))))
                                    Nil))))
                                (Cons
                                 ((Cons
                                   ((Number 6)
                                    (Cons
                                     ((Cons
                                       ((Cons
                                         ((Cons
                                           ((Number 1)
                                            (Cons
                                             ((Number 0)
                                              (Cons
                                               ((Cons ((Number 23) (Number 38)))
                                                (Cons
                                                 ((Cons ((Number -4) (Number 3)))
                                                  (Cons
                                                   ((Cons
                                                     ((Number 201)
                                                      (Cons
                                                       ((Number 30)
                                                        (Cons
                                                         ((Number 10)
                                                          (Cons ((Number 1) Nil))))))))
                                                    (Cons
                                                     ((Number 3)
                                                      (Cons
                                                       ((Number 64)
                                                        (Cons ((Number 1) Nil))))))))))))))))
                                          (Cons
                                           ((Cons
                                             ((Cons
                                               ((Number 0)
                                                (Cons
                                                 ((Cons
                                                   ((Number -1) (Number -1)))
                                                  Nil))))
                                              Nil))
                                            Nil))))
                                        (Cons
                                         ((Cons
                                           ((Cons
                                             ((Number 0)
                                              (Cons
                                               ((Number 1)
                                                (Cons
                                                 ((Cons
                                                   ((Number -22) (Number -32)))
                                                  (Cons
                                                   ((Cons
                                                     ((Number 5) (Number -2)))
                                                    (Cons
                                                     ((Cons
                                                       ((Number 105)
                                                        (Cons
                                                         ((Number 40)
                                                          (Cons
                                                           ((Number 20)
                                                            (Cons
                                                             ((Number 1) Nil))))))))
                                                      (Cons
                                                       ((Number 0)
                                                        (Cons
                                                         ((Number 128)
                                                          (Cons ((Number 2) Nil))))))))))))))))
                                            (Cons (Nil Nil))))
                                          Nil))))
                                      Nil))))
                                  (Cons
                                   ((Cons
                                     ((Number 7)
                                      (Cons
                                       ((Cons
                                         ((Cons
                                           ((Cons
                                             ((Number 1)
                                              (Cons
                                               ((Number 0)
                                                (Cons
                                                 ((Cons
                                                   ((Number 19) (Number 41)))
                                                  (Cons
                                                   ((Cons
                                                     ((Number -4) (Number 3)))
                                                    (Cons
                                                     ((Cons
                                                       ((Number 200)
                                                        (Cons
                                                         ((Number 30)
                                                          (Cons
                                                           ((Number 10)
                                                            (Cons
                                                             ((Number 1) Nil))))))))
                                                      (Cons
                                                       ((Number 1)
                                                        (Cons
                                                         ((Number 64)
                                                          (Cons ((Number 1) Nil))))))))))))))))
                                            (Cons
                                             ((Cons
                                               ((Cons
                                                 ((Number 0)
                                                  (Cons
                                                   ((Cons
                                                     ((Number 0) (Number -1)))
                                                    Nil))))
                                                Nil))
                                              Nil))))
                                          (Cons
                                           ((Cons
                                             ((Cons
                                               ((Number 0)
                                                (Cons
                                                 ((Number 1)
                                                  (Cons
                                                   ((Cons
                                                     ((Number -16) (Number -33)))
                                                    (Cons
                                                     ((Cons
                                                       ((Number 6) (Number -1)))
                                                      (Cons
                                                       ((Cons
                                                         ((Number 104)
                                                          (Cons
                                                           ((Number 40)
                                                            (Cons
                                                             ((Number 20)
                                                              (Cons
                                                               ((Number 1) Nil))))))))
                                                        (Cons
                                                         ((Number 0)
                                                          (Cons
                                                           ((Number 128)
                                                            (Cons
                                                             ((Number 2) Nil))))))))))))))))
                                              (Cons
                                               ((Cons
                                                 ((Cons
                                                   ((Number 0)
                                                    (Cons
                                                     ((Cons
                                                       ((Number -1) (Number 0)))
                                                      Nil))))
                                                  Nil))
                                                Nil))))
                                            Nil))))
                                        Nil))))
                                    (Cons
                                     ((Cons
                                       ((Number 8)
                                        (Cons
                                         ((Cons
                                           ((Cons
                                             ((Cons
                                               ((Number 1)
                                                (Cons
                                                 ((Number 0)
                                                  (Cons
                                                   ((Cons
                                                     ((Number 16) (Number 44)))
                                                    (Cons
                                                     ((Cons
                                                       ((Number -3) (Number 3)))
                                                      (Cons
                                                       ((Cons
                                                         ((Number 199)
                                                          (Cons
                                                           ((Number 30)
                                                            (Cons
                                                             ((Number 10)
                                                              (Cons
                                                               ((Number 1) Nil))))))))
                                                        (Cons
                                                         ((Number 0)
                                                          (Cons
                                                           ((Number 64)
                                                            (Cons
                                                             ((Number 1) Nil))))))))))))))))
                                              (Cons
                                               ((Cons
                                                 ((Cons
                                                   ((Number 0)
                                                    (Cons
                                                     ((Cons
                                                       ((Number -1) (Number -1)))
                                                      Nil))))
                                                  Nil))
                                                Nil))))
                                            (Cons
                                             ((Cons
                                               ((Cons
                                                 ((Number 0)
                                                  (Cons
                                                   ((Number 1)
                                                    (Cons
                                                     ((Cons
                                                       ((Number -10)
                                                        (Number -33)))
                                                      (Cons
                                                       ((Cons
                                                         ((Number 6) (Number 0)))
                                                        (Cons
                                                         ((Cons
                                                           ((Number 104)
                                                            (Cons
                                                             ((Number 40)
                                                              (Cons
                                                               ((Number 20)
                                                                (Cons
                                                                 ((Number 1) Nil))))))))
                                                          (Cons
                                                           ((Number 0)
                                                            (Cons
                                                             ((Number 128)
                                                              (Cons
                                                               ((Number 2) Nil))))))))))))))))
                                                (Cons (Nil Nil))))
                                              Nil))))
                                          Nil))))
                                      (Cons
                                       ((Cons
                                         ((Number 9)
                                          (Cons
                                           ((Cons
                                             ((Cons
                                               ((Cons
                                                 ((Number 1)
                                                  (Cons
                                                   ((Number 0)
                                                    (Cons
                                                     ((Cons
                                                       ((Number 14) (Number 47)))
                                                      (Cons
                                                       ((Cons
                                                         ((Number -2) (Number 3)))
                                                        (Cons
                                                         ((Cons
                                                           ((Number 198)
                                                            (Cons
                                                             ((Number 30)
                                                              (Cons
                                                               ((Number 10)
                                                                (Cons
                                                                 ((Number 1) Nil))))))))
                                                          (Cons
                                                           ((Number 0)
                                                            (Cons
                                                             ((Number 64)
                                                              (Cons
                                                               ((Number 1) Nil))))))))))))))))
                                                (Cons
                                                 ((Cons
                                                   ((Cons
                                                     ((Number 0)
                                                      (Cons
                                                       ((Cons
                                                         ((Number -1)
                                                          (Number -1)))
                                                        Nil))))
                                                    Nil))
                                                  Nil))))
                                              (Cons
                                               ((Cons
                                                 ((Cons
                                                   ((Number 0)
                                                    (Cons
                                                     ((Number 1)
                                                      (Cons
                                                       ((Cons
                                                         ((Number -5)
                                                          (Number -33)))
                                                        (Cons
                                                         ((Cons
                                                           ((Number 5)
                                                            (Number 0)))
                                                          (Cons
                                                           ((Cons
                                                             ((Number 103)
                                                              (Cons
                                                               ((Number 40)
                                                                (Cons
                                                                 ((Number 20)
                                                                  (Cons
                                                                   ((Number 1)
                                                                    Nil))))))))
                                                            (Cons
                                                             ((Number 0)
                                                              (Cons
                                                               ((Number 128)
                                                                (Cons
                                                                 ((Number 2) Nil))))))))))))))))
                                                  (Cons
                                                   ((Cons
                                                     ((Cons
                                                       ((Number 0)
                                                        (Cons
                                                         ((Cons
                                                           ((Number 1)
                                                            (Number 1)))
                                                          Nil))))
                                                      Nil))
                                                    Nil))))
                                                Nil))))
                                            Nil))))
                                        (Cons
                                         ((Cons
                                           ((Number 10)
                                            (Cons
                                             ((Cons
                                               ((Cons
                                                 ((Cons
                                                   ((Number 1)
                                                    (Cons
                                                     ((Number 0)
                                                      (Cons
                                                       ((Cons
                                                         ((Number 13)
                                                          (Number 49)))
                                                        (Cons
                                                         ((Cons
                                                           ((Number -1)
                                                            (Number 2)))
                                                          (Cons
                                                           ((Cons
                                                             ((Number 197)
                                                              (Cons
                                                               ((Number 30)
                                                                (Cons
                                                                 ((Number 10)
                                                                  (Cons
                                                                   ((Number 1)
                                                                    Nil))))))))
                                                            (Cons
                                                             ((Number 0)
                                                              (Cons
                                                               ((Number 64)
                                                                (Cons
                                                                 ((Number 1) Nil))))))))))))))))
                                                  (Cons
                                                   ((Cons
                                                     ((Cons
                                                       ((Number 0)
                                                        (Cons
                                                         ((Cons
                                                           ((Number -1)
                                                            (Number 0)))
                                                          Nil))))
                                                      Nil))
                                                    Nil))))
                                                (Cons
                                                 ((Cons
                                                   ((Cons
                                                     ((Number 0)
                                                      (Cons
                                                       ((Number 1)
                                                        (Cons
                                                         ((Cons
                                                           ((Number 0)
                                                            (Number -31)))
                                                          (Cons
                                                           ((Cons
                                                             ((Number 5)
                                                              (Number 2)))
                                                            (Cons
                                                             ((Cons
                                                               ((Number 102)
                                                                (Cons
                                                                 ((Number 40)
                                                                  (Cons
                                                                   ((Number 20)
                                                                    (Cons
                                                                     ((Number 1)
                                                                      Nil))))))))
                                                              (Cons
                                                               ((Number 0)
                                                                (Cons
                                                                 ((Number 128)
                                                                  (Cons
                                                                   ((Number 2)
                                                                    Nil))))))))))))))))
                                                    (Cons
                                                     ((Cons
                                                       ((Cons
                                                         ((Number 0)
                                                          (Cons
                                                           ((Cons
                                                             ((Number 0)
                                                              (Number -1)))
                                                            Nil))))
                                                        Nil))
                                                      Nil))))
                                                  Nil))))
                                              Nil))))
                                          (Cons
                                           ((Cons
                                             ((Number 11)
                                              (Cons
                                               ((Cons
                                                 ((Cons
                                                   ((Cons
                                                     ((Number 1)
                                                      (Cons
                                                       ((Number 0)
                                                        (Cons
                                                         ((Cons
                                                           ((Number 13)
                                                            (Number 50)))
                                                          (Cons
                                                           ((Cons
                                                             ((Number 0)
                                                              (Number 1)))
                                                            (Cons
                                                             ((Cons
                                                               ((Number 196)
                                                                (Cons
                                                                 ((Number 30)
                                                                  (Cons
                                                                   ((Number 10)
                                                                    (Cons
                                                                     ((Number 1)
                                                                      Nil))))))))
                                                              (Cons
                                                               ((Number 3)
                                                                (Cons
                                                                 ((Number 64)
                                                                  (Cons
                                                                   ((Number 1)
                                                                    Nil))))))))))))))))
                                                    (Cons
                                                     ((Cons
                                                       ((Cons
                                                         ((Number 0)
                                                          (Cons
                                                           ((Cons
                                                             ((Number -1)
                                                              (Number 0)))
                                                            Nil))))
                                                        Nil))
                                                      Nil))))
                                                  (Cons
                                                   ((Cons
                                                     ((Cons
                                                       ((Number 0)
                                                        (Cons
                                                         ((Number 1)
                                                          (Cons
                                                           ((Cons
                                                             ((Number 5)
                                                              (Number -28)))
                                                            (Cons
                                                             ((Cons
                                                               ((Number 5)
                                                                (Number 3)))
                                                              (Cons
                                                               ((Cons
                                                                 ((Number 102)
                                                                  (Cons
                                                                   ((Number 40)
                                                                    (Cons
                                                                     ((Number 20)
                                                                      (Cons
                                                                       ((Number
                                                                        1) Nil))))))))
                                                                (Cons
                                                                 ((Number 20)
                                                                  (Cons
                                                                   ((Number 128)
                                                                    (Cons
                                                                     ((Number 2)
                                                                      Nil))))))))))))))))
                                                      (Cons
                                                       ((Cons
                                                         ((Cons
                                                           ((Number 2)
                                                            (Cons
                                                             ((Cons
                                                               ((Number 12)
                                                                (Number 50)))
                                                              (Cons
                                                               ((Number 40)
                                                                (Cons
                                                                 ((Number 22)
                                                                  (Cons
                                                                   ((Number 4)
                                                                    Nil))))))))))
                                                          Nil))
                                                        Nil))))
                                                    Nil))))
                                                Nil))))
                                            (Cons
                                             ((Cons
                                               ((Number 12)
                                                (Cons
                                                 ((Cons
                                                   ((Cons
                                                     ((Cons
                                                       ((Number 1)
                                                        (Cons
                                                         ((Number 0)
                                                          (Cons
                                                           ((Cons
                                                             ((Number 14)
                                                              (Number 50)))
                                                            (Cons
                                                             ((Cons
                                                               ((Number 1)
                                                                (Number 0)))
                                                              (Cons
                                                               ((Cons
                                                                 ((Number 195)
                                                                  (Cons
                                                                   ((Number 30)
                                                                    (Cons
                                                                     ((Number 10)
                                                                      (Cons
                                                                       ((Number
                                                                        1) Nil))))))))
                                                                (Cons
                                                                 ((Number 10)
                                                                  (Cons
                                                                   ((Number 64)
                                                                    (Cons
                                                                     ((Number 1)
                                                                      Nil))))))))))))))))
                                                      (Cons
                                                       ((Cons
                                                         ((Cons
                                                           ((Number 0)
                                                            (Cons
                                                             ((Cons
                                                               ((Number -1)
                                                                (Number 0)))
                                                              Nil))))
                                                          Nil))
                                                        Nil))))
                                                    (Cons
                                                     ((Cons
                                                       ((Cons
                                                         ((Number 0)
                                                          (Cons
                                                           ((Number 1)
                                                            (Cons
                                                             ((Cons
                                                               ((Number 10)
                                                                (Number -24)))
                                                              (Cons
                                                               ((Cons
                                                                 ((Number 5)
                                                                  (Number 4)))
                                                                (Cons
                                                                 ((Cons
                                                                   ((Number 102)
                                                                    (Cons
                                                                     ((Number 40)
                                                                      (Cons
                                                                       ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                  (Cons
                                                                   ((Number 40)
                                                                    (Cons
                                                                     ((Number
                                                                       128)
                                                                      (Cons
                                                                       ((Number
                                                                        2) Nil))))))))))))))))
                                                        (Cons
                                                         ((Cons
                                                           ((Cons
                                                             ((Number 2)
                                                              (Cons
                                                               ((Cons
                                                                 ((Number 13)
                                                                  (Number 50)))
                                                                (Cons
                                                                 ((Number 40)
                                                                  (Cons
                                                                   ((Number 38)
                                                                    (Cons
                                                                     ((Number 4)
                                                                      Nil))))))))))
                                                            Nil))
                                                          Nil))))
                                                      Nil))))
                                                  Nil))))
                                              (Cons
                                               ((Cons
                                                 ((Number 13)
                                                  (Cons
                                                   ((Cons
                                                     ((Cons
                                                       ((Cons
                                                         ((Number 1)
                                                          (Cons
                                                           ((Number 0)
                                                            (Cons
                                                             ((Cons
                                                               ((Number 16)
                                                                (Number 49)))
                                                              (Cons
                                                               ((Cons
                                                                 ((Number 2)
                                                                  (Number -1)))
                                                                (Cons
                                                                 ((Cons
                                                                   ((Number 194)
                                                                    (Cons
                                                                     ((Number 30)
                                                                      (Cons
                                                                       ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                  (Cons
                                                                   ((Number 50)
                                                                    (Cons
                                                                     ((Number 64)
                                                                      (Cons
                                                                       ((Number
                                                                        1) Nil))))))))))))))))
                                                        (Cons
                                                         ((Cons
                                                           ((Cons
                                                             ((Number 2)
                                                              (Cons
                                                               ((Cons
                                                                 ((Number 15)
                                                                  (Number -19)))
                                                                (Cons
                                                                 ((Number 30)
                                                                  (Cons
                                                                   ((Number 21)
                                                                    (Cons
                                                                     ((Number 4)
                                                                      Nil))))))))))
                                                            (Cons
                                                             ((Cons
                                                               ((Number 0)
                                                                (Cons
                                                                 ((Cons
                                                                   ((Number -1)
                                                                    (Number 0)))
                                                                  Nil))))
                                                              Nil))))
                                                          Nil))))
                                                      (Cons
                                                       ((Cons
                                                         ((Cons
                                                           ((Number 0)
                                                            (Cons
                                                             ((Number 1)
                                                              (Cons
                                                               ((Cons
                                                                 ((Number 14)
                                                                  (Number -18)))
                                                                (Cons
                                                                 ((Cons
                                                                   ((Number 4)
                                                                    (Number 6)))
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Number
                                                                       101)
                                                                      (Cons
                                                                       ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                    (Cons
                                                                     ((Number 73)
                                                                      (Cons
                                                                       ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                          (Cons
                                                           ((Cons
                                                             ((Cons
                                                               ((Number 2)
                                                                (Cons
                                                                 ((Cons
                                                                   ((Number 15)
                                                                    (Number 49)))
                                                                  (Cons
                                                                   ((Number 40)
                                                                    (Cons
                                                                     ((Number 51)
                                                                      (Cons
                                                                       ((Number
                                                                        4) Nil))))))))))
                                                              (Cons
                                                               ((Cons
                                                                 ((Number 0)
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Number 1)
                                                                      (Number -1)))
                                                                    Nil))))
                                                                Nil))))
                                                            Nil))))
                                                        Nil))))
                                                    Nil))))
                                                (Cons
                                                 ((Cons
                                                   ((Number 14)
                                                    (Cons
                                                     ((Cons
                                                       ((Cons
                                                         ((Cons
                                                           ((Number 1)
                                                            (Cons
                                                             ((Number 0)
                                                              (Cons
                                                               ((Cons
                                                                 ((Number 18)
                                                                  (Number 46)))
                                                                (Cons
                                                                 ((Cons
                                                                   ((Number 2)
                                                                    (Number -3)))
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Number
                                                                       170)
                                                                      (Cons
                                                                       ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                    (Cons
                                                                     ((Number 64)
                                                                      (Cons
                                                                       ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                          (Cons
                                                           ((Cons
                                                             ((Cons
                                                               ((Number 2)
                                                                (Cons
                                                                 ((Cons
                                                                   ((Number 18)
                                                                    (Number -11)))
                                                                  (Cons
                                                                   ((Number 24)
                                                                    (Cons
                                                                     ((Number 16)
                                                                      (Cons
                                                                       ((Number
                                                                        4) Nil))))))))))
                                                              (Cons
                                                               ((Cons
                                                                 ((Number 0)
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Number 0)
                                                                      (Number 1)))
                                                                    Nil))))
                                                                Nil))))
                                                            Nil))))
                                                        (Cons
                                                         ((Cons
                                                           ((Cons
                                                             ((Number 0)
                                                              (Cons
                                                               ((Number 1)
                                                                (Cons
                                                                 ((Cons
                                                                   ((Number 18)
                                                                    (Number -11)))
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Number 4)
                                                                      (Number 7)))
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        101)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                      (Cons
                                                                       ((Number
                                                                        109)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                            (Cons
                                                             ((Cons
                                                               ((Cons
                                                                 ((Number 2)
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Number 18)
                                                                      (Number 47)))
                                                                    (Cons
                                                                     ((Number 40)
                                                                      (Cons
                                                                       ((Number
                                                                        63)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                Nil))
                                                              Nil))))
                                                          Nil))))
                                                      Nil))))
                                                  (Cons
                                                   ((Cons
                                                     ((Number 15)
                                                      (Cons
                                                       ((Cons
                                                         ((Cons
                                                           ((Cons
                                                             ((Number 1)
                                                              (Cons
                                                               ((Number 0)
                                                                (Cons
                                                                 ((Cons
                                                                   ((Number 20)
                                                                    (Number 42)))
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Number 2)
                                                                      (Number -4)))
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        112)
                                                                        (Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                      (Cons
                                                                       ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                            (Cons (Nil Nil))))
                                                          (Cons
                                                           ((Cons
                                                             ((Cons
                                                               ((Number 0)
                                                                (Cons
                                                                 ((Number 1)
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Number 21)
                                                                      (Number -3)))
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        3)
                                                                        (Number
                                                                        8)))
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        92)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                              (Cons
                                                               ((Cons
                                                                 ((Cons
                                                                   ((Number 2)
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        20)
                                                                        (Number
                                                                        42)))
                                                                      (Cons
                                                                       ((Number
                                                                        39)
                                                                        (Cons
                                                                        ((Number
                                                                        68)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Number 0)
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Number
                                                                        -1)))
                                                                        Nil))))
                                                                    Nil))))
                                                                Nil))))
                                                            Nil))))
                                                        Nil))))
                                                    (Cons
                                                     ((Cons
                                                       ((Number 16)
                                                        (Cons
                                                         ((Cons
                                                           ((Cons
                                                             ((Cons
                                                               ((Number 1)
                                                                (Cons
                                                                 ((Number 0)
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Number 22)
                                                                      (Number 37)))
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        2)
                                                                        (Number
                                                                        -5)))
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        94)
                                                                        (Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                              (Cons (Nil Nil))))
                                                            (Cons
                                                             ((Cons
                                                               ((Cons
                                                                 ((Number 0)
                                                                  (Cons
                                                                   ((Number 1)
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        22)
                                                                        (Number
                                                                        4)))
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Number
                                                                        7)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        83)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                (Cons
                                                                 ((Cons
                                                                   ((Cons
                                                                     ((Number 2)
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        22)
                                                                        (Number
                                                                        37)))
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        28)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Number
                                                                        1))) Nil))))
                                                                      Nil))))
                                                                  Nil))))
                                                              Nil))))
                                                          Nil))))
                                                      (Cons
                                                       ((Cons
                                                         ((Number 17)
                                                          (Cons
                                                           ((Cons
                                                             ((Cons
                                                               ((Cons
                                                                 ((Number 1)
                                                                  (Cons
                                                                   ((Number 0)
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        25)
                                                                        (Number
                                                                        32)))
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        3)
                                                                        (Number
                                                                        -5)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        76)
                                                                        (Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                (Cons
                                                                 ((Cons
                                                                   ((Cons
                                                                     ((Number 2)
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        22)
                                                                        (Number
                                                                        11)))
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -1)
                                                                        (Number
                                                                        -1)))
                                                                        Nil))))
                                                                      Nil))))
                                                                  Nil))))
                                                              (Cons
                                                               ((Cons
                                                                 ((Cons
                                                                   ((Number 0)
                                                                    (Cons
                                                                     ((Number 1)
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        23)
                                                                        (Number
                                                                        12)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Number
                                                                        8)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        74)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        24)
                                                                        (Number
                                                                        31)))
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        36)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -1)
                                                                        (Number
                                                                        -1)))
                                                                        Nil))))
                                                                        Nil))))
                                                                    Nil))))
                                                                Nil))))
                                                            Nil))))
                                                        (Cons
                                                         ((Cons
                                                           ((Number 18)
                                                            (Cons
                                                             ((Cons
                                                               ((Cons
                                                                 ((Cons
                                                                   ((Number 1)
                                                                    (Cons
                                                                     ((Number 0)
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        28)
                                                                        (Number
                                                                        25)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        3)
                                                                        (Number
                                                                        -7)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        59)
                                                                        (Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        23)
                                                                        (Number
                                                                        20)))
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        26)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Number
                                                                        1))) Nil))))
                                                                        Nil))))
                                                                    Nil))))
                                                                (Cons
                                                                 ((Cons
                                                                   ((Cons
                                                                     ((Number 0)
                                                                      (Cons
                                                                       ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        22)
                                                                        (Number
                                                                        21)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -1)
                                                                        (Number
                                                                        9)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        59)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        28)
                                                                        (Number
                                                                        26)))
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        35)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Number
                                                                        -1)))
                                                                        Nil))))
                                                                        Nil))))
                                                                      Nil))))
                                                                  Nil))))
                                                              Nil))))
                                                          (Cons
                                                           ((Cons
                                                             ((Number 19)
                                                              (Cons
                                                               ((Cons
                                                                 ((Cons
                                                                   ((Cons
                                                                     ((Number 1)
                                                                      (Cons
                                                                       ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        31)
                                                                        (Number
                                                                        19)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        3)
                                                                        (Number
                                                                        -6)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        38)
                                                                        (Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        20)
                                                                        (Number
                                                                        30)))
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -1)
                                                                        (Number
                                                                        -1)))
                                                                        Nil))))
                                                                        Nil))))
                                                                      Nil))))
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        19)
                                                                        (Number
                                                                        29)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -3)
                                                                        (Number
                                                                        8)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        45)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        30)
                                                                        (Number
                                                                        18)))
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        50)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Number
                                                                        1))) Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                    Nil))))
                                                                Nil))))
                                                            (Cons
                                                             ((Cons
                                                               ((Number 20)
                                                                (Cons
                                                                 ((Cons
                                                                   ((Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        33)
                                                                        (Number
                                                                        14)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Number
                                                                        -5)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        35)
                                                                        (Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Number
                                                                        -1)))
                                                                        Nil))))
                                                                        Nil))
                                                                        Nil))))
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        15)
                                                                        (Number
                                                                        35)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -4)
                                                                        (Number
                                                                        6)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        36)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        33)
                                                                        (Number
                                                                        13)))
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        18)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Number
                                                                        1))) Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                      Nil))))
                                                                  Nil))))
                                                              (Cons
                                                               ((Cons
                                                                 ((Number 21)
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        35)
                                                                        (Number
                                                                        10)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Number
                                                                        -4)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        34)
                                                                        (Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        62)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -1)
                                                                        (Number
                                                                        -1)))
                                                                        Nil))))
                                                                        Nil))
                                                                        Nil))))
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        10)
                                                                        (Number
                                                                        39)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -5)
                                                                        (Number
                                                                        4)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        35)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        116)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Number
                                                                        1))) Nil))))
                                                                        Nil))
                                                                        Nil))))
                                                                        Nil))))
                                                                    Nil))))
                                                                (Cons
                                                                 ((Cons
                                                                   ((Number 22)
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        37)
                                                                        (Number
                                                                        7)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Number
                                                                        -3)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        26)
                                                                        (Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -1)
                                                                        (Number
                                                                        -1)))
                                                                        Nil))))
                                                                        Nil))
                                                                        Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        4)
                                                                        (Number
                                                                        41)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -6)
                                                                        (Number
                                                                        2)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        26)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        36)
                                                                        (Number
                                                                        6)))
                                                                        (Cons
                                                                        ((Number
                                                                        32)
                                                                        (Cons
                                                                        ((Number
                                                                        46)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Number
                                                                        1))) Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                      Nil))))
                                                                  (Cons
                                                                   ((Cons
                                                                     ((Number 23)
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        37)
                                                                        (Number
                                                                        3)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Number
                                                                        -4)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        22)
                                                                        (Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Number
                                                                        1))) Nil))))
                                                                        Nil))
                                                                        Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -1)
                                                                        (Number
                                                                        43)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -5)
                                                                        (Number
                                                                        2)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        17)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        38)
                                                                        (Number
                                                                        4)))
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        22)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -1)
                                                                        (Number
                                                                        -1)))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                    (Cons
                                                                     ((Cons
                                                                       ((Number
                                                                        24)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        37)
                                                                        (Number
                                                                        -1)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Number
                                                                        -4)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        21)
                                                                        (Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -1)
                                                                        (Number
                                                                        0))) Nil))))
                                                                        Nil))
                                                                        Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -6)
                                                                        (Number
                                                                        44)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -5)
                                                                        (Number
                                                                        1)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        17)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        36)
                                                                        (Number
                                                                        -1)))
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        8)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                        Nil))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                      (Cons
                                                                       ((Cons
                                                                        ((Number
                                                                        25)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        36)
                                                                        (Number
                                                                        -5)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -1)
                                                                        (Number
                                                                        -4)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        21)
                                                                        (Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        61)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                        (Cons
                                                                        (Nil Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -12)
                                                                        (Number
                                                                        45)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -6)
                                                                        (Number
                                                                        1)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        8)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        36)
                                                                        (Number
                                                                        -5)))
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        7)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Number
                                                                        -1)))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        26)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        35)
                                                                        (Number
                                                                        -10)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -1)
                                                                        (Number
                                                                        -5)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        59)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -1)
                                                                        (Number
                                                                        1))) Nil))))
                                                                        Nil))
                                                                        Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -18)
                                                                        (Number
                                                                        45)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -6)
                                                                        (Number
                                                                        0)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        8)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        34)
                                                                        (Number
                                                                        -9)))
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        3)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                        Nil))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        27)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        33)
                                                                        (Number
                                                                        -15)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -2)
                                                                        (Number
                                                                        -5)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        49)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                        (Cons
                                                                        (Nil Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -24)
                                                                        (Number
                                                                        44)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -6)
                                                                        (Number
                                                                        -1)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        8)
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        108)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                        (Cons
                                                                        (Nil Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        28)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        30)
                                                                        (Number
                                                                        -20)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -3)
                                                                        (Number
                                                                        -5)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        22)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                        (Cons
                                                                        (Nil Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -29)
                                                                        (Number
                                                                        41)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -5)
                                                                        (Number
                                                                        -3)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        39)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        30)
                                                                        (Number
                                                                        -20)))
                                                                        (Cons
                                                                        ((Number
                                                                        40)
                                                                        (Cons
                                                                        ((Number
                                                                        53)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -1)
                                                                        (Number
                                                                        1))) Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        29)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        26)
                                                                        (Number
                                                                        -25)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -4)
                                                                        (Number
                                                                        -5)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        22)
                                                                        (Cons
                                                                        ((Number
                                                                        10)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        54)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                        (Cons
                                                                        (Nil Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -34)
                                                                        (Number
                                                                        37)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -5)
                                                                        (Number
                                                                        -4)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        39)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        108)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                        (Cons
                                                                        (Nil Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        30)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        21)
                                                                        (Number
                                                                        -30)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -5)
                                                                        (Number
                                                                        -5)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        3)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                        (Cons
                                                                        (Nil Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -39)
                                                                        (Number
                                                                        32)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -5)
                                                                        (Number
                                                                        -5)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        39)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        127)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        21)
                                                                        (Number
                                                                        -30)))
                                                                        (Cons
                                                                        ((Number
                                                                        39)
                                                                        (Cons
                                                                        ((Number
                                                                        49)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                        Nil))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        31)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        16)
                                                                        (Number
                                                                        -34)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -5)
                                                                        (Number
                                                                        -4)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        3)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        61)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                        (Cons
                                                                        (Nil Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -43)
                                                                        (Number
                                                                        27)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -4)
                                                                        (Number
                                                                        -5)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        39)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        107)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                        (Cons
                                                                        (Nil Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        32)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        11)
                                                                        (Number
                                                                        -37)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -5)
                                                                        (Number
                                                                        -3)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        0) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        64)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))))))))))
                                                                        (Cons
                                                                        (Nil Nil))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        1)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -46)
                                                                        (Number
                                                                        22)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        -3)
                                                                        (Number
                                                                        -5)))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        0)
                                                                        (Cons
                                                                        ((Number
                                                                        39)
                                                                        (Cons
                                                                        ((Number
                                                                        20)
                                                                        (Cons
                                                                        ((Number
                                                                        1) Nil))))))))
                                                                        (Cons
                                                                        ((Number
                                                                        126)
                                                                        (Cons
                                                                        ((Number
                                                                        128)
                                                                        (Cons
                                                                        ((Number
                                                                        2) Nil))))))))))))))))
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        2)
                                                                        (Cons
                                                                        ((Cons
                                                                        ((Number
                                                                        11)
                                                                        (Number
                                                                        -37)))
                                                                        (Cons
                                                                        ((Number
                                                                        39)
                                                                        (Cons
                                                                        ((Number
                                                                        52)
                                                                        (Cons
                                                                        ((Number
                                                                        4) Nil))))))))))
                                                                        Nil))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))
                                                                        Nil))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                    Nil))))
                Nil)))))))))))) |}]
;;
