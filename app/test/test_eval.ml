open! Core
module Eval = Icfpc2020.Eval

let%expect_test "parsing" =
  let test str =
    match Eval.parse (String.split ~on:' ' str) with
    | Error err -> printf !"Error: %{Error#hum}\n" err
    | Ok (res, leftover) ->
      print_endline (Eval.to_string_hum res);
      if not (List.is_empty leftover)
      then printf !"Leftover: %{sexp: string list}\n" leftover
  in
  test "ap ap statelessdraw x0 x1";
  [%expect {|
    ap (ap "statelessdraw" "x0") "x1" |}];
  test "ap ap add ap ap add 2 3 ap ap add 4 5";
  [%expect {|
    ap (ap "add" (ap (ap "add" "2") "3")) (ap (ap "add" "4") "5") |}];
  test "ap ap ap c x y ap ap add 1 2";
  [%expect {|
    ap (ap (ap "c" "x") "y") (ap (ap "add" "1") "2") |}]
;;

let defs =
  [ "statelessdraw = ap ap c ap ap b b ap ap b ap b ap cons 0 ap ap c ap ap b b cons ap \
     ap c cons nil ap ap c ap ap b cons ap ap c cons nil nil"
  ; "pwr2 = ap ap s ap ap c ap eq 0 1 ap ap b ap mul 2 ap ap b pwr2 ap add -1"
  ]
  |> List.map ~f:Eval.parse_def_exn
  |> List.fold_left ~init:Eval.base_defs ~f:(fun acc (key, data) ->
         Map.set acc ~key ~data)
  |> Map.set ~key:"f2048" ~data:(Eval.app (Eval.var "f") (Eval.var "f2048"))
;;

let%expect_test "base combinators" =
  let test str =
    printf "Starting evaluation: %s\n" str;
    let res =
      Eval.eval_custom ~verbose:false ~defs (Eval.parse_exn (String.split str ~on:' '))
    in
    printf "Result: %s\n" (Eval.to_string_hum res)
  in
  (* #5 *)
  test "ap inc 0";
  test "ap inc 300";
  test "ap inc -1";
  test "ap inc -2";
  [%expect
    {|
    Starting evaluation: ap inc 0
    Result: ap "inc" "0"
    Starting evaluation: ap inc 300
    Result: ap "inc" "300"
    Starting evaluation: ap inc -1
    Result: ap "inc" "-1"
    Starting evaluation: ap inc -2
    Result: ap "inc" "-2" |}];
  (* #6 *)
  test "ap dec 1";
  test "ap dec 1024";
  test "ap dec 0";
  test "ap dec -1";
  [%expect
    {|
    Starting evaluation: ap dec 1
    Result: ap "dec" "1"
    Starting evaluation: ap dec 1024
    Result: ap "dec" "1024"
    Starting evaluation: ap dec 0
    Result: ap "dec" "0"
    Starting evaluation: ap dec -1
    Result: ap "dec" "-1" |}];
  (* #7 *)
  test "ap ap add 1 2";
  test "ap ap add 2 1";
  test "ap ap add 0 1";
  test "ap ap add 2 3";
  test "ap ap add 3 5";
  [%expect
    {|
    Starting evaluation: ap ap add 1 2
    Result: "3"
    Starting evaluation: ap ap add 2 1
    Result: "3"
    Starting evaluation: ap ap add 0 1
    Result: "1"
    Starting evaluation: ap ap add 2 3
    Result: "5"
    Starting evaluation: ap ap add 3 5
    Result: "8" |}];
  (* #8 *)
  (* test "ap ap add 0 x0";
   * test "ap ap add x1 0";
   * test "ap ap add x0 x1";
   * [%expect {||}] *)
  (* #9 *)
  test "ap ap mul 4 2";
  test "ap ap mul 3 4";
  test "ap ap mul 3 -2";
  (* test "ap ap mul x0 x1";
   * test "ap ap mul x0 0";
   * test "ap ap mul x0 1"; *)
  [%expect
    {|
    Starting evaluation: ap ap mul 4 2
    Result: "8"
    Starting evaluation: ap ap mul 3 4
    Result: "12"
    Starting evaluation: ap ap mul 3 -2
    Result: "-6" |}];
  (* #10 *)
  test "ap ap div 4 2";
  test "ap ap div 4 3";
  test "ap ap div 4 4";
  test "ap ap div 4 5";
  test "ap ap div 5 2";
  test "ap ap div 6 -2";
  test "ap ap div 5 -3";
  (* TODO: -1 *)
  test "ap ap div -5 3";
  (* TODO: 1 *)
  test "ap ap div -5 -3";
  (* test "ap ap div x0 1"; *)
  [%expect
    {|
    Starting evaluation: ap ap div 4 2
    Result: "2"
    Starting evaluation: ap ap div 4 3
    Result: "1"
    Starting evaluation: ap ap div 4 4
    Result: "1"
    Starting evaluation: ap ap div 4 5
    Result: "0"
    Starting evaluation: ap ap div 5 2
    Result: "2"
    Starting evaluation: ap ap div 6 -2
    Result: "-3"
    Starting evaluation: ap ap div 5 -3
    Result: "-1"
    Starting evaluation: ap ap div -5 3
    Result: "-1"
    Starting evaluation: ap ap div -5 -3
    Result: "1" |}];
  (* #11 *)
  (* test "ap ap eq x0 x0"; *)
  test "ap ap eq 0 -2";
  test "ap ap eq 0 -1";
  test "ap ap eq 0 0";
  test "ap ap eq 0 1";
  test "ap ap eq 0 2";
  [%expect
    {|
    Starting evaluation: ap ap eq 0 -2
    Result: "f"
    Starting evaluation: ap ap eq 0 -1
    Result: "f"
    Starting evaluation: ap ap eq 0 0
    Result: "t"
    Starting evaluation: ap ap eq 0 1
    Result: "f"
    Starting evaluation: ap ap eq 0 2
    Result: "f" |}];
  test "ap ap eq 1 -1";
  test "ap ap eq 1 0";
  test "ap ap eq 1 1";
  test "ap ap eq 1 2";
  test "ap ap eq 1 3";
  [%expect
    {|
    Starting evaluation: ap ap eq 1 -1
    Result: "f"
    Starting evaluation: ap ap eq 1 0
    Result: "f"
    Starting evaluation: ap ap eq 1 1
    Result: "t"
    Starting evaluation: ap ap eq 1 2
    Result: "f"
    Starting evaluation: ap ap eq 1 3
    Result: "f" |}];
  (* #12 *)
  test "ap ap lt 0 -1";
  test "ap ap lt 0 0";
  test "ap ap lt 0 1";
  test "ap ap lt 0 2";
  [%expect
    {|
    Starting evaluation: ap ap lt 0 -1
    Result: "f"
    Starting evaluation: ap ap lt 0 0
    Result: "f"
    Starting evaluation: ap ap lt 0 1
    Result: "t"
    Starting evaluation: ap ap lt 0 2
    Result: "t" |}];
  test "ap ap lt 1 0";
  test "ap ap lt 1 1";
  test "ap ap lt 1 2";
  test "ap ap lt 1 3";
  [%expect
    {|
    Starting evaluation: ap ap lt 1 0
    Result: "f"
    Starting evaluation: ap ap lt 1 1
    Result: "f"
    Starting evaluation: ap ap lt 1 2
    Result: "t"
    Starting evaluation: ap ap lt 1 3
    Result: "t" |}];
  (* #16 *)
  test "ap neg 0";
  test "ap neg 1";
  test "ap neg -1";
  test "ap neg 2";
  test "ap neg -2";
  [%expect
    {|
    Starting evaluation: ap neg 0
    Result: "0"
    Starting evaluation: ap neg 1
    Result: "-1"
    Starting evaluation: ap neg -1
    Result: "1"
    Starting evaluation: ap neg 2
    Result: "-2"
    Starting evaluation: ap neg -2
    Result: "2" |}];
  (* #17 *)
  test "ap inc ap inc 0";
  test "ap inc ap inc ap inc 0";
  test "ap inc ap dec x0";
  test "ap dec ap inc x0";
  test "ap dec ap ap add x0 1";
  test "ap ap add ap ap add 2 3 4";
  test "ap ap add 2 ap ap add 3 4";
  test "ap ap add ap ap mul 2 3 4";
  test "ap ap mul 2 ap ap add 3 4";
  [%expect
    {|
    Starting evaluation: ap inc ap inc 0
    Result: ap "inc" (ap "inc" "0")
    Starting evaluation: ap inc ap inc ap inc 0
    Result: ap "inc" (ap "inc" (ap "inc" "0"))
    Starting evaluation: ap inc ap dec x0
    Result: ap "inc" (ap "dec" "x0")
    Starting evaluation: ap dec ap inc x0
    Result: ap "dec" (ap "inc" "x0")
    Starting evaluation: ap dec ap ap add x0 1
    Result: ap "dec" (ap (ap "add" "x0") "1")
    Starting evaluation: ap ap add ap ap add 2 3 4
    Result: "9"
    Starting evaluation: ap ap add 2 ap ap add 3 4
    Result: "9"
    Starting evaluation: ap ap add ap ap mul 2 3 4
    Result: "10"
    Starting evaluation: ap ap mul 2 ap ap add 3 4
    Result: "14" |}];
  (* #21 *)
  test "ap ap t x0 x1";
  test "ap ap t 1 5";
  test "ap ap t t i";
  test "ap ap t t ap inc 5";
  test "ap ap t ap inc 5 t";
  [%expect
    {|
    Starting evaluation: ap ap t x0 x1
    Result: "x0"
    Starting evaluation: ap ap t 1 5
    Result: "1"
    Starting evaluation: ap ap t t i
    Result: "t"
    Starting evaluation: ap ap t t ap inc 5
    Result: "t"
    Starting evaluation: ap ap t ap inc 5 t
    Result: ap "inc" "5" |}];
  (* #37 *)
  test "ap ap ap if0 0 x0 x1";
  test "ap ap ap if0 1 x0 x1";
  [%expect
    {|
    Starting evaluation: ap ap ap if0 0 x0 x1
    Result: ap (ap (ap "if0" "0") "x0") "x1"
    Starting evaluation: ap ap ap if0 1 x0 x1
    Result: ap (ap (ap "if0" "1") "x0") "x1" |}];
  (* Our tests *)
  test "ap ap add 1 2";
  test "ap ap add 3 ap ap add 1 2";
  test "ap car ap ap cons x0 x1";
  test "ap cdr ap ap cons x0 x1";
  test "ap car ap ap cons x0 ap ap cons x1 x2";
  test "ap cdr ap ap cons x0 ap ap cons x1 x2";
  test "ap ap div 4 2";
  (* test "ap ap div x0 1"; *)
  (* test "ap ap eq x0 x0"; *)
  test "ap ap eq 0 0";
  test "ap ap eq 0 -2";
  test "ap ap eq 0 ap car ap ap cons 0 1";
  test "ap i x0";
  test "ap i ap add 1";
  test "ap isnil nil";
  test "ap isnil ap ap cons x0 x1";
  test "ap isnil ap car ap ap cons nil x0";
  test "ap isnil ap cdr ap ap cons x0 nil";
  test "ap ap lt 0 -1";
  test "ap ap lt 0 0";
  test "ap ap lt 0 1";
  test "ap neg 0";
  test "ap neg 1";
  test "ap neg -1";
  test "ap ap mul 4 2";
  (* test "ap ap mul x0 x1"; *)
  (* test "ap ap mul x0 0"; *)
  (* test "ap ap mul x0 1"; *)
  (* test "ap ap mul 0 x0"; *)
  (* test "ap ap mul 1 x0"; *)
  test "ap ap ap s x0 x1 x2";
  (* test "ap ap ap s add inc 1"; *)
  test "ap ap ap s mul ap add 1 6";
  test "ap ap ap c x0 x1 x2";
  test "ap ap ap c add 1 2";
  test "ap ap ap b x0 x1 x2";
  (* test "ap ap ap b inc dec x0"; *)
  [%expect
    {|
    Starting evaluation: ap ap add 1 2
    Result: "3"
    Starting evaluation: ap ap add 3 ap ap add 1 2
    Result: "6"
    Starting evaluation: ap car ap ap cons x0 x1
    Result: "x0"
    Starting evaluation: ap cdr ap ap cons x0 x1
    Result: "x1"
    Starting evaluation: ap car ap ap cons x0 ap ap cons x1 x2
    Result: "x0"
    Starting evaluation: ap cdr ap ap cons x0 ap ap cons x1 x2
    Result: ap (ap "cons" "x1") "x2"
    Starting evaluation: ap ap div 4 2
    Result: "2"
    Starting evaluation: ap ap eq 0 0
    Result: "t"
    Starting evaluation: ap ap eq 0 -2
    Result: "f"
    Starting evaluation: ap ap eq 0 ap car ap ap cons 0 1
    Result: "t"
    Starting evaluation: ap i x0
    Result: "x0"
    Starting evaluation: ap i ap add 1
    Result: ap "add" "1"
    Starting evaluation: ap isnil nil
    Result: "t"
    Starting evaluation: ap isnil ap ap cons x0 x1
    Result: "f"
    Starting evaluation: ap isnil ap car ap ap cons nil x0
    Result: "t"
    Starting evaluation: ap isnil ap cdr ap ap cons x0 nil
    Result: "t"
    Starting evaluation: ap ap lt 0 -1
    Result: "f"
    Starting evaluation: ap ap lt 0 0
    Result: "f"
    Starting evaluation: ap ap lt 0 1
    Result: "t"
    Starting evaluation: ap neg 0
    Result: "0"
    Starting evaluation: ap neg 1
    Result: "-1"
    Starting evaluation: ap neg -1
    Result: "1"
    Starting evaluation: ap ap mul 4 2
    Result: "8"
    Starting evaluation: ap ap ap s x0 x1 x2
    Result: ap (ap "x0" "x2") (ap "x1" "x2")
    Starting evaluation: ap ap ap s mul ap add 1 6
    Result: "42"
    Starting evaluation: ap ap ap c x0 x1 x2
    Result: ap (ap "x0" "x2") "x1"
    Starting evaluation: ap ap ap c add 1 2
    Result: "3"
    Starting evaluation: ap ap ap b x0 x1 x2
    Result: ap "x0" (ap "x1" "x2") |}]
;;

let%expect_test "eval" =
  print_endline "Defs:";
  List.iter (Map.to_alist defs) ~f:(fun (name, expansion) ->
      printf "%s := %s\n" name (Eval.to_string_hum expansion));
  [%expect
    {|
    Defs:
    f2048 := ap "f" "f2048"
    pwr2 := ap (ap "s" (ap (ap "c" (ap "eq" "0")) "1")) (ap (ap "b" (ap "mul" "2")) (ap (ap "b" "pwr2") (ap "add" "-1")))
    statelessdraw := ap (ap "c" (ap (ap "b" "b") (ap (ap "b" (ap "b" (ap "cons" "0"))) (ap (ap "c" (ap (ap "b" "b") "cons")) (ap (ap "c" "cons") "nil"))))) (ap (ap "c" (ap (ap "b" "cons") (ap (ap "c" "cons") "nil"))) "nil") |}];
  let test str =
    let res =
      Eval.eval_custom ~verbose:false ~defs (Eval.parse_exn (String.split str ~on:' '))
    in
    printf "Result: %s\n" (Eval.to_string_hum res)
  in
  test "ap pwr2 2";
  [%expect {| Result: "4" |}];
  test "ap pwr2 3";
  [%expect {| Result: "8" |}];
  test "ap pwr2 4";
  [%expect {| Result: "16" |}];
  test "ap pwr2 5";
  [%expect {| Result: "32" |}];
  test "ap pwr2 6";
  [%expect {| Result: "64" |}];
  test "ap pwr2 7";
  [%expect {| Result: "128" |}];
  test "ap pwr2 8";
  [%expect {| Result: "256" |}];
  test "ap f2048 42";
  [%expect {| Result: "42" |}]
;;

(* test "ap ap ap s add inc 1";
 * [%expect.unreachable];
 * test "ap ap ap c x y ap ap add 1 2";
 * [%expect.unreachable];
 * test "ap ap statelessdraw x0 x1";
 * [%expect.unreachable] *)
