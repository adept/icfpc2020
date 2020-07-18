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
  ]
  |> List.map ~f:Eval.parse_def_exn
  |> List.fold_left ~init:Eval.base_defs ~f:(fun acc (key, data) ->
         Map.set acc ~key ~data)
  |> Map.set ~key:"f2048" ~data:(Eval.App (Var "f", Var "f2048"))
;;

let%expect_test "base combinators" =
  let test str =
    printf "Starting evaluation: %s\n" str;
    let res =
      (Eval.eval_custom ~verbose:false ~defs |> Staged.unstage)
        (Eval.parse_exn (String.split str ~on:' '))
    in
    printf "Result: %s\n" (Eval.to_string_hum res)
  in
  test "ap ap add 1 2";
  test "ap ap add 3 ap ap add 1 2";
  test "ap car ap ap cons x0 x1";
  test "ap cdr ap ap cons x0 x1";
  test "ap car ap ap cons x0 ap ap cons x1 x2";
  test "ap cdr ap ap cons x0 ap ap cons x1 x2";
  test "ap ap div 4 2";
  test "ap ap div x0 1";
  test "ap ap eq x0 x0";
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
  test "ap ap mul x0 x1";
  test "ap ap mul x0 0";
  test "ap ap mul x0 1";
  test "ap ap mul 0 x0";
  test "ap ap mul 1 x0";
  test "ap ap ap s x0 x1 x2";
  test "ap ap ap s add inc 1";
  test "ap ap ap s mul ap add 1 6";
  test "ap ap ap c x0 x1 x2";
  test "ap ap ap c add 1 2";
  test "ap ap ap b x0 x1 x2";
  test "ap ap ap b inc dec x0";
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
    Starting evaluation: ap ap div x0 1
    Result: "x0"
    Starting evaluation: ap ap eq x0 x0
    Result: "t"
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
    Result: ap "isnil" (ap (ap "cons" "x0") "x1")
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
    Starting evaluation: ap ap mul x0 x1
    Result: ap (ap "mul" "x0") "x1"
    Starting evaluation: ap ap mul x0 0
    Result: "0"
    Starting evaluation: ap ap mul x0 1
    Result: "x0"
    Starting evaluation: ap ap mul 0 x0
    Result: "0"
    Starting evaluation: ap ap mul 1 x0
    Result: "x0"
    Starting evaluation: ap ap ap s x0 x1 x2
    Result: ap (ap "x0" "x2") (ap "x1" "x2")
    Starting evaluation: ap ap ap s add inc 1
    Result: "3"
    Starting evaluation: ap ap ap s mul ap add 1 6
    Result: "42"
    Starting evaluation: ap ap ap c x0 x1 x2
    Result: ap (ap "x0" "x2") "x1"
    Starting evaluation: ap ap ap c add 1 2
    Result: "3"
    Starting evaluation: ap ap ap b x0 x1 x2
    Result: ap "x0" (ap "x1" "x2")
    Starting evaluation: ap ap ap b inc dec x0
    Result: "x0" |}]
;;

let%expect_test "eval" =
  print_endline "Defs:";
  List.iter (Map.to_alist defs) ~f:(fun (name, expansion) ->
      printf "%s := %s\n" name (Eval.to_string_hum expansion));
  let test str =
    let res =
      (Eval.eval_custom ~verbose:true ~defs |> Staged.unstage)
        (Eval.parse_exn (String.split str ~on:' '))
    in
    printf "Result: %s\n" (Eval.to_string_hum res)
  in
  test "ap f2048 42";
  [%expect
    {|
    Defs:
    f2048 := ap "f" "f2048"
    statelessdraw := ap (ap "c" (ap (ap "b" "b") (ap (ap "b" (ap "b" (ap "cons" "0"))) (ap (ap "c" (ap (ap "b" "b") "cons")) (ap (ap "c" "cons") "nil"))))) (ap (ap "c" (ap (ap "b" "cons") (ap (ap "c" "cons") "nil"))) "nil")
    Eval (length: 3): ap "f2048" "42"
    Result: "42" |}];
  test "ap ap ap s add inc 1";
  [%expect {|
    Eval (length: 7): ap (ap (ap "s" "add") "inc") "1"
    Result: "3" |}];
  test "ap ap ap c x y ap ap add 1 2";
  [%expect
    {|
    Eval (length: 11): ap (ap (ap "c" "x") "y") (ap (ap "add" "1") "2")
    Result: ap (ap "x" "3") "y" |}];
  test "ap ap statelessdraw x0 x1";
  [%expect
    {|
    Eval (length: 5): ap (ap "statelessdraw" "x0") "x1"
    Result: ap (ap "cons" "0") (ap (ap "cons" "x0") (ap (ap "cons" (ap (ap "cons" (ap (ap "cons" "x1") "nil")) "nil")) "nil"))
  |}]
;;
