open! Core

type t =
  | Ap of t * t
  | Name of string
[@@deriving equal, sexp]

let rec to_string_hum = function
  | Name name -> sprintf {|"%s"|} name
  | Ap (x, y) ->
    let str_of_arg arg =
      match arg with
      | Name name -> sprintf {|"%s"|} name
      | _ -> sprintf "(%s)" (to_string_hum arg)
    in
    sprintf "ap %s %s" (str_of_arg x) (str_of_arg y)
;;

let parse ws =
  let rec loop = function
    | "ap" :: rest ->
      let arg1, leftover = loop rest in
      let arg2, leftover = loop leftover in
      Ap (arg1, arg2), leftover
    | name :: rest -> Name name, rest
    | ws -> raise_s [%sexp "Failed to parse ap", (ws : string list)]
  in
  Or_error.try_with (fun () -> loop ws)
;;

let%expect_test "parsing" =
  let test str =
    match parse (String.split ~on:' ' str) with
    | Error err -> printf !"Error: %{Error#hum}\n" err
    | Ok (res, leftover) ->
      print_endline (to_string_hum res);
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

let parse_exn ws =
  let t, leftover = parse ws |> Or_error.ok_exn in
  if not (List.is_empty leftover)
  then raise_s [%sexp "Leftover tokens", { ws : string list; leftover : string list }];
  t
;;

let parse_def_exn str =
  match String.split ~on:' ' str with
  | name :: "=" :: expansion -> name, parse_exn expansion
  | _ -> failwithf "Could not parse def from: '%s'" str ()
;;

let defs =
  [ "statelessdraw = ap ap c ap ap b b ap ap b ap b ap cons 0 ap ap c ap ap b b cons ap \
     ap c cons nil ap ap c ap ap b cons ap ap c cons nil nil"
  ]
  |> List.map ~f:parse_def_exn
  |> String.Map.of_alist_exn
;;

let reduce t =
  let rec loop = function
    | Ap (Ap (Ap (Name "c", arg1), arg2), arg3) -> Ap (Ap (arg1, arg3), arg2)
    | Ap (Ap (Ap (Name "b", arg1), arg2), arg3) -> Ap (arg1, Ap (arg2, arg3))
    | Ap (arg1, arg2) -> Ap (loop arg1, loop arg2)
    | t -> t
  in
  let rec outer_loop t =
    let t' = loop t in
    if equal t t' then t else outer_loop t'
  in
  outer_loop t
;;

let rec eval t ~defs =
  printf "Eval: %s\n" (to_string_hum t);
  match t with
  | Ap (arg1, arg2) -> reduce (Ap (eval arg1 ~defs, eval arg2 ~defs))
  | Name name ->
    (match Map.find defs name with
    | None -> Name name
    | Some expansion -> reduce expansion)
;;

let%expect_test "eval" =
  print_endline "Defs:";
  List.iter (Map.to_alist defs) ~f:(fun (name, expansion) ->
      printf "%s := %s\n" name (to_string_hum expansion));
  let test str =
    let res = eval (parse_exn (String.split str ~on:' ')) ~defs in
    printf "Result: %s\n" (to_string_hum res)
  in
  test "ap ap statelessdraw x0 x1";
  [%expect
    {|
    Defs:
    statelessdraw := ap (ap "c" (ap (ap "b" "b") (ap (ap "b" (ap "b" (ap "cons" "0"))) (ap (ap "c" (ap (ap "b" "b") "cons")) (ap (ap "c" "cons") "nil"))))) (ap (ap "c" (ap (ap "b" "cons") (ap (ap "c" "cons") "nil"))) "nil")
    Eval: ap (ap "statelessdraw" "x0") "x1"
    Eval: "x1"
    Eval: ap "statelessdraw" "x0"
    Eval: "x0"
    Eval: "statelessdraw"
    Result: ap (ap "cons" "0") (ap (ap "cons" "x0") (ap (ap "cons" (ap (ap "cons" (ap (ap "cons" "x1") "nil")) "nil")) "nil")) |}]
;;
