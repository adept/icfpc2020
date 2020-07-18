open! Core
module Lambda = Lambda_vendor

type t = Lambda.L.term =
  | Var of string
  | Abs of (string * t)
  | App of t * t
[@@deriving equal, sexp]

let rec to_string_hum = function
  | Var name -> sprintf {|"%s"|} name
  | App (x, y) ->
    let str_of_arg arg =
      match arg with
      | Var name -> sprintf {|"%s"|} name
      | _ -> sprintf "(%s)" (to_string_hum arg)
    in
    sprintf "ap %s %s" (str_of_arg x) (str_of_arg y)
  | Abs (x, term) -> sprintf "\\%s -> %s" x (to_string_hum term)
;;

let parse ws =
  let rec loop = function
    | "ap" :: rest ->
      let arg1, leftover = loop rest in
      let arg2, leftover = loop leftover in
      App (arg1, arg2), leftover
    | name :: rest -> Var name, rest
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
  (* C x y z = x z y *)
  |> Map.set ~key:"c" ~data:(Lambda.Parse.parse "(/x./y./z.x z y)")
  (* B x y z = x (y z)) *)
  |> Map.set ~key:"b" ~data:(Lambda.Parse.parse "(/x./y./z.x (y z))")
  (*cons: λh.λt.(λs.s h t) *)
  |> Map.set ~key:"cons" ~data:(Lambda.Parse.parse "(/h./t.(/m.m h t))")
  |> Map.set ~key:"car" ~data:(Lambda.Parse.parse "(/z.(z (/p./q.p)))")
  |> Map.set ~key:"cdr" ~data:(Lambda.Parse.parse "(/z.(z (/p./q.q)))")
  |> Map.set ~key:"nil" ~data:(Lambda.Parse.parse "(/z.t)")
  (* Sxyz = xz(yz) *)
  |> Map.set ~key:"s" ~data:(Lambda.Parse.parse "(/x./y./z.x z (y z))")
  |> Map.set ~key:"i" ~data:(Lambda.Parse.parse "(/x.x)")
  |> Map.set ~key:"t" ~data:Lambda.Bool.ltrue
  |> Map.set ~key:"f" ~data:Lambda.Bool.lfalse
;;

let is_int str =
  try
    let (_ : int) = Int.of_string str in
    true
  with
  | _ -> false
;;

let reduce t =
  let rec loop = function
    | App (Var "inc", Var x) when is_int x -> Var (Int.to_string (Int.of_string x + 1))
    | App (Var "dec", Var x) when is_int x -> Var (Int.to_string (Int.of_string x - 1))
    | App (Var "inc", App (Var "dec", Var x)) -> Var x
    | App (Var "dec", App (Var "inc", Var x)) -> Var x
    | App (App (Var "add", Var x), Var y) when is_int x && is_int y ->
      Var (Int.to_string (Int.of_string x + Int.of_string y))
    | App (App (Var "div", Var x), Var y) when is_int x && is_int y ->
      Var (Int.to_string (Int.of_string x / Int.of_string y))
    | App (App (Var "div", Var x), Var "1") -> Var x
    | App (App (Var "mul", Var x), Var y) when is_int x && is_int y ->
      Var (Int.to_string (Int.of_string x * Int.of_string y))
    | App (App (Var "mul", Var x), Var "1") -> Var x
    | App (App (Var "mul", Var _), Var "0") -> Var "0"
    | App (Var "neg", Var x) when is_int x ->
      Var (Int.to_string (Int.neg (Int.of_string x)))
    | App (App (Var "eq", Var arg1), Var arg2) ->
      if String.equal arg1 arg2 then Var "t" else Var "f"
    | App (App (Var "lt", Var x), Var y) when is_int x && is_int y ->
      if Int.( < ) (Int.of_string x) (Int.of_string y) then Var "t" else Var "f"
    | App (Var "isnil", Abs (_, x)) ->
      if Lambda.Bool.is_bool x && Lambda.Bool.to_bool x then Var "t" else Var "f"
    | App (Var "isnil", _) -> Var "f"
    | App (arg1, arg2) -> App (loop arg1, loop arg2)
    | t -> t
  in
  let rec outer_loop t =
    let t' = loop t in
    if equal t t' then t else outer_loop t'
  in
  try outer_loop t with
  | exn ->
    eprint_s [%sexp "Exception while reducing", { t : t; exn : Exn.t }];
    raise exn
;;

let rec subst_fix t ~verbose ~defs =
  let free_vars = Lambda.L.fv_l t in
  if verbose then printf !"Free vars: %{sexp: string list}\n" free_vars;
  let t' =
    List.fold free_vars ~init:t ~f:(fun acc free_var ->
        match Map.find defs free_var with
        | None -> acc
        | Some definition ->
          if verbose then printf !"Substituting %s => %{sexp: t}\n" free_var definition;
          Lambda.L.subst free_var definition acc)
  in
  if Lambda.L.len t' > Lambda.L.len t then subst_fix t' ~verbose ~defs else t'
;;

let eval t ~verbose ~defs =
  if verbose then printf "Eval: %s\n" (to_string_hum t);
  let t = subst_fix ~verbose ~defs t in
  reduce (Lambda.L.reduce_fix t)
;;

(*
let rec eval t ~verbose ~defs =
  if verbose then printf "Eval: %s\n" (to_string_hum t);
  match t with
  | App (arg1, arg2) -> reduce (App (eval arg1 ~verbose ~defs, eval arg2 ~verbose ~defs))
  | Name name ->
    (match Map.find defs name with
    | None -> Name name
    | Some expansion -> reduce expansion)
 *)

let%expect_test "base combinators" =
  let test str =
    printf "Starting evaluation: %s\n" str;
    let res = eval ~verbose:false (parse_exn (String.split str ~on:' ')) ~defs in
    printf "Result: %s\n" (to_string_hum res)
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
    Result: \m -> ap (ap "m" "x1") "x2"
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
    Result: "f"
    Starting evaluation: ap isnil ap car ap ap cons nil x0
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
      printf "%s := %s\n" name (to_string_hum expansion));
  let test str =
    let res = eval ~verbose:true (parse_exn (String.split str ~on:' ')) ~defs in
    printf "Result: %s\n" (to_string_hum res)
  in
  test "ap ap ap c x y ap ap add 1 2";
  [%expect
    {|
    Defs:
    b := \x -> \y -> \z -> ap "x" (ap "y" "z")
    c := \x -> \y -> \z -> ap (ap "x" "z") "y"
    car := \z -> ap "z" (\p -> \q -> "p")
    cdr := \z -> ap "z" (\p -> \q -> "q")
    cons := \h -> \t -> \m -> ap (ap "m" "h") "t"
    f := \x -> \y -> "y"
    i := \x -> "x"
    nil := \z -> "t"
    s := \x -> \y -> \z -> ap (ap "x" "z") (ap "y" "z")
    statelessdraw := ap (ap "c" (ap (ap "b" "b") (ap (ap "b" (ap "b" (ap "cons" "0"))) (ap (ap "c" (ap (ap "b" "b") "cons")) (ap (ap "c" "cons") "nil"))))) (ap (ap "c" (ap (ap "b" "cons") (ap (ap "c" "cons") "nil"))) "nil")
    t := \x -> \y -> "x"
    Eval: ap (ap (ap "c" "x") "y") (ap (ap "add" "1") "2")
    Free vars: (c x y add 1 2)
    Substituting c => (Abs (x (Abs (y (Abs (z (App (App (Var x) (Var z)) (Var y))))))))
    Free vars: (x y add 1 2)
    Result: ap (ap "x" "3") "y" |}];
  test "ap ap statelessdraw x0 x1";
  [%expect
    {|
    Eval: ap (ap "statelessdraw" "x0") "x1"
    Free vars: (statelessdraw x0 x1)
    Substituting statelessdraw => (App
     (App (Var c)
      (App (App (Var b) (Var b))
       (App (App (Var b) (App (Var b) (App (Var cons) (Var 0))))
        (App (App (Var c) (App (App (Var b) (Var b)) (Var cons)))
         (App (App (Var c) (Var cons)) (Var nil))))))
     (App
      (App (Var c)
       (App (App (Var b) (Var cons)) (App (App (Var c) (Var cons)) (Var nil))))
      (Var nil)))
    Free vars: (0 b c cons nil x0 x1)
    Substituting b => (Abs (x (Abs (y (Abs (z (App (Var x) (App (Var y) (Var z)))))))))
    Substituting c => (Abs (x (Abs (y (Abs (z (App (App (Var x) (Var z)) (Var y))))))))
    Substituting cons => (Abs (h (Abs (t (Abs (m (App (App (Var m) (Var h)) (Var t))))))))
    Substituting nil => (Abs (z (Var t)))
    Free vars: (0 t x0 x1)
    Substituting t => (Abs (x (Abs (y (Var x)))))
    Free vars: (0 x0 x1)
    Result: \m -> ap (ap "m" "0") (\x -> \y -> "x") |}]
;;
