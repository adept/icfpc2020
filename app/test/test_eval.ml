open! Core

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
  |> Map.set ~key:"cons" ~data:(Lambda.Parse.parse "(λh.λt.(λs.s h t))")
  (* Sxyz = xz(yz) *)
  |> Map.set ~key:"s" ~data:(Lambda.Parse.parse "(/x./y./z.x z (y z))")
;;

(*
let is_int str =
  try
    let (_ : int) = Int.of_string str in
    true
  with
  | _ -> false
;;

let reduce t =
  let rec loop = function
    | App (Ap (Ap (Name "c", arg1), arg2), arg3) -> Ap (Ap (arg1, arg3), arg2)
    | Ap (Ap (Ap (Name "b", arg1), arg2), arg3) -> Ap (arg1, Ap (arg2, arg3))
    | Ap (Ap (Ap (Name "s", arg1), arg2), arg3) -> Ap (Ap (arg1, arg3), Ap (arg2, arg3))
    | Ap (Name "i", arg1) -> arg1
    | Ap (Name "inc", Name x) when is_int x -> Name (Int.to_string (Int.of_string x + 1))
    | Ap (Name "dec", Name x) when is_int x -> Name (Int.to_string (Int.of_string x - 1))
    | Ap (Name "inc", Ap (Name "dec", Name x)) -> Name x
    | Ap (Name "dec", Ap (Name "inc", Name x)) -> Name x
    | Ap (Ap (Name "add", Name x), Name y) when is_int x && is_int y ->
      Name (Int.to_string (Int.of_string x + Int.of_string y))
    | Ap (Name "car", Ap (Ap (Name "cons", arg1), _)) -> arg1
    | Ap (Name "cdr", Ap (Ap (Name "cons", _), arg2)) -> arg2
    | Ap (Ap (Name "div", Name x), Name y) when is_int x && is_int y ->
      Name (Int.to_string (Int.of_string x / Int.of_string y))
    | Ap (Ap (Name "div", Name x), Name "1") -> Name x
    | Ap (Ap (Name "mul", Name x), Name y) when is_int x && is_int y ->
      Name (Int.to_string (Int.of_string x * Int.of_string y))
    | Ap (Ap (Name "mul", Name x), Name "1") -> Name x
    | Ap (Ap (Name "mul", Name _), Name "0") -> Name "0"
    | Ap (Name "neg", Name x) when is_int x ->
      Name (Int.to_string (Int.neg (Int.of_string x)))
    | Ap (Ap (Name "eq", Name arg1), Name arg2) ->
      if String.equal arg1 arg2 then Name "t" else Name "f"
    | Ap (Ap (Name "lt", Name x), Name y) when is_int x && is_int y ->
      if Int.( < ) (Int.of_string x) (Int.of_string y) then Name "t" else Name "f"
    | Ap (Name "isnil", Name "nil") -> Name "t"
    | Ap (Name "isnil", _) -> Name "f"
    | Ap (arg1, arg2) -> Ap (loop arg1, loop arg2)
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
 *)

let rec subst_fix t ~defs =
  let free_vars = Lambda.L.fv_l t in
  printf !"Free vars: %{sexp: string list}\n" free_vars;
  let t' =
    List.fold free_vars ~init:t ~f:(fun acc free_var ->
        match Map.find defs free_var with
        | None -> acc
        | Some definition ->
          printf !"Substituting %s => %{sexp: t}\n" free_var definition;
          Lambda.L.subst free_var definition acc)
  in
  if Lambda.L.len t' > Lambda.L.len t then subst_fix t' ~defs else t'
;;

let eval t ~defs =
  printf "Eval: %s\n" (to_string_hum t);
  let t = subst_fix ~defs t in
  Lambda.L.reduce_fix t
;;

(*
let rec eval t ~verbose ~defs =
  if verbose then printf "Eval: %s\n" (to_string_hum t);
  match t with
  | Ap (arg1, arg2) -> reduce (Ap (eval arg1 ~verbose ~defs, eval arg2 ~verbose ~defs))
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
  test "ap car ap ap cons x0 x1";
  test "ap cdr ap ap cons x0 x1";
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
    Starting evaluation: ap car ap ap cons x0 x1
    Result: "x0"
    Starting evaluation: ap cdr ap ap cons x0 x1
    Result: "x1"
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
  [%expect {||}];
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
