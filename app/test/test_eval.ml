open! Core

type t =
  | Ap of t * t
  | Name of string
[@@deriving sexp]

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
    ap (ap "add" (ap (ap "add" "2") "3")) (ap (ap "add" "4") "5") |}]
;;

let parse_def_exn str =
  match String.split ~on:' ' str with
  | name :: "=" :: expansion ->
    let expansion, leftover = parse expansion |> Or_error.ok_exn in
    if not (List.is_empty leftover)
    then
      raise_s
        [%sexp "Leftover tokens in definition", { str : string; leftover : string list }];
    name, expansion
  | _ -> failwithf "Could not parse def from: '%s'" str ()
;;

let defs =
  [ "statelessdraw = ap ap c ap ap b b ap ap b ap b ap cons 0 ap ap c ap ap b b cons ap \
     ap c cons nil ap ap c ap ap b cons ap ap c cons nil nil"
  ]
  |> List.map ~f:parse_def_exn
  |> String.Map.of_alist_exn
;;

(* let rec eval words ~defs =
 *   printf "Eval: %s\n" (String.concat ~sep:" " words);
 *   match words with
 *   | "ap" :: f :: args ->
 *     let evaluated_args = eval args ~defs in
 *     (match Map.find defs f with
 *      | None -> f :: evaluated_args
 *      | Some def -> eval (def @ evaluated_args) ~defs)
 *   | f :: args ->
 *     (match Map.find defs f with
 *      | None -> f :: args
 *      | Some def -> def @ args)
 *   | _ -> words
 * ;; *)

let%expect_test "eval" =
  print_endline "Defs:";
  List.iter (Map.to_alist defs) ~f:(fun (name, expansion) ->
      printf "%s := %s\n" name (to_string_hum expansion));
  [%expect
    {|
    Defs:
    statelessdraw := ap (ap "c" (ap (ap "b" "b") (ap (ap "b" (ap "b" (ap "cons" "0"))) (ap (ap "c" (ap (ap "b" "b") "cons")) (ap (ap "c" "cons") "nil"))))) (ap (ap "c" (ap (ap "b" "cons") (ap (ap "c" "cons") "nil"))) "nil") |}]
;;
