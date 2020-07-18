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

let load_defs_exn ~filename =
  let contents = In_channel.read_all filename in
  let defs = List.map (String.split_lines contents) ~f:parse_def_exn in
  String.Map.of_alist_exn defs
;;

let base_defs =
  String.Map.empty
  (* C x y z = x z y *)
  |> Map.set ~key:"c" ~data:(Lambda.Parse.parse "(/x./y./z.x z y)")
  (* B x y z = x (y z)) *)
  |> Map.set ~key:"b" ~data:(Lambda.Parse.parse "(/x./y./z.x (y z))")
  (*cons: λh.λt.(λs.s h t) *)
  |> Map.set ~key:"cons" ~data:(Lambda.Parse.parse "(/x./y.(/m.m x y))")
  |> Map.set ~key:"car" ~data:(Lambda.Parse.parse "(/z.(z (/p./q.p)))")
  |> Map.set ~key:"cdr" ~data:(Lambda.Parse.parse "(/z.(z (/p./q.q)))")
  |> Map.set ~key:"nil" ~data:(Lambda.Parse.parse "(/z.(/p./q.p))")
  (* Sxyz = xz(yz) *)
  |> Map.set ~key:"s" ~data:(Lambda.Parse.parse "(/x./y./z.x z (y z))")
  |> Map.set ~key:"i" ~data:(Lambda.Parse.parse "(/x.x)")
  |> Map.set ~key:"t" ~data:Lambda.Bool.ltrue
  |> Map.set ~key:"f" ~data:Lambda.Bool.lfalse
  (* for tests only *)
  |> Map.set ~key:"f2048" ~data:(Lambda.Parse.parse "(f f2048)")
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
    | App (App (App (Var "c", arg1), arg2), arg3) -> App (App (arg1, arg3), arg2)
    | App (App (App (Var "b", arg1), arg2), arg3) -> App (arg1, App (arg2, arg3))
    | App (App (App (Var "s", arg1), arg2), arg3) ->
      App (App (arg1, arg3), App (arg2, arg3))
    | App (Var "i", arg1) -> arg1
    | App (Var "car", App (App (Var "cons", arg1), _)) -> arg1
    | App (Var "cdr", App (App (Var "cons", _), arg2)) -> arg2
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
    | App (Var "isnil", Var "nil") -> Var "t"
    | App (Var "isnil", x) ->
      if Lambda.Bool.is_bool x && Lambda.Bool.to_bool x
      then Var "t"
      else (
        match x with
        | Abs (_, x) ->
          if Lambda.Bool.is_bool x && Lambda.Bool.to_bool x then Var "t" else Var "f"
        | _ -> Var "f")
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

let subst t ~verbose ~defs =
  let free_vars = Lambda.L.fv_l t in
  if verbose then printf !"Free vars: %{sexp: string list}\n" free_vars;
  List.fold free_vars ~init:t ~f:(fun acc free_var ->
      match Map.find defs free_var with
      | None -> acc
      | Some definition ->
        if verbose then printf !"Substituting %s => %{sexp: t}\n" free_var definition;
        Lambda.L.subst free_var definition acc)
;;

let rec eval t ~verbose ~defs =
  if verbose then printf "Eval: %s\n" (to_string_hum t);
  let t' = subst ~verbose ~defs t in
  let t' = Lambda.L.reduce_fix t' in
  if Lambda.L.len t' > Lambda.L.len t then eval t' ~verbose ~defs else reduce t'
;;
