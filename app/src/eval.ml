open! Core
module Id = Unique_id.Int ()

module Big_int = struct
  include Big_int_Z

  type t = big_int

  let zero = zero_big_int
  let one = big_int_of_int 1
  let minus_one = big_int_of_int (-1)
  let is_zero t = eq_big_int zero t
  let is_one t = eq_big_int one t
  let is_minus_one t = eq_big_int minus_one t
  let sexp_of_t t = Sexp.of_string (string_of_big_int t)
  let t_of_sexp s = big_int_of_string (Sexp.to_string s)
end

type t =
  | Nil
  | True
  | False
  | Var of string
  | Num of Big_int.t
  | App of t * t
[@@deriving sexp]

let rec equal t1 t2 =
  match t1, t2 with
  | True, True -> true
  | False, False -> true
  | Nil, Nil -> true
  | Num n1, Num n2 -> Big_int.eq_big_int n1 n2
  | Var n1, Var n2 -> String.equal n1 n2
  | App (t11, t12), App (t21, t22) -> equal t11 t21 && equal t12 t22
  | _, _ -> false
;;

let rec length = function
  | Nil -> 1
  | True -> 1
  | False -> 1
  | Num _ -> 1
  | Var _ -> 1
  | App (t1, t2) -> 1 + length t1 + length t2
;;

let car = function
  | App (App (Var "cons", h), _) -> h
  | x -> failwithf !"Unexpected %{sexp:t} in car" x ()
;;

let cdr = function
  | App (App (Var "cons", _), t) -> t
  | x -> failwithf !"Unexpected %{sexp:t} in cdr" x ()
;;

let rec to_string_hum = function
  | Nil -> {|"nil"|}
  | True -> {|"t"|}
  | False -> {|"f"|}
  | Num x -> sprintf {|%s|} (Big_int.string_of_big_int x)
  | Var name -> sprintf {|"%s"|} name
  | App (x, y) ->
    let str_of_arg arg =
      match arg with
      | Var name -> sprintf {|"%s"|} name
      | Nil -> {|"nil"|}
      | True -> {|"t"|}
      | False -> {|"f"|}
      | Num x -> sprintf {|%s|} (Big_int.string_of_big_int x)
      | _ -> sprintf "(%s)" (to_string_hum arg)
    in
    sprintf "ap %s %s" (str_of_arg x) (str_of_arg y)
;;

(* Decode multidraw vector: list of lists of coordinate pairs *)
let decode_vector t : (Big_int.t * Big_int.t) list list =
  let decode_pair t =
    match t with
    | App (App (Var "cons", Num x), Num y) -> x, y
    | x -> failwithf !"pair: %{sexp:t}" x ()
  in
  let _ = decode_pair in
  let rec decode_list t =
    match t with
    | Nil -> []
    | x ->
      let pair = decode_pair (car x) in
      let rest = decode_list (cdr x) in
      pair :: rest
  in
  let rec loop t =
    match t with
    | Nil -> []
    | x ->
      let lst = decode_list (car x) in
      let rest = loop (cdr x) in
      lst :: rest
  in
  loop t
;;

let is_int str =
  try
    let (_ : Big_int.t) = Big_int.big_int_of_string str in
    true
  with
  | _ -> false
;;

let parse ws =
  let rec loop = function
    | "ap" :: rest ->
      let arg1, leftover = loop rest in
      let arg2, leftover = loop leftover in
      App (arg1, arg2), leftover
    | "nil" :: rest -> Nil, rest
    | "t" :: rest -> True, rest
    | "f" :: rest -> False, rest
    | name :: rest ->
      if is_int name then Num (Big_int.big_int_of_string name), rest else Var name, rest
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

let base_defs = String.Map.empty

let reduce_maximally ~defs t =
  let rec step t =
    (* printf !"step: %{sexp:t}\n" t; *)
    match t with
    | True | False | Nil -> t
    | Num _ -> t
    | Var x ->
      (match Map.find defs x with
      | None -> t
      | Some expansion ->
        (* if verbose then printf !"Substituting %s\n" name; *)
        (* if verbose then printf !"Substituting %s => %{sexp: t}\n%!" name expansion; *)
        step expansion)
    | App (App (False, _), y) -> step y
    | App (App (True, x), _) -> step x
    | App (Var "i", arg1) -> step arg1
    (* car/cdr *)
    | App (Var "car", App (App (Var "cons", arg1), _)) -> step arg1
    | App (Var "cdr", App (App (Var "cons", _), arg2)) -> step arg2
    | App (Var "car", x) ->
      (match step x with
      | Nil -> True (* TODO: ? *)
      | App (App (Var "cons", arg1), _) -> step arg1
      | x -> App (Var "car", x))
    | App (Var "cdr", x) ->
      (match step x with
      | Nil -> True (* TODO: ? *)
      | App (App (Var "cons", _), arg1) -> step arg1
      | x -> App (Var "cdr", x))
    (* cons *)
    | App (App (App (Var "cons", x), y), z) -> step (App (App (z, x), y))
    (* inc (dec) and dec (inc) *)
    | App (Var "inc", App (Var "dec", x)) -> step x
    | App (Var "dec", App (Var "inc", x)) -> step x
    (* inc (add) and dec (add) *)
    | App (Var "dec", App (App (Var "add", Num x), y)) ->
      step (App (App (Var "add", Num (Big_int.add_int_big_int (-1) x)), y))
    | App (Var "dec", App (App (Var "add", y), Num x)) ->
      step (App (App (Var "add", y), Num (Big_int.add_int_big_int (-1) x)))
    | App (Var "inc", App (App (Var "add", Num x), y)) ->
      step (App (App (Var "add", Num (Big_int.add_int_big_int 1 x)), y))
    | App (Var "inc", App (App (Var "add", y), Num x)) ->
      step (App (App (Var "add", y), Num (Big_int.add_int_big_int 1 x)))
    (* inc and dec *)
    | App (Var "inc", x) ->
      (match step x with
      | Num x -> Num (Big_int.add_int_big_int 1 x)
      | x -> App (Var "inc", x))
    | App (Var "dec", x) ->
      (match step x with
      | Num x -> Num (Big_int.add_int_big_int (-1) x)
      | x -> App (Var "dec", x))
    (* add 0 *)
    | App (App (Var "add", x), Num d) when Big_int.is_zero d -> step x
    | App (Var "add", Num d) when Big_int.is_zero d -> Var "i"
    | App (Var "add", Num d) when Big_int.is_minus_one d -> Var "dec"
    | App (Var "add", Num d) when Big_int.is_one d -> Var "inc"
    | App (App (Var "add", x), y) ->
      (match step x, step y with
      | Num x, Num y -> Num (Big_int.add_big_int x y)
      | x, y -> App (step (App (Var "add", x)), y))
    (* arithmetics *)
    | App (App (Var "div", x), Num d) when Big_int.is_one d -> step x
    | App (App (Var "div", x), y) ->
      (match step x, step y with
      | Num x, Num y -> Num (Big_int.div_big_int x y)
      | x, y -> App (step (App (Var "div", x)), y))
    | App (App (Var "mul", x), Num d) when Big_int.is_one d -> step x
    | App (App (Var "mul", _), Num d) when Big_int.is_zero d -> Num Big_int.zero
    | App (App (Var "mul", Num d), x) when Big_int.is_one d -> step x
    | App (App (Var "mul", Num d), _) when Big_int.is_zero d -> Num Big_int.zero
    | App (App (Var "mul", x), y) ->
      (match step x, step y with
      | Num x, Num y -> Num (Big_int.mult_big_int x y)
      | x, y -> App (step (App (Var "mul", x)), y))
    | App (Var "neg", x) ->
      (match step x with
      | Num x -> Num (Big_int.minus_big_int x)
      | x -> App (Var "neg", x))
    (* eq *)
    | App (App (Var "eq", x), y) ->
      (match step x, step y with
      | Num x, Num y -> if Big_int.eq_big_int x y then True else False
      | Var x, Var y -> if String.equal x y then True else False
      | x, y -> App (step (App (Var "eq", x)), y))
    (*lt*)
    | App (App (Var "lt", x), y) ->
      (match step x, step y with
      | Num x, Num y -> if Big_int.lt_big_int x y then True else False
      | x, y -> App (step (App (Var "lt", x)), y))
    | App (Var "isnil", x) ->
      (match step x with
      | Nil -> True
      | App (App (Var "cons", _), _) -> False
      | x -> App (Var "isnil", x))
    | App (App (App (Var "if0", x), then_branch), else_branch) ->
      (match step x with
      | Num x -> if Big_int.is_zero x then step then_branch else step else_branch
      | x -> App (App (App (Var "if0", x), then_branch), else_branch))
    (* B *)
    | App (App (App (Var "b", x), y), z) -> step (App (x, App (y, z)))
    (* C *)
    | App (App (App (Var "c", x), y), z) -> step (App (App (x, z), y))
    (* S *)
    | App (App (App (Var "s", x), y), z) ->
      let z = step z in
      step (App (App (x, z), App (y, z)))
    (* Descent left *)
    | App ((Var _ as arg1), arg2) -> App (step arg1, arg2)
    | App ((App _ as arg1), arg2) -> App (step arg1, arg2)
    | App ((False | True | Nil), _) -> t
    | t -> failwithf !"end of line with %{sexp:t}" t ()
    (* | App (arg1, arg2) -> App (step arg1, arg2) *)
  in
  try step t with
  | exn ->
    eprint_s [%sexp "Exception while reducing", { t : t; exn : Exn.t }];
    raise exn
;;

let eval_custom ~verbose ~defs =
  Staged.stage (fun t ->
      if verbose then printf "Eval (length: %d): %s\n%!" (length t) (to_string_hum t);
      let rec reduce_fix t =
        let t' = reduce_maximally ~defs t in
        if equal t' t then t else reduce_fix t'
      in
      let rec force_list e =
        match reduce_fix e with
        | App (App (Var "cons", e1), e2) ->
          App (App (Var "cons", force_list e1), force_list e2)
        | e -> e
      in
      let rec loop t =
        if verbose then printf "Eval_custom loop (length = %d)\n%!" (length t);
        let t' = force_list t in
        if verbose then printf !"Reduced:\n%{sexp: t}\n%!" t';
        if verbose then printf !"Reduced hum:\n%s\n%!" (to_string_hum t');
        if equal t' t
        then t
        else
          (* printf "===========\n%!";
           * let (_ : string) = In_channel.input_line_exn In_channel.stdin in *)
          loop t'
      in
      loop t)
;;
