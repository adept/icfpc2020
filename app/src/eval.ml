open! Core

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
  { mutable evaluated : (t[@sexp.opaque]) option
  ; u : u
  }

and u =
  | Var of string
  | App of t * t
[@@deriving sexp_of]

let var str = { u = Var str; evaluated = None }
let app x y = { u = App (x, y); evaluated = None }

let rec equal t1 t2 =
  match t1.u, t2.u with
  | Var n1, Var n2 -> String.equal n1 n2
  | App (t11, t12), App (t21, t22) -> equal t11 t21 && equal t12 t22
  | _, _ -> false
;;

let rec length { u; _ } =
  match u with
  | Var _ -> 1
  | App (t1, t2) -> 1 + length t1 + length t2
;;

let car { u; _ } =
  match u with
  | App ({ u = App ({ u = Var "cons"; _ }, h); _ }, _) -> h
  | x -> failwithf !"Unexpected %{sexp: u} in car" x ()
;;

let cdr { u; _ } =
  match u with
  | App ({ u = App ({ u = Var "cons"; _ }, _); _ }, t) -> t
  | x -> failwithf !"Unexpected %{sexp: u} in cd" x ()
;;

(* Decode multidraw vector: list of lists of coordinate pairs *)
let decode_vector t : (int * int) list list =
  let decode_pair t =
    match t with
    | App (App (Var "cons", Num x), Num y) ->
      Big_int_Z.int_of_big_int x, Big_int_Z.int_of_big_int y
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
      app arg1 arg2, leftover
    | name :: rest -> var name, rest
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

let to_int_exn t =
  match t.u with
  | Var str -> Int.of_string str
  | _ -> raise_s [%sexp "Not a number", (t : t)]
;;

let rec eval_custom t ~verbose ~defs =
  match t.evaluated with
  | Some x -> x
  | None ->
    let t0 = t in
    let rec loop t =
      if verbose then printf "(length = %d) Eval_custom loop\n%!" (length t);
      let res = eval_once t ~verbose ~defs in
      if verbose then printf !"Evaluated once:\n%{sexp: t}\n%!" res;
      if equal t res
      then (
        t0.evaluated <- Some res;
        res)
      else loop res
    in
    loop t

and eval_once t ~verbose ~defs =
  let eval = eval_custom ~verbose ~defs in
  match t.evaluated with
  | Some x -> x
  | None ->
    (match t.u with
    | Var name ->
      (match Map.find defs name with
      | Some expansion -> expansion
      | None -> t)
    | App (f, x) ->
      let f = eval f in
      (match f.u with
      | Var "neg" -> var (Int.to_string (~-1 * to_int_exn (eval x)))
      | Var "i" -> x
      | Var "nil" -> var "t"
      | Var "isnil" -> app x (app (var "t") (app (var "t") (var "f")))
      | Var "car" -> app x (var "t")
      | Var "cdr" -> app x (var "f")
      | App (f2, y) ->
        let f2 = eval f2 in
        (match f2.u with
        | Var "t" -> y
        | Var "f" -> x
        | Var "add" -> var (Int.to_string (to_int_exn (eval x) + to_int_exn (eval y)))
        | Var "mul" -> var (Int.to_string (to_int_exn (eval x) * to_int_exn (eval y)))
        | Var "div" -> var (Int.to_string (to_int_exn (eval y) / to_int_exn (eval x)))
        | Var "lt" -> var (if to_int_exn (eval x) < to_int_exn (eval y) then "t" else "f")
        | Var "eq" -> var (if to_int_exn (eval x) = to_int_exn (eval y) then "t" else "f")
        | Var "cons" -> eval_cons y x ~verbose ~defs
        | App (f3, z) ->
          let f3 = eval f3 in
          (match f3.u with
          | Var "s" -> app (app z x) (app y x)
          | Var "c" -> app (app z x) y
          | Var "b" -> app z (app y x)
          | Var "cons" -> app (app x z) y
          | _ -> t)
        | _ -> t)
      | _ -> t))

and eval_cons a b ~verbose ~defs =
  let eval = eval_custom ~verbose ~defs in
  let res = app (app (var "cons") (eval a)) (eval b) in
  res.evaluated <- Some res;
  res
;;
