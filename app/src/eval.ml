open! Core
module Id = Unique_id.Int ()

module T = struct
  module U = struct
    type t =
      | Var of string
      | Num of int
      | Arg1 of string * t
      | Arg2 of string * t * t
      | App of t * t
    [@@deriving compare, equal, hash, sexp]
  end

  include U
  include Comparable.Make (U)
  include Hashable.Make (U)
end

include T.U

let rec length = function
  | Num _ -> 1
  | Var _ -> 1
  | App (t1, t2) -> 1 + length t1 + length t2
  | Arg1 (_, t1) -> 1 + length t1
  | Arg2 (_, t1, t2) -> 1 + length t1 + length t2
;;

let car = function
  | App (App (Var "cons", h), _) -> h
  | x -> failwithf !"Unexpected %{sexp:t} in car" x ()
;;

let cdr = function
  | App (App (Var "cons", _), t) -> t
  | x -> failwithf !"Unexpected %{sexp:t} in cd" x ()
;;

(* let rec has_colon = function
 *   | Var n -> String.is_prefix ~prefix:":" n
 *   | App (t1, t2) -> has_colon t1 || has_colon t2
 *   | Arg1 (n, t1) -> String.is_prefix ~prefix:":" n || has_colon t1
 *   | Arg2 (n, t1, t2) -> String.is_prefix ~prefix:":" n || has_colon t1 || has_colon t2
 * ;; *)

let rec to_string_hum = function
  | Num x -> sprintf {|%d|} x
  | Var name -> sprintf {|"%s"|} name
  | Arg1 (n, x) -> sprintf {|[%s %s]|} n (to_string_hum x)
  | Arg2 (n, x, y) -> sprintf {|[%s %s %s]|} n (to_string_hum x) (to_string_hum y)
  | App (x, y) ->
    let str_of_arg arg =
      match arg with
      | Var name -> sprintf {|"%s"|} name
      | _ -> sprintf "(%s)" (to_string_hum arg)
    in
    sprintf "ap %s %s" (str_of_arg x) (str_of_arg y)
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

let base_defs = String.Map.empty

let is_int str =
  try
    let (_ : int) = Int.of_string str in
    true
  with
  | _ -> false
;;

let reduce_maximally t =
  let rec step = function
    | Var x when is_int x -> Num (Int.of_string x)
    | App (App (Var "f", _), y) -> y
    | App (App (Var "t", x), _) -> x
    | App (Var "i", arg1) -> arg1
    (* car/cdr *)
    | App (Var "car", App (App (Var "cons", arg1), _)) -> arg1
    | App (Var "cdr", App (App (Var "cons", _), arg2)) -> arg2
    | App (Var "car", x) -> App (x, Var "t")
    | App (Var "cdr", x) -> App (x, Var "f")
    (* cons *)
    | App (App (App (Var "cons", x), y), z) -> App (App (z, x), y)
    (* inc and dec *)
    | App (Var "inc", Num x) -> Num (x + 1)
    | App (Var "dec", Num x) -> Num (x - 1)
    (* inc (dec) and dec (inc) *)
    | App (Var "inc", App (Var "dec", Var x)) -> Var x
    | App (Var "dec", App (Var "inc", Var x)) -> Var x
    (* inc (add) and dec (add) *)
    | App (Var "dec", App (App (Var "add", Num x), y)) ->
      App (App (Var "add", Num (x - 1)), y)
    | App (Var "dec", App (App (Var "add", y), Num x)) ->
      App (App (Var "add", y), Num (x - 1))
    | App (Var "inc", App (App (Var "add", Num x), y)) ->
      App (App (Var "add", Num (x + 1)), y)
    | App (Var "inc", App (App (Var "add", y), Num x)) ->
      App (App (Var "add", y), Num (x + 1))
    (* add 0 *)
    | App (App (Var "add", x), Num 0) -> x
    | App (Var "add", Num 0) -> Var "i"
    | App (Var "add", Num -1) -> Var "dec"
    | App (Var "add", Num 1) -> Var "inc"
    (* arithmetics *)
    | App (App (Var "add", Num x), Num y) -> Num (x + y)
    | App (App (Var "div", Num x), Num y) -> Num (x / y)
    | App (App (Var "div", x), Num 1) -> x
    | App (App (Var "mul", Num x), Num y) -> Num (x * y)
    | App (App (Var "mul", x), Num 1) -> x
    | App (App (Var "mul", _), Num 0) -> Num 0
    | App (App (Var "mul", Num 1), x) -> x
    | App (App (Var "mul", Num 0), _) -> Num 0
    | App (Var "neg", Num 0) -> Num 0
    | App (Var "neg", Num x) -> Num (Int.neg x)
    (* eq *)
    | App (App (Var "eq", Num arg1), Num arg2) ->
      if Int.equal arg1 arg2 then Var "t" else Var "f"
    | App (App (Var "eq", Var arg1), Var arg2) ->
      if String.equal arg1 arg2 then Var "t" else Var "f"
    (*lt*)
    | App (App (Var "lt", Num x), Num y) -> if Int.( < ) x y then Var "t" else Var "f"
    | App (Var "isnil", Var "nil") -> Var "t"
    | App (Var "isnil", App (App (Var "cons", _), _)) -> Var "f"
    | App (Var "isnil", Num _) -> Var "f"
    | App (App (App (Var "if0", Num x), then_branch), else_branch) ->
      if Int.equal x 0 then then_branch else else_branch
    (* B *)
    | App (Var "b", x) -> Arg1 ("b", x)
    | App (Arg1 ("b", x), y) -> Arg2 ("b", x, y)
    | App (Arg2 ("b", x, y), z) -> App (x, App (y, z))
    (* C *)
    | App (Var "c", x) -> Arg1 ("c", x)
    | App (Arg1 ("c", x), y) -> Arg2 ("c", x, y)
    | App (Arg2 ("c", x, y), z) -> step (App (App (x, z), y))
    (* S *)
    | App (Var "s", x) -> Arg1 ("s", x)
    | App (Arg1 ("s", x), y) -> Arg2 ("s", x, y)
    | App (Arg2 ("s", x, y), z) ->
      let z = step z in
      step (App (App (x, z), App (y, z)))
    (* Descent *)
    | Arg1 (n, t1) -> Arg1 (n, step t1)
    | Arg2 (n, t1, t2) -> Arg2 (n, step t1, step t2)
    | App (arg1, arg2) ->
      App (step arg1, step arg2)
      (* (match t with
       * | App (App (App (Var "c", arg1), arg2), arg3) -> App (App (arg1, arg3), arg2)
       * | App (App (App (Var "b", arg1), arg2), arg3) -> App (arg1, App (arg2, arg3))
       * (\* | App (App (App (Var "s", arg1), arg2), arg3) ->
       *  *   App (App (arg1, arg3), App (arg2, arg3)) *\)
       * | t -> t) *)
    | t -> t
  in
  try step t with
  | exn ->
    eprint_s [%sexp "Exception while reducing", { t : t; exn : Exn.t }];
    raise exn
;;

let eval_custom ~verbose ~defs =
  Staged.stage (fun t ->
      if verbose then printf "Eval (length: %d): %s\n%!" (length t) (to_string_hum t);
      let rec expand_once t =
        match t with
        | Num _ -> t
        | Arg1 (n, t) -> Arg1 (n, expand_once t)
        | Arg2 (n, t1, t2) ->
          (match expand_once t1 with
          | t1' when not (equal t1 t1') -> Arg2 (n, t1', t2)
          | _ ->
            (match expand_once t2 with
            | t2' when not (equal t2 t2') -> Arg2 (n, t1, t2')
            | _ -> t))
        | Var name ->
          (match String.Map.find defs name with
          | None -> t
          | Some expansion ->
            if verbose then printf !"Substituting %s\n" name;
            (* if verbose then printf !"Substituting %s => %{sexp: t}\n%!" name expansion; *)
            expansion)
        | App (t1, t2) ->
          (match expand_once t1 with
          | t1' when not (equal t1 t1') -> App (t1', t2)
          | _ ->
            (match expand_once t2 with
            | t2' when not (equal t2 t2') -> App (t1, t2')
            | _ -> t))
      in
      let rec reduce_fix t =
        let t' = reduce_maximally t in
        if equal t' t then t else reduce_fix t'
      in
      let seen = T.Hash_set.create () in
      let rec loop t =
        if Hash_set.mem seen t
        then (
          print_endline "LOOP!";
          let (_ : string) = In_channel.input_line_exn In_channel.stdin in
          ());
        Hash_set.add seen t;
        if verbose
        then printf "(length = %d) Eval_custom loop (%d)\n%!" (length t) (T.hash t);
        let t' = expand_once t in
        (* if verbose then printf !"Expanded:\n%{sexp: t}\n%!" t'; *)
        let t' = reduce_fix t' in
        (* if verbose then printf !"Reduced:\n%{sexp: t}\n%!" t';
         * if verbose then printf !"Reduced hum:\n%s\n%!" (to_string_hum t'); *)
        if equal t' t
        then t
        else
          (* printf "===========\n%!";
           * let (_ : string) = In_channel.input_line_exn In_channel.stdin in *)
          loop t'
      in
      loop t)
;;
