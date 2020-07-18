open! Core

module T = struct
  module U = struct
    type t = Lambda.L.term =
      | Var of string
      | Abs of (string * t)
      | App of t * t
    [@@deriving compare, equal, sexp]
  end

  include U
  include Comparable.Make (U)
end

include T.U

let rec length = function
  | Var _ -> 1
  | Abs (_, t) -> 1 + length t
  | App (t1, t2) -> 1 + length t1 + length t2
;;

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

let base_defs = String.Map.empty

(* C x y z = x z y *)
(* |> Map.set ~key:"c" ~data:(Lambda.Parse.parse "(/x./y./z.x z y)") *)
(* B x y z = x (y z)) *)
(* |> Map.set ~key:"b" ~data:(Lambda.Parse.parse "(/x./y./z.x (y z))") *)
(*cons: λh.λt.(λs.s h t) *)
(* |> Map.set ~key:"cons" ~data:(Lambda.Parse.parse "(/x./y.(/m.m x y))")
 * |> Map.set ~key:"car" ~data:(Lambda.Parse.parse "(/z.(z (/p./q.p)))")
 * |> Map.set ~key:"cdr" ~data:(Lambda.Parse.parse "(/z.(z (/p./q.q)))")
 * |> Map.set ~key:"nil" ~data:(Lambda.Parse.parse "(/z.(/p./q.p))") *)
(* Sxyz = xz(yz) *)
(* |> Map.set ~key:"s" ~data:(Lambda.Parse.parse "(/x./y./z.x z (y z))") *)
(* |> Map.set ~key:"i" ~data:(Lambda.Parse.parse "(/x.x)") *)
(* |> Map.set ~key:"t" ~data:Lambda.Bool.ltrue
 * |> Map.set ~key:"f" ~data:Lambda.Bool.lfalse *)
(* for tests only *)

let is_int str =
  try
    let (_ : int) = Int.of_string str in
    true
  with
  | _ -> false
;;

let reduce t =
  let rec loop = function
    | App (App (Var "f", _), y) -> y
    | App (App (Var "t", x), _) -> x
    | App (App (App (Var "c", arg1), arg2), arg3) -> App (App (arg1, arg3), arg2)
    | App (App (App (Var "b", arg1), arg2), arg3) -> App (arg1, App (arg2, arg3))
    | App (App (App (Var "s", arg1), arg2), arg3) ->
      App (App (arg1, arg3), App (arg2, arg3))
    | App (Var "i", arg1) -> arg1
    | App (Var "car", App (App (Var "cons", arg1), _)) -> arg1
    | App (Var "cdr", App (App (Var "cons", _), arg2)) -> arg2
    | App (Var "inc", Var x) when is_int x -> Var (Int.to_string (Int.of_string x + 1))
    | App (Var "dec", Var x) when is_int x -> Var (Int.to_string (Int.of_string x - 1))
    (* inc (dec) and dec (inc) *)
    | App (Var "inc", App (Var "dec", Var x)) -> Var x
    | App (Var "dec", App (Var "inc", Var x)) -> Var x
    (* inc (add) and dec (add) *)
    | App (Var "dec", App (App (Var "add", Var x), y)) when is_int x ->
      App (App (Var "add", Var (Int.to_string (Int.of_string x - 1))), y)
    | App (Var "dec", App (App (Var "add", y), Var x)) when is_int x ->
      App (App (Var "add", y), Var (Int.to_string (Int.of_string x - 1)))
    | App (Var "inc", App (App (Var "add", Var x), y)) when is_int x ->
      App (App (Var "add", Var (Int.to_string (Int.of_string x + 1))), y)
    | App (Var "inc", App (App (Var "add", y), Var x)) when is_int x ->
      App (App (Var "add", y), Var (Int.to_string (Int.of_string x + 1)))
    (* add 0 *)
    | App (App (Var "add", x), Var "0") -> x
    | App (App (Var "add", Var "0"), x) -> x
    (* arithmetics *)
    | App (App (Var "add", Var x), Var y) when is_int x && is_int y ->
      Var (Int.to_string (Int.of_string x + Int.of_string y))
    | App (App (Var "div", Var x), Var y) when is_int x && is_int y ->
      Var (Int.to_string (Int.of_string x / Int.of_string y))
    | App (App (Var "div", Var x), Var "1") -> Var x
    | App (App (Var "mul", Var x), Var y) when is_int x && is_int y ->
      Var (Int.to_string (Int.of_string x * Int.of_string y))
    | App (App (Var "mul", Var x), Var "1") -> Var x
    | App (App (Var "mul", Var _), Var "0") -> Var "0"
    | App (App (Var "mul", Var "1"), Var x) -> Var x
    | App (App (Var "mul", Var "0"), _) -> Var "0"
    | App (Var "neg", Var x) when is_int x ->
      Var (Int.to_string (Int.neg (Int.of_string x)))
    | App (App (Var "eq", Var arg1), Var arg2) ->
      if String.equal arg1 arg2 then Var "t" else Var "f"
    | App (App (Var "lt", Var x), Var y) when is_int x && is_int y ->
      if Int.( < ) (Int.of_string x) (Int.of_string y) then Var "t" else Var "f"
    | App (Var "isnil", Var "nil") -> Var "t"
    (* | App (Var "isnil", x) ->
     *   if Lambda.Bool.is_bool x && Lambda.Bool.to_bool x
     *   then Var "t"
     *   else (
     *     match x with
     *     | Abs (_, x) ->
     *       if Lambda.Bool.is_bool x && Lambda.Bool.to_bool x then Var "t" else Var "f"
     *     | _ -> Var "f") *)
    | App (App (App (Var "if0", Var "0"), then_branch), _else_branch) -> then_branch
    | App (App (App (Var "if0", Var x), _then_branch), else_branch)
      when is_int x && Int.of_string x <> 0 -> else_branch
    | App (arg1, arg2) -> App (loop arg1, loop arg2)
    | t -> t
  in
  try loop t with
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
  let t' = reduce t' in
  if equal t' t then t' else eval t' ~verbose ~defs
;;

(** [eval_custom] is like [eval] but 1) only expands the left+inner-most leaf,
   and 2) memoizes results. *)
let eval_custom ~verbose ~defs =
  let reduce = Memo.of_comparable (module T) reduce in
  Staged.stage (fun t ->
      if verbose then printf "Eval (length: %d): %s\n%!" (length t) (to_string_hum t);
      let rec expand_once t =
        match t with
        | Var name ->
          (match Map.find defs name with
          | None -> t
          | Some expansion ->
            if verbose then printf !"Substituting %s => %{sexp: t}\n%!" name expansion;
            reduce expansion)
        | Abs _ -> t
        | App (t1, t2) ->
          (match expand_once t1 with
          | t1' when not (equal t1 t1') -> reduce (App (t1', t2))
          | _ ->
            (match expand_once t2 with
            | t2' when not (equal t2 t2') -> reduce (App (t1, t2'))
            | _ -> t))
      in
      let rec loop t =
        (* if verbose
         * then printf "(length = %d) Eval_custom loop: %s\n%!" (length t) (to_string_hum t);
         * let (_ : string) = In_channel.input_line_exn In_channel.stdin in *)
        let t' = reduce (expand_once t) in
        if equal t' t then t else loop t'
      in
      loop t)
;;
