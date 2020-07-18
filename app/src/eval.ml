open! Core
module Id = Unique_id.Int ()

type t =
  | Var of string
  | App of t * t
[@@deriving sexp]

let rec equal t1 t2 =
  match t1, t2 with
  | Var n1, Var n2 -> String.equal n1 n2
  | App (t11, t12), App (t21, t22) -> equal t11 t21 && equal t12 t22
  | _, _ -> false
;;

let rec length = function
  | Var _ -> 1
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
    | App (App (Var "f", _), y) -> y
    | App (App (Var "t", x), _) -> x
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
    | App (App (Var "mul", Var "1"), Var x) -> Var x
    | App (App (Var "mul", Var "0"), _) -> Var "0"
    | App (Var "neg", Var x) when is_int x ->
      Var (Int.to_string (Int.neg (Int.of_string x)))
    | App (App (Var "eq", Var arg1), Var arg2) ->
      if String.equal arg1 arg2 then Var "t" else Var "f"
    | App (App (Var "lt", Var x), Var y) when is_int x && is_int y ->
      if Int.( < ) (Int.of_string x) (Int.of_string y) then Var "t" else Var "f"
    | App (Var "isnil", Var "nil") -> Var "t"
    | App (arg1, arg2) ->
      let t = App (step arg1, step arg2) in
      (match t with
      | App (App (App (Var "c", arg1), arg2), arg3) -> App (App (arg1, arg3), arg2)
      | App (App (App (Var "b", arg1), arg2), arg3) -> App (arg1, App (arg2, arg3))
      | App (App (App (Var "s", arg1), arg2), arg3) ->
        App (App (arg1, arg3), App (arg2, arg3))
      | t -> t)
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
        | Var name ->
          (match Map.find defs name with
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
      let rec loop t =
        if verbose then printf "(length = %d) Eval_custom loop\n%!" (length t);
        let t' = expand_once t in
        if verbose then printf !"Expanded:\n%{sexp: t}\n%!" t';
        let t' = reduce_fix t' in
        if verbose then printf !"Reduced:\n%{sexp: t}\n%!" t';
        if equal t' t
        then t
        else (* let (_ : string) = In_channel.input_line_exn In_channel.stdin in *)
          loop t'
      in
      loop t)
;;
