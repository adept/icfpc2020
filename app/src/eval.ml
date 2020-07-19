open! Core

type t =
  { mutable evaluated : (t[@sexp.opaque]) option
  ; u : u
  }

and u =
  | Var of string
  | App of t * t
[@@deriving sexp_of]

let rec to_string_hum t =
  match t.u with
  | Var name -> sprintf {|"%s"|} name
  | App (x, y) ->
    let str_of_arg arg =
      match arg.u with
      | Var name -> sprintf {|"%s"|} name
      | _ -> sprintf "(%s)" (to_string_hum arg)
    in
    sprintf "ap %s %s" (str_of_arg x) (str_of_arg y)
;;

let rec to_string_mach t =
  match t.u with
  | Var name -> name
  | App (x, y) -> sprintf "ap %s %s" (to_string_mach x) (to_string_mach y)
;;

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

let car t =
  match t.u with
  | App ({ u = App ({ u = Var "cons"; _ }, h); _ }, _) -> h
  | _ -> failwithf !"Unexpected %s in car" (to_string_hum t) ()
;;

let cdr t =
  match t.u with
  | App ({ u = App ({ u = Var "cons"; _ }, _); _ }, t) -> t
  | _ -> failwithf !"Unexpected %s in cdr" (to_string_hum t) ()
;;

let is_int str =
  try
    let (_ : Big_int.t) = Big_int.of_string str in
    true
  with
  | _ -> false
;;

let decode_list t =
  let rec loop t =
    match t.u with
    | Var "nil" -> []
    | _ ->
      let h = car t in
      let rest = loop (cdr t) in
      h :: rest
  in
  loop t
;;

(* Decode multidraw vector: list of lists of coordinate pairs *)
let decode_vector t : (int * int) list list =
  let decode_pair t =
    match t.u with
    | App ({ u = App ({ u = Var "cons"; _ }, { u = Var x; _ }); _ }, { u = Var y; _ })
      when is_int x && is_int y ->
      ( Big_int_Z.(int_of_big_int (big_int_of_string x))
      , Big_int_Z.(int_of_big_int (big_int_of_string y)) )
    | x -> failwithf !"pair: %{sexp: u}" x ()
  in
  List.map (decode_list t) ~f:(fun l -> List.map (decode_list l) ~f:decode_pair)
;;

let id t = t

let tuple2 fa fb t =
  match decode_list t with
  | [ a; b ] -> fa a, fb b
  | x -> failwithf !"tuple2: %{sexp:t list}" x ()
;;

let tuple3 fa fb fc t =
  match decode_list t with
  | [ a; b; c ] -> fa a, fb b, fc c
  | x -> failwithf !"tuple3: %{sexp:t list}" x ()
;;

let tuple4 fa fb fc fd t =
  match decode_list t with
  | [ a; b; c; d ] -> fa a, fb b, fc c, fd d
  | x -> failwithf !"tuple4: %{sexp:t list}" x ()
;;

let rec encode_list x =
  match x with
  | [] -> var "nil"
  | h :: t -> app (app (var "cons") (var (Int.to_string h))) (encode_list t)
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
  | Var str -> Big_int.of_string str
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
      | Var "neg" ->
        var (Big_int.to_string (Big_int.mult_int_big_int ~-1 (to_int_exn (eval x))))
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
        | Var "add" ->
          var
            (Big_int.to_string
               (Big_int.( + ) (to_int_exn (eval x)) (to_int_exn (eval y))))
        | Var "mul" ->
          var
            (Big_int.to_string
               (Big_int.( * ) (to_int_exn (eval x)) (to_int_exn (eval y))))
        | Var "div" ->
          var
            (Big_int.to_string
               (Big_int.( / ) (to_int_exn (eval y)) (to_int_exn (eval x))))
        | Var "lt" ->
          var
            (if Big_int.( < ) (to_int_exn (eval y)) (to_int_exn (eval x))
            then "t"
            else "f")
        | Var "eq" ->
          var
            (if Big_int.equal (to_int_exn (eval x)) (to_int_exn (eval y))
            then "t"
            else "f")
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
