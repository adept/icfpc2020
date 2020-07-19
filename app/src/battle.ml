open Core

let maybe_create ~api_key player_key =
  if String.equal player_key "ATTACK" || String.equal player_key "DEFEND"
  then (
    let create_msg = Encode.(encode (of_eval_exn (Eval.encode_int_list [ 1; 0 ]))) in
    let response = Http.send_api_exn ~api_key ~method_path:"aliens/send" create_msg in
    printf !"CREATE RESP: %{Encode#mach}\n" response;
    let response = Encode.to_eval response in
    let status, ((_, attack_key), (_, defend_key)) =
      Eval.(tuple2 id (tuple2 (tuple2 id id) (tuple2 id id)) response)
    in
    let status = Eval.(to_int_exn status) in
    if not (Big_int.equal status Big_int.one)
    then failwithf !"CREATE failed %{Big_int}" status ()
    else (
      printf !"ATTACK KEY: %{Eval#mach}\n%!" attack_key;
      printf !"DEFEND KEY: %{Eval#mach}\n%!" defend_key;
      match player_key with
      | "ATTACK" -> Eval.to_string_mach attack_key
      | "DEFEND" -> Eval.to_string_mach defend_key
      | _ -> assert false))
  else player_key
;;

module Stage = struct
  type t =
    | NotStarted
    | Started
    | Finished
  [@@deriving sexp_of]

  let of_eval t =
    match t.Eval.u with
    | Var "0" -> NotStarted
    | Var "1" -> Started
    | Var "2" -> Finished
    | _ -> assert false
  ;;
end

module Role = struct
  type t =
    | Attacker
    | Defender
  [@@deriving sexp_of]

  let of_eval t =
    match t.Eval.u with
    | Var "0" -> Attacker
    | Var "1" -> Defender
    | _ -> assert false
  ;;
end

module Game_info = struct
  type t =
    { stage : Stage.t
    ; x0 : Eval.t
    ; role : Role.t
    ; x2 : Eval.t
    ; x3 : Eval.t
    ; x4 : Eval.t
    ; tick : Eval.t
    ; x1 : Eval.t
    ; ships_commands : Eval.t
    }
  [@@deriving sexp_of]

  let of_eval stage info state =
    printf !"INFO: %{Eval#hum}\n%!" info;
    let x0, role, x2, x3, x4 = Eval.(tuple5 id id id id id info) in
    printf !"STATE: %{Eval#hum}\n%!" state;
    let tick, x1, ships_commands =
      match Eval.decode_list state with
      | [] -> Eval.var "0", Eval.var "0", Eval.var "0"
      | _ -> Eval.(tuple3 id id id state)
    in
    { stage = Stage.of_eval stage
    ; x0
    ; role = Role.of_eval role
    ; x2
    ; x3
    ; x4
    ; tick
    ; x1
    ; ships_commands
    }
  ;;
end

let game_response response =
  match Eval.decode_list response with
  | [ mb_zero ] -> failwithf !"Game Response: WRONG REQUEST %{Eval#mach}" mb_zero ()
  | [ success; stage; info; state ] ->
    if not (Big_int.equal (Eval.to_int_exn success) Big_int.one)
    then failwithf !"Game Reponse: success status is not one?! %{Eval#mach}" success ()
    else Game_info.of_eval stage info state
  | _ -> failwithf !"Game Response: CANT PARSE %{Eval#mach}" response ()
;;

let join ~api_key player_key =
  let join_msg =
    Encode.(
      encode (of_eval_exn Eval.(encode_list [ var "2"; var player_key; var "nil" ])))
  in
  let response = Http.send_api_exn ~api_key ~method_path:"aliens/send" join_msg in
  printf !"JOIN RESP: %{Encode#mach}\n" response;
  let game_info = game_response (Encode.to_eval response) in
  printf !"GAME INFO: %{sexp: Game_info.t}\n" game_info;
  ()
;;

let run ~server_url:_ ~player_key ~api_key =
  let player_key = maybe_create ~api_key player_key in
  join ~api_key player_key
;;
