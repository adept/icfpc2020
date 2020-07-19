open Core

let maybe_create ~server_url ~api_key player_key =
  if String.equal player_key "ATTACK" || String.equal player_key "DEFEND"
  then (
    let create_msg = Encode.(encode (of_eval_exn (Eval.encode_int_list [ 1; 0 ]))) in
    let response =
      Http.send_api_exn ~server_url ~api_key ~method_path:"aliens/send" create_msg
    in
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
  [@@deriving sexp_of, compare, equal]

  let of_eval t =
    match t.Eval.u with
    | Var "0" -> Attacker
    | Var "1" -> Defender
    | _ -> assert false
  ;;
end

module Vec2 = struct
  type t = Big_int.t * Big_int.t [@@deriving sexp_of]

  let of_eval t = Eval.(tuple2 to_int_exn to_int_exn t)

  let to_eval (x, y) =
    Eval.cons (Eval.var (Big_int.to_string x)) (Eval.var (Big_int.to_string y))
  ;;

  let neg (x, y) =
    let open Big_int in
    neg x, neg y
  ;;
end

module Ship_stats = struct
  type t =
    { a : Big_int.t
    ; b : Big_int.t
    ; c : Big_int.t
    ; d : Big_int.t
    }
  [@@deriving sexp_of, fields]

  let sample =
    { a = Big_int_Z.big_int_of_int 20
    ; b = Big_int_Z.big_int_of_int 20
    ; c = Big_int_Z.big_int_of_int 20
    ; d = Big_int_Z.big_int_of_int 1
    }
  ;;

  let of_eval t =
    let a, b, c, d = Eval.(tuple4 to_int_exn to_int_exn to_int_exn to_int_exn t) in
    { a; b; c; d }
  ;;

  let to_eval t =
    Eval.(
      encode_list
        [ var (Big_int.to_string t.a)
        ; var (Big_int.to_string t.b)
        ; var (Big_int.to_string t.c)
        ; var (Big_int.to_string t.d)
        ])
  ;;
end

module Ship = struct
  type t =
    { role : Role.t
    ; id : Big_int.t
    ; pos : Vec2.t
    ; velocity : Vec2.t
    ; stats : Ship_stats.t
    ; x5 : Big_int.t
    ; x6 : Big_int.t
    ; x7 : Big_int.t
    }
  [@@deriving sexp_of, fields]

  let of_eval t =
    printf !"SHIP: %{Eval#hum}\n" t;
    List.iteri (Eval.decode_list t) ~f:(fun i t -> printf !"SHIP[%d]: %{Eval#hum}\n" i t);
    let role, id, pos, velocity, stats, x5, x6, x7 =
      Eval.(
        tuple8
          Role.of_eval
          to_int_exn
          Vec2.of_eval
          Vec2.of_eval
          Ship_stats.of_eval
          to_int_exn
          to_int_exn
          to_int_exn
          t)
    in
    { role; id; pos; velocity; stats; x5; x6; x7 }
  ;;
end

module Game_info = struct
  type t =
    { stage : Stage.t
    ; x0 : Big_int.t
    ; role : Role.t
    ; x2 : Eval.t
    ; x3 : Eval.t
    ; x4 : Eval.t
    ; tick : Big_int.t
    ; x1 : Eval.t
    ; our_ship : (Ship.t * Eval.t list) option (* ship, commands *)
    ; their_ship : (Ship.t * Eval.t list) option (* ship, commands *)
    }
  [@@deriving fields, sexp_of]

  let of_eval stage info state =
    printf !"INFO: %{Eval#hum}\n%!" info;
    let x0, role, x2, x3, x4 = Eval.(tuple5 to_int_exn Role.of_eval id id id info) in
    printf !"STATE: %{Eval#hum}\n%!" state;
    let tick, x1, ships =
      match Eval.decode_list state with
      | [] -> Big_int.zero, Eval.var "0", []
      | _ -> Eval.(tuple3 to_int_exn id decode_list state)
    in
    let ships = List.map ~f:Eval.(tuple2 Ship.of_eval decode_list) ships in
    let our_ship, their_ship =
      match ships with
      | [] -> None, None
      | [ _ ] -> failwith "one ship?!"
      | [ ((ship1, _) as info1); info2 ] ->
        if Role.equal (Ship.role ship1) role
        then Some info1, Some info2
        else Some info2, Some info1
      | _ -> failwith "more than two ships?!"
    in
    { stage = Stage.of_eval stage; x0; role; x2; x3; x4; tick; x1; our_ship; their_ship }
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

let accelerate_cmd ~ship_id ~vector =
  let open Eval in
  encode_list [ var "0"; var (Big_int.to_string ship_id); Vec2.to_eval vector ]
;;

let detonate_cmd ~ship_id =
  let open Eval in
  encode_list [ var "1"; var (Big_int.to_string ship_id) ]
;;

let shoot_cmd ~ship_id ~target ~x3 =
  let open Eval in
  encode_list
    [ var "2"
    ; var (Big_int.to_string ship_id)
    ; Vec2.to_eval target
    ; var (Big_int.to_string x3)
    ]
;;

(** Returns a unit vector pointing to the planet from [pos]. *)
let planet_vec (x, y) =
  let open Big_int in
  if y < zero && abs y >= abs x
  then (* Top quadrant *)
    zero, one
  else if y > zero && abs y >= abs x
  then (* Bottom quadrant *)
    zero, minus_one
  else if x < zero && abs x >= abs y
  then (* Left quadrant *)
    minus_one, zero
  else (* Right quadrant *)
    one, zero
;;

let join ~server_url ~api_key player_key =
  let join_msg =
    Encode.(
      encode (of_eval_exn Eval.(encode_list [ var "2"; var player_key; var "nil" ])))
  in
  let response =
    Http.send_api_exn ~server_url ~api_key ~method_path:"aliens/send" join_msg
  in
  printf !"JOIN RESP: %{Encode#mach}\n" response;
  let game_info = game_response (Encode.to_eval response) in
  printf !"GAME INFO: %{sexp: Game_info.t}\n" game_info;
  game_info
;;

let start ~server_url ~api_key player_key =
  let start_msg =
    Encode.(
      encode
        (of_eval_exn
           Eval.(
             encode_list
               [ var "3"; var player_key (* TODO *); Ship_stats.(to_eval sample) ])))
  in
  let response =
    Http.send_api_exn ~server_url ~api_key ~method_path:"aliens/send" start_msg
  in
  printf !"START RESP: %{Encode#mach}\n" response;
  let game_info = game_response (Encode.to_eval response) in
  printf !"GAME INFO: %{sexp: Game_info.t}\n" game_info;
  game_info
;;

let commands ~server_url ~api_key player_key cmds =
  let commands_msg =
    Encode.(
      encode
        (of_eval_exn Eval.(encode_list [ var "4"; var player_key; encode_list cmds ])))
  in
  let response =
    Http.send_api_exn ~server_url ~api_key ~method_path:"aliens/send" commands_msg
  in
  printf !"COMMANDS RESP: %{Encode#mach}\n" response;
  let game_info = game_response (Encode.to_eval response) in
  printf !"GAME INFO: %{sexp: Game_info.t}\n" game_info;
  game_info
;;

let run ~server_url ~player_key ~api_key =
  let player_key = maybe_create ~server_url ~api_key player_key in
  let info = join ~server_url ~api_key player_key in
  match Game_info.stage info with
  | Finished -> ()
  | _ ->
    let info = start ~server_url ~api_key player_key in
    (match Game_info.stage info with
    | Finished -> ()
    | _ ->
      let rec loop (info : Game_info.t) =
        let cmds =
          match info.our_ship with
          | None ->
            (* No ship :,() *)
            []
          | Some ({ id; pos; _ }, _) ->
            [ accelerate_cmd ~ship_id:id ~vector:(Vec2.neg (planet_vec pos)) ]
        in
        let info = commands ~server_url ~api_key player_key cmds in
        match Game_info.stage info with
        | Finished -> ()
        | _ -> loop info
      in
      loop info)
;;
