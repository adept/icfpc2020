open Core

let version = "0.100 HACKY ORBITS 2"

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
  type t = int * int [@@deriving sexp_of, equal]

  let of_eval t =
    Eval.(
      tuple2
        (fun x -> to_int_exn x |> Big_int.to_int_exn)
        (fun x -> to_int_exn x |> Big_int.to_int_exn)
        t)
  ;;

  let to_eval (x, y) = Eval.cons (Eval.var (Int.to_string x)) (Eval.var (Int.to_string y))
  let neg (x, y) = Int.neg x, Int.neg y
  let add (x1, y1) (x2, y2) = x1 + x2, y1 + y2
  let sub (x1, y1) (x2, y2) = x1 - x2, y1 - y2
end

module Ship_stats = struct
  type t =
    { fuel : Big_int.t
    ; b : Big_int.t
    ; c : Big_int.t
    ; d : Big_int.t
    }
  [@@deriving sexp_of, fields]

  let sample =
    (* We previously tried:

       - (255, 1, 1, 1) but we start to use lots of fuel after a few turns.
    *)
    { fuel = Big_int_Z.big_int_of_int 254
    ; b = Big_int_Z.big_int_of_int 0
    ; c = Big_int_Z.big_int_of_int 16
    ; d = Big_int_Z.big_int_of_int 1
    }
  ;;

  let of_eval t =
    let fuel, b, c, d = Eval.(tuple4 to_int_exn to_int_exn to_int_exn to_int_exn t) in
    { fuel; b; c; d }
  ;;

  let to_eval t =
    Eval.(
      encode_list
        [ var (Big_int.to_string t.fuel)
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
  [@@deriving fields]

  let sexp_of_t { role; id; pos; velocity; stats; x5; x6; x7 } =
    [%sexp
      "Ship"
      , { role : Role.t
        ; id : Big_int.t
        ; pos : Vec2.t
        ; velocity : Vec2.t
        ; stats : Ship_stats.t
        ; x5 : Big_int.t
        ; x6 : Big_int.t
        ; x7 : Big_int.t
        }]
  ;;

  let of_eval t =
    (* printf !"SHIP: %{Eval#hum}\n" t; *)
    (* List.iteri (Eval.decode_list t) ~f:(fun i t -> printf !"SHIP[%d]: %{Eval#hum}\n" i t); *)
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
    ; max_ticks : Big_int.t
    ; role : Role.t
    ; x2 : Eval.t
    ; x3 : Eval.t
    ; x4 : Eval.t
    ; tick : Big_int.t
    ; x1 : Eval.t
    ; our_ship : (Ship.t * Eval.t list) option (* ship, commands *)
    ; their_ship : (Ship.t * Eval.t list) option (* ship, commands *)
    }
  [@@deriving fields]

  let sexp_of_t { stage; max_ticks; role; x2; x3; x4; tick; x1; our_ship; their_ship } =
    [%sexp
      "Game_info"
      , { stage : Stage.t
        ; max_ticks : Big_int.t
        ; role : Role.t
        ; tick : Big_int.t
        ; x2 : string = Eval.to_string_mach x2
        ; x3 : string = Eval.to_string_mach x3
        ; x4 : string = Eval.to_string_mach x4
        ; x1 : string = Eval.to_string_mach x1
        ; our_ship : (Ship.t * string list) option =
            Option.map our_ship ~f:(fun (ship, commands) ->
                ship, List.map commands ~f:Eval.to_string_mach)
        ; their_ship : (Ship.t * string list) option =
            Option.map their_ship ~f:(fun (ship, commands) ->
                ship, List.map commands ~f:Eval.to_string_mach)
        }]
  ;;

  let of_eval stage info state =
    (* printf !"INFO: %{Eval#hum}\n%!" info; *)
    let max_ticks, role, x2, x3, x4 =
      Eval.(tuple5 to_int_exn Role.of_eval id id id info)
    in
    (* printf !"STATE: %{Eval#hum}\n%!" state; *)
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
    { stage = Stage.of_eval stage
    ; max_ticks
    ; role
    ; x2
    ; x3
    ; x4
    ; tick
    ; x1
    ; our_ship
    ; their_ship
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
let gravity (x, y) =
  if y < 0 && Int.abs y >= Int.abs x
  then (* Top quadrant *)
       (* printf "QUADRANT: TOP\n%!"; *)
    0, 1
  else if y > 0 && Int.abs y >= Int.abs x
  then (* Bottom quadrant *)
       (* printf "QUADRANT: BOTTOM\n%!"; *)
    0, -1
  else if x < 0 && Int.abs x >= Int.abs y
  then (* Left quadrant *)
       (* printf "QUADRANT: LEFT\n%!"; *)
    1, 0
  else (* Right quadrant *)
       (* printf "QUADRANT: RIGHT\n%!"; *)
    -1, 0
;;

(* the planet is -16 to 16 *)
let safety_margin = 4
let will_hit_planet coord = coord >= -16 - safety_margin && coord >= 16 + safety_margin

(* sanity helpers *)
let is_left x = x < 0
let is_right x = x >= 0
let is_above y = y < 0
let is_below y = y >= 0
let sign x = if x > 0 then 1 else if x < 0 then -1 else 0

(* Accel away from the planet, broken *)
let away_from_planet (x, y) =
  let x = if x >= -16 || x <= 16 then 0 else -sign x in
  let y = if y >= -16 || y <= 16 then 0 else -sign y in
  x, y
;;

let towards (x, y) = sign x, sign y

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
  (* printf !"COMMANDS RESP: %{Encode#mach}\n" response; *)
  let game_info = game_response (Encode.to_eval response) in
  printf !"GAME INFO: %{sexp: Game_info.t}\n" game_info;
  game_info
;;

module Simulator = struct
  let side a b c =
    let d = ((snd c - snd a) * (fst b - fst a)) - ((snd b - snd a) * (fst c - fst a)) in
    if d > 0 then 1 else if d < 0 then -1 else 0
  ;;

  let is_point_in_closed_segment a b c =
    if fst a < fst b
    then fst a <= fst c && fst c <= fst b
    else if fst b < fst a
    then fst b <= fst c && fst c <= fst a
    else if snd a < snd b
    then snd a <= snd c && snd c <= snd b
    else if snd b < snd a
    then snd b <= snd c && snd c <= snd a
    else fst a = fst c && snd a = snd c
  ;;

  let segments_intersect a b c d =
    if Vec2.equal a b
    then Vec2.equal a c || Vec2.equal a d
    else if Vec2.equal c d
    then Vec2.equal c a || Vec2.equal c b
    else (
      let s1 = side a b c in
      let s2 = side a b d in
      (* All points are collinear *)
      if s1 = 0 && s2 = 0
      then
        is_point_in_closed_segment a b c
        || is_point_in_closed_segment a b d
        || is_point_in_closed_segment c d a
        || is_point_in_closed_segment c d b
      else if (* No touching and on the same side *)
              s1 <> 0 && s1 = s2
      then false
      else (
        let s1 = side c d a in
        let s2 = side c d b in
        (* No touching and on the same side *)
        if s1 <> 0 && s1 = s2 then false else true))
  ;;

  let intersects_planet a b =
    segments_intersect a b (16, -16) (16, 16)
    || segments_intersect a b (-16, -16) (-16, 16)
    || segments_intersect a b (-16, -16) (16, -16)
    || segments_intersect a b (-16, 16) (16, 16)
  ;;

  let%expect_test _ =
    printf "%b\n" (intersects_planet (5, -18) (-2, 13));
    printf "%b\n" (intersects_planet (5, -18) (-18, -17));
    printf "%b\n" (intersects_planet (-18, 15) (-6, 15));
    [%expect {|
      true
      false
      true |}]
  ;;

  (* let in_planet (x, y) = -16 <= x && x <= 16 && -16 <= y && y <= 16 *)

  (* How many ticks until we crash into the planet?  May return None. *)
  let planet_crash_eta ~pos ~velocity ~max_ticks =
    let rec loop ~pos ~velocity ~ticks =
      if ticks = 0
      then (* We did not crash. *)
        None
      else (
        let new_pos = Vec2.add velocity pos in
        let velocity = Vec2.add velocity (gravity pos) in
        let ticks = ticks - 1 in
        if intersects_planet pos new_pos
        then Some (max_ticks - ticks)
        else loop ~pos:new_pos ~velocity ~ticks)
    in
    loop ~pos ~velocity ~ticks:max_ticks
  ;;
end

let maybe_movement_command ~id ~pos ~velocity =
  match Simulator.planet_crash_eta ~pos ~velocity ~max_ticks:30 with
  | None ->
    (* We're not going to crash, so nothing to do. *)
    None
  | Some _ ->
    (* We're falling into the planet!  Do something! *)
    let gravity = gravity pos in
    printf !"Gravity: (%d, %d)\n" (fst gravity) (snd gravity);
    let sideways_dir =
      if fst gravity <> 0
      then (
        print_endline "Sideways is Y";
        `Y)
      else (
        print_endline "Sideways is X";
        `X)
    in
    let sideways_velocity =
      match sideways_dir with
      | `X -> fst velocity
      | `Y -> snd velocity
    in
    printf "Sideways velocity: %d\n" sideways_velocity;
    let mk_sideways_vector sign =
      match sideways_dir with
      | `X -> sign, 0
      | `Y -> 0, sign
    in
    let sideways_vector =
      if sideways_velocity = 0
      then mk_sideways_vector 1
      else mk_sideways_vector (~-1 * sign sideways_velocity)
    in
    let vector =
      match
        Simulator.planet_crash_eta
          ~pos
          ~velocity:(Vec2.add velocity sideways_vector)
          ~max_ticks:30
      with
      | None ->
        (* We're good. *)
        sideways_vector
      | Some _ ->
        print_endline "Still crashing, so adjusting trajectory harder";
        Vec2.add sideways_vector gravity
    in
    Some (accelerate_cmd ~ship_id:id ~vector)
;;

let blindly_simulate ~pos ~velocity =
  let ((dx, dy) as velocity) = Vec2.add velocity (gravity pos) in
  let velocities =
    [ dx, dy
    ; dx - 1, dy
    ; dx + 1, dy
    ; dx, dy - 1
    ; dx, dy + 1
    ; dx + 1, dy + 1
    ; dx - 1, dy - 1
    ; dx + 1, dy - 1
    ; dx - 1, dy + 1
    ]
  in
  let fuel_cost = [ 0; 1; 1; 1; 1; 1; 1; 1; 1 ] in
  let estimates =
    List.zip_exn velocities fuel_cost
    |> List.map ~f:(fun (velocity, cost) ->
           let eta =
             Simulator.planet_crash_eta ~pos ~velocity ~max_ticks:256
             |> Option.value ~default:256
           in
           (eta, cost), velocity)
  in
  let (eta, cost), best_velocity =
    List.max_elt estimates ~compare:(fun (eta_cost1, _) (eta_cost2, _) ->
        Poly.compare eta_cost1 eta_cost2)
    |> Option.value_exn
  in
  let vec = Vec2.sub velocity best_velocity in
  printf !"CHOSEN: ETA: %d, cost %d, vec: %{sexp:Vec2.t}\n" eta cost vec;
  vec
;;

let run ~server_url ~player_key ~api_key =
  printf "VERSION: %s\n\n" version;
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
        printf
          "\n\n\
           ********************************************************************************\n";
        printf !"                         TICK %{Big_int}\n" info.tick;
        printf
          "********************************************************************************\n\n";
        let cmds =
          match info.our_ship with
          | None ->
            (* No ship :,() *)
            []
          | Some ({ id; pos; velocity; _ }, _) ->
            printf
              !"CRASH ETA: %{sexp: int option} ticks\n"
              (Simulator.planet_crash_eta ~pos ~velocity ~max_ticks:256);
            List.filter_opt
              [ (* maybe_movement_command ~id ~pos ~velocity *)
                Some
                  (accelerate_cmd ~ship_id:id ~vector:(blindly_simulate ~pos ~velocity))
              ; Option.map info.their_ship ~f:(fun (ship, _) ->
                    shoot_cmd
                      ~ship_id:id
                      ~target:
                        (Vec2.add ship.pos (Vec2.add ship.velocity (gravity ship.pos)))
                      ~x3:Big_int.one)
              ]
        in
        let info = commands ~server_url ~api_key player_key cmds in
        match Game_info.stage info with
        | Finished -> ()
        | _ -> loop info
      in
      loop info)
;;
