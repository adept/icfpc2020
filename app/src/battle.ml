open Core

let version = "0.104 DONT CRASH MULTI"

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
  let radius (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))
end

module Ship_stats = struct
  type t =
    { fuel : Big_int.t
    ; guns : Big_int.t
    ; c : Big_int.t
    ; d : Big_int.t
    }
  [@@deriving sexp_of, fields]

  (* We previously tried:

     - (255, 1, 1, 1) but we start to use lots of fuel after a few turns.
     - (254, 0, 16, 1) we have good fuel efficiency, but the cannons don't fire
     - (214, 10, 16, 1) good fuel efficiency and the cannons fire.
     - (140, 0, 24, 10) decoys loadout
  *)

  let guns_enabled =
    { fuel = Big_int_Z.big_int_of_int 214
    ; guns = Big_int_Z.big_int_of_int 10
    ; c = Big_int_Z.big_int_of_int 16
    ; d = Big_int_Z.big_int_of_int 1
    }
  ;;

  let fuel_efficient =
    { fuel = Big_int_Z.big_int_of_int 254
    ; guns = Big_int_Z.big_int_of_int 0
    ; c = Big_int_Z.big_int_of_int 16
    ; d = Big_int_Z.big_int_of_int 1
    }
  ;;

  let decoys_loadout =
    { fuel = Big_int_Z.big_int_of_int 140
    ; guns = Big_int_Z.big_int_of_int 0
    ; c = Big_int_Z.big_int_of_int 24
    ; d = Big_int_Z.big_int_of_int 10
    }
  ;;

  let of_eval t =
    let fuel, guns, c, d = Eval.(tuple4 to_int_exn to_int_exn to_int_exn to_int_exn t) in
    { fuel; guns; c; d }
  ;;

  let to_eval t =
    Eval.(
      encode_list
        [ var (Big_int.to_string t.fuel)
        ; var (Big_int.to_string t.guns)
        ; var (Big_int.to_string t.c)
        ; var (Big_int.to_string t.d)
        ])
  ;;
end

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

  let next_pos_estimate t ~acceleration =
    t.pos
    |> Vec2.add t.velocity
    |> Vec2.add (Vec2.neg acceleration)
    |> Vec2.add (gravity t.pos)
  ;;

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

module Ship_command = struct
  type t =
    | Accelerate of { vector : Vec2.t }
    | Detonate of
        { d0 : Big_int.t
        ; d1 : Big_int.t
        }
    | Shoot of
        { target : Vec2.t
        ; s0 : Big_int.t
        ; s1 : Big_int.t
        ; s2 : Big_int.t
        }
    | Unknown of Eval.t
  [@@deriving sexp_of]

  let sexp_of_t t =
    match t with
    | Accelerate _ | Detonate _ | Shoot _ -> sexp_of_t t
    | Unknown eval -> [%sexp "Unknown", (Eval.to_string_mach eval : string)]
  ;;

  let of_eval eval =
    (* ap ap cons 2 ap ap cons ap ap cons 40 14 ap ap cons 1 ap ap cons 0 ap ap cons 4 nil *)
    match Eval.(tuple2 to_int_exn Vec2.of_eval eval) with
    | cmd, vector when Big_int.equal cmd Big_int.zero -> Accelerate { vector }
    | exception _ ->
      (match
         Eval.(tuple5 to_int_exn Vec2.of_eval to_int_exn to_int_exn to_int_exn eval)
       with
      | cmd, target, s0, s1, s2 when Big_int.equal cmd Big_int.two ->
        Shoot { target; s0; s1; s2 }
      | exception _ ->
        (match Eval.(tuple3 to_int_exn to_int_exn to_int_exn eval) with
        | cmd, d0, d1 when Big_int.equal cmd Big_int.one -> Detonate { d0; d1 }
        | _ | (exception _) -> Unknown eval)
      | _ -> Unknown eval)
    | _ -> Unknown eval
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
    ; num_ships : int
    ; our_ship : (Ship.t * Ship_command.t list) option (* ship, commands *)
    ; their_ship : (Ship.t * Ship_command.t list) option (* ship, commands *)
    }
  [@@deriving fields]

  let sexp_of_t
      { stage; max_ticks; role; x2; x3; x4; tick; x1; num_ships; our_ship; their_ship }
    =
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
        ; num_ships : int
        ; our_ship : (Ship.t * Ship_command.t list) option
        ; their_ship : (Ship.t * Ship_command.t list) option
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
    let num_ships = List.length ships in
    let our_ship, their_ship =
      match ships with
      | [] -> None, None
      | [ _ ] -> failwith "one ship?!"
      | (ship1, commands1) :: (ship2, commands2) :: rest ->
        let parse_commands commands = List.map commands ~f:Ship_command.of_eval in
        if not (List.is_empty rest)
        then (
          printf "more than two ships?!\n";
          List.iteri ships ~f:(fun i (ship, cmds) ->
              printf !"Ship %d: %{sexp: Ship.t}\n%!" i ship;
              printf
                !"Ship %d commands: %{sexp: Ship_command.t list}\n%!"
                i
                (parse_commands cmds)));
        let commands1 = parse_commands commands1 in
        printf "Ship 1 commands: %d\n" (List.length commands1);
        let commands2 = parse_commands commands2 in
        printf "Ship 2 commands: %d\n" (List.length commands2);
        if Role.equal (Ship.role ship1) role
        then Some (ship1, commands1), Some (ship2, commands2)
        else Some (ship2, commands2), Some (ship1, commands1)
    in
    { stage = Stage.of_eval stage
    ; max_ticks
    ; role
    ; x2
    ; x3
    ; x4
    ; tick
    ; x1
    ; num_ships
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

let split_decoy_cmd ~ship_id =
  let open Eval in
  encode_list
    [ var "3"
    ; var (Big_int.to_string ship_id)
    ; encode_list [ var "0"; var "0"; var "0"; var "1" ]
    ]
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

let start ~server_url ~api_key ~player_key ~role =
  let start_msg =
    Encode.(
      encode
        (of_eval_exn
           Eval.(
             encode_list
               [ var "3"
               ; var player_key
               ; Ship_stats.(
                   to_eval
                     (match (role : Role.t) with
                     | Attacker -> fuel_efficient (* guns_enabled *)
                     | Defender -> decoys_loadout (* fuel_efficient *)))
               ])))
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

  let planet_boundary = 20

  let intersects_planet a b =
    (fst b <= planet_boundary
    && fst b >= -planet_boundary
    && snd b <= planet_boundary
    && snd b >= -planet_boundary)
    || segments_intersect
         a
         b
         (planet_boundary, -planet_boundary)
         (planet_boundary, planet_boundary)
    || segments_intersect
         a
         b
         (-planet_boundary, -planet_boundary)
         (-planet_boundary, planet_boundary)
    || segments_intersect
         a
         b
         (-planet_boundary, -planet_boundary)
         (planet_boundary, -planet_boundary)
    || segments_intersect
         a
         b
         (-planet_boundary, planet_boundary)
         (planet_boundary, planet_boundary)
  ;;

  let bounds = 130

  let out_of_bounds a b =
    segments_intersect a b (bounds, -bounds) (bounds, bounds)
    || segments_intersect a b (-bounds, -bounds) (-bounds, bounds)
    || segments_intersect a b (-bounds, -bounds) (bounds, -bounds)
    || segments_intersect a b (-bounds, bounds) (bounds, bounds)
  ;;

  let%expect_test _ =
    printf "%b\n" (intersects_planet (5, -18) (-2, 13));
    printf "%b\n" (intersects_planet (5, -18) (-18, -17));
    printf "%b\n" (intersects_planet (-18, 15) (-6, 15));
    [%expect {|
      true
      true
      true |}];
    printf "%b\n" (intersects_planet (9, 19) (5, 15));
    [%expect {|
      true |}]
  ;;

  (* let in_planet (x, y) = -16 <= x && x <= 16 && -16 <= y && y <= 16 *)

  (* How many ticks until we crash into the planet?  May return None. *)
  let planet_crash_eta ?velocity_change ~pos ~velocity ~max_ticks () =
    let rec loop ~pos ~velocity ~ticks =
      if ticks = 0
      then (* We did not crash. *)
        None
      else (
        let new_pos = Vec2.add velocity pos in
        let velocity = Vec2.add velocity (gravity pos) in
        let velocity =
          match velocity_change with
          | None -> velocity
          | Some change -> Vec2.add velocity change
        in
        let ticks = ticks - 1 in
        if intersects_planet pos new_pos || out_of_bounds pos new_pos
        then Some (max_ticks - ticks)
        else loop ~pos:new_pos ~velocity ~ticks)
    in
    loop ~pos ~velocity ~ticks:max_ticks
  ;;
end

let maybe_movement_command ~id ~pos ~velocity =
  match Simulator.planet_crash_eta ~pos ~velocity ~max_ticks:30 () with
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
          ()
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

let avoid_planet_in_a_fuel_efficient_way ~pos ~velocity =
  if Vec2.equal velocity (0, 0)
  then (
    let x, y = gravity pos in
    y, x (* move sideways a bit *))
  else (
    let velocity = Vec2.add velocity (gravity pos) in
    let velocity_changes =
      [ 0, 0; -1, 0; 1, 0; 0, -1; 0, 1; 1, 1; -1, -1; 1, -1; -1, 1 ]
    in
    let fuel_cost = [ 0; 1; 1; 1; 1; 1; 1; 1; 1 ] in
    let estimates =
      List.zip_exn velocity_changes fuel_cost
      |> List.map ~f:(fun (velocity_change, cost) ->
             let velocity = Vec2.add velocity velocity_change in
             let eta =
               Simulator.planet_crash_eta ~pos ~velocity ~max_ticks:256 ()
               |> Option.value ~default:256
             in
             (eta, cost), velocity_change)
    in
    let (eta, cost), best_velocity_change =
      List.max_elt estimates ~compare:(fun ((eta1, cost1), _) ((eta2, cost2), _) ->
          let eta_cmp = compare eta1 eta2 in
          if eta_cmp = 0 then compare cost2 cost1 else eta_cmp)
      |> Option.value_exn
    in
    let vec = Vec2.sub (0, 0) best_velocity_change in
    printf !"CHOSEN: ETA: %d, cost %d, vec: %{sexp:Vec2.t}\n" eta cost vec;
    vec)
;;

let avoid_planet_in_agressive_way ~pos ~velocity =
  printf "EVASIVE ACTION!\n";
  let gravity = gravity pos in
  let velocity = Vec2.add velocity gravity in
  (* dont go directly against gravity *)
  let against_gravity = Vec2.sub (0, 0) gravity in
  let velocity_changes =
    [ -1, 0; 1, 0; 0, -1; 0, 1; 1, 1; -1, -1; 1, -1; -1, 1 ]
    |> List.filter ~f:(fun c -> not (Vec2.equal against_gravity c))
  in
  let estimates =
    List.map velocity_changes ~f:(fun velocity_change ->
        let velocity = Vec2.add velocity velocity_change in
        let eta =
          Simulator.planet_crash_eta ~pos ~velocity ~velocity_change ~max_ticks:256 ()
          |> Option.value ~default:256
        in
        eta, velocity_change)
  in
  let eta, best_velocity_change =
    List.max_elt estimates ~compare:(fun (eta1, _) (eta2, _) -> compare eta1 eta2)
    |> Option.value_exn
  in
  let vec = Vec2.sub (0, 0) best_velocity_change in
  printf !"CHOSEN: ETA: %d, vec: %{sexp:Vec2.t}\n" eta vec;
  vec
;;

let get_acceleration (commands : Ship_command.t list) =
  List.find_map commands ~f:(function
      | Accelerate { vector; _ } -> Some vector
      | _ -> None)
  |> Option.value ~default:(0, 0)
;;

(* at 10 ticks to crash or less take evasive actions *)
let evasive_action_limit = 10
let safe_distance = 64

let steering our_ship their_ship =
  match our_ship, their_ship with
  | Some ((our_ship : Ship.t), our_commands), Some ((their_ship : Ship.t), their_commands)
    ->
    let current_distance = Vec2.radius our_ship.pos their_ship.pos in
    let eta =
      Simulator.planet_crash_eta
        ~pos:our_ship.pos
        ~velocity:our_ship.velocity
        ~max_ticks:256
        ()
      |> Option.value ~default:256
    in
    if eta <= evasive_action_limit
    then avoid_planet_in_agressive_way ~pos:our_ship.pos ~velocity:our_ship.velocity
    else (
      let velocity_changes =
        [ 0, 0; -1, 0; 1, 0; 0, -1; 0, 1; 1, 1; -1, -1; 1, -1; -1, 1 ]
      in
      let fuel_cost = [ 0; 1; 1; 1; 1; 1; 1; 1; 1 ] in
      (* are we going towards the enemy ship or away from it? *)
      let cost_f, bad_distance_f =
        match our_ship.role with
        | Role.Attacker -> (fun a b -> compare a b), fun _ -> false
        | Role.Defender -> (fun a b -> compare b a), fun d -> d >= safe_distance
      in
      let estimates =
        List.zip_exn velocity_changes fuel_cost
        |> List.filter_map ~f:(fun (velocity_change, cost) ->
               let our_ship =
                 { our_ship with velocity = Vec2.add our_ship.velocity velocity_change }
               in
               (* Make sure this does not make us crash *)
               let eta =
                 Simulator.planet_crash_eta
                   ~pos:our_ship.pos
                   ~velocity:our_ship.velocity
                   ~max_ticks:256
                   ()
                 |> Option.value ~default:256
               in
               if eta <= evasive_action_limit
               then None
               else (
                 let our_pos =
                   Ship.next_pos_estimate
                     our_ship
                     ~acceleration:(get_acceleration our_commands)
                 in
                 let their_pos =
                   Ship.next_pos_estimate
                     their_ship
                     ~acceleration:(get_acceleration their_commands)
                 in
                 let distance = Vec2.radius our_pos their_pos in
                 if bad_distance_f distance
                 then None
                 else Some ((distance, cost, eta), velocity_change)))
      in
      if List.is_empty estimates
      then
        avoid_planet_in_a_fuel_efficient_way ~pos:our_ship.pos ~velocity:our_ship.velocity
      else (
        let estimates =
          List.sort
            estimates
            ~compare:(fun ((dist1, cost1, eta1), _) ((dist2, cost2, eta2), _) ->
              let dist_cmp = cost_f dist1 dist2 in
              if dist_cmp <> 0
              then dist_cmp
              else (
                let cost_cmp = compare cost1 cost2 in
                if cost_cmp = 0 then compare eta2 eta1 else cost_cmp))
        in
        List.iteri estimates ~f:(fun i ((dist, cost, eta), change) ->
            printf
              !"ESTIMATE[%d]: dist %d cost %d eta %d vector %{sexp:Vec2.t}\n%!"
              i
              dist
              cost
              eta
              change);
        let (distance, cost, eta), best_velocity_change = List.hd_exn estimates in
        let vec = Vec2.sub (0, 0) best_velocity_change in
        printf
          !"CHOSEN: eta %d current distance %d coast distance: %d steering distance: %d, \
            cost %d, vec: %{sexp:Vec2.t}\n"
          eta
          current_distance
          (Vec2.radius
             (Ship.next_pos_estimate
                our_ship
                ~acceleration:(get_acceleration our_commands))
             (Ship.next_pos_estimate
                their_ship
                ~acceleration:(get_acceleration their_commands)))
          distance
          cost
          vec;
        vec))
  | _ -> 0, 0
;;

let maybe_detonate our_ship their_ship =
  match our_ship, their_ship with
  | Some (our_ship, our_commands), Some (their_ship, their_commands) ->
    (* Blast radius is 10, not knowing their command could introduce
       an error of 1, and not knowing our is another one, so at most
       we are wrong by 2 But we use 7, to be on the safe side. *)
    if Vec2.radius
         (Ship.next_pos_estimate our_ship ~acceleration:(get_acceleration our_commands))
         (Ship.next_pos_estimate
            their_ship
            ~acceleration:(get_acceleration their_commands))
       <= 7
    then Some (detonate_cmd ~ship_id:our_ship.id)
    else None
  | _ -> None
;;

let run ~server_url ~player_key ~api_key =
  printf "VERSION: %s\n\n" version;
  let player_key = maybe_create ~server_url ~api_key player_key in
  let info = join ~server_url ~api_key player_key in
  let decoys = ref 0 in
  match Game_info.stage info with
  | Finished -> ()
  | _ ->
    let info = start ~server_url ~api_key ~player_key ~role:info.role in
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
          | Some ({ id; role; pos; velocity; x5; _ }, _) ->
            printf
              !"CRASH ETA: %{sexp: int option} ticks\n"
              (Simulator.planet_crash_eta ~pos ~velocity ~max_ticks:256 ());
            List.filter_opt
              [ (* maybe_movement_command ~id ~pos ~velocity *)
                Some
                  (accelerate_cmd
                     ~ship_id:id
                     ~vector:(steering info.our_ship info.their_ship))
              ; (if Role.equal role Role.Defender || info.num_ships > 2
                    (* dont detonate if they have split *)
                then None
                else maybe_detonate info.our_ship info.their_ship)
              ; Option.map
                  info.their_ship
                  ~f:(fun (ship, (their_commands : Ship_command.t list)) ->
                    shoot_cmd
                      ~ship_id:id
                      ~target:
                        (Ship.next_pos_estimate
                           ship
                           ~acceleration:(get_acceleration their_commands))
                      ~x3:Big_int.one)
              ; (if Role.equal role Role.Defender
                    && Big_int.( > ) x5 Big_int.zero
                    && !decoys < 10
                then (
                  incr decoys;
                  print_endline "Splitting decoy!\n";
                  Some (split_decoy_cmd ~ship_id:id))
                else None)
              ]
        in
        let info = commands ~server_url ~api_key player_key cmds in
        match Game_info.stage info with
        | Finished -> ()
        | _ -> loop info
      in
      loop info)
;;
