open Core

let create_msg = Encode.(encode (of_eval_exn (Eval.encode_list [ 1; 0 ])))

let maybe_create ~api_key player_key =
  if String.equal player_key "ATTACK" || String.equal player_key "DEFEND"
  then (
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
      printf !"ATTACK KEY: %{Eval#mach}\n" attack_key;
      printf !"DEFEND KEY: %{Eval#mach}\n" defend_key;
      match player_key with
      | "ATTACK" -> Eval.to_string_mach attack_key
      | "DEFEND" -> Eval.to_string_mach defend_key
      | _ -> assert false))
  else player_key
;;

let run ~server_url:_ ~player_key ~api_key =
  let player_key = maybe_create ~api_key player_key in
  let _ = player_key in
  ()
;;
