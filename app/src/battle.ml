open Core

let create_msg = Encode.(encode (of_eval_exn (Eval.encode_list [ 1; 0 ])))

let run ~server_url:_ ~player_key ~api_key =
  let player_key =
    if String.equal player_key "CREATE"
    then (
      let response = Http.send_api_exn ~api_key ~method_path:"aliens/send" create_msg in
      printf !"CREATE RESP: %{Encode#mach}\n" response;
      player_key)
    else player_key
  in
  let _ = player_key in
  ()
;;
