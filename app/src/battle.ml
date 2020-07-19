open Core

let create_msg = Encode.(encode (of_eval_exn (Eval.encode_list [ 1; 0 ])))

let run ~server_url:_ ~player_key ~api_key =
  let player_key =
    if String.equal player_key "ATTACK" || String.equal player_key "DEFEND"
    then (
      let response = Http.send_api_exn ~api_key ~method_path:"aliens/send" create_msg in
      printf !"CREATE RESP: %{Encode#mach}\n" response;
      let response = Encode.to_eval response in
      let status = Eval.(to_int_exn (car response)) in
      if not (Big_int.equal status Big_int.one)
      then failwithf !"CREATE failed %{Big_int}" status ()
      else (
        let attack_key = Eval.(cdr response |> car |> car |> cdr |> car) in
        let defend_key = Eval.(cdr response |> car |> cdr |> car |> cdr |> car) in
        printf !"ATTACK KEY: %{Eval#mach}\n" attack_key;
        printf !"DEFEND KEY: %{Eval#mach}\n" defend_key;
        player_key))
    else player_key
  in
  let _ = player_key in
  ()
;;
