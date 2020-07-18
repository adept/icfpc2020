open! Core

let run ~server_url ~player_key =
  printf "ServerUrl: %s; PlayerKey: %s\n" server_url player_key;
  let body = Http.send_post_exn player_key ~server_url in
  printf "Server response 1: %s\n" body;
  let body = Http.send_post_exn (player_key ^ player_key) ~server_url in
  printf "Server response 2: %s\n" body
;;

let ping ~api_key =
  let open Encode in
  let response =
    Http.send_api_exn ~api_key ~method_path:"aliens/send" (encode (Cons (Number 0, Nil)))
  in
  printf !"Server response: %{sexp: Encode.t}\n" response
;;

let commands =
  let open Command.Let_syntax in
  Command.group
    ~summary:"Solution to ICFP Contest 2020"
    [ ( "test"
      , Command.basic
          ~summary:"Test the build"
          (let%map_open server_url = anon ("SERVER-URL" %: string)
           and player_key = anon ("PLAYER-KEY" %: string) in
           fun () -> run ~server_url ~player_key) )
    ; ( "ping"
      , Command.basic
          ~summary:"Send ping to API server"
          (let%map_open api_key = anon ("API-KEY" %: string) in
           fun () -> ping ~api_key) )
    ; ( "decode"
      , Command.basic
          ~summary:"Decode the given string"
          (let%map_open str = anon ("STR" %: string)
           and mach =
             flag "-mach" no_arg ~doc:" Output in their machine readable format"
           in
           fun () ->
             match Encode.decode str with
             | Error err -> eprintf "Error: %s" (Error.to_string_hum err)
             | Ok (result, leftover) ->
               if mach
               then print_endline (Encode.to_string_mach result)
               else print_s (Encode.sexp_of_t result);
               printf "Leftover: '%s'\n" leftover) )
    ; ( "parse"
      , Command.basic
          ~summary:"Parse a file of definition like galaxy.txt"
          (let%map_open filename = anon ("FILE" %: Filename.arg_type) in
           fun () ->
             Map.iteri (Eval.load_defs_exn ~filename) ~f:(fun ~key:name ~data:expansion ->
                 printf "%10s := %s\n" name (Eval.to_string_hum expansion))) )
    ; ( "interact-galaxy"
      , Command.basic
          ~summary:"Interact with a galaxy.txt"
          (let%map_open filename = anon ("FILE" %: Filename.arg_type)
           and protocol =
             flag
               "-protocol"
               (optional_with_default "galaxy" string)
               ~doc:"TERM The protocol to run"
           and state =
             flag
               "-state"
               (optional_with_default "nil" string)
               ~doc:"TERM The initial state of the protocol"
           and vector =
             flag
               "-vector"
               (optional_with_default "ap ap cons 0 0" string)
               ~doc:"TERM The initial vector to give to the protocol"
           in
           fun () -> Interact.run ~filename ~protocol ~state ~vector) )
    ]
;;
