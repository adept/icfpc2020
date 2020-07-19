open! Core

let ping ~api_key =
  let open Encode in
  let response =
    Http.send_api_exn
      ~api_key
      ~method_path:"aliens/send"
      (encode (Cons (Number Big_int.zero, Nil)))
  in
  printf !"Server response: %{sexp: Encode.t}\n" response
;;

let commands =
  let open Command.Let_syntax in
  Command.group
    ~summary:"Solution to ICFP Contest 2020"
    [ ( "ping"
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
           and api_key =
             flag
               "-api-key"
               (required string)
               ~doc:"API-KEY API-KEY for identifying with the server"
           in
           fun () -> Interact.run ~filename ~protocol ~state ~vector ~api_key) )
    ; ( "battle"
      , Command.basic
          ~summary:"Connect to the tournament server"
          (let%map_open server_url = anon ("SERVER-URL" %: string)
           and player_key = anon ("PLAYER-KEY" %: string)
           and api_key =
             flag
               "-api-key"
               (required string)
               ~doc:"API-KEY API-KEY for identifying with the server"
           in
           fun () -> Battle.run ~api_key ~server_url ~player_key) )
    ]
;;
