open! Core
open Lwt
open Cohttp
open Cohttp_lwt_unix

let send_post str ~server_url =
  printf "POSTing to server %s\n" str;
  Client.post
    ~body:(Cohttp_lwt.Body.of_string str)
    ~headers:
      (Cohttp.Header.of_list [ "Content-Length", string_of_int (String.length str) ])
    (Uri.of_string server_url)
;;

let response_ok_exn (resp, body) =
  body
  |> Cohttp_lwt.Body.to_string
  >>= fun body ->
  match Response.status resp with
  | `OK -> return body
  | status ->
    printf "Unexpected server response:\n";
    printf "HTTP code: %d\n" (Code.code_of_status status);
    printf "Response body: %s\n" body;
    exit 2
;;

let run ~server_url ~player_key =
  printf "ServerUrl: %s; PlayerKey: %s\n" server_url player_key;
  Lwt_main.run
    (send_post player_key ~server_url
    >>= response_ok_exn
    >>= fun body ->
    printf "Server response 1: %s\n" body;
    send_post (player_key ^ player_key) ~server_url
    >>= response_ok_exn
    >>= fun body ->
    printf "Server response 2: %s\n" body;
    return ())
;;

let commands =
  let open Command.Let_syntax in
  Command.group
    ~summary:"Solution to ICFP Contest 2020"
    [ ( "run"
      , Command.basic
          ~summary:"Solve the problem"
          (let%map_open server_url = anon ("SERVER-URL" %: string)
           and player_key = anon ("PLAYER-KEY" %: string) in
           fun () -> run ~server_url ~player_key) )
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
    ]
;;
