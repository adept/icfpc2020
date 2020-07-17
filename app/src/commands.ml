open! Core
open Lwt
open Cohttp
open Cohttp_lwt_unix

let something_to_test = "it works!"

let run ~server_url ~player_key =
  printf "ServerUrl: %s; PlayerKey: %s\n" server_url player_key;
  Lwt_main.run
    ( Client.post
        ~body:(Cohttp_lwt.Body.of_string player_key)
        ~headers:
          (Cohttp.Header.of_list
             [ ("Content-Length", string_of_int (String.length player_key)) ])
        (Uri.of_string server_url)
    >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      match Response.status resp with
      | `OK -> printf "Server response: %s\n" body
      | status ->
          printf "Unexpected server response:\n";
          printf "HTTP code: %d\n" (Code.code_of_status status);
          printf "Response body: %s\n" body;
          exit 2 )
;;

let commands =
  let open Command.Let_syntax in
  Command.group ~summary:"Solution to ICFP Contest 2020"
    [
      ( "run",
        Command.basic ~summary:"Solve the problem"
          (let%map_open server_url = anon ("SERVER-URL" %: string)
           and player_key = anon ("PLAYER-KEY" %: string) in
           fun () -> run ~server_url ~player_key) );
    ]
;;
