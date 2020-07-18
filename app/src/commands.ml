open! Core
open Lwt
open Cohttp
open Cohttp_lwt_unix

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

let send_post_exn str ~server_url =
  printf "POSTing to server %s\n" str;
  Client.post
    ~body:(Cohttp_lwt.Body.of_string str)
    ~headers:
      (Cohttp.Header.of_list
         [ "Content-Length", string_of_int (String.length str)
         ; "Content-Type", "text/plain"
         ])
    (Uri.of_string server_url)
  >>= response_ok_exn
;;

let decode_response_ok_exn response =
  match Encode.decode response with
  | Ok (res, _leftover) -> return res
  | Error err -> failwithf !"Error decoding %s: %{Error#hum}" response err ()
;;

let run ~server_url ~player_key =
  printf "ServerUrl: %s; PlayerKey: %s\n" server_url player_key;
  Lwt_main.run
    (send_post_exn player_key ~server_url
    >>= fun body ->
    printf "Server response 1: %s\n" body;
    send_post_exn (player_key ^ player_key) ~server_url
    >>= fun body ->
    printf "Server response 2: %s\n" body;
    return ())
;;

let proxy_uri = "https://icfpc2020-api.testkontur.ru"

let send_api ~api_key method_path payload =
  send_post_exn
    payload
    ~server_url:(sprintf "%s/%s?apiKey=%s" proxy_uri method_path api_key)
;;

let ping ~api_key =
  let open Encode in
  Lwt_main.run
    (send_api ~api_key "aliens/send" (encode (Cons (Number 0, Nil)))
    >>= decode_response_ok_exn
    >>= fun response ->
    printf !"Server response: %{sexp: Encode.t}\n" response;
    return ())
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
    ]
;;
