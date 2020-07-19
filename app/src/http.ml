open! Core
open Lwt
open Cohttp
open Cohttp_lwt_unix

let rec response_ok_exn server (resp, body) =
  body
  |> Cohttp_lwt.Body.to_string
  >>= fun body ->
  match Response.status resp with
  | `OK -> return body
  | `Found ->
    (match Header.get_location (Response.headers resp) with
    | None -> failwith "Got 302, but no Location"
    | Some uri ->
      printf "Got 302, following to %s%s\n%!" server (Uri.to_string uri);
      Client.get (Uri.of_string (sprintf !"%s%{Uri}" server uri))
      >>= response_ok_exn server)
  | status ->
    printf "Unexpected server response:\n";
    printf "HTTP code: %d\n" (Code.code_of_status status);
    printf "Response body: %s\n" body;
    exit 2
;;

let send_post_exn str ~server ~uri =
  printf "POSTing to server %s\n" str;
  Lwt_main.run
    (Client.post
       ~body:(Cohttp_lwt.Body.of_string str)
       ~headers:
         (Cohttp.Header.of_list
            [ "Content-Length", string_of_int (String.length str)
            ; "Content-Type", "text/plain"
            ])
       (Uri.of_string (sprintf "%s/%s" server uri))
    >>= response_ok_exn server)
;;

let decode_response_ok_exn response =
  printf "Received: %s\n%!" response;
  match Encode.decode response with
  | Ok (res, _leftover) -> res
  | Error err -> failwithf !"Error decoding %s: %{Error#hum}" response err ()
;;

let proxy_url = "https://icfpc2020-api.testkontur.ru"

let send_api_exn ~api_key ~method_path payload =
  send_post_exn
    payload
    ~server:proxy_url
    ~uri:(sprintf "%s?apiKey=%s" method_path api_key)
  |> decode_response_ok_exn
;;
