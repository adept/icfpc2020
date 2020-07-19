open! Core

val send_post_exn : string -> server:string -> uri:string -> string
val send_api_exn : api_key:string -> method_path:string -> string -> Encode.t
