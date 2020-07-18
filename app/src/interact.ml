open! Core

let rec interact ~protocol ~state ~vector ~eval =
  printf
    !"\n\
      Interact:\n\
     \  protocol: %{Eval#hum}\n\
     \  state: %{Eval#hum}\n\
     \  vector:  %{Eval#hum}\n\
      %!"
    protocol
    state
    vector;
  printf "\nContinue? (Ctrl-C to quit)\n%!";
  let (_ : string) = In_channel.input_line_exn In_channel.stdin in
  let res = eval Eval.(App (App (protocol, state), vector)) in
  let flag = Eval.car res in
  let newState = Eval.car (Eval.cdr res) in
  let vector = Eval.cdr (Eval.cdr res) in
  printf !"res = %{sexp: Eval.t}\n%!" res;
  printf !"flag = %{Eval#hum}\n%!" flag;
  match flag with
  | Num 0 ->
    (* TODO: multipledraw *)
    printf !"newState = %{Eval#hum}\n%!" newState;
    printf !"vector = %{Eval#hum}\n%!" vector
  | _ ->
    (* TODO : send *)
    interact ~protocol ~state ~vector ~eval
;;

let run ~filename ~protocol ~state ~vector =
  let defs = Eval.load_defs_exn ~filename in
  let p str = Eval.parse_exn (String.split ~on:' ' str) in
  let eval = Eval.eval_custom ~verbose:false ~defs |> Staged.unstage in
  interact ~protocol:(p protocol) ~state:(p state) ~vector:(p vector) ~eval
;;
