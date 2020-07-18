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
  printf !"res = %{Eval#hum}\n%!" res;
  interact ~protocol ~state ~vector ~eval
;;

let run ~filename =
  let defs = Eval.load_defs_exn ~filename in
  let p str = Eval.parse_exn (String.split ~on:' ' str) in
  let eval = Eval.eval_custom ~verbose:true ~defs |> Staged.unstage in
  interact ~protocol:(p "galaxy") ~state:(p "nil") ~vector:(p "ap ap cons 0 0") ~eval
;;
