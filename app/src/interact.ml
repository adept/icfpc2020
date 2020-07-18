open! Core

let rec interact ~protocol ~state ~vector ~defs =
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
  let res =
    Eval.eval_custom Eval.(App (App (protocol, state), vector)) ~verbose:true ~defs
  in
  printf !"res = %{Eval#hum}\n%!" res;
  interact ~protocol ~state ~vector ~defs
;;

let run ~filename =
  let defs = Eval.load_defs_exn ~filename in
  let p str = Eval.parse_exn (String.split ~on:' ' str) in
  interact ~protocol:(p "galaxy") ~state:(p "nil") ~vector:(p "ap ap cons 0 0") ~defs
;;
