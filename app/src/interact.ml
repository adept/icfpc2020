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
  printf !"res = %{sexp:Eval.t}\n%!" res;
  let flag = Eval.car res in
  let newState = Eval.car (Eval.cdr res) in
  let vector = Eval.car (Eval.cdr (Eval.cdr res)) in
  printf !"res = %{sexp: Eval.t}\n%!" res;
  printf !"flag = %{Eval#hum}\n%!" flag;
  match flag with
  | Num d when Eval.Big_int.is_zero d ->
    printf !"newState = %{Eval#hum}\n%!" newState;
    printf
      !"vector = %{sexp:(Eval.Big_int.t * Eval.Big_int.t) list list}\n%!"
      (Eval.decode_vector vector);
    (* TODO: multipledraw vector *)
    (* TODO: choose where to click *)
    let clicked =
      Eval.(App (App (Var "cons", Num Eval.Big_int.zero), Num Eval.Big_int.zero))
    in
    interact ~protocol ~state:newState ~vector:clicked ~eval
  | _ ->
    (* TODO : send *)
    interact ~protocol ~state ~vector ~eval
;;

let run ~filename ~protocol ~state ~vector =
  let defs = Eval.load_defs_exn ~filename in
  let p str = Eval.parse_exn (String.split ~on:' ' str) in
  let eval = Eval.eval_custom ~verbose:true ~defs |> Staged.unstage in
  interact ~protocol:(p protocol) ~state:(p state) ~vector:(p vector) ~eval
;;
