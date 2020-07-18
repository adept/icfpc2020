open! Core
module G = Graphics

let pixel_size = 5
let pixel_shift = 16

let put_pixel (x, y) =
  G.set_color G.black;
  G.fill_rect
    ((pixel_shift + x) * pixel_size)
    ((pixel_shift + y) * pixel_size)
    pixel_size
    pixel_size
;;

let draw_picture pic = List.iter ~f:put_pixel pic
let draw_pictures pics = List.iter ~f:draw_picture pics

let get_click () =
  printf "waiting for click\n";
  let status = G.wait_next_event [ G.Button_down ] in
  let x = (status.G.mouse_x / pixel_size) - pixel_shift in
  let y = (status.G.mouse_y / pixel_size) - pixel_shift in
  printf "clicked: (%d,%d) => (%d,%d)\n%!" status.G.mouse_x status.G.mouse_y x y;
  x, y
;;

let continue () =
  G.moveto 10 10;
  G.set_color G.black;
  G.draw_string "Click to continue";
  let _status = G.wait_next_event [ G.Button_down ] in
  G.clear_graph ()
;;

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
  continue ();
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
    let pictures = Eval.decode_vector vector in
    printf !"pictures = %{sexp:(int*int) list list}\n%!" pictures;
    G.clear_graph ();
    draw_pictures pictures;
    let x, y = get_click () in
    let clicked =
      Eval.(
        App
          ( App (Var "cons", Num (Big_int_Z.big_int_of_int x))
          , Num (Big_int_Z.big_int_of_int y) ))
    in
    interact ~protocol ~state:newState ~vector:clicked ~eval
  | _ ->
    (* (\* TODO : send *\)
     * interact ~protocol ~state ~vector ~eval *)
    failwith "flag = 1 not implemented"
;;

let run ~filename ~protocol ~state ~vector =
  G.open_graph " 1000x1000";
  let defs = Eval.load_defs_exn ~filename in
  let p str = Eval.parse_exn (String.split ~on:' ' str) in
  let eval = Eval.eval_custom ~verbose:true ~defs |> Staged.unstage in
  interact ~protocol:(p protocol) ~state:(p state) ~vector:(p vector) ~eval
;;
