open! Core
module G = Graphics

let pixel_size = 4

let put_pixel (shift_x, shift_y) (x, y) =
  G.fill_rect
    ((shift_x + x) * pixel_size)
    ((shift_y - y) * pixel_size)
    pixel_size
    pixel_size
;;

let draw_picture pixel_shift pic = List.iter ~f:(put_pixel pixel_shift) pic

(* i-th gray *)
let color color_step i = G.rgb (i * color_step) (i * color_step) (i * color_step)

let compute_pixel_shift pics =
  let min_x, min_y =
    List.fold ~init:(0, 0) pics ~f:(fun acc pic ->
        List.fold ~init:acc pic ~f:(fun (min_x, min_y) (x, y) -> min x min_x, min y min_y))
  in
  -min_x + 5, -min_y + 5
;;

let draw_pictures pixel_shift pics =
  let color_step = 250 / List.length pics in
  List.iteri (List.rev pics) ~f:(fun i pic ->
      G.set_color (color color_step i);
      draw_picture pixel_shift pic)
;;

let get_click (shift_x, shift_y) () =
  G.moveto 10 10;
  G.set_color G.black;
  G.draw_string "Click on pixel";
  printf "waiting for click\n%!";
  let status = G.wait_next_event [ G.Button_down ] in
  let x = (status.G.mouse_x / pixel_size) - shift_x in
  let y = -((status.G.mouse_y / pixel_size) - shift_y) in
  printf "clicked: (%d,%d) => (%d,%d)\n%!" status.G.mouse_x status.G.mouse_y x y;
  G.moveto 10 10;
  G.set_color G.white;
  G.draw_string "Click on pixel";
  G.moveto 10 10;
  G.set_color G.black;
  G.draw_string "Computing...";
  x, y
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
  let res = eval Eval.(app (app protocol state) vector) in
  let flag = Eval.car res in
  let newState = Eval.car (Eval.cdr res) in
  let vector = Eval.car (Eval.cdr (Eval.cdr res)) in
  printf !"res = %{Eval#hum}\n%!" res;
  printf !"flag = %{Eval#hum}\n%!" flag;
  match flag.u with
  | App _ -> raise_s [%sexp "Unexpected flag value"]
  | Var x ->
    (match Eval.Big_int.is_zero (Eval.Big_int.big_int_of_string x) with
    | true ->
      printf !"newState = %{Eval#hum}\n%!" newState;
      let pictures = Eval.decode_vector vector in
      printf !"pictures = %{sexp:(int*int) list list}\n%!" pictures;
      G.clear_graph ();
      let pixel_shift = compute_pixel_shift pictures in
      draw_pictures pixel_shift pictures;
      let x, y = get_click pixel_shift () in
      let clicked =
        Eval.(app (app (var "cons") (var (Int.to_string x))) (var (Int.to_string y)))
      in
      interact ~protocol ~state:newState ~vector:clicked ~eval
    | false | (exception _) ->
      printf "\nPOSTing... Continue? (Ctrl-C to quit)\n%!";
      let (_ : string) = In_channel.input_line_exn In_channel.stdin in
      (* (\* TODO : send *\)
       * interact ~protocol ~state ~vector ~eval *)
      failwith "flag = 1 not implemented")
;;

let run ~filename ~protocol ~state ~vector =
  G.open_graph " 1000x1000";
  G.set_window_title "Messages From Space";
  let defs = Eval.load_defs_exn ~filename in
  let p str = Eval.parse_exn (String.split ~on:' ' str) in
  let eval = Eval.eval_custom ~verbose:false ~defs in
  interact ~protocol:(p protocol) ~state:(p state) ~vector:(p vector) ~eval
;;
