let format_timestamp f =
  let buf = Buffer.create 0 in
  let formatter = Format.formatter_of_buffer buf in
  let timestamp = Option.get (Ptime.of_float_s f) in
  Ptime.pp_human () formatter timestamp;
  Buffer.contents buf

let svg ?(margin = 100.) ~width ~height ?(abscissa = "") ?(ordinate = "") ?x_min
    ?x_max ?y_min ?y_max points =
  let x_min', x_max', y_min', y_max' =
    let x, y = List.hd points in
    List.fold_left
      (fun (x_min', x_max', y_min', y_max') (x, y) ->
        (min x x_min', max x x_max', min y y_min', max y y_max'))
      (x, x, y, y) points
  in
  let x_min = Option.value x_min ~default:x_min' in
  let x_max = Option.value x_max ~default:x_max' in
  let y_min = Option.value y_min ~default:y_min' in
  let y_max = Option.value y_max ~default:y_max' in
  let ( ++ ) (x, y) (x', y') = (x +. x', y +. y') in
  let coord (x, y) =
    ( margin +. ((x -. x_min) /. (x_max -. x_min) *. width),
      margin +. height -. ((y -. y_min) /. (y_max -. y_min) *. height) )
  in
  let points =
    points |> List.sort (fun (x, _) (x', _) -> compare x x') |> List.map coord
  in
  let svg =
    SVG.create
      ~width:(width +. (2. *. margin))
      ~height:(height +. (2. *. margin))
      ()
  in
  SVG.line svg ~stroke:"black" (coord (x_min, y_min)) (coord (x_max, y_min));
  SVG.line svg ~stroke:"black" (coord (x_min, y_min)) (coord (x_min, y_max));
  let ticks = 5 in
  let tick = 10. in
  for i = 0 to ticks - 1 do
    let x = x_min +. (float i *. (x_max -. x_min) /. float ticks) in
    let y = y_min in
    SVG.line svg ~stroke:"black"
      (coord (x, y) ++ (0., -.tick /. 2.))
      (coord (x, y) ++ (0., tick /. 2.));
    SVG.text svg
      (coord (x, y))
      ~fill:"black" ~text_anchor:`Middle ~transform:"translate(0,20)"
      (format_timestamp x);
    let x = x_min in
    let y = y_min +. (float i *. (y_max -. y_min) /. float ticks) in
    SVG.line svg ~stroke:"black"
      (coord (x, y) ++ (-.tick /. 2., 0.))
      (coord (x, y) ++ (tick /. 2., 0.));
    SVG.text svg
      (coord (x, y))
      ~fill:"black" ~text_anchor:`End (Printf.sprintf "%.02f" y)
  done;
  (* Arrow heads *)
  (let x, y = coord (x_max, y_min) in
   SVG.polyline svg ~stroke:"black" ~fill:"none"
     [
       (x -. (tick /. 2.), y +. (tick /. 2.));
       (x, y);
       (x -. (tick /. 2.), y -. (tick /. 2.));
     ];
   let x, y = coord (x_min, y_max) in
   SVG.polyline svg ~stroke:"black" ~fill:"none"
     [
       (x -. (tick /. 2.), y +. (tick /. 2.));
       (x, y);
       (x +. (tick /. 2.), y +. (tick /. 2.));
     ]);
  if abscissa <> "" then
    SVG.text svg (coord (x_max, y_min)) ~fill:"black" abscissa;
  if ordinate <> "" then
    SVG.text svg
      (coord (x_min, y_max))
      ~fill:"black" ~text_anchor:`Middle ordinate;
  (* Actual plot *)
  SVG.polyline svg ~stroke:"red" ~stroke_width:2. ~fill:"none" points;
  SVG.to_string svg
