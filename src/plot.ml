let svg ~width ~height ?x_min ?x_max ?y_min ?y_max points =
  let x_min', x_max', y_min', y_max' =
    let x, y = List.hd points in
    List.fold_left
      (fun (x_min', x_max', y_min', y_max') (x,y) ->
         min x x_min',
         max x x_max',
         min y y_min',
         max y y_max'
      )
      (x,x,y,y)
      points
  in
  let x_min = Option.value x_min ~default:x_min' in
  let x_max = Option.value x_max ~default:x_max' in
  let y_min = Option.value y_min ~default:y_min' in
  let y_max = Option.value y_max ~default:y_max' in
  let points =
    points
    |> List.sort (fun (x,_) (x',_) -> compare x x')
    |> List.map
      (fun (x,y) ->
         (x -. x_min) /. (x_max -. x_min) *. width,
         height -. ((y -. y_min) /. (y_max -. y_min) *. height)
      )
  in
  let svg = SVG.create ~width ~height () in
  SVG.polyline svg ~stroke:"black" ~fill:"none" points;
  SVG.to_string svg
