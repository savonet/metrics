let svg ~width ~height _points =
  let svg = SVG.create ~width ~height () in
  SVG.to_string svg
