let () =
  Printf.printf "Testing svg...\n%!";
  let svg = SVG.create ~width:210 ~height:500 () in
  SVG.line svg ~stroke:"red" ~stroke_width:5 (0,0) (200,200);
  SVG.text svg ~fill:"blue" (20,20) ~transform:"rotate(30 20,40)" "hello!";
  let f = open_out "test.html" in
  output_string f (SVG.to_string svg);
  close_out f
