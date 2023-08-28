let () =
  Printf.printf "Testing svg...\n%!";
  let svg = SVG.create ~width:210. ~height:500. () in
  SVG.line svg ~stroke:"red" ~stroke_width:5. (0.,0.) (200.,200.);
  SVG.text svg ~fill:"blue" (20.,20.) ~transform:"rotate(30 20,40)" "hello!";
  SVG.polyline svg ~stroke:"green" ~fill:"none" [10.,10.;20.,80.;30.,20.;40.,120.];
  let f = open_out "svg.html" in
  output_string f (SVG.to_string svg);
  close_out f

let () =
  Printf.printf "Testing plots...\n%!";
  let s = Plot.svg ~width:500. ~height:400. [0.,100.;1.,80.;2.,70.;3.,110.] in
  let f = open_out "plot.html" in
  output_string f s;
  close_out f
  
let () =
  Printf.printf "Testing metrics...\n%!";
  if not (Sys.file_exists "liquidsoap") then
    assert (Sys.command "git clone -b metrics https://github.com/savonet/liquidsoap.git" = 0)
  else
    assert (Sys.command "cd liquidsoap && git pull" = 0);
  Metrics.load_dir "liquidsoap"
