let () =
  let update = ref false in
  let dir = ref "/tmp/metrics" in
  let fname = ref "metrics.html" in
  Arg.parse
    [
      "-i", Arg.Set_string dir, "Set directory containing metrics";
      "-o", Arg.Set_string fname, "Set output file name";
      "--update", Arg.Set update, "Update data"
    ]
    (fun _ -> ()) "metrics [option]";
  Metrics.load_liquidsoap ~update:!update ~directory:!dir ();
  let oc = open_out !fname in
  let w fmt = Printf.ksprintf (fun s -> output_string oc (s^"\n")) fmt in
  w "<html>";
  w "<head>";
  w "</head>";
  w "<body>";
  w "<h1>Liquidsoap metrics</h1>";
  List.iter
    (fun (name, _category, _unit, data) ->
       Printf.printf "Plotting '%s'\n" name;
       w "<h2>%s</h2>" name;
       w "%s" (Plot.svg ~width:600. ~height:400. data)
    )
    (Metrics.series ());
  w "</body>";
  w "</html>";
