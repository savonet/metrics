let () =
  let fname = ref "metrics.html" in
  Arg.parse
    [
      "-o", Arg.Set_string fname, "Set output file name"
    ]
    (fun _ -> ()) "metrics [option]";
  Metrics.load_liquidsoap ~directory:"/tmp/metrics" ();
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
