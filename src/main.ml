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
  w "<!DOCTYPE html>";
  w "<html>";
  w "<head>";
  w "<title>Liquidsoap metrics</title>";
  w "<link rel=\"stylesheet\" href=\"metrics.css\">";
  w "</head>";
  w "<body>";
  w "<h1>Liquidsoap metrics</h1>";
  List.iter
    (fun (name, _category, _unit, data) ->
       Printf.printf "Plotting '%s'\n" name;
       w "<h2>%s</h2>" (String.capitalize_ascii name);
       w "<div class=\"plot\">";
       w "%s" (Plot.svg ~width:800. ~height:400. data);
       w "</div>"
    )
    (Metrics.series ());
  w "</body>";
  w "</html>";
