let () =
  let update = ref false in
  let dir = ref "/tmp/metrics" in
  let fname = ref "metrics.html" in
  let branch = ref "" in
  Arg.parse
    [
      "-i", Arg.Set_string dir, "Set directory containing metrics";
      "-o", Arg.Set_string fname, "Set output file name";
      "--branch", Arg.Set_string branch, "Set branch";
      "--update", Arg.Set update, "Update data"
    ]
    (fun _ -> ()) "metrics [option]";
  let branch = if !branch = "" then None else Some !branch in
  Metrics.load_liquidsoap ~update:!update ~directory:!dir ?branch ();
  let oc = open_out !fname in
  let w fmt = Printf.ksprintf (fun s -> output_string oc (s^"\n")) fmt in
  w "<!DOCTYPE html>";
  w "<html>";
  w "<head>";
  w "<title>Liquidsoap metrics</title>";
  w "<link rel=\"stylesheet\" href=\"style.css\">";
  w "</head>";
  w "<body>";
  w "<h1>Liquidsoap metrics</h1>";
  List.iter
    (fun (name, _category, unit, data) ->
       Printf.printf "Plotting '%s'\n" name;
       w "<h2>%s</h2>" (String.capitalize_ascii name);
       w "<div class=\"plot\">";
       let ordinate = Printf.sprintf "value (%s)" unit in
       w "%s" (Plot.svg ~abscissa:"time (s)" ~ordinate ~width:800. ~height:400. data);
       w "</div>"
    )
    (Metrics.series ());
  w "</body>";
  w "</html>";
