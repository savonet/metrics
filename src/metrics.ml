type entry =
  {
    name : string;
    category : string;
    value : float;
    unit : string;
    time : float
  }

let parse_file fname =
  let ic = open_in fname in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let yaml = Yaml.yaml_of_string s |> Result.get_ok |> Yaml.to_json |> Result.get_ok in
  let yaml =
    match yaml with
    | `A l -> l
    | _ -> assert false
  in
  let yaml =
    List.map
      (function
        | `O l -> l
        | _ -> assert false
      ) yaml
  in
  let yaml =
    List.filter_map
      (fun l ->
         let string = function `String s -> s | _ -> assert false in
         let float = function `Float x -> x | _ -> assert false in
         let string k = List.assoc k l |> string in
         let float k = List.assoc k l |> float in
         if List.mem_assoc "commit" l then None else
           Some {name = string "name"; category = string "category"; value = float "value"; unit = string "unit"; time = float "time"}
      ) yaml
  in
  yaml

let db = ref []

let add_file fname =
  Printf.printf "Adding file %s\n%!" fname;
  db := parse_file fname @ !db

let load_dir dir =
  let files =
    Sys.readdir dir
    |> Array.to_list
    |> List.map (fun s -> dir ^ "/" ^ s)
    |> List.filter (fun s -> Filename.check_suffix s ".yaml")
  in
  List.iter add_file files

let load_liquidsoap ?(directory="/tmp/liquidsoap") () =
  if not (Sys.file_exists directory) then
    assert (Sys.command (Printf.sprintf "git clone -b metrics https://github.com/savonet/liquidsoap.git %s" directory) = 0)
  else
    assert (Sys.command (Printf.sprintf "cd %s && git pull" directory) = 0);
  load_dir directory

(** All the possible names for tests. *)
let names () =
  List.map (fun e -> e.name) !db
  |> List.sort_uniq compare

(** All metrics series. *)
let series () =
  List.map
    (fun n ->
       let l = List.filter (fun e -> e.name = n) !db in
       let e = List.hd l in
       let l = List.map (fun e -> e.time, e.value) l in
       let l = List.sort (fun (t,_) (t',_) -> compare t t') l in
       n, e.category, e.unit, l
    ) (names ())
