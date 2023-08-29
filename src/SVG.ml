(* See
   https://www.w3schools.com/graphics/svg_intro.asp
   https://developer.mozilla.org/en-US/docs/Web/SVG
*)

type t = Buffer.t

let w (svg:t) f = Printf.ksprintf (fun s -> Buffer.add_string svg s; Buffer.add_char svg '\n') f

let prop name o = Option.fold ~none:"" ~some:(Printf.sprintf " %s=\"%s\"" name) o

let prop_int name o = Option.fold ~none:"" ~some:(Printf.sprintf " %s=\"%d\"" name) o

let prop_float name o = Option.fold ~none:"" ~some:(Printf.sprintf " %s=\"%f\"" name) o

let empty () : t = Buffer.create 0

let create ?width ?height () =
  let svg = empty () in
  let width = prop_float "width" width in
  let height = prop_float "height" height in
  w svg "<svg%s%s>" width height;
  svg

let line svg ?stroke ?stroke_width ?style (x0,y0) (x1,y1) =
  let stroke = prop "stroke" stroke in
  let stroke_width = prop_float "stroke-width" stroke_width in
  let style = prop "style" style in
  w svg {|  <line x1="%f" y1="%f" x2="%f" y2="%f"%s%s%s/>|} x0 y0 x1 y1 stroke stroke_width style

let text svg ?text_anchor ?fill ?transform (x,y) t =
  let text_anchor =
    match text_anchor with
    | Some `Start -> "start"
    | Some `Middle -> "middle"
    | Some `End -> "end"
    | None -> ""
  in
  let text_anchor = if text_anchor = "" then "" else Printf.sprintf {| text-anchor="%s"|} text_anchor in
  let fill = prop "fill" fill in
  let transform = prop "transform" transform in
  w svg {|  <text x="%f" y="%f"%s%s%s>%s</text>|} x y fill text_anchor transform t

let polyline svg ?stroke ?stroke_width ?fill p =
  let p =
    List.map (fun (x,y) -> Printf.sprintf "%f,%f" x y) p
    |> String.concat " "
  in
  let stroke = prop "stroke" stroke in
  let stroke_width = prop_float "stroke-width" stroke_width in
  let fill = prop "fill" fill in
  w svg {|  <polyline points="%s"%s%s%s/>|} p stroke stroke_width fill

let to_string (svg:t) =
  Buffer.contents svg ^ "</svg>\n"
