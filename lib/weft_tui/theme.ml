open Notty

(* Color palette for search terms *)
let term_colors = [|
  A.(fg lightred);
  A.(fg lightgreen);
  A.(fg lightyellow);
  A.(fg lightblue);
  A.(fg lightmagenta);
  A.(fg lightcyan);
  A.(fg lightwhite);
  A.(fg (rgb_888 ~r:255 ~g:165 ~b:0));  (* orange *)
|]

let term_color idx =
  term_colors.(idx mod Array.length term_colors)

(* Source status indicators *)
let connected_indicator = A.(fg lightgreen)
let disconnected_indicator = A.(fg lightred)
let reconnecting_indicator = A.(fg lightyellow)

let source_status_attr = function
  | Weft_types.Connected -> connected_indicator
  | Weft_types.Disconnected -> disconnected_indicator
  | Weft_types.Reconnecting -> reconnecting_indicator
  | Weft_types.Failed _ -> disconnected_indicator

let source_status_char = function
  | Weft_types.Connected -> "●"
  | Weft_types.Disconnected -> "○"
  | Weft_types.Reconnecting -> "◌"
  | Weft_types.Failed _ -> "✗"

(* UI element styling *)
let title_attr = A.(st bold)
let selected_attr = A.(st reverse)
let dim_attr = A.(fg lightblack)
let border_attr = A.(fg lightblack)
let search_bar_attr = A.(fg lightwhite ++ st bold)
let timestamp_attr = A.(fg lightblack)
let source_tag_attr = A.(fg lightcyan)
let detail_label_attr = A.(fg lightyellow)
let progress_bar_attr = A.(fg lightgreen)

(* Box drawing *)
let hline width =
  I.string border_attr (String.make width '-')

let vline height =
  I.vcat (List.init height (fun _ -> I.string border_attr "|"))
