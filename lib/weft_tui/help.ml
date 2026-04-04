open Notty

let render ~width ~height =
  let lines = [
    (A.(st bold), "Weft — Unified Log Search");
    (A.empty, "");
    (A.(fg lightyellow), "Navigation");
    (A.empty, "  j/k, Up/Down   Scroll timeline (or sidebar list)");
    (A.empty, "  Tab            Cycle focus: Sources → Terms → Timeline");
    (A.empty, "  Enter          Toggle detail pane");
    (A.empty, "  q              Quit");
    (A.empty, "");
    (A.(fg lightyellow), "Search");
    (A.empty, "  /              Add a search term");
    (A.empty, "  d              Delete selected term");
    (A.empty, "  s              Toggle selected source on/off");
    (A.empty, "  t              Toggle selected term visibility");
    (A.empty, "  i              Isolate term (disable all others)");
    (A.empty, "  I              Restore all terms (enable all)");
    (A.empty, "");
    (A.(fg lightyellow), "Time Range");
    (A.empty, "  < , >          Shift window earlier / later");
    (A.empty, "  - +            Narrow / widen window");
    (A.empty, "  r              Reset to full time range");
    (A.empty, "");
    (A.(fg lightyellow), "Views");
    (A.empty, "  o              Toggle sort order (asc/desc)");
    (A.empty, "  ?              Toggle this help screen");
    (A.empty, "  H              Toggle time heatmap overview");
    (A.empty, "");
    (A.(fg lightblack), "Press ? or Escape to close");
  ] in
  let rendered = List.map (fun (attr, text) ->
    let safe = String.map (fun c ->
      if Char.code c < 0x20 && c <> ' ' then ' ' else c
    ) text in
    I.string attr ("  " ^ safe)
  ) lines in
  let content = I.vcat rendered in
  (* Center in the available space *)
  let box_w = min 56 (width - 4) in
  let box_h = min (List.length lines + 2) (height - 2) in
  let padded = I.vsnap ~align:`Top box_h content in
  let padded = I.hsnap ~align:`Left box_w padded in
  (* Border using ASCII *)
  let border_line = String.make (box_w - 2) '-' in
  let top_border = I.string A.(fg lightblack) ("+" ^ border_line ^ "+") in
  let bot_border = I.string A.(fg lightblack) ("+" ^ border_line ^ "+") in
  let with_borders = I.vcat [top_border; padded; bot_border] in
  I.hsnap ~align:`Middle width with_borders
  |> I.vsnap ~align:`Middle height
