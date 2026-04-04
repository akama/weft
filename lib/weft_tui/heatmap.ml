open Notty
open Weft_types

(* Render a time-based heatmap showing log density across the time range. *)

let density_char count max_count =
  if max_count = 0 || count = 0 then ' '
  else
    let ratio = float_of_int count /. float_of_int max_count in
    if ratio < 0.25 then '.'
    else if ratio < 0.50 then ':'
    else if ratio < 0.75 then '#'
    else '@'

let density_attr count max_count =
  if max_count = 0 || count = 0 then A.empty
  else
    let ratio = float_of_int count /. float_of_int max_count in
    if ratio < 0.15 then A.(fg lightblack)
    else if ratio < 0.30 then A.(fg blue)
    else if ratio < 0.50 then A.(fg lightblue)
    else if ratio < 0.70 then A.(fg lightyellow)
    else if ratio < 0.85 then A.(fg yellow)
    else A.(fg lightred)

let format_hm_time t =
  let (_, ((hh, mm, _), _)) = Ptime.to_date_time t in
  Printf.sprintf "%02d:%02d" hh mm

let make_bar buckets max_count n =
  List.init n (fun i ->
    let c = density_char buckets.(i) max_count in
    I.string (density_attr buckets.(i) max_count) (String.make 1 c)
  )

let render ~entries ~sources ~width ~height =
  let entries : log_entry array = entries in
  let sources : (source_id * _) list = sources in
  let n = Array.length entries in
  if n = 0 then
    I.string A.(fg lightblack) "  No entries to visualize"
    |> I.hsnap ~align:`Left width
    |> I.vsnap ~align:`Middle height
  else
    let source_names = List.map fst sources in
    let n_sources = List.length source_names in
    let min_t = ref entries.(0).timestamp in
    let max_t = ref entries.(0).timestamp in
    Array.iter (fun (e : log_entry) ->
      if Ptime.is_earlier e.timestamp ~than:!min_t then min_t := e.timestamp;
      if Ptime.is_later e.timestamp ~than:!max_t then max_t := e.timestamp
    ) entries;

    let lw = 10 in
    let cw = max 10 (width - lw - 4) in
    let total_s = max 1.0 (Ptime.diff !max_t !min_t |> Ptime.Span.to_float_s) in
    let bucket_s = total_s /. float_of_int cw in

    (* Bucket entries *)
    let per_src = Array.init n_sources (fun _ -> Array.make cw 0) in
    let total = Array.make cw 0 in
    Array.iter (fun (e : log_entry) ->
      let off = Ptime.diff e.timestamp !min_t |> Ptime.Span.to_float_s in
      let b = min (cw - 1) (max 0 (int_of_float (off /. bucket_s))) in
      total.(b) <- total.(b) + 1;
      List.iteri (fun i name ->
        if name = e.source then
          per_src.(i).(b) <- per_src.(i).(b) + 1
      ) source_names
    ) entries;

    let gmax = Array.fold_left max 0 total in

    (* Count sources with entries *)
    let n_active = List.init n_sources (fun si ->
      Array.fold_left (+) 0 per_src.(si)) |> List.filter (fun c -> c > 0)
      |> List.length in
    let title = I.string A.(st bold)
      (Printf.sprintf "  Log Density: %s -> %s  (%d entries across %d sources)"
         (format_hm_time !min_t) (format_hm_time !max_t) n n_active) in

    let total_label = I.string A.(fg lightcyan) (Printf.sprintf " %-*s" lw "TOTAL") in
    let total_bar = I.hcat (total_label :: make_bar total gmax cw) in

    let sep_line = I.string A.(fg lightblack)
      (String.make (lw + 1) ' ' ^ String.make (min cw (width - lw - 2)) '-') in

    let avail = max 0 (height - 6) in
    (* Only show sources that have entries *)
    let active_sources = List.init n_sources (fun si ->
      let count = Array.fold_left (+) 0 per_src.(si) in
      (si, List.nth source_names si, count)
    ) |> List.filter (fun (_, _, count) -> count > 0) in
    let shown = min (List.length active_sources) avail in
    let src_rows = List.filteri (fun i _ -> i < shown) active_sources
      |> List.map (fun (si, name, _count) ->
      let short = if String.length name > lw - 1 then
        String.sub name 0 (lw - 1) else name in
      let label = I.string A.(fg lightblack) (Printf.sprintf " %-*s" lw short) in
      let smax = max 1 (Array.fold_left max 0 per_src.(si)) in
      I.hcat (label :: make_bar per_src.(si) smax cw)
    ) in

    (* Time axis — show evenly spaced labels *)
    let n_ticks = min 8 (cw / 10) in
    let tick_iv = if n_ticks > 0 then cw / n_ticks else cw in
    let axis_str = Bytes.make cw ' ' in
    for tick = 0 to n_ticks do
      let col = min (cw - 1) (tick * tick_iv) in
      let off = float_of_int col *. bucket_s in
      let span = Option.value ~default:Ptime.Span.zero
        (Ptime.Span.of_float_s off) in
      let t = Option.value ~default:!min_t (Ptime.add_span !min_t span) in
      let label = format_hm_time t in
      let label_len = min (String.length label) (cw - col) in
      Bytes.blit_string label 0 axis_str col label_len
    done;
    let axis_label = I.string A.empty (String.make (lw + 1) ' ') in
    let axis = I.hcat [axis_label;
      I.string A.(fg lightblack) (Bytes.to_string axis_str)] in

    let legend = I.string A.(fg lightblack)
      "  .=low :=med #=high @=peak  |  H=close  </>:pan  -/+:zoom  r=reset" in

    let content = I.vcat ([title; I.empty; total_bar; sep_line] @
      src_rows @ [sep_line; axis; I.empty; legend]) in
    content |> I.vsnap ~align:`Top height |> I.hsnap ~align:`Left width
