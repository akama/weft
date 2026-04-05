open Notty
open Weft_types

type t = {
  mutable sources : (source_id * source_status) list;
  mutable disabled_sources : source_id list;
  mutable selected_source : int;
  mutable selected_term : int;
  mutable cache_size_mb : int;
  mutable cache_segments : int;
  mutable cache_from_date : string;
  mutable cache_from_time : string;
  mutable cache_to_date : string;
  mutable cache_to_time : string;
}

let create () =
  { sources = []; disabled_sources = [];
    selected_source = 0; selected_term = 0;
    cache_size_mb = 0; cache_segments = 0;
    cache_from_date = ""; cache_from_time = "";
    cache_to_date = ""; cache_to_time = "" }

let update_sources t sources = t.sources <- sources
let update_cache_info t ~size_mb ~segments
    ~from_date ~from_time ~to_date ~to_time =
  t.cache_size_mb <- size_mb;
  t.cache_segments <- segments;
  t.cache_from_date <- from_date;
  t.cache_from_time <- from_time;
  t.cache_to_date <- to_date;
  t.cache_to_time <- to_time

let source_count t = List.length t.sources
let term_count terms = List.length terms

let move_source_selection t delta =
  let n = source_count t in
  if n > 0 then
    t.selected_source <- (t.selected_source + delta + n) mod n

let move_term_selection t ~terms delta =
  let n = term_count terms in
  if n > 0 then
    t.selected_term <- (t.selected_term + delta + n) mod n

let toggle_selected_source t =
  match List.nth_opt t.sources t.selected_source with
  | None -> None
  | Some (sid, _) ->
    if List.mem sid t.disabled_sources then
      t.disabled_sources <- List.filter (fun s -> s <> sid) t.disabled_sources
    else
      t.disabled_sources <- sid :: t.disabled_sources;
    Some sid

let is_source_enabled t sid =
  not (List.mem sid t.disabled_sources)

let isolate_selected_source t =
  match List.nth_opt t.sources t.selected_source with
  | None -> None
  | Some (sid, _) ->
    let all_sids = List.map fst t.sources in
    t.disabled_sources <- List.filter (fun s -> s <> sid) all_sids;
    Some sid

let enable_all_sources t =
  t.disabled_sources <- []

let selected_term_name t ~(terms : search_term list) =
  List.nth_opt terms t.selected_term
  |> Option.map (fun (st : search_term) -> st.term)

let render_sources t ~width ~is_focused =
  let title = I.string Theme.title_attr "SOURCES" in
  let source_lines = List.mapi (fun i (name, status) ->
    let indicator = Theme.source_status_char status in
    let base_attr = Theme.source_status_attr status in
    let enabled = is_source_enabled t name in
    let attr = if not enabled then Theme.dim_attr
      else base_attr in
    let selected = is_focused && i = t.selected_source in
    let sel_attr = if selected then A.(attr ++ st reverse) else attr in
    let max_name = width - 3 in
    let display_name = if String.length name > max_name then
      String.sub name 0 max_name
    else name in
    I.string sel_attr (Printf.sprintf " %s %s" indicator display_name)
  ) t.sources in
  I.vcat (title :: source_lines)

let render_terms ~(terms : search_term list) ~selected ~width ~is_focused =
  let title = I.string Theme.title_attr "TERMS" in
  let term_lines = List.mapi (fun i (st : search_term) ->
    let badge = if st.enabled then "■" else "□" in
    let base_attr = Theme.term_color st.color_idx in
    let attr = if not st.enabled then Theme.dim_attr else base_attr in
    let sel = is_focused && i = selected in
    let sel_attr = if sel then A.(attr ++ st reverse) else attr in
    let max_name = width - 3 in
    let display = if String.length st.term > max_name then
      String.sub st.term 0 max_name
    else st.term in
    I.string sel_attr (Printf.sprintf " %s %s" badge display)
  ) terms in
  I.vcat (title :: term_lines)

let render_cache t ~width =
  let title = I.string Theme.title_attr "CACHE" in
  ignore width;
  let size_str = if t.cache_size_mb > 0 then
    Printf.sprintf " %d MB" t.cache_size_mb
  else " <1 MB" in
  let lines = [
    I.string Theme.dim_attr size_str;
    I.string Theme.dim_attr (Printf.sprintf " %d segs" t.cache_segments);
  ] in
  let range_lines = if t.cache_from_date <> "" then [
    I.string Theme.dim_attr " from:";
    I.string Theme.dim_attr (Printf.sprintf "  %s" t.cache_from_date);
    I.string Theme.dim_attr (Printf.sprintf "  %s" t.cache_from_time);
    I.string Theme.dim_attr " to:";
    I.string Theme.dim_attr (Printf.sprintf "  %s" t.cache_to_date);
    I.string Theme.dim_attr (Printf.sprintf "  %s" t.cache_to_time);
  ] else [] in
  I.vcat (title :: lines @ range_lines)

let render t ~terms ~width ~height ~focus =
  let sources = render_sources t ~width ~is_focused:(focus = `Sources) in
  let spacer = I.string A.empty "" in
  let terms_view = render_terms ~terms ~selected:t.selected_term
    ~width ~is_focused:(focus = `Terms) in
  let cache = render_cache t ~width in
  let content = I.vcat [sources; spacer; terms_view; spacer; cache] in
  content |> I.vsnap ~align:`Top height |> I.hsnap ~align:`Left width
