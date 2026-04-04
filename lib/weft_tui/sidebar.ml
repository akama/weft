open Notty
open Weft_types

type t = {
  mutable sources : (source_id * source_status) list;
  mutable cache_size_mb : int;
  mutable cache_segments : int;
  mutable cache_time_range : string;
}

let create () =
  { sources = []; cache_size_mb = 0; cache_segments = 0;
    cache_time_range = "" }

let update_sources t sources = t.sources <- sources
let update_cache_info t ~size_mb ~segments ~time_range =
  t.cache_size_mb <- size_mb;
  t.cache_segments <- segments;
  t.cache_time_range <- time_range

let render_sources t ~width =
  let title = I.string Theme.title_attr "SOURCES" in
  let source_lines = List.map (fun (name, status) ->
    let indicator = Theme.source_status_char status in
    let attr = Theme.source_status_attr status in
    let max_name = width - 3 in
    let display_name = if String.length name > max_name then
      String.sub name 0 max_name
    else name in
    I.string attr (Printf.sprintf " %s %s" indicator display_name)
  ) t.sources in
  I.vcat (title :: source_lines)

let render_terms ~(terms : search_term list) ~width =
  let title = I.string Theme.title_attr "TERMS" in
  let term_lines = List.map (fun (st : search_term) ->
    let badge = if st.enabled then "■" else "□" in
    let attr = Theme.term_color st.color_idx in
    let max_name = width - 3 in
    let display = if String.length st.term > max_name then
      String.sub st.term 0 max_name
    else st.term in
    I.string attr (Printf.sprintf " %s %s" badge display)
  ) terms in
  I.vcat (title :: term_lines)

let render_cache t ~width =
  let title = I.string Theme.title_attr "CACHE" in
  let _ = width in
  let lines = [
    I.string Theme.dim_attr (Printf.sprintf " %d MB" t.cache_size_mb);
    I.string Theme.dim_attr (Printf.sprintf " %d segments" t.cache_segments);
    I.string Theme.dim_attr (Printf.sprintf " %s" t.cache_time_range);
  ] in
  I.vcat (title :: lines)

let render t ~terms ~width ~height =
  let sources = render_sources t ~width in
  let spacer = I.string A.empty "" in
  let terms_view = render_terms ~terms ~width in
  let cache = render_cache t ~width in
  let content = I.vcat [sources; spacer; terms_view; spacer; cache] in
  content |> I.vsnap ~align:`Top height |> I.hsnap ~align:`Left width
