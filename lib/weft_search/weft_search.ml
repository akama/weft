open Weft_types

type source_adapter = {
  name : source_id;
  config : source_config;
  format_config : format_config option;
  pipeline : Weft_middleware.Pipeline.t option;
  has_multiline : bool;
}

type t = {
  term_manager : Term_manager.t;
  cache : Weft_cache.t;
  sources : source_adapter list;
  merge_config : general_config;
  mutable tail_cancel : bool Atomic.t option;
}

let create ~cache ~sources ~formats ~general =
  let sources = List.map (fun (src : source_config) ->
    let fmt = Weft_config.resolve_format formats src.format in
    let pipeline = Option.map Weft_middleware.Pipeline.create fmt in
    let has_multiline = match fmt with
      | Some f -> Option.is_some f.multiline
      | None -> false
    in
    { name = src.name; config = src; format_config = fmt;
      pipeline; has_multiline }
  ) sources in
  {
    term_manager = Term_manager.create ();
    cache;
    sources;
    merge_config = general;
    tail_cancel = None;
  }

(* Tag entries with which search terms they match *)
let tag_terms terms (entry : log_entry) =
  let matched = List.filter (fun term ->
    try
      let re = Re.compile (Re.Pcre.re (Re.Pcre.quote term)) in
      Re.execp re entry.raw
    with _ -> false
  ) terms in
  { entry with terms = matched }

(* Process raw lines through a pipeline *)
let process_through_pipeline pipeline ~source lines =
  match pipeline with
  | None ->
    List.filter_map (fun line ->
      if String.length line = 0 then None
      else Some {
        timestamp = Ptime_clock.now ();
        raw = line;
        source;
        terms = [];
        metadata = [];
      }
    ) lines
  | Some pl ->
    Weft_middleware.Pipeline.process_lines pl ~source lines

(* Batch search for a single source *)
let search_source _t adapter ~terms ~time_range =
  if adapter.has_multiline then begin
    (* Multiline: scan from cache *)
    let lines = Weft_cache.read_cached_lines _t.cache ~source_name:adapter.name in
    let entries = process_through_pipeline adapter.pipeline ~source:adapter.name lines in
    let terms_set = terms in
    let filtered = List.filter (fun (entry : log_entry) ->
      List.exists (fun term ->
        try
          let re = Re.compile (Re.Pcre.re (Re.Pcre.quote term)) in
          Re.execp re entry.raw
        with _ -> false
      ) terms_set
    ) entries in
    let tagged = List.map (tag_terms terms) filtered in
    (* Filter by time range *)
    let in_range = match time_range with
      | None -> tagged
      | Some tr ->
        List.filter (fun (e : log_entry) ->
          Ptime.is_later e.timestamp ~than:tr.start_ &&
          (match tr.end_ with
           | None -> true
           | Some end_t -> Ptime.is_earlier e.timestamp ~than:end_t)
        ) tagged
    in
    List.to_seq in_range
  end else
    (* Simple format: grep returns empty for now — would need source handle *)
    Seq.empty

(* Full batch search across all sources *)
let search t ~time_range =
  let terms = Term_manager.enabled_terms t.term_manager in
  if terms = [] then Seq.empty
  else
    let streams = List.map (fun adapter ->
      (adapter.name, search_source t adapter ~terms ~time_range)
    ) t.sources in
    Weft_merge.Batch_merge.merge_with_dedup streams

(* Add a new search term with incremental catch-up *)
let add_term t term_str =
  match Term_manager.add_term t.term_manager term_str with
  | None -> None (* duplicate *)
  | Some search_term ->
    (* Step 1: would restart tail with updated terms (tail-first strategy) *)
    (* Step 2: catch-up from cache for the new term *)
    let _catchup_entries = List.concat_map (fun adapter ->
      if adapter.has_multiline then begin
        let lines = Weft_cache.read_cached_lines t.cache ~source_name:adapter.name in
        let entries = process_through_pipeline adapter.pipeline ~source:adapter.name lines in
        List.filter (fun (entry : log_entry) ->
          try
            let re = Re.compile (Re.Pcre.re (Re.Pcre.quote term_str)) in
            Re.execp re entry.raw
          with _ -> false
        ) entries
      end else
        []
    ) t.sources in
    Some search_term

let remove_term t term_str =
  Term_manager.remove_term t.term_manager term_str

let toggle_term t term_str =
  Term_manager.toggle_term t.term_manager term_str

let enabled_terms t =
  Term_manager.enabled_terms t.term_manager

let all_terms t =
  Term_manager.all_terms t.term_manager
