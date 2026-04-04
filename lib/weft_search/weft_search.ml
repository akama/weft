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

(* Read raw lines from a local file *)
let read_file_lines path =
  try
    let ic = open_in path in
    let lines = ref [] in
    (try while true do
       lines := input_line ic :: !lines
     done with End_of_file -> ());
    close_in ic;
    List.rev !lines
  with _ -> []

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

(* Discover and cache archives for a source *)
let discover_and_cache_archives adapter cache =
  match adapter.config.path with
  | None -> ()
  | Some path ->
    let archives = Weft_source.Archive.discover_local ~path
      |> Weft_source.Archive.sort_by_mtime in
    if archives <> [] then begin
      (* Update known archives in manifest *)
      Weft_cache.update_archives cache ~source_name:adapter.name archives;
      (* Cache each archive that isn't already cached *)
      List.iter (fun (archive : archive_info) ->
        let archive_origin = Filename.basename archive.remote_path in
        (* Check if we already have a segment for this archive *)
        let already_cached = match Weft_cache.get_manifest cache adapter.name with
          | None -> false
          | Some m ->
            List.exists (fun (seg : segment) ->
              seg.origin = archive_origin
            ) m.segments
        in
        if not already_cached then begin
          (* Decompress if needed, then cache *)
          if Weft_source.Archive.is_compressed archive.remote_path then begin
            match Weft_source.Archive.decompressor_for archive.remote_path with
            | Some decomp_cmd ->
              (* Decompress to a temp file, then cache it *)
              let tmp = Filename.temp_file "weft_archive_" ".log" in
              let ret = Sys.command
                (Printf.sprintf "%s '%s' > '%s' 2>/dev/null"
                   decomp_cmd archive.remote_path tmp) in
              if ret = 0 then begin
                ignore (Weft_cache.cache_file cache
                  ~source_name:adapter.name
                  ~origin:archive_origin
                  ~path:tmp)
              end;
              (try Sys.remove tmp with _ -> ())
            | None -> ()
          end else begin
            (* Uncompressed archive — cache directly *)
            ignore (Weft_cache.cache_file cache
              ~source_name:adapter.name
              ~origin:archive_origin
              ~path:archive.remote_path)
          end
        end
      ) archives
    end

(* Read lines for a source — check cache first, populate if needed.
   Also discovers and caches any rotated archives. *)
let read_source_lines adapter cache =
  if Weft_cache.is_cached cache ~source_name:adapter.name then
    Weft_cache.read_cached_lines cache ~source_name:adapter.name
  else begin
    (* Cache the active file *)
    (match adapter.config.path with
     | Some path when Sys.file_exists path ->
       ignore (Weft_cache.cache_file cache
         ~source_name:adapter.name
         ~origin:(Filename.basename path)
         ~path)
     | _ -> ());
    (* Discover and cache archives *)
    discover_and_cache_archives adapter cache;
    (* Read everything from cache *)
    Weft_cache.read_cached_lines cache ~source_name:adapter.name
  end

(* Batch search for a single source — reads file, runs pipeline, filters *)
let search_source t adapter ~terms ~time_range =
  let lines = read_source_lines adapter t.cache in
  if lines = [] then Seq.empty
  else begin
    let entries = process_through_pipeline adapter.pipeline ~source:adapter.name lines in
    let term_res = List.map (fun term ->
      (term, Re.compile (Re.Pcre.re (Re.Pcre.quote term)))
    ) terms in
    let filtered = List.filter (fun (entry : log_entry) ->
      List.exists (fun (_term, re) -> Re.execp re entry.raw) term_res
    ) entries in
    let tagged = List.map (tag_terms terms) filtered in
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
  end

(* Full batch search across all sources *)
let search t ~time_range =
  let terms = Term_manager.enabled_terms t.term_manager in
  if terms = [] then Seq.empty
  else
    let streams = List.map (fun adapter ->
      (adapter.name, search_source t adapter ~terms ~time_range)
    ) t.sources in
    Weft_merge.Batch_merge.merge_with_dedup streams

(* Load all entries without term filtering *)
let load_all t =
  let streams = List.map (fun adapter ->
    let lines = read_source_lines adapter t.cache in
    let entries = process_through_pipeline adapter.pipeline ~source:adapter.name lines in
    (adapter.name, List.to_seq entries)
  ) t.sources in
  Weft_merge.Batch_merge.merge streams

let add_term t term_str =
  match Term_manager.add_term t.term_manager term_str with
  | None -> None
  | Some search_term ->
    Some search_term

let remove_term t term_str =
  Term_manager.remove_term t.term_manager term_str

let toggle_term t term_str =
  Term_manager.toggle_term t.term_manager term_str

let enabled_terms t =
  Term_manager.enabled_terms t.term_manager

let all_terms t =
  Term_manager.all_terms t.term_manager
