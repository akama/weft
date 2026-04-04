open Weft_types

type source_adapter = {
  name : source_id;
  config : source_config;
  format_config : format_config option;
  pipeline : Weft_middleware.Pipeline.t option;
  has_multiline : bool;
  ssh : Weft_connection.Ssh_control.t option;
}

type t = {
  term_manager : Term_manager.t;
  cache : Weft_cache.t;
  sources : source_adapter list;
  merge_config : general_config;
  mutable tail_cancel : bool Atomic.t option;
}

let create ~cache ~sources ~formats ~general
    ?(conn_pool : Weft_connection.Conn_pool.t option) () =
  let sources = List.map (fun (src : source_config) ->
    let fmt = Weft_config.resolve_format formats src.format in
    let pipeline = Option.map Weft_middleware.Pipeline.create fmt in
    let has_multiline = match fmt with
      | Some f -> Option.is_some f.multiline
      | None -> false
    in
    let ssh = match conn_pool with
      | Some pool ->
        (match Weft_connection.Conn_pool.get_connection pool src.name with
         | Some conn -> conn.ssh
         | None -> None)
      | None -> None
    in
    { name = src.name; config = src; format_config = fmt;
      pipeline; has_multiline; ssh }
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
    let re = Re.compile (Re.Pcre.re (Re.Pcre.quote term)) in
    Re.execp re entry.raw
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
  with Sys_error msg ->
    Printf.eprintf "Warning: could not read %s: %s\n" path msg;
    []

(* Fetch a remote file via SSH and return its contents *)
let fetch_remote_file ssh path =
  try
    let data = Weft_connection.Ssh_control.run_command ssh ["cat"; path] in
    Some data
  with Failure msg ->
    Printf.eprintf "Warning: could not fetch remote %s: %s\n" path msg;
    None

(* Fetch and decompress a remote archive *)
let fetch_remote_archive ssh path =
  let cmd = match Weft_source.Archive.decompressor_for path with
    | Some decomp -> decomp
    | None -> "cat"
  in
  try
    let data = Weft_connection.Ssh_control.run_command ssh [cmd; path] in
    Some data
  with Failure msg ->
    Printf.eprintf "Warning: could not fetch remote archive %s: %s\n" path msg;
    None

(* Discover remote archives via SSH *)
let discover_remote_archives ssh path =
  Weft_source.Archive.discover_remote ~ssh ~path
  |> Weft_source.Archive.sort_by_mtime

(* Process raw lines through a pipeline *)
let process_through_pipeline pipeline ~source lines =
  match pipeline with
  | None ->
    List.filter_map (fun line ->
      if String.length line = 0 then None
      else Some {
        timestamp = Ptime_clock.now ();
        raw = line; source;
        terms = []; metadata = [];
      }
    ) lines
  | Some pl ->
    Weft_middleware.Pipeline.process_lines pl ~source lines

(* Cache raw string data as a segment *)
let cache_string_data cache ~source_name ~origin data =
  if String.length data = 0 then ()
  else begin
    let tmp = Filename.temp_file "weft_remote_" ".log" in
    (try
       let oc = open_out tmp in
       output_string oc data;
       close_out oc;
       ignore (Weft_cache.cache_file cache ~source_name ~origin ~path:tmp)
     with Sys_error msg ->
       Printf.eprintf "Warning: could not cache %s: %s\n" origin msg);
    (try Sys.remove tmp
     with Sys_error msg ->
       Printf.eprintf "Warning: could not remove temp %s: %s\n" tmp msg)
  end

(* Discover and cache archives for a local source *)
let discover_and_cache_local_archives adapter cache =
  match adapter.config.path with
  | None -> ()
  | Some path ->
    let archives = Weft_source.Archive.discover_local ~path
      |> Weft_source.Archive.sort_by_mtime in
    if archives <> [] then begin
      Weft_cache.update_archives cache ~source_name:adapter.name archives;
      List.iter (fun (archive : archive_info) ->
        let origin = Filename.basename archive.remote_path in
        let already = match Weft_cache.get_manifest cache adapter.name with
          | None -> false
          | Some m -> List.exists (fun (s : segment) -> s.origin = origin) m.segments
        in
        if not already then begin
          if Weft_source.Archive.is_compressed archive.remote_path then begin
            match Weft_source.Archive.decompressor_for archive.remote_path with
            | Some cmd ->
              let tmp = Filename.temp_file "weft_archive_" ".log" in
              let ret = Sys.command
                (Printf.sprintf "%s '%s' > '%s' 2>/dev/null" cmd archive.remote_path tmp) in
              if ret = 0 then
                ignore (Weft_cache.cache_file cache ~source_name:adapter.name ~origin ~path:tmp);
              (try Sys.remove tmp
               with Sys_error msg ->
                 Printf.eprintf "Warning: could not remove temp %s: %s\n" tmp msg)
            | None -> ()
          end else
            ignore (Weft_cache.cache_file cache ~source_name:adapter.name ~origin
              ~path:archive.remote_path)
        end
      ) archives
    end

(* Discover and cache archives for a remote source *)
let discover_and_cache_remote_archives adapter cache ssh =
  match adapter.config.path with
  | None -> ()
  | Some path ->
    let archives = discover_remote_archives ssh path in
    if archives <> [] then begin
      Weft_cache.update_archives cache ~source_name:adapter.name archives;
      List.iter (fun (archive : archive_info) ->
        let origin = Filename.basename archive.remote_path in
        let already = match Weft_cache.get_manifest cache adapter.name with
          | None -> false
          | Some m -> List.exists (fun (s : segment) -> s.origin = origin) m.segments
        in
        if not already then begin
          match fetch_remote_archive ssh archive.remote_path with
          | Some data ->
            cache_string_data cache ~source_name:adapter.name ~origin data
          | None -> ()
        end
      ) archives
    end

(* Read lines for a source — check cache first, fetch if needed *)
let read_source_lines adapter cache =
  if Weft_cache.is_cached cache ~source_name:adapter.name then
    Weft_cache.read_cached_lines cache ~source_name:adapter.name
  else begin
    (match adapter.config.source_type with
     | File ->
       (* Local file — read directly *)
       (match adapter.config.path with
        | Some path when Sys.file_exists path ->
          ignore (Weft_cache.cache_file cache
            ~source_name:adapter.name
            ~origin:(Filename.basename path) ~path)
        | _ -> ());
       discover_and_cache_local_archives adapter cache

     | Remote ->
       (* Remote file — fetch via SSH *)
       (match adapter.ssh, adapter.config.path with
        | Some ssh, Some path ->
          Printf.eprintf "Fetching %s from %s...\n%!"
            path (Option.value ~default:"remote" adapter.config.transport);
          (match fetch_remote_file ssh path with
           | Some data ->
             cache_string_data cache ~source_name:adapter.name
               ~origin:(Filename.basename path) data
           | None -> ());
          discover_and_cache_remote_archives adapter cache ssh
        | _ ->
          Printf.eprintf "Warning: remote source %s has no SSH connection\n"
            adapter.name)

     | Directory | Loki -> ()
    );
    Weft_cache.read_cached_lines cache ~source_name:adapter.name
  end

(* Batch search for a single source *)
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

let search t ~time_range =
  let terms = Term_manager.enabled_terms t.term_manager in
  if terms = [] then Seq.empty
  else
    let streams = List.map (fun adapter ->
      (adapter.name, search_source t adapter ~terms ~time_range)
    ) t.sources in
    Weft_merge.Batch_merge.merge_with_dedup streams

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
  | Some search_term -> Some search_term

let remove_term t term_str =
  Term_manager.remove_term t.term_manager term_str

let toggle_term t term_str =
  Term_manager.toggle_term t.term_manager term_str

let enabled_terms t =
  Term_manager.enabled_terms t.term_manager

let all_terms t =
  Term_manager.all_terms t.term_manager
