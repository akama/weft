open Weft_types

type source_adapter = {
  name : source_id;
  config : source_config;
  format_config : format_config option;
  pipeline : Weft_middleware.Pipeline.t option;
  has_multiline : bool;
  ssh : Weft_connection.Ssh_control.t option;
}

type loki_query_fn =
  url:string -> headers:(string * string) list -> string option

(* Status callback — set by the TUI to capture progress messages *)
let status_callback : (string -> unit) ref = ref (fun msg ->
  Printf.eprintf "%s\n%!" msg)

let set_status_callback f = status_callback := f
let report_status msg = !status_callback msg

type t = {
  term_manager : Term_manager.t;
  cache : Weft_cache.t;
  sources : source_adapter list;
  merge_config : general_config;
  mutable tail_cancel : bool Atomic.t option;
  loki_query : loki_query_fn option;
}

let create ~cache ~sources ~formats ~general
    ?(conn_pool : Weft_connection.Conn_pool.t option)
    ?(loki_query : loki_query_fn option) () =
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
    loki_query;
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
      let total = List.length archives in
      Weft_cache.update_archives cache ~source_name:adapter.name archives;
      List.iteri (fun i (archive : archive_info) ->
        let origin = Filename.basename archive.remote_path in
        report_status (Printf.sprintf "Caching %s/%s [%d/%d]"
          adapter.name origin (i + 1) total);
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
      let total = List.length archives in
      Weft_cache.update_archives cache ~source_name:adapter.name archives;
      List.iteri (fun i (archive : archive_info) ->
        let origin = Filename.basename archive.remote_path in
        report_status (Printf.sprintf "Fetching %s/%s [%d/%d]"
          adapter.name origin (i + 1) total);
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

(* Ensure source data is cached, fetching if needed *)
let ensure_cached ?t_opt ?time_range adapter cache =
  if Weft_cache.is_cached cache ~source_name:adapter.name then ()
  else begin
    match adapter.config.source_type with
    | File ->
      (match adapter.config.path with
       | Some path when Sys.file_exists path ->
         ignore (Weft_cache.cache_file cache
           ~source_name:adapter.name
           ~origin:(Filename.basename path) ~path)
       | _ -> ());
      discover_and_cache_local_archives adapter cache

    | Remote ->
      (* Resolve glob to file list if needed *)
      let remote_paths = match adapter.ssh, adapter.config.glob, adapter.config.path with
        | Some ssh, Some glob, _ ->
          (* Remote glob: expand via ssh ls *)
          report_status (Printf.sprintf "Expanding glob %s..." glob);
          (try
             let lines = Weft_connection.Ssh_control.run_command_lines ssh
               ["ls"; "-1"; glob] in
             List.filter (fun s -> String.length s > 0) lines
           with Failure _ -> [])
        | _, _, Some path -> [path]
        | _ -> []
      in
      (match adapter.ssh with
       | Some ssh ->
         List.iter (fun path ->
           let origin = Filename.basename path in
           let sub_name = if List.length remote_paths > 1 then
             Printf.sprintf "%s:%s" adapter.name origin
           else adapter.name in
           report_status (Printf.sprintf "Fetching %s from %s..."
             path (Option.value ~default:"remote" adapter.config.transport));
           ignore sub_name; (* used for multi-file glob naming *)
           (match fetch_remote_file ssh path with
            | Some data ->
              cache_string_data cache ~source_name:adapter.name
                ~origin data
            | None -> ());
           (* Only fetch archives needed for the requested range *)
           (match time_range with
            | Some tr ->
              let needed = Weft_cache.archives_needed_for_range cache
                ~source_name:adapter.name ~time_range:tr in
            if needed <> [] then begin
              report_status (Printf.sprintf "Fetching %d archives for gap..."
                (List.length needed));
              List.iter (fun (archive : archive_info) ->
                let origin = Filename.basename archive.remote_path in
                match fetch_remote_archive ssh archive.remote_path with
                | Some data ->
                  cache_string_data cache ~source_name:adapter.name ~origin data
                | None -> ()
              ) needed
            end
            | None ->
              discover_and_cache_remote_archives adapter cache ssh)
         ) remote_paths
       | None ->
         Printf.eprintf "Warning: remote source %s has no SSH connection\n"
           adapter.name)

    | Loki ->
      (match t_opt with
       | Some t ->
         (match t.loki_query, adapter.config.url with
          | Some query_fn, Some base_url ->
            let default_labels = Option.value ~default:"{}" adapter.config.default_labels in
            let logql = default_labels in
            (* Scope the Loki query to the requested time range *)
            let (start_ns, end_ns) = match time_range with
              | Some tr ->
                let to_ns t = Printf.sprintf "%Ld"
                  (Int64.mul (Int64.of_float (Ptime.to_float_s t)) 1_000_000_000L) in
                let end_t = match tr.end_ with
                  | Some e -> e | None -> Ptime_clock.now () in
                (to_ns tr.start_, to_ns end_t)
              | None ->
                let now = Unix.gettimeofday () in
                (Printf.sprintf "%Ld" (Int64.mul (Int64.of_float (now -. 3600.0)) 1_000_000_000L),
                 Printf.sprintf "%Ld" (Int64.mul (Int64.of_float now) 1_000_000_000L))
            in
            let url = Printf.sprintf
              "%s/loki/api/v1/query_range?query=%s&start=%s&end=%s&limit=5000&direction=forward"
              base_url (Uri.pct_encode logql) start_ns end_ns in
            let headers = match adapter.config.auth with
              | Bearer { token_env } ->
                (match Sys.getenv_opt token_env with
                 | Some tok -> [("Authorization", "Bearer " ^ tok)]
                 | None -> [])
              | _ -> []
            in
            report_status (Printf.sprintf "Querying Loki at %s..." base_url);
            (match query_fn ~url ~headers with
             | Some body ->
               let entries = Weft_source.Loki.parse_query_response
                 ~source:adapter.name body in
               let data = String.concat "\n"
                 (List.map (fun (e : log_entry) -> e.raw) entries) in
               if data <> "" then
                 cache_string_data cache ~source_name:adapter.name
                   ~origin:"loki-query" data
             | None ->
               Printf.eprintf "Warning: Loki query returned no data\n")
          | _ ->
            Printf.eprintf "Warning: Loki source %s has no query function or URL\n"
              adapter.name)
       | None ->
         Printf.eprintf "Warning: Loki source %s cannot query without runtime\n"
           adapter.name)

    | Directory -> ()
  end

(* Read lines for a source — ensure cached, then read overlapping segments *)
let read_source_lines ?t_opt ?time_range adapter cache =
  ensure_cached ?t_opt ?time_range adapter cache;
  Weft_cache.read_cached_lines_in_range cache
    ~source_name:adapter.name ~time_range

(* Batch search for a single source *)
let search_source ctx adapter ~terms ~(time_range : time_range option) =
  let lines = read_source_lines ~t_opt:ctx ?time_range adapter ctx.cache in
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

let search ctx ~time_range =
  let terms = Term_manager.enabled_terms ctx.term_manager in
  if terms = [] then Seq.empty
  else
    let streams = List.map (fun adapter ->
      (adapter.name, search_source ctx adapter ~terms ~time_range)
    ) ctx.sources in
    Weft_merge.Batch_merge.merge_with_dedup streams

let load_all ?time_range ctx =
  let streams = List.map (fun adapter ->
    let lines = read_source_lines ~t_opt:ctx ?time_range adapter ctx.cache in
    let entries = process_through_pipeline adapter.pipeline ~source:adapter.name lines in
    (* Post-filter by time range since segments are coarse *)
    let entries = match time_range with
      | None -> entries
      | Some tr ->
        List.filter (fun (e : log_entry) ->
          Ptime.is_later e.timestamp ~than:tr.start_ &&
          (match tr.end_ with
           | None -> true
           | Some end_t -> Ptime.is_earlier e.timestamp ~than:end_t)
        ) entries
    in
    (adapter.name, List.to_seq entries)
  ) ctx.sources in
  Weft_merge.Batch_merge.merge streams

let add_term t term_str =
  match Term_manager.add_term t.term_manager term_str with
  | None -> None
  | Some search_term -> Some search_term

let remove_term t term_str =
  Term_manager.remove_term t.term_manager term_str

let toggle_term t term_str =
  Term_manager.toggle_term t.term_manager term_str

let isolate_term t term_str =
  Term_manager.isolate_term t.term_manager term_str

let enable_all_terms t =
  Term_manager.enable_all_terms t.term_manager

let enabled_terms t =
  Term_manager.enabled_terms t.term_manager

let all_terms t =
  Term_manager.all_terms t.term_manager
