(* Engine: Eio fiber tree for weft runtime.

   Fiber tree (from design §17):

   main fiber (Eio.Switch)
   ├── source fibers (one per source)
   │   ├── tail sub-fiber (reads new entries, emits to source_stream)
   │   └── cache writer sub-fiber (appends to active segment)
   ├── merge fiber
   │   ├── reads all source_streams
   │   ├── reorder buffer
   │   └── emits to tui_stream
   ├── cache maintenance fiber (periodic TTL eviction)
   └── TUI fiber (reads terminal events, renders)

   Communication:
   - source -> merge: Eio.Stream per source
   - merge -> TUI: Eio.Stream of log_entry
   - TUI -> sources: term updates via shared mutable state + signal
*)

open Weft_types

type entry_event =
  | New_entry of log_entry
  | Batch_done of source_id
  | Source_error of source_id * string

type tui_event =
  | Terminal_event of [ Notty.Unescape.event | `Resize of (int * int) | `End ]
  | Log_entries of log_entry list
  | Status_update of source_id * source_status
  | Progress of string * float  (* label, 0.0-1.0 *)

(* Per-source fiber: reads file, runs pipeline, emits entries *)
let source_fiber ~sw:_ ~source_name ~config ~pipeline ~cache
    ~(entry_stream : entry_event Eio.Stream.t)
    ~terms_ref =
  (* Initial batch load *)
  let lines =
    if Weft_cache.is_cached cache ~source_name then
      Weft_cache.read_cached_lines cache ~source_name
    else begin
      (match config.path with
       | Some path when Sys.file_exists path ->
         ignore (Weft_cache.cache_file cache ~source_name
           ~origin:(Filename.basename path) ~path)
       | _ -> ());
      (* Discover archives *)
      (match config.path with
       | Some path ->
         let archives = Weft_source.Archive.discover_local ~path
           |> Weft_source.Archive.sort_by_mtime in
         if archives <> [] then begin
           Weft_cache.update_archives cache ~source_name archives;
           List.iter (fun (archive : archive_info) ->
             let origin = Filename.basename archive.remote_path in
             let already = match Weft_cache.get_manifest cache source_name with
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
                     ignore (Weft_cache.cache_file cache ~source_name ~origin ~path:tmp);
                   (try Sys.remove tmp
                    with Sys_error msg ->
                      Printf.eprintf "Warning: could not remove temp %s: %s\n" tmp msg)
                 | None -> ()
               end else
                 ignore (Weft_cache.cache_file cache ~source_name ~origin
                   ~path:archive.remote_path)
             end
           ) archives
         end
       | None -> ());
      Weft_cache.read_cached_lines cache ~source_name
    end
  in
  (* Process through pipeline *)
  let process_lines lines =
    match pipeline with
    | None ->
      List.filter_map (fun line ->
        if String.length line = 0 then None
        else Some {
          timestamp = Ptime_clock.now ();
          raw = line; source = source_name;
          terms = []; metadata = [];
        }
      ) lines
    | Some pl ->
      Weft_middleware.Pipeline.process_lines pl ~source:source_name lines
  in
  let entries = process_lines lines in
  (* Filter by current terms and emit *)
  let terms = !terms_ref in
  let term_res = List.map (fun term ->
    (term, Re.compile (Re.Pcre.re (Re.Pcre.quote term)))
  ) terms in
  let filtered = if terms = [] then entries
    else List.filter (fun (entry : log_entry) ->
      List.exists (fun (_t, re) -> Re.execp re entry.raw) term_res
    ) entries in
  let tagged = List.map (fun (entry : log_entry) ->
    let matched = List.filter_map (fun (term, re) ->
      if Re.execp re entry.raw then Some term else None
    ) term_res in
    { entry with terms = matched }
  ) filtered in
  List.iter (fun entry ->
    Eio.Stream.add entry_stream (New_entry entry)
  ) tagged;
  Eio.Stream.add entry_stream (Batch_done source_name)

(* Merge fiber: collects entries from all sources, sorts, sends to TUI *)
let merge_fiber ~entry_stream ~tui_stream ~source_count ~reorder_window_ms =
  let tail_merge = Weft_merge.Tail_merge.create ~reorder_window_ms in
  let sources_done = ref 0 in
  (* Collect all batch entries *)
  while !sources_done < source_count do
    match Eio.Stream.take entry_stream with
    | New_entry entry ->
      Weft_merge.Tail_merge.add tail_merge entry
    | Batch_done _ ->
      incr sources_done
    | Source_error (sid, msg) ->
      Printf.eprintf "Source %s error: %s\n" sid msg;
      incr sources_done
  done;
  (* Flush all entries to TUI *)
  let all = Weft_merge.Tail_merge.flush_all tail_merge in
  if all <> [] then
    Eio.Stream.add tui_stream (Log_entries all)

(* Cache maintenance fiber: periodic eviction *)
let cache_maintenance_fiber ~sw:_ ~cache ~sources ~interval_s =
  ignore interval_s;
  (* Run eviction once at startup *)
  List.iter (fun (src : source_config) ->
    Weft_cache.run_eviction cache src.name
  ) sources

(* Run the full engine with TUI *)
let run_with_tui ~env ~formats_config ~sources_config ~initial_terms =
  let fs = Eio.Stdenv.fs env in
  let proc = Eio.Stdenv.process_mgr env in
  let cache = Weft_cache.create ~fs sources_config.cache in

  (* Init cache and connections *)
  List.iter (fun (src : source_config) ->
    ignore (Weft_cache.init_source cache ~source_name:src.name ~format:src.format)
  ) sources_config.sources;

  let pool = Weft_connection.Conn_pool.create ~limits:sources_config.limits in
  List.iter (fun src -> Weft_connection.Conn_pool.add_source pool src)
    sources_config.sources;
  List.iter (fun (src : source_config) ->
    match Weft_connection.Conn_pool.connect_source proc pool src.name with
    | Ok () -> ()
    | Error e -> Printf.eprintf "Warning: connect %s: %s\n" src.name e
  ) sources_config.sources;

  (* Build search engine for TUI *)
  let search = Weft_search.create ~cache
    ~sources:sources_config.sources
    ~formats:formats_config
    ~general:sources_config.general in
  List.iter (fun t -> ignore (Weft_search.add_term search t)) initial_terms;

  (* Shared terms state *)
  let terms_ref = ref initial_terms in

  (* Streams *)
  let entry_stream = Eio.Stream.create 4096 in
  let tui_stream = Eio.Stream.create 256 in

  (* Build source adapters info *)
  let source_adapters = List.map (fun (src : source_config) ->
    let fmt = Weft_config.resolve_format formats_config src.format in
    let pipeline = Option.map Weft_middleware.Pipeline.create fmt in
    (src, pipeline)
  ) sources_config.sources in

  let source_count = List.length source_adapters in

  Eio.Switch.run @@ fun sw ->

  (* Spawn source fibers *)
  List.iter (fun ((config : source_config), pipeline) ->
    Eio.Fiber.fork ~sw (fun () ->
      (try
         source_fiber ~sw ~source_name:config.name ~config ~pipeline
           ~cache ~entry_stream ~terms_ref
       with exn ->
         Eio.Stream.add entry_stream
           (Source_error (config.name, Printexc.to_string exn)))
    )
  ) source_adapters;

  (* Spawn merge fiber *)
  Eio.Fiber.fork ~sw (fun () ->
    merge_fiber ~entry_stream ~tui_stream ~source_count
      ~reorder_window_ms:sources_config.general.reorder_window_ms
  );

  (* Spawn cache maintenance *)
  Eio.Fiber.fork ~sw (fun () ->
    cache_maintenance_fiber ~sw ~cache
      ~sources:sources_config.sources ~interval_s:300
  );

  (* Create TUI *)
  let model = Weft_tui.create ~search in
  let source_statuses = List.map (fun (src : source_config) ->
    (src.name, Weft_connection.Conn_pool.get_status pool src.name)
  ) sources_config.sources in
  Weft_tui.Sidebar.update_sources model.sidebar source_statuses;

  (* TUI event loop — runs in main fiber *)
  let term = Notty_unix.Term.create () in
  let (w, h) = Notty_unix.Term.size term in
  model.width <- w;
  model.height <- h;

  (* Drain entries from tui_stream before first render *)
  let drain_entries () =
    let rec drain () =
      match Eio.Stream.take_nonblocking tui_stream with
      | None -> ()
      | Some (Log_entries entries) ->
        List.iter (fun e -> Weft_tui.Timeline.append_entry model.timeline e) entries;
        drain ()
      | Some (Status_update (sid, status)) ->
        let statuses = List.map (fun (src : source_config) ->
          (src.name, if src.name = sid then status
                     else Weft_connection.Conn_pool.get_status pool src.name)
        ) sources_config.sources in
        Weft_tui.Sidebar.update_sources model.sidebar statuses;
        drain ()
      | Some (Progress (_label, _pct)) ->
        drain ()
      | Some (Terminal_event _) ->
        drain ()
    in
    drain ()
  in

  (* Interleave terminal events with entry updates *)
  let running = ref true in
  Fun.protect (fun () ->
    while !running do
      drain_entries ();
      let img = Weft_tui.render model in
      Notty_unix.Term.image term img;
      (* Poll for terminal event with short timeout *)
      if Notty_unix.Term.pending term then begin
        match Notty_unix.Term.event term with
        | `End | `Key (`ASCII 'C', [`Ctrl]) ->
          running := false
        | `Key (key, _mods) ->
          Weft_tui.handle_key model key;
          (* Check if search term was added *)
          let new_terms = Weft_search.enabled_terms search in
          if new_terms <> !terms_ref then begin
            terms_ref := new_terms;
            (* Re-search with new terms *)
            let entries = Weft_search.search search ~time_range:None in
            let entry_list = List.of_seq (Seq.take 10000 entries) in
            Weft_tui.Timeline.set_entries model.timeline entry_list
          end;
          if model.quit then running := false;
          let (w, h) = Notty_unix.Term.size term in
          model.width <- w;
          model.height <- h
        | `Resize (w, h) ->
          model.width <- w;
          model.height <- h
        | `Mouse _ | `Paste _ -> ()
      end else
        (* No terminal input — sleep briefly to avoid busy-wait *)
        Unix.sleepf 0.016  (* ~60fps *)
    done
  ) ~finally:(fun () ->
    Notty_unix.Term.release term;
    Weft_connection.Conn_pool.close_all pool
  )

(* Run in dump mode — same fiber tree but output to stdout *)
let run_dump ~env ~formats_config ~sources_config ~initial_terms
    ~limit ~json =
  let fs = Eio.Stdenv.fs env in
  let cache = Weft_cache.create ~fs sources_config.cache in
  List.iter (fun (src : source_config) ->
    ignore (Weft_cache.init_source cache ~source_name:src.name ~format:src.format)
  ) sources_config.sources;

  let search = Weft_search.create ~cache
    ~sources:sources_config.sources
    ~formats:formats_config
    ~general:sources_config.general in
  List.iter (fun t -> ignore (Weft_search.add_term search t)) initial_terms;

  let has_terms = initial_terms <> [] in
  let entries = if has_terms then
    Weft_search.search search ~time_range:None
  else
    Weft_search.load_all search
  in
  let count = ref 0 in
  let rec print_seq seq =
    if limit > 0 && !count >= limit then ()
    else match seq () with
    | Seq.Nil -> ()
    | Seq.Cons ((entry : log_entry), rest) ->
      incr count;
      if json then
        print_endline (Weft_app_fmt.entry_to_json entry)
      else
        print_endline (Weft_app_fmt.format_entry entry);
      print_seq rest
  in
  print_seq entries;
  if not json then begin
    Printf.eprintf "-- %d entries\n" !count;
    Printf.eprintf "-- cache: %s\n" (Weft_app_fmt.format_cache_stats cache)
  end
