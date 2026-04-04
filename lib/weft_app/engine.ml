(* Engine: Eio fiber tree for weft runtime.

   Fiber tree (from design §17):

   main fiber (Eio.Switch)
   ├── source fibers (one per source)
   │   └── batch load + emit to entry_stream
   ├── merge fiber
   │   ├── reads entry_stream
   │   ├── reorder buffer
   │   └── emits to tui_stream
   ├── cache maintenance fiber
   └── TUI fiber (reads terminal events, renders)
*)

open Weft_types

type entry_event =
  | New_entry of log_entry
  | Batch_done of source_id
  | Source_error of source_id * string

type tui_event =
  | Log_entries of log_entry list
  | Status_update of source_id * source_status

(* Per-source fiber: reads file, runs pipeline, emits entries *)
let source_fiber ~source_name ~(config : source_config) ~pipeline ~cache
    ~(entry_stream : entry_event Eio.Stream.t)
    ~terms_ref =
  let lines =
    if Weft_cache.is_cached cache ~source_name then
      Weft_cache.read_cached_lines cache ~source_name
    else begin
      (match config.path with
       | Some path when Sys.file_exists path ->
         ignore (Weft_cache.cache_file cache ~source_name
           ~origin:(Filename.basename path) ~path)
       | _ -> ());
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
  let all = Weft_merge.Tail_merge.flush_all tail_merge in
  if all <> [] then
    Eio.Stream.add tui_stream (Log_entries all)

(* Cache maintenance fiber *)
let cache_maintenance_fiber ~cache ~sources =
  List.iter (fun (src : source_config) ->
    Weft_cache.run_eviction cache src.name
  ) sources

(* Run the full engine with TUI *)
let run_with_tui ~env ~formats_config ~sources_config ~initial_terms =
  let fs = Eio.Stdenv.fs env in
  let proc = Eio.Stdenv.process_mgr env in
  let cache = Weft_cache.create ~fs sources_config.cache in

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

  let search = Weft_search.create ~cache
    ~sources:sources_config.sources
    ~formats:formats_config
    ~general:sources_config.general
    ~conn_pool:pool () in
  List.iter (fun t -> ignore (Weft_search.add_term search t)) initial_terms;

  let terms_ref = ref initial_terms in

  let entry_stream : entry_event Eio.Stream.t = Eio.Stream.create 8192 in
  let tui_stream : tui_event Eio.Stream.t = Eio.Stream.create 256 in

  let source_adapters = List.map (fun (src : source_config) ->
    let fmt = Weft_config.resolve_format formats_config src.format in
    let pipeline = Option.map Weft_middleware.Pipeline.create fmt in
    (src, pipeline)
  ) sources_config.sources in

  let source_count = List.length source_adapters in

  (* Create TUI model before starting fibers so we can show initial state *)
  let model = Weft_tui.create ~search in

  let update_source_statuses () =
    let statuses = List.map (fun (src : source_config) ->
      (src.name, Weft_connection.Conn_pool.get_status pool src.name)
    ) sources_config.sources in
    Weft_tui.Sidebar.update_sources model.sidebar statuses
  in
  let update_cache_stats () =
    let (total_size, total_segments, (earliest, latest)) =
      Weft_cache.cache_stats cache in
    let size_mb = Int64.to_int (Int64.div total_size (Int64.of_int (1024 * 1024))) in
    let range = match earliest, latest with
      | Some s, Some e ->
        let (sd, _) = Ptime.to_date_time s in
        let (ed, _) = Ptime.to_date_time e in
        let fmt (y, m, d) = Printf.sprintf "%04d-%02d-%02d" y m d in
        if sd = ed then fmt sd else Printf.sprintf "%s to %s" (fmt sd) (fmt ed)
      | _ -> ""
    in
    Weft_tui.Sidebar.update_cache_info model.sidebar
      ~size_mb ~segments:total_segments ~time_range:range
  in
  update_source_statuses ();

  let term = Notty_unix.Term.create () in
  let (w, h) = Notty_unix.Term.size term in
  model.width <- w;
  model.height <- h;

  Fun.protect (fun () ->
    Eio.Switch.run @@ fun sw ->

    (* Spawn source fibers *)
    List.iter (fun ((config : source_config), pipeline) ->
      Eio.Fiber.fork ~sw (fun () ->
        (try
           source_fiber ~source_name:config.name ~config ~pipeline
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
      cache_maintenance_fiber ~cache ~sources:sources_config.sources
    );

    (* TUI event loop — main fiber *)
    let drain_entries () =
      let rec drain () =
        match Eio.Stream.take_nonblocking tui_stream with
        | None -> ()
        | Some (Log_entries entries) ->
          List.iter (fun e ->
            Weft_tui.Timeline.append_entry model.timeline e
          ) entries;
          update_cache_stats ();
          drain ()
        | Some (Status_update (sid, status)) ->
          ignore (sid, status);
          update_source_statuses ();
          drain ()
      in
      drain ()
    in

    (* Get the terminal input fd for select-based polling *)
    let (input_fd, _output_fd) = Notty_unix.Term.fds term in

    let handle_terminal_event () =
      match Notty_unix.Term.event term with
      | `End | `Key (`ASCII 'C', [`Ctrl]) -> false
      | `Key (key, _mods) ->
        Weft_tui.handle_key model key;
        let new_terms = Weft_search.enabled_terms search in
        if new_terms <> !terms_ref then begin
          terms_ref := new_terms;
          Weft_tui.refresh_search model
        end;
        let (w, h) = Notty_unix.Term.size term in
        model.width <- w;
        model.height <- h;
        not model.quit
      | `Resize (w, h) ->
        model.width <- w;
        model.height <- h;
        true
      | `Mouse _ | `Paste _ -> true
    in

    let running = ref true in
    while !running do
      Eio.Fiber.yield ();
      drain_entries ();

      let img = Weft_tui.render model in
      Notty_unix.Term.image term img;

      (* Check if terminal has input ready (50ms timeout) *)
      let ready, _, _ = Unix.select [input_fd] [] [] 0.05 in
      if ready <> [] || Notty_unix.Term.pending term then
        running := handle_terminal_event ()
      else
        (* No input — yield to let other fibers run *)
        Eio.Fiber.yield ()
    done;

    (* Cancel switch to stop all fibers *)
    Eio.Switch.fail sw Exit
  ) ~finally:(fun () ->
    Notty_unix.Term.release term;
    Weft_connection.Conn_pool.close_all pool
  )

(* Common initialization for dump/live modes *)
let init_runtime ~env ~formats_config ~sources_config ~initial_terms =
  let fs = Eio.Stdenv.fs env in
  let proc = Eio.Stdenv.process_mgr env in
  let cache = Weft_cache.create ~fs sources_config.cache in
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
  let search = Weft_search.create ~cache
    ~sources:sources_config.sources
    ~formats:formats_config
    ~general:sources_config.general
    ~conn_pool:pool () in
  List.iter (fun t -> ignore (Weft_search.add_term search t)) initial_terms;
  (cache, pool, search)

(* Run in dump mode *)
let run_dump ~env ~formats_config ~sources_config ~initial_terms
    ~limit ~json =
  let (cache, pool, search) =
    init_runtime ~env ~formats_config ~sources_config ~initial_terms in
  ignore pool;

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

(* Run in live/follow mode — like dump but keeps watching for new entries *)
let run_live ~env ~formats_config ~sources_config ~initial_terms ~json =
  let fs = Eio.Stdenv.fs env in
  let (_cache, pool, search) =
    init_runtime ~env ~formats_config ~sources_config ~initial_terms in

  (* Print existing entries first *)
  let has_terms = initial_terms <> [] in
  let entries = if has_terms then
    Weft_search.search search ~time_range:None
  else
    Weft_search.load_all search
  in
  let count = ref 0 in
  Seq.iter (fun (entry : log_entry) ->
    incr count;
    if json then print_endline (Weft_app_fmt.entry_to_json entry)
    else print_endline (Weft_app_fmt.format_entry entry)
  ) entries;
  Printf.eprintf "-- %d historical entries, now tailing...\n%!" !count;

  let cancel = Atomic.make false in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Atomic.set cancel true));

  let terms = if has_terms then initial_terms else [] in
  let term_res = List.map (fun term ->
    (term, Re.compile (Re.Pcre.re (Re.Pcre.quote term)))
  ) terms in

  (* Shared emit function: tag terms, run pipeline, print *)
  let emit_raw ~source ~pipeline_state line =
    let should_emit = match term_res with
      | [] -> true
      | _ -> List.exists (fun (_t, re) -> Re.execp re line) term_res
    in
    if should_emit then begin
      let entries_to_emit = match pipeline_state with
        | None ->
          [{
            timestamp = Ptime_clock.now ();
            raw = line; source;
            terms = []; metadata = [];
          }]
        | Some state ->
          Weft_middleware.Pipeline.feed_line state line
      in
      List.iter (fun (entry : log_entry) ->
        incr count;
        let entry = match term_res with
          | [] -> entry
          | _ ->
            let matched = List.filter_map (fun (term, re) ->
              if Re.execp re entry.raw then Some term else None
            ) term_res in
            { entry with terms = matched }
        in
        if json then print_endline (Weft_app_fmt.entry_to_json entry)
        else print_endline (Weft_app_fmt.format_entry entry)
      ) entries_to_emit
    end
  in

  Eio.Switch.run @@ fun sw ->

  List.iter (fun (src : source_config) ->
    let fmt = Weft_config.resolve_format formats_config src.format in
    let pipeline = Option.map Weft_middleware.Pipeline.create fmt in
    let pipeline_state = Option.map (fun pl ->
      Weft_middleware.Pipeline.create_stream_state pl ~source:src.name
    ) pipeline in

    match src.source_type with
    | File ->
      (match src.path with
       | Some path when Sys.file_exists path ->
         Eio.Fiber.fork ~sw (fun () ->
           let adapter : Weft_source.Local_file.t = {
             config = src; path; fs;
           } in
           Weft_source.Local_file.tail_simple adapter ~terms:[]
             ~emit:(fun entry ->
               emit_raw ~source:src.name ~pipeline_state entry.raw)
             ~cancel
         )
       | _ -> ())

    | Remote ->
      (* Tail via ssh tail -F *)
      (match src.transport, src.path with
       | Some _transport, Some path ->
         let ssh = match Weft_connection.Conn_pool.get_connection pool src.name with
           | Some conn -> conn.ssh
           | None -> None
         in
         (match ssh with
          | Some ssh ->
            Eio.Fiber.fork ~sw (fun () ->
              Printf.eprintf "Tailing %s:%s via SSH...\n%!" src.name path;
              let tail_cmd = ["tail"; "-n"; "0"; "-F"; path] in
              (try
                 Weft_connection.Ssh_control.run_streaming ssh tail_cmd
                   ~on_line:(fun line ->
                     emit_raw ~source:src.name ~pipeline_state line)
                   ~cancel
               with Failure msg ->
                 Printf.eprintf "SSH tail for %s ended: %s\n%!" src.name msg)
            )
          | None ->
            Printf.eprintf "Warning: no SSH connection for %s, skipping tail\n%!"
              src.name)
       | _ -> ())

    | Directory | Loki -> ()
  ) sources_config.sources;

  (* Wait until cancelled *)
  while not (Atomic.get cancel) do
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.5
  done;
  Eio.Switch.fail sw Exit
