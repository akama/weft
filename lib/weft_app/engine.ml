(* Engine: Eio fiber-based runtime for weft.

   Fiber tree:
   main switch
   ├── search fiber (picks up search requests, runs them, posts results)
   └── TUI fiber (polls terminal input via Eio, renders, dispatches)

   Communication via Eio.Stream:
   - search_requests: TUI -> search fiber (search params)
   - search_results:  search fiber -> TUI (entry list)
*)

open Weft_types

(* Build a Loki query function using Eio networking *)
let make_loki_query (net : _ Eio.Net.t) : Weft_search.loki_query_fn =
  fun ~url ~headers ->
    try
      Eio.Switch.run @@ fun sw ->
      let client = Cohttp_eio.Client.make ~https:None net in
      let uri = Uri.of_string url in
      let h = List.fold_left (fun h (k, v) -> Http.Header.add h k v)
        (Http.Header.init_with "Accept" "application/json") headers in
      let (resp, body) = Cohttp_eio.Client.get client ~sw ~headers:h uri in
      let status = Http.Response.status resp in
      let buf = Buffer.create 4096 in
      let br = Eio.Buf_read.of_flow ~max_size:(10 * 1024 * 1024) body in
      (try while true do
         let chunk = Eio.Buf_read.line br in
         Buffer.add_string buf chunk;
         Buffer.add_char buf '\n'
       done with End_of_file -> ());
      let body_str = Buffer.contents buf in
      if Http.Status.to_int status >= 400 then begin
        Printf.eprintf "Loki HTTP %d: %s\n"
          (Http.Status.to_int status)
          (String.sub body_str 0 (min 200 (String.length body_str)));
        None
      end else
        Some body_str
    with
    | Eio.Io _ as e ->
      Printf.eprintf "Loki connection error: %s\n" (Printexc.to_string e);
      None

(* Common initialization *)
let init_runtime ~env ~formats_config ~sources_config ~initial_terms =
  let fs = Eio.Stdenv.fs env in
  let proc = Eio.Stdenv.process_mgr env in
  let net = Eio.Stdenv.net env in
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
  let loki_query = make_loki_query net in
  let search = Weft_search.create ~cache
    ~sources:sources_config.sources
    ~formats:formats_config
    ~general:sources_config.general
    ~conn_pool:pool ~loki_query () in
  List.iter (fun t -> ignore (Weft_search.add_term search t)) initial_terms;
  (cache, pool, search)

(* Run the full engine with TUI *)
let run_with_tui ~env ~formats_config ~sources_config ~initial_terms
    ~(time_range : time_range option) =
  let (cache, pool, search) =
    init_runtime ~env ~formats_config ~sources_config ~initial_terms in

  let clock = Eio.Stdenv.clock env in
  let terms_ref = ref initial_terms in

  (* Parse default time range from config *)
  let default_time_range_sec =
    let default_str = sources_config.general.default_time_range in
    let re = Re.compile (Re.Pcre.re {|^(?:(\d+)h)?(?:(\d+)m)?(?:(\d+)s)?$|}) in
    match Re.exec_opt re default_str with
    | Some g ->
      let h = try int_of_string (Re.Group.get g 1) with Not_found -> 0 in
      let m = try int_of_string (Re.Group.get g 2) with Not_found -> 0 in
      let s = try int_of_string (Re.Group.get g 3) with Not_found -> 0 in
      let total = h * 3600 + m * 60 + s in
      if total > 0 then total else Weft_constants.default_time_range_sec
    | None -> Weft_constants.default_time_range_sec
  in

  let model = Weft_tui.create ~search ~time_range
    ~default_time_range_sec () in

  (* Wire status callback *)
  Weft_search.set_status_callback (fun msg ->
    Weft_tui.Status.set model.status msg);

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
    let fmt_date t =
      let ((_y, mo, d), _) = Ptime.to_date_time t in
      Printf.sprintf "%02d-%02d" mo d in
    let fmt_time t =
      let (_, ((hh, mm, ss), _)) = Ptime.to_date_time t in
      Printf.sprintf "%02d:%02d:%02d" hh mm ss in
    let (fd, ft, td, tt) = match earliest, latest with
      | Some s, Some e -> (fmt_date s, fmt_time s, fmt_date e, fmt_time e)
      | Some s, None -> (fmt_date s, fmt_time s, "", "now")
      | _ -> ("", "", "", "")
    in
    Weft_tui.Sidebar.update_cache_info model.sidebar
      ~size_mb ~segments:total_segments
      ~from_date:fd ~from_time:ft ~to_date:td ~to_time:tt
  in
  update_source_statuses ();

  let term = Notty_unix.Term.create () in
  let (w, h) = Notty_unix.Term.size term in
  model.width <- w;
  model.height <- h;

  (* Default to configured default_time_range when no --since/--until provided *)
  if model.time_range = None then begin
    let now = Ptime_clock.now () in
    let span = Ptime.Span.of_int_s default_time_range_sec in
    model.time_range <- Some {
      start_ = (match Ptime.sub_span now span with
                | Some t -> t | None -> now);
      end_ = None;
    }
  end;

  (* Queue initial load as a background search — TUI renders immediately *)
  Weft_tui.Status.set model.status "Loading...";
  Weft_tui.needs_refresh := true;

  (* Communication channels *)
  let search_requests : Weft_tui.search_params Eio.Stream.t =
    Eio.Stream.create 1 in
  let search_results : (log_entry list) Eio.Stream.t =
    Eio.Stream.create 1 in
  (* Tail entries — new entries from live source fibers *)
  let tail_entries : log_entry Eio.Stream.t =
    Eio.Stream.create 4096 in
  let tail_cancel = Atomic.make false in
  let tail_dedup = Weft_merge.Dedup.create
    ~max_size:Weft_constants.tail_dedup_capacity () in

  let fs = Eio.Stdenv.fs env in
  (* Collect tail buffer flush functions so we can flush before search *)
  let tail_flushers : (unit -> unit) list ref = ref [] in

  Fun.protect ~finally:(fun () ->
    Atomic.set tail_cancel true;
    Notty_unix.Term.release term;
    Weft_connection.Conn_pool.close_all pool
  ) (fun () ->
    try Eio.Switch.run (fun sw ->

    (* Search fiber — picks up requests, runs search with timeout, posts results *)
    let catch_up_timeout =
      float_of_int sources_config.limits.catch_up_timeout_sec in
    Eio.Fiber.fork ~sw (fun () ->
      (try while not (Atomic.get tail_cancel) do
        let params = Eio.Stream.take search_requests in
        let params = ref params in
        let rec drain () =
          match Eio.Stream.take_nonblocking search_requests with
          | Some p -> params := p; drain ()
          | None -> ()
        in
        drain ();
        (match Eio.Time.with_timeout clock catch_up_timeout (fun () ->
           Ok (Weft_tui.do_search_with search !params)
         ) with
         | Ok results ->
           ignore (Eio.Stream.take_nonblocking search_results);
           Eio.Stream.add search_results results
         | Error `Timeout ->
           Weft_tui.Status.set model.status
             (Printf.sprintf "Search timed out after %ds"
                sources_config.limits.catch_up_timeout_sec))
      done with Eio.Cancel.Cancelled _ -> ())
    );

    (* Per-source tail fibers — watch for new entries and push to tail_entries.
       Each source gets an active cache segment for tail writes (design §17). *)
    List.iter (fun (src : source_config) ->
      let fmt = Weft_config.resolve_format formats_config src.format in
      let pipeline = Option.map Weft_middleware.Pipeline.create fmt in
      let pipeline_state = Option.map (fun pl ->
        Weft_middleware.Pipeline.create_stream_state pl ~source:src.name
      ) pipeline in

      (* Active cache segment for this source's tail data (design §17).
         Created lazily on first flush to avoid empty segment files. *)
      let active_seg : Weft_types.segment option ref = ref None in
      let cache_buf = Buffer.create 4096 in
      let cache_line_count = ref 0 in
      let cache_flush_interval = Weft_constants.cache_flush_interval in

      let ensure_seg () =
        match !active_seg with
        | Some s -> s
        | None ->
          let s = Weft_cache.new_segment cache
            ~source_name:src.name ~origin:"tail" in
          active_seg := Some s; s
      in

      let flush_cache_buf () =
        if Buffer.length cache_buf > 0 then begin
          let seg = ensure_seg () in
          let data = Buffer.contents cache_buf in
          Buffer.clear cache_buf;
          ignore (Weft_cache.store_data cache ~source_name:src.name
            seg data)
        end
      in

      (* Register this source's flush for pre-search cache sync *)
      tail_flushers := flush_cache_buf :: !tail_flushers;

      let seal_active_seg () =
        flush_cache_buf ();
        match !active_seg with
        | None -> ()  (* nothing to seal — no data was written *)
        | Some seg ->
          let end_time = Ptime_clock.now () in
          ignore (Weft_cache.seal_segment cache
            ~source_name:src.name seg ~end_time)
      in

      let new_active_seg _origin =
        (* Reset — next flush will create a fresh segment lazily *)
        active_seg := None;
        cache_line_count := 0
      in

      let emit_line source line =
        (* Write raw line to active cache segment *)
        Buffer.add_string cache_buf line;
        Buffer.add_char cache_buf '\n';
        incr cache_line_count;
        if !cache_line_count mod cache_flush_interval = 0 then
          flush_cache_buf ();
        let entries_to_emit = match pipeline_state with
          | None ->
            [{ timestamp = Ptime_clock.now (); raw = line; source;
               terms = []; metadata = [] }]
          | Some state ->
            Weft_middleware.Pipeline.feed_line state line
        in
        (* Tag with current search terms *)
        let current_terms = !terms_ref in
        let term_res = List.map (fun t ->
          (t, Re.compile (Re.Pcre.re (Re.Pcre.quote t)))
        ) current_terms in
        List.iter (fun (entry : log_entry) ->
          (* Check time range *)
          let in_range = match model.time_range with
            | None -> true
            | Some tr ->
              Ptime.is_later entry.timestamp ~than:tr.start_ &&
              (match tr.end_ with
               | None -> true
               | Some end_t -> Ptime.is_earlier entry.timestamp ~than:end_t)
          in
          (* Check source is enabled *)
          let source_ok = Weft_tui.Sidebar.is_source_enabled
            model.sidebar entry.source in
          let dominated = match term_res with
            | [] -> true
            | _ -> List.exists (fun (_t, re) -> Re.execp re entry.raw) term_res
          in
          if dominated && source_ok && in_range then begin
            let entry = match term_res with
              | [] -> entry
              | _ ->
                let matched = List.filter_map (fun (t, re) ->
                  if Re.execp re entry.raw then Some t else None
                ) term_res in
                { entry with terms = matched }
            in
            Eio.Stream.add tail_entries entry
          end
        ) entries_to_emit
      in

      match src.source_type with
      | File ->
        (match src.path with
         | Some path when Sys.file_exists path ->
           Eio.Fiber.fork ~sw (fun () ->
             let adapter : Weft_source.Local_file.t = {
               config = src; path; fs;
             } in
             let drain_timeout = match fmt with
               | Some f -> (match f.rotation with
                 | Some rc -> float_of_int rc.drain_timeout_sec
                 | None -> 5.0)
               | None -> 5.0
             in
             let rotation_cbs : Weft_source.Local_file.rotation_callbacks = {
               on_seal = (fun () ->
                 (* Seal and remember the old tail segment for removal *)
                 let old_seg = !active_seg in
                 seal_active_seg ();
                 Weft_tui.Status.set model.status
                   (Printf.sprintf "Rotation: sealed %s, re-discovering archives..."
                      src.name);
                 (* Re-discover archives — the rotated file (.1) is the
                    authoritative source. Fetch it to replace our tail segment. *)
                 let archives = Weft_source.Archive.discover_local ~path
                   |> Weft_source.Archive.sort_by_mtime in
                 Weft_cache.update_archives cache ~source_name:src.name archives;
                 let fetched_any = ref false in
                 List.iter (fun (archive : archive_info) ->
                   let origin = Filename.basename archive.remote_path in
                   let already = match Weft_cache.get_manifest cache src.name with
                     | None -> false
                     | Some m -> List.exists (fun (s : segment) ->
                         s.origin = origin) m.segments in
                   if not already then begin
                     if Weft_source.Archive.is_compressed archive.remote_path then begin
                       match Weft_source.Archive.decompressor_for archive.remote_path with
                       | Some cmd ->
                         let tmp = Filename.temp_file "weft_rotate_" ".log" in
                         let shell_quote s =
                           "'" ^ String.concat "'\\''"
                             (String.split_on_char '\'' s) ^ "'" in
                         let ret = Sys.command
                           (Printf.sprintf "%s %s > %s 2>/dev/null"
                              cmd (shell_quote archive.remote_path)
                              (shell_quote tmp)) in
                         if ret = 0 then begin
                           ignore (Weft_cache.cache_file cache
                             ~source_name:src.name ~origin ~path:tmp);
                           fetched_any := true
                         end;
                         (try Sys.remove tmp with Sys_error _ -> ())
                       | None -> ()
                     end else begin
                       ignore (Weft_cache.cache_file cache
                         ~source_name:src.name ~origin
                         ~path:archive.remote_path);
                       fetched_any := true
                     end
                   end
                 ) archives;
                 (* Remove the old tail segment — the fetched archive is
                    the authoritative copy of the same data *)
                 if !fetched_any then
                   (match old_seg with
                    | Some seg -> Weft_cache.remove_segment cache
                        ~source_name:src.name seg
                    | None -> ());
                 Weft_tui.Status.set model.status
                   (Printf.sprintf "Rotation: %s archives updated" src.name));
               on_new = (fun () ->
                 new_active_seg (Filename.basename path ^ " (post-rotate)");
                 Weft_tui.Status.set model.status
                   (Printf.sprintf "Rotation: tailing new %s" src.name));
             } in
             (* Eio-aware wait: yields to scheduler instead of blocking *)
             let eio_wait_readable fd timeout =
               match Eio.Time.with_timeout clock timeout (fun () ->
                 Eio_unix.await_readable fd; Ok true
               ) with
               | Ok true -> true
               | Ok false -> false
               | Error `Timeout -> false
             in
             let eio_sleep secs = Eio.Time.sleep clock secs in
             (try
                Weft_source.Local_file.tail adapter ~terms:[]
                  ~emit:(fun entry -> emit_line src.name entry.raw)
                  ~cancel:tail_cancel ~on_rotation:rotation_cbs
                  ~drain_timeout ~wait_readable:eio_wait_readable
                  ~sleep:eio_sleep ()
              with exn ->
                Weft_tui.Status.set model.status
                  (Printf.sprintf "Tail %s ended: %s" src.name
                     (Printexc.to_string exn)))
           )
         | _ -> ())

      | Remote ->
        (match src.transport, src.path with
         | Some _transport, Some path ->
           let ssh = match Weft_connection.Conn_pool.get_connection pool src.name with
             | Some conn -> conn.ssh
             | None -> None
           in
           (match ssh with
            | Some ssh ->
              Eio.Fiber.fork ~sw (fun () ->
                Weft_tui.Status.set model.status
                  (Printf.sprintf "Tailing %s via SSH..." src.name);
                let tail_cmd = ["tail"; "-n"; "0"; "-F"; path] in
                let eio_wait_fds fds timeout =
                  (* Wait for any fd to be readable, Eio-cooperatively.
                     We check each fd — first one ready wins. *)
                  match Eio.Time.with_timeout clock timeout (fun () ->
                    (* Try each fd in turn with a tiny timeout *)
                    let ready = ref [] in
                    let found = ref false in
                    while not !found do
                      List.iter (fun fd ->
                        if not !found then
                          let r, _, _ = Unix.select [fd] [] [] 0.0 in
                          if r <> [] then begin
                            ready := fd :: !ready;
                            found := true
                          end
                      ) fds;
                      if not !found then Eio.Fiber.yield ()
                    done;
                    Ok !ready
                  ) with
                  | Ok ready -> ready
                  | Error `Timeout -> []
                in
                (try
                   Weft_connection.Ssh_control.run_streaming ssh tail_cmd
                     ~on_line:(fun line -> emit_line src.name line)
                     ~on_stderr:(fun line ->
                       match Weft_source.Rotation.detect_from_tail_stderr line with
                       | Some Weft_source.Rotation.File_renamed ->
                         let old_seg = !active_seg in
                         seal_active_seg ();
                         Weft_tui.Status.set model.status
                           (Printf.sprintf "SSH rotation: %s, fetching archives..."
                              src.name);
                         let archives = Weft_source.Archive.discover_remote
                           ~ssh ~path
                           |> Weft_source.Archive.sort_by_mtime in
                         Weft_cache.update_archives cache
                           ~source_name:src.name archives;
                         let fetched_any = ref false in
                         List.iter (fun (archive : archive_info) ->
                           let origin = Filename.basename archive.remote_path in
                           let already = match Weft_cache.get_manifest cache
                             src.name with
                             | None -> false
                             | Some m -> List.exists (fun (s : segment) ->
                                 s.origin = origin) m.segments in
                           if not already then begin
                             let decomp = match
                               Weft_source.Archive.decompressor_for
                                 archive.remote_path with
                               | Some cmd -> cmd | None -> "cat" in
                             (try
                                let data =
                                  Weft_connection.Ssh_control.run_command ssh
                                    [decomp; archive.remote_path] in
                                if String.length data > 0 then begin
                                  let tmp = Filename.temp_file "weft_ssh_rot_" ".log" in
                                  let oc = open_out tmp in
                                  output_string oc data;
                                  close_out oc;
                                  ignore (Weft_cache.cache_file cache
                                    ~source_name:src.name ~origin ~path:tmp);
                                  fetched_any := true;
                                  (try Sys.remove tmp with Sys_error _ -> ())
                                end
                              with Failure msg ->
                                Weft_tui.Status.set model.status
                                  (Printf.sprintf "Fetch %s failed: %s" origin msg))
                           end
                         ) archives;
                         (* Remove old tail segment — archive is authoritative *)
                         if !fetched_any then
                           (match old_seg with
                            | Some seg -> Weft_cache.remove_segment cache
                                ~source_name:src.name seg
                            | None -> ());
                         new_active_seg (Filename.basename path ^ " (post-rotate)");
                         Weft_tui.Status.set model.status
                           (Printf.sprintf "SSH rotation: %s archives updated"
                              src.name)
                       | Some Weft_source.Rotation.File_truncated ->
                         seal_active_seg ();
                         new_active_seg (Filename.basename path ^ " (truncated)");
                         Weft_tui.Status.set model.status
                           (Printf.sprintf "SSH truncate: %s" src.name)
                       | None -> ())
                     ~cancel:tail_cancel
                     ~wait_fds:eio_wait_fds ()
                 with Failure msg ->
                   Weft_tui.Status.set model.status
                     (Printf.sprintf "SSH tail %s: %s" src.name msg))
              )
            | None -> ())
         | _ -> ())

      | Loki ->
        (match src.url with
         | Some _url ->
           Eio.Fiber.fork ~sw (fun () ->
             while not (Atomic.get tail_cancel) do
               Eio.Time.sleep clock Weft_constants.loki_tui_poll_sec;
               if not (Atomic.get tail_cancel) then begin
                 let now = Ptime_clock.now () in
                 let lookback = match Ptime.sub_span now
                   (Ptime.Span.of_int_s Weft_constants.loki_tail_lookback_sec) with
                   | Some t -> t | None -> now in
                 let params = {
                   Weft_tui.sp_time_range = Some { start_ = lookback;
                                                    end_ = Some now };
                   sp_terms = !terms_ref;
                   sp_disabled = model.sidebar.disabled_sources;
                 } in
                 let results = Weft_tui.do_search_with search params in
                 List.iter (fun entry ->
                   Eio.Stream.add tail_entries entry
                 ) results
               end
             done
           )
         | None -> ())

      | Directory ->
        (* Expand glob and create per-file tail fibers (§12.2) *)
        (match src.glob with
         | Some glob_pattern ->
           let files = Weft_source.Local_dir.expand_glob glob_pattern in
           List.iter (fun filepath ->
             if Sys.file_exists filepath then begin
               let sub_name = Printf.sprintf "%s:%s" src.name
                 (Filename.basename filepath) in
               let sub_pipeline_state = Option.map (fun pl ->
                 Weft_middleware.Pipeline.create_stream_state pl ~source:sub_name
               ) pipeline in
               (* Each sub-file gets its own tail fiber *)
               let sub_seg = ref (Weft_cache.new_segment cache
                 ~source_name:sub_name ~origin:"tail") in
               ignore sub_seg;
               let sub_buf = Buffer.create 4096 in
               let sub_line_count = ref 0 in
               let flush_sub () =
                 if Buffer.length sub_buf > 0 then begin
                   let data = Buffer.contents sub_buf in
                   Buffer.clear sub_buf;
                   ignore (Weft_cache.store_data cache
                     ~source_name:src.name !sub_seg data)
                 end in
               Eio.Fiber.fork ~sw (fun () ->
                 let adapter : Weft_source.Local_file.t = {
                   config = { src with name = sub_name; path = Some filepath };
                   path = filepath; fs;
                 } in
                 let eio_wait fd timeout =
                   match Eio.Time.with_timeout clock timeout (fun () ->
                     Eio_unix.await_readable fd; Ok true
                   ) with
                   | Ok true -> true | Ok false -> false
                   | Error `Timeout -> false
                 in
                 let sub_emit_line line =
                   Buffer.add_string sub_buf line;
                   Buffer.add_char sub_buf '\n';
                   incr sub_line_count;
                   if !sub_line_count mod 50 = 0 then flush_sub ();
                   let entries_to_emit = match sub_pipeline_state with
                     | None ->
                       [{ timestamp = Ptime_clock.now (); raw = line;
                          source = sub_name; terms = []; metadata = [] }]
                     | Some state ->
                       Weft_middleware.Pipeline.feed_line state line
                   in
                   let current_terms = !terms_ref in
                   let term_res = List.map (fun t ->
                     (t, Re.compile (Re.Pcre.re (Re.Pcre.quote t)))
                   ) current_terms in
                   List.iter (fun (entry : log_entry) ->
                     let in_range = match model.time_range with
                       | None -> true
                       | Some tr ->
                         Ptime.is_later entry.timestamp ~than:tr.start_ &&
                         (match tr.end_ with
                          | None -> true
                          | Some end_t -> Ptime.is_earlier entry.timestamp ~than:end_t)
                     in
                     let source_ok = Weft_tui.Sidebar.is_source_enabled
                       model.sidebar entry.source in
                     let dominated = match term_res with
                       | [] -> true
                       | _ -> List.exists (fun (_t, re) ->
                           Re.execp re entry.raw) term_res in
                     if dominated && source_ok && in_range then begin
                       let entry = match term_res with
                         | [] -> entry
                         | _ ->
                           let matched = List.filter_map (fun (t, re) ->
                             if Re.execp re entry.raw then Some t else None
                           ) term_res in
                           { entry with terms = matched }
                       in
                       Eio.Stream.add tail_entries entry
                     end
                   ) entries_to_emit
                 in
                 let eio_sleep secs = Eio.Time.sleep clock secs in
                 (try
                    Weft_source.Local_file.tail adapter ~terms:[]
                      ~emit:(fun entry -> sub_emit_line entry.raw)
                      ~cancel:tail_cancel ~wait_readable:eio_wait
                      ~sleep:eio_sleep ()
                  with exn ->
                    Weft_tui.Status.set model.status
                      (Printf.sprintf "Tail %s: %s" sub_name
                         (Printexc.to_string exn)))
               )
             end
           ) files
         | None -> ())

      | Journald ->
        (match src.journal_unit with
         | Some _unit_name ->
           let ssh = match Weft_connection.Conn_pool.get_connection pool src.name with
             | Some conn -> conn.ssh
             | None -> None
           in
           let journald : Weft_source.Journald.t = {
             config = src; ssh;
           } in
           Eio.Fiber.fork ~sw (fun () ->
             Weft_tui.Status.set model.status
               (Printf.sprintf "Tailing journal %s..." src.name);
             let eio_sleep secs = Eio.Time.sleep clock secs in
             (try
                Weft_source.Journald.tail journald
                  ~emit:(fun (entry : log_entry) ->
                    (* Write to cache + emit to TUI *)
                    Buffer.add_string cache_buf entry.raw;
                    Buffer.add_char cache_buf '\n';
                    incr cache_line_count;
                    if !cache_line_count mod cache_flush_interval = 0 then
                      flush_cache_buf ();
                    let current_terms = !terms_ref in
                    let term_res = List.map (fun t ->
                      (t, Re.compile (Re.Pcre.re (Re.Pcre.quote t)))
                    ) current_terms in
                    let in_range = match model.time_range with
                      | None -> true
                      | Some tr ->
                        Ptime.is_later entry.timestamp ~than:tr.start_ &&
                        (match tr.end_ with
                         | None -> true
                         | Some end_t ->
                           Ptime.is_earlier entry.timestamp ~than:end_t)
                    in
                    let source_ok = Weft_tui.Sidebar.is_source_enabled
                      model.sidebar entry.source in
                    let dominated = match term_res with
                      | [] -> true
                      | _ -> List.exists (fun (_t, re) ->
                          Re.execp re entry.raw) term_res
                    in
                    if dominated && source_ok && in_range then begin
                      let entry = match term_res with
                        | [] -> entry
                        | _ ->
                          let matched = List.filter_map (fun (t, re) ->
                            if Re.execp re entry.raw then Some t else None
                          ) term_res in
                          { entry with terms = matched }
                      in
                      Eio.Stream.add tail_entries entry
                    end)
                  ~cancel:tail_cancel ~sleep:eio_sleep ()
              with exn ->
                Weft_tui.Status.set model.status
                  (Printf.sprintf "Journal tail %s: %s" src.name
                     (Printexc.to_string exn)))
           )
         | None -> ())
    ) sources_config.sources;

    (* TUI event loop *)
    let (input_fd, _output_fd) = Notty_unix.Term.fds term in

    let handle_terminal_event () =
      match Notty_unix.Term.event term with
      | `End | `Key (`ASCII 'C', [`Ctrl]) -> false
      | `Key (key, _mods) ->
        Weft_tui.handle_key model key;
        terms_ref := Weft_search.enabled_terms search;
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
      (* Dispatch pending search requests *)
      if !(Weft_tui.needs_refresh) then begin
        Weft_tui.needs_refresh := false;
        Weft_tui.Timeline.freeze_auto_follow := true;
        (* Flush all tail buffers to cache so search finds recent data *)
        List.iter (fun f -> f ()) !tail_flushers;
        let params = Weft_tui.snapshot_params model in
        ignore (Eio.Stream.take_nonblocking search_requests);
        Eio.Stream.add search_requests params
      end;

      (* Pick up completed search results *)
      (match Eio.Stream.take_nonblocking search_results with
       | Some results ->
         Weft_tui.Timeline.freeze_auto_follow := false;
         Weft_tui.Timeline.set_entries model.timeline results;
         update_cache_stats ();
         let range_desc = Weft_tui.format_time_range model.time_range in
         Weft_tui.Status.set model.status
           (Printf.sprintf "%d entries [%s]" (List.length results) range_desc)
       | None -> ());

      (* Pick up new tail entries — dedup against existing timeline (§13.3) *)
      let new_count = ref 0 in
      let rec drain_tail () =
        match Eio.Stream.take_nonblocking tail_entries with
        | Some entry ->
          if not (Weft_merge.Dedup.check_and_mark tail_dedup entry) then begin
            Weft_tui.Timeline.append_entry model.timeline entry;
            incr new_count
          end;
          drain_tail ()
        | None -> ()
      in
      drain_tail ();
      if !new_count > 0 then
        Weft_tui.Status.set model.status
          (Printf.sprintf "+%d new (%d total)"
             !new_count (Weft_tui.Timeline.entry_count model.timeline));

      (* Render *)
      let img = Weft_tui.render model in
      Notty_unix.Term.image term img;

      (* Wait for terminal input OR timeout (cooperative with Eio scheduler) *)
      let got_input =
        match Eio.Time.with_timeout clock 0.05 (fun () ->
          Eio_unix.await_readable input_fd;
          Ok true
        ) with
        | Ok true -> true
        | Ok false -> false
        | Error `Timeout -> false
      in
      if got_input || Notty_unix.Term.pending term then
        running := handle_terminal_event ()
    done;

    (* Signal all fibers to stop *)
    Atomic.set tail_cancel true;
    Eio.Switch.fail sw Exit)
    with Exit | Eio.Cancel.Cancelled _ -> ())

(* Run in dump mode *)
let run_dump ~env ~formats_config ~sources_config ~initial_terms
    ~limit ~json ~(time_range : time_range option) =
  let (cache, _pool, search) =
    init_runtime ~env ~formats_config ~sources_config ~initial_terms in

  let has_terms = initial_terms <> [] in
  let entries = if has_terms then
    Weft_search.search search ~time_range
  else
    Weft_search.load_all ?time_range search
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

(* Run in live/follow mode *)
let run_live ~env ~formats_config ~sources_config ~initial_terms ~json =
  let fs = Eio.Stdenv.fs env in
  let (cache, pool, search) =
    init_runtime ~env ~formats_config ~sources_config ~initial_terms in

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

  let emit_raw ~source ~pipeline_state line =
    let should_emit = match term_res with
      | [] -> true
      | _ -> List.exists (fun (_t, re) -> Re.execp re line) term_res
    in
    if should_emit then begin
      let entries_to_emit = match pipeline_state with
        | None ->
          [{ timestamp = Ptime_clock.now (); raw = line; source;
             terms = []; metadata = [] }]
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
           let drain_timeout = match fmt with
             | Some f -> (match f.rotation with
               | Some rc -> float_of_int rc.drain_timeout_sec
               | None -> 5.0)
             | None -> 5.0
           in
           let rotation_cbs : Weft_source.Local_file.rotation_callbacks = {
             on_seal = (fun () ->
               Printf.eprintf "Sealed segment for %s on rotation\n%!" src.name);
             on_new = (fun () ->
               ignore (Weft_cache.new_segment cache
                 ~source_name:src.name ~origin:(Filename.basename path));
               Printf.eprintf "New segment for %s after rotation\n%!" src.name);
           } in
           Weft_source.Local_file.tail adapter ~terms:[]
             ~emit:(fun entry ->
               emit_raw ~source:src.name ~pipeline_state entry.raw)
             ~cancel ~on_rotation:rotation_cbs ~drain_timeout ()
         )
       | _ -> ())

    | Remote ->
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
                   ~on_stderr:(fun line ->
                     match Weft_source.Rotation.detect_from_tail_stderr line with
                     | Some Weft_source.Rotation.File_renamed ->
                       Printf.eprintf "SSH rotation (rename) for %s:%s\n%!"
                         src.name path
                     | Some Weft_source.Rotation.File_truncated ->
                       Printf.eprintf "SSH rotation (truncate) for %s:%s\n%!"
                         src.name path
                     | None ->
                       Printf.eprintf "SSH stderr [%s]: %s\n%!" src.name line)
                   ~cancel ()
               with Failure msg ->
                 Printf.eprintf "SSH tail for %s ended: %s\n%!" src.name msg)
            )
          | None ->
            Printf.eprintf "Warning: no SSH connection for %s, skipping tail\n%!"
              src.name)
       | _ -> ())

    | Directory | Loki -> ()

    | Journald ->
      (match src.journal_unit with
       | Some _unit_name ->
         let ssh = match Weft_connection.Conn_pool.get_connection pool src.name with
           | Some conn -> conn.ssh
           | None -> None
         in
         let journald : Weft_source.Journald.t = {
           config = src; ssh;
         } in
         Eio.Fiber.fork ~sw (fun () ->
           Printf.eprintf "Tailing journal %s...\n%!" src.name;
           (try
              Weft_source.Journald.tail journald
                ~emit:(fun (entry : log_entry) ->
                  emit_raw ~source:src.name ~pipeline_state entry.raw)
                ~cancel ()
            with Failure msg ->
              Printf.eprintf "Journal tail %s ended: %s\n%!" src.name msg)
         )
       | None -> ())
  ) sources_config.sources;

  while not (Atomic.get cancel) do
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.5
  done;
  Eio.Switch.fail sw Exit
