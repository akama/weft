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

  let model = Weft_tui.create ~search ~time_range in

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

  (* Default to last hour when no range and no terms *)
  if model.time_range = None && initial_terms = [] then begin
    let now = Ptime_clock.now () in
    let one_hour = Ptime.Span.of_int_s 3600 in
    model.time_range <- Some {
      start_ = (match Ptime.sub_span now one_hour with
                | Some t -> t | None -> now);
      end_ = None;
    }
  end;

  (* Initial synchronous load *)
  Weft_tui.refresh_search model;
  update_cache_stats ();

  (* Communication channels *)
  let search_requests : Weft_tui.search_params Eio.Stream.t =
    Eio.Stream.create 1 in
  let search_results : (log_entry list) Eio.Stream.t =
    Eio.Stream.create 1 in

  Fun.protect ~finally:(fun () ->
    Notty_unix.Term.release term;
    Weft_connection.Conn_pool.close_all pool
  ) (fun () ->
    try Eio.Switch.run (fun sw ->

    (* Search fiber — picks up requests, runs search, posts results *)
    Eio.Fiber.fork ~sw (fun () ->
      while true do
        (* Wait for a search request *)
        let params = Eio.Stream.take search_requests in
        (* Drain any queued requests — only run the latest *)
        let params = ref params in
        let rec drain () =
          match Eio.Stream.take_nonblocking search_requests with
          | Some p -> params := p; drain ()
          | None -> ()
        in
        drain ();
        let results = Weft_tui.do_search_with search !params in
        (* Clear any old results and post new *)
        ignore (Eio.Stream.take_nonblocking search_results);
        Eio.Stream.add search_results results
      done
    );

    (* TUI fiber — main event loop *)
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
        let params = Weft_tui.snapshot_params model in
        (* Non-blocking add — if channel full, drain and re-add *)
        ignore (Eio.Stream.take_nonblocking search_requests);
        Eio.Stream.add search_requests params
      end;

      (* Pick up completed search results *)
      (match Eio.Stream.take_nonblocking search_results with
       | Some results ->
         Weft_tui.Timeline.set_entries model.timeline results;
         update_cache_stats ();
         let range_desc = Weft_tui.format_time_range model.time_range in
         Weft_tui.Status.set model.status
           (Printf.sprintf "%d entries [%s]" (List.length results) range_desc)
       | None -> ());

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

    Eio.Switch.fail sw Exit)
    with Exit -> ())

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

  while not (Atomic.get cancel) do
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.5
  done;
  Eio.Switch.fail sw Exit
