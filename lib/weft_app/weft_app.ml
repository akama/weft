open Weft_types

let default_formats_path = "formats.toml"
let default_sources_path = "sources.toml"

let find_config_file name =
  let candidates = [
    name;
    Filename.concat (Filename.concat (Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp") ".config/weft") name;
    Filename.concat "/etc/weft" name;
  ] in
  List.find_opt Sys.file_exists candidates

type cli_args = {
  formats_path : string option;
  sources_path : string option;
  initial_terms : string list;
  dump_mode : bool;
  dump_limit : int;
  dump_json : bool;
}

let parse_cli () =
  let args = ref {
    formats_path = None;
    sources_path = None;
    initial_terms = [];
    dump_mode = false;
    dump_limit = 0;
    dump_json = false;
  } in
  let argv = Array.to_list Sys.argv |> List.tl in
  let rec parse = function
    | [] -> ()
    | "--formats" :: path :: rest ->
      args := { !args with formats_path = Some path }; parse rest
    | "--sources" :: path :: rest ->
      args := { !args with sources_path = Some path }; parse rest
    | "--search" :: term :: rest | "-s" :: term :: rest ->
      args := { !args with initial_terms = term :: !args.initial_terms }; parse rest
    | "--dump" :: rest ->
      args := { !args with dump_mode = true }; parse rest
    | "--limit" :: n :: rest ->
      args := { !args with dump_limit = int_of_string n }; parse rest
    | "--json" :: rest ->
      args := { !args with dump_json = true; dump_mode = true }; parse rest
    | "--help" :: _ | "-h" :: _ ->
      Printf.printf "weft — unified log search TUI\n\n";
      Printf.printf "Usage: weft [OPTIONS]\n\n";
      Printf.printf "Options:\n";
      Printf.printf "  --formats <path>     Path to formats.toml\n";
      Printf.printf "  --sources <path>     Path to sources.toml\n";
      Printf.printf "  -s, --search <term>  Search term (can repeat)\n";
      Printf.printf "  --dump               One-shot: print results to stdout and exit\n";
      Printf.printf "  --json               Dump as JSON lines (implies --dump)\n";
      Printf.printf "  --limit <n>          Max entries to output (0 = unlimited)\n";
      Printf.printf "  -h, --help           Show this help\n\n";
      Printf.printf "TUI Keys:\n";
      Printf.printf "  /     Add search term\n";
      Printf.printf "  j/k   Scroll timeline\n";
      Printf.printf "  Enter Toggle detail pane\n";
      Printf.printf "  Tab   Cycle focus\n";
      Printf.printf "  d     Delete selected term\n";
      Printf.printf "  s     Toggle source\n";
      Printf.printf "  t     Toggle term visibility\n";
      Printf.printf "  q     Quit\n";
      exit 0
    | unknown :: _ ->
      Printf.eprintf "Unknown argument: %s (try --help)\n" unknown;
      exit 1
  in
  parse argv;
  { !args with initial_terms = List.rev !args.initial_terms }

let load_config cli =
  let formats_file = match cli.formats_path with
    | Some p -> p
    | None -> match find_config_file default_formats_path with
      | Some p -> p
      | None ->
        Printf.eprintf "Warning: No formats.toml found. Using defaults.\n";
        ""
  in
  let sources_file = match cli.sources_path with
    | Some p -> p
    | None -> match find_config_file default_sources_path with
      | Some p -> p
      | None ->
        Printf.eprintf "Error: No sources.toml found. Create one or use --sources.\n";
        exit 1
  in
  let formats_config = if formats_file = "" then { formats = [] }
    else Weft_config.parse_formats_file formats_file in
  let sources_config = Weft_config.parse_sources_file sources_file in
  Weft_config.validate_sources sources_config formats_config;
  (formats_config, sources_config)

(* Format a log entry for terminal output *)
let format_entry (entry : log_entry) =
  let (_, ((hh, mm, ss), _)) = Ptime.to_date_time entry.timestamp in
  let _, ps = Ptime.to_span entry.timestamp |> Ptime.Span.to_d_ps in
  let ms = Int64.to_int (Int64.rem (Int64.div ps 1_000_000_000L) 1000L) in
  let ts = Printf.sprintf "%02d:%02d:%02d.%03d" hh mm ss ms in
  let src = if String.length entry.source > 12 then
    String.sub entry.source 0 12
  else entry.source in
  let terms_str = match entry.terms with
    | [] -> ""
    | terms -> " [" ^ String.concat "," terms ^ "]"
  in
  let meta_str = match entry.metadata with
    | [] -> ""
    | pairs ->
      let shown = List.filteri (fun i _ -> i < 4) pairs in
      " {" ^ String.concat ", " (List.map (fun (k, v) ->
        let v_short = if String.length v > 30 then String.sub v 0 30 ^ "..." else v in
        k ^ "=" ^ v_short
      ) shown) ^ "}"
  in
  Printf.sprintf "%s [%-12s] %s%s%s" ts src entry.raw terms_str meta_str

let entry_to_json (entry : log_entry) =
  let fields = [
    ("timestamp", `String (Ptime.to_rfc3339 entry.timestamp));
    ("source", `String entry.source);
    ("raw", `String entry.raw);
    ("terms", `List (List.map (fun t -> `String t) entry.terms));
    ("metadata", `Assoc (List.map (fun (k, v) -> (k, `String v)) entry.metadata));
  ] in
  Yojson.Basic.to_string (`Assoc fields)

let run_dump ~search ~cli =
  let has_terms = cli.initial_terms <> [] in
  let entries = if has_terms then
    Weft_search.search search ~time_range:None
  else
    Weft_search.load_all search
  in
  let count = ref 0 in
  let limit = cli.dump_limit in
  let rec print_seq seq =
    if limit > 0 && !count >= limit then ()
    else match seq () with
    | Seq.Nil -> ()
    | Seq.Cons ((entry : log_entry), rest) ->
      incr count;
      if cli.dump_json then
        print_endline (entry_to_json entry)
      else
        print_endline (format_entry entry);
      print_seq rest
  in
  print_seq entries;
  if not cli.dump_json then
    Printf.eprintf "-- %d entries\n" !count

let run env =
  let cli = parse_cli () in
  let (formats_config, sources_config) = load_config cli in

  let fs = Eio.Stdenv.fs env in
  let proc = Eio.Stdenv.process_mgr env in

  (* Initialize cache *)
  let cache = Weft_cache.create ~fs sources_config.cache in
  List.iter (fun (src : source_config) ->
    ignore (Weft_cache.init_source cache ~source_name:src.name ~format:src.format)
  ) sources_config.sources;

  (* Initialize connection pool *)
  let pool = Weft_connection.Conn_pool.create ~limits:sources_config.limits in
  List.iter (fun src -> Weft_connection.Conn_pool.add_source pool src)
    sources_config.sources;

  (* Connect to sources *)
  List.iter (fun (src : source_config) ->
    match Weft_connection.Conn_pool.connect_source proc pool src.name with
    | Ok () -> ()
    | Error e -> Printf.eprintf "Warning: Failed to connect to %s: %s\n" src.name e
  ) sources_config.sources;

  (* Initialize search engine *)
  let search = Weft_search.create
    ~cache
    ~sources:sources_config.sources
    ~formats:formats_config
    ~general:sources_config.general in

  (* Add initial search terms *)
  List.iter (fun term ->
    ignore (Weft_search.add_term search term)
  ) cli.initial_terms;

  if cli.dump_mode then begin
    run_dump ~search ~cli;
    Weft_connection.Conn_pool.close_all pool
  end else begin
    (* Create TUI model *)
    let model = Weft_tui.create ~search in

    let source_statuses = List.map (fun (src : source_config) ->
      (src.name, Weft_connection.Conn_pool.get_status pool src.name)
    ) sources_config.sources in
    Weft_tui.Sidebar.update_sources model.sidebar source_statuses;

    if cli.initial_terms <> [] then begin
      let entries = Weft_search.search search ~time_range:None in
      let entry_list = List.of_seq (Seq.take 10000 entries) in
      Weft_tui.Timeline.set_entries model.timeline entry_list
    end;

    let term = Notty_unix.Term.create () in
    Fun.protect (fun () ->
      Weft_tui.run_ui model term
    ) ~finally:(fun () ->
      Notty_unix.Term.release term;
      Weft_connection.Conn_pool.close_all pool
    )
  end
