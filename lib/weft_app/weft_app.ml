open Weft_types

let default_formats_path = "formats.toml"
let default_sources_path = "sources.toml"

let find_config_file name =
  (* Search order: current dir, ~/.config/weft/, /etc/weft/ *)
  let candidates = [
    name;
    Filename.concat (Filename.concat (Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp") ".config/weft") name;
    Filename.concat "/etc/weft" name;
  ] in
  List.find_opt Sys.file_exists candidates

let run env =
  let fs = Eio.Stdenv.fs env in
  let proc = Eio.Stdenv.process_mgr env in

  (* Parse CLI args *)
  let formats_path = ref None in
  let sources_path = ref None in
  let initial_terms = ref [] in
  let args = Array.to_list Sys.argv |> List.tl in
  let rec parse_args = function
    | [] -> ()
    | "--formats" :: path :: rest ->
      formats_path := Some path; parse_args rest
    | "--sources" :: path :: rest ->
      sources_path := Some path; parse_args rest
    | "--search" :: term :: rest ->
      initial_terms := term :: !initial_terms; parse_args rest
    | "-s" :: term :: rest ->
      initial_terms := term :: !initial_terms; parse_args rest
    | "--help" :: _ | "-h" :: _ ->
      Printf.printf "weft — unified log search TUI\n\n";
      Printf.printf "Usage: weft [OPTIONS]\n\n";
      Printf.printf "Options:\n";
      Printf.printf "  --formats <path>   Path to formats.toml\n";
      Printf.printf "  --sources <path>   Path to sources.toml\n";
      Printf.printf "  -s, --search <term>  Initial search term (can repeat)\n";
      Printf.printf "  -h, --help         Show this help\n\n";
      Printf.printf "Keys:\n";
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
      Printf.eprintf "Unknown argument: %s\n" unknown;
      exit 1
  in
  parse_args args;

  (* Find and load config files *)
  let formats_file = match !formats_path with
    | Some p -> p
    | None -> match find_config_file default_formats_path with
      | Some p -> p
      | None ->
        Printf.eprintf "Warning: No formats.toml found. Using defaults.\n";
        ""
  in
  let sources_file = match !sources_path with
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

  (* Validate references *)
  Weft_config.validate_sources sources_config formats_config;

  (* Initialize cache *)
  let cache = Weft_cache.create ~fs sources_config.cache in

  (* Initialize cache for each source *)
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
  ) (List.rev !initial_terms);

  (* Create TUI model *)
  let model = Weft_tui.create ~search in

  (* Update sidebar with source statuses *)
  let source_statuses = List.map (fun (src : source_config) ->
    (src.name, Weft_connection.Conn_pool.get_status pool src.name)
  ) sources_config.sources in
  Weft_tui.Sidebar.update_sources model.sidebar source_statuses;

  (* Run initial search if terms provided *)
  if !initial_terms <> [] then begin
    let entries = Weft_search.search search ~time_range:None in
    let entry_list = List.of_seq (Seq.take 10000 entries) in
    Weft_tui.Timeline.set_entries model.timeline entry_list
  end;

  (* Run the TUI *)
  let term = Notty_unix.Term.create () in
  Fun.protect (fun () ->
    Weft_tui.run_ui model term
  ) ~finally:(fun () ->
    Notty_unix.Term.release term;
    Weft_connection.Conn_pool.close_all pool
  )
