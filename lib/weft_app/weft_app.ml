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

type mode = Tui | Dump | Live

type cli_args = {
  formats_path : string option;
  sources_path : string option;
  base_path : string option;
  initial_terms : string list;
  mode : mode;
  dump_limit : int;
  dump_json : bool;
  since : string option;
  until : string option;
}

(* Parse a time spec: "1h", "30m", "2h30m", ISO8601, or HH:MM *)
let parse_time_spec s =
  (* Try relative: "1h", "30m", "2h30m", "90s" *)
  let try_relative () =
    let re = Re.compile (Re.Pcre.re {|^(?:(\d+)h)?(?:(\d+)m)?(?:(\d+)s)?$|}) in
    match Re.exec_opt re s with
    | Some g ->
      let h = try int_of_string (Re.Group.get g 1) with Not_found -> 0 in
      let m = try int_of_string (Re.Group.get g 2) with Not_found -> 0 in
      let sec = try int_of_string (Re.Group.get g 3) with Not_found -> 0 in
      let total = h * 3600 + m * 60 + sec in
      if total > 0 then
        let now = Ptime_clock.now () in
        Ptime.sub_span now (Ptime.Span.of_int_s total)
      else None
    | None -> None
  in
  (* Try HH:MM or HH:MM:SS (today) *)
  let try_time_only () =
    let re = Re.compile (Re.Pcre.re {|^(\d{1,2}):(\d{2})(?::(\d{2}))?$|}) in
    match Re.exec_opt re s with
    | Some g ->
      let hh = int_of_string (Re.Group.get g 1) in
      let mm = int_of_string (Re.Group.get g 2) in
      let ss = try int_of_string (Re.Group.get g 3) with Not_found -> 0 in
      let now = Ptime_clock.now () in
      let ((y, mo, d), _) = Ptime.to_date_time now in
      Ptime.of_date_time ((y, mo, d), ((hh, mm, ss), 0))
    | None -> None
  in
  (* Try ISO8601 *)
  let try_iso () = Weft_time.parse_iso8601 s in
  (* Try in order *)
  match try_relative () with
  | Some t -> Some t
  | None ->
    match try_time_only () with
    | Some t -> Some t
    | None -> try_iso ()

let build_time_range ~since ~until =
  let start_ = match since with
    | None -> None
    | Some s ->
      match parse_time_spec s with
      | Some t -> Some t
      | None ->
        Printf.eprintf "Warning: could not parse --since '%s'\n" s;
        None
  in
  let end_ = match until with
    | None -> None
    | Some s ->
      match parse_time_spec s with
      | Some t -> Some t
      | None ->
        Printf.eprintf "Warning: could not parse --until '%s'\n" s;
        None
  in
  match start_, end_ with
  | Some s, e -> Some { start_ = s; end_ = e }
  | None, Some e ->
    (* --until without --since: from epoch to until *)
    Some { start_ = Ptime.epoch; end_ = Some e }
  | None, None -> None

let parse_cli () =
  let args = ref {
    formats_path = None;
    sources_path = None;
    base_path = None;
    initial_terms = [];
    mode = Tui;
    dump_limit = 0;
    dump_json = false;
    since = None;
    until = None;
  } in
  let argv = Array.to_list Sys.argv |> List.tl in
  let rec parse = function
    | [] -> ()
    | "--formats" :: path :: rest ->
      args := { !args with formats_path = Some path }; parse rest
    | "--sources" :: path :: rest ->
      args := { !args with sources_path = Some path }; parse rest
    | "--base-path" :: path :: rest ->
      args := { !args with base_path = Some path }; parse rest
    | "--search" :: term :: rest | "-s" :: term :: rest ->
      args := { !args with initial_terms = term :: !args.initial_terms }; parse rest
    | "--dump" :: rest ->
      args := { !args with mode = Dump }; parse rest
    | "--live" :: rest | "--follow" :: rest | "-f" :: rest ->
      args := { !args with mode = Live }; parse rest
    | "--limit" :: n :: rest ->
      args := { !args with dump_limit = int_of_string n }; parse rest
    | "--json" :: rest ->
      args := { !args with dump_json = true; mode = Dump }; parse rest
    | "--since" :: t :: rest ->
      args := { !args with since = Some t }; parse rest
    | "--until" :: t :: rest ->
      args := { !args with until = Some t }; parse rest
    | "--help" :: _ | "-h" :: _ ->
      Printf.printf "weft — unified log search TUI\n\n";
      Printf.printf "Usage: weft [OPTIONS]\n\n";
      Printf.printf "Modes:\n";
      Printf.printf "  (default)            Interactive TUI\n";
      Printf.printf "  --dump               One-shot: print matching entries and exit\n";
      Printf.printf "  --live, -f           Print entries then tail for new ones (Ctrl-C to stop)\n";
      Printf.printf "  --json               Dump as JSON lines (implies --dump)\n\n";
      Printf.printf "Time range:\n";
      Printf.printf "  --since <spec>       Start of time range\n";
      Printf.printf "  --until <spec>       End of time range\n";
      Printf.printf "  Time specs: 1h, 30m, 2h30m, 10:30, 10:30:00, 2026-04-04T10:30:00Z\n\n";
      Printf.printf "Options:\n";
      Printf.printf "  --formats <path>     Path to formats.toml\n";
      Printf.printf "  --sources <path>     Path to sources.toml\n";
      Printf.printf "  --base-path <dir>    Base directory for relative source paths\n";
      Printf.printf "  -s, --search <term>  Search term (can repeat)\n";
      Printf.printf "  --limit <n>          Max entries for --dump (0 = unlimited)\n";
      Printf.printf "  -h, --help           Show this help\n\n";
      Printf.printf "TUI Keys:\n";
      Printf.printf "  /     Add search term    j/k   Scroll\n";
      Printf.printf "  Enter Toggle detail      Tab   Cycle focus\n";
      Printf.printf "  s     Toggle source      t     Toggle term\n";
      Printf.printf "  d     Delete term        q     Quit\n";
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
  (* CLI --base-path overrides config base_path *)
  let sources_config = match cli.base_path with
    | Some bp ->
      { sources_config with
        general = { sources_config.general with base_path = Some bp } }
    | None -> sources_config
  in
  let sources_config = Weft_config.resolve_base_path sources_config in
  Weft_config.validate_sources sources_config formats_config;
  (formats_config, sources_config)

let run env =
  let cli = parse_cli () in
  let (formats_config, sources_config) = load_config cli in
  let time_range = build_time_range ~since:cli.since ~until:cli.until in
  match cli.mode with
  | Dump ->
    Engine.run_dump ~env ~formats_config ~sources_config
      ~initial_terms:cli.initial_terms
      ~limit:cli.dump_limit ~json:cli.dump_json ~time_range
  | Live ->
    Engine.run_live ~env ~formats_config ~sources_config
      ~initial_terms:cli.initial_terms ~json:cli.dump_json
  | Tui ->
    Engine.run_with_tui ~env ~formats_config ~sources_config
      ~initial_terms:cli.initial_terms ~time_range
