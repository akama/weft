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

let run env =
  let cli = parse_cli () in
  let (formats_config, sources_config) = load_config cli in
  if cli.dump_mode then
    Engine.run_dump ~env ~formats_config ~sources_config
      ~initial_terms:cli.initial_terms
      ~limit:cli.dump_limit ~json:cli.dump_json
  else
    Engine.run_with_tui ~env ~formats_config ~sources_config
      ~initial_terms:cli.initial_terms
