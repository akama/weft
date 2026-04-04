open Weft_types

let get_string_opt tbl key =
  match Otoml.find_opt tbl (Otoml.get_string) [key] with
  | Some s -> Some s
  | None -> None
  | exception (Otoml.Type_error _) -> None

let get_string tbl key ~default =
  Option.value ~default (get_string_opt tbl key)

let get_int tbl key ~default =
  match Otoml.find_opt tbl (Otoml.get_integer) [key] with
  | Some n -> n
  | None -> default
  | exception (Otoml.Type_error _) -> default

let get_string_list tbl key =
  match Otoml.find_opt tbl (Otoml.get_array (Otoml.get_string)) [key] with
  | Some l -> l
  | None -> []
  | exception (Otoml.Type_error _) -> []

let parse_rotation_style = function
  | "rename" -> Rename
  | "copytruncate" -> Copytruncate
  | s -> failwith (Printf.sprintf "Unknown rotation style: %s" s)

let parse_timestamp_strategy tbl =
  let position = get_string_opt tbl "position" in
  let format_ = get_string_opt tbl "format" in
  let regex = get_string_opt tbl "regex" in
  let json_field = get_string_opt tbl "json_field" in
  match position, regex, json_field, format_ with
  | Some "prefix", _, _, Some "auto" -> Some Prefix_auto
  | Some "prefix", _, _, Some fmt -> Some (Prefix_format fmt)
  | Some "prefix", _, _, None -> Some Prefix_auto
  | _, Some re, _, Some fmt -> Some (Regex_capture { regex = re; format = fmt })
  | _, _, Some jf, Some fmt -> Some (Json_field { field = jf; format = fmt })
  | _, _, _, Some fmt ->
    (* shorthand: just a format name like "iso8601", "syslog_bsd" *)
    Some (Prefix_format fmt)
  | _ -> None

let parse_multiline_config tbl =
  match get_string_opt tbl "continuation" with
  | None -> None
  | Some cont ->
    Some {
      continuation = cont;
      max_lines = get_int tbl "max_lines" ~default:50;
    }

let parse_rotation_config tbl =
  match get_string_opt tbl "style" with
  | None -> None
  | Some s ->
    Some {
      style = parse_rotation_style s;
      drain_timeout_sec = get_int tbl "drain_timeout_sec" ~default:5;
    }

let parse_middleware_step tbl =
  let typ = get_string tbl "type" ~default:"" in
  match typ with
  | "strip_ansi" -> Strip_ansi
  | "regex_extract" ->
    let pattern = get_string tbl "pattern" ~default:"" in
    let fields = match get_string_list tbl "fields" with
      | [] -> None
      | l -> Some l
    in
    Regex_extract { pattern; fields }
  | "grok" ->
    Grok { pattern = get_string tbl "pattern" ~default:"" }
  | "json_field_extract" ->
    Json_field_extract {
      fields = get_string_list tbl "fields";
      source_field = get_string_opt tbl "source_field";
    }
  | "regex_filter" ->
    Regex_filter {
      include_ = get_string_opt tbl "include";
      exclude = get_string_opt tbl "exclude";
    }
  | "field_rename" ->
    let mapping =
      match Otoml.find_opt tbl (Otoml.get_table) ["mapping"] with
      | Some pairs ->
        List.filter_map (fun (k, v) ->
          match v with
          | Otoml.TomlString s -> Some (k, s)
          | _ -> None
        ) pairs
      | None -> []
    in
    Field_rename { mapping }
  | s -> failwith (Printf.sprintf "Unknown middleware type: %s" s)

let parse_format_config name tbl =
  let timestamp =
    match Otoml.find_opt tbl (Otoml.get_table) ["timestamp"] with
    | Some t -> parse_timestamp_strategy (Otoml.table t)
    | None -> None
  in
  let multiline =
    match Otoml.find_opt tbl (Otoml.get_table) ["multiline"] with
    | Some t -> parse_multiline_config (Otoml.table t)
    | None -> None
  in
  let rotation =
    match Otoml.find_opt tbl (Otoml.get_table) ["rotation"] with
    | Some t -> parse_rotation_config (Otoml.table t)
    | None -> None
  in
  let middleware =
    match Otoml.find_opt tbl (Otoml.get_array (Otoml.get_table)) ["middleware"] with
    | Some tables ->
      List.map (fun t -> parse_middleware_step (Otoml.table t)) tables
    | None -> []
  in
  { name; timestamp; multiline; rotation; middleware }

let parse_formats_file path =
  let toml = Otoml.Parser.from_file path in
  let format_tbl =
    match Otoml.find_opt toml (Otoml.get_table) ["format"] with
    | Some pairs -> pairs
    | None -> []
  in
  let formats = List.map (fun (name, subtbl) ->
    match subtbl with
    | Otoml.TomlTable pairs -> parse_format_config name (Otoml.table pairs)
    | _ -> parse_format_config name (Otoml.table [])
  ) format_tbl in
  { formats }

let parse_auth tbl =
  match get_string_opt tbl "auth" with
  | Some "bearer" ->
    let token_env = get_string tbl "token_env" ~default:"" in
    Bearer { token_env }
  | Some "basic" ->
    let user_env = get_string tbl "user_env" ~default:"" in
    let pass_env = get_string tbl "pass_env" ~default:"" in
    Basic { user_env; pass_env }
  | _ -> None_

let parse_source_type = function
  | "file" -> File
  | "directory" -> Directory
  | "remote" -> Remote
  | "loki" -> Loki
  | s -> failwith (Printf.sprintf "Unknown source type: %s" s)

let parse_source_config tbl =
  let name = get_string tbl "name" ~default:"" in
  let source_type = parse_source_type (get_string tbl "type" ~default:"file") in
  {
    name;
    source_type;
    transport = get_string_opt tbl "transport";
    path = get_string_opt tbl "path";
    glob = get_string_opt tbl "glob";
    url = get_string_opt tbl "url";
    auth = parse_auth tbl;
    default_labels = get_string_opt tbl "default_labels";
    format = get_string tbl "format" ~default:"raw";
  }

let parse_general_config tbl =
  {
    default_time_range = get_string tbl "default_time_range" ~default:"1h";
    reorder_window_ms = get_int tbl "reorder_window_ms" ~default:500;
  }

let parse_limits_config tbl =
  {
    max_ssh_connections = get_int tbl "max_ssh_connections" ~default:4;
    max_loki_concurrent = get_int tbl "max_loki_concurrent" ~default:2;
    catch_up_timeout_sec = get_int tbl "catch_up_timeout_sec" ~default:30;
  }

let parse_cache_config tbl =
  {
    dir = get_string tbl "dir" ~default:"~/.cache/weft";
    max_mb_per_source = get_int tbl "max_mb_per_source" ~default:200;
    default_ttl_hours = get_int tbl "default_ttl_hours" ~default:72;
  }

let parse_sources_file path =
  let toml = Otoml.Parser.from_file path in
  let general =
    match Otoml.find_opt toml (Otoml.get_table) ["general"] with
    | Some t -> parse_general_config (Otoml.table t)
    | None -> parse_general_config (Otoml.table [])
  in
  let limits =
    match Otoml.find_opt toml (Otoml.get_table) ["limits"] with
    | Some t -> parse_limits_config (Otoml.table t)
    | None -> parse_limits_config (Otoml.table [])
  in
  let cache =
    match Otoml.find_opt toml (Otoml.get_table) ["cache"] with
    | Some t -> parse_cache_config (Otoml.table t)
    | None -> parse_cache_config (Otoml.table [])
  in
  let sources =
    match Otoml.find_opt toml (Otoml.get_array (Otoml.get_table)) ["source"] with
    | Some tables -> List.map (fun t -> parse_source_config (Otoml.table t)) tables
    | None -> []
  in
  { general; limits; cache; sources }

let resolve_format formats_config name =
  List.find_opt (fun (f : format_config) -> f.name = name) formats_config.formats

let validate_sources sources_config formats_config =
  List.iter (fun (src : source_config) ->
    match resolve_format formats_config src.format with
    | None ->
      if src.format <> "raw" then
        Printf.eprintf "Warning: source '%s' references unknown format '%s'\n"
          src.name src.format
    | Some _ -> ()
  ) sources_config.sources
