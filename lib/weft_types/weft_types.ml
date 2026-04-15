type source_id = string

type time_range = {
  start_ : Ptime.t;
  end_ : Ptime.t option; (* None = open-ended / now *)
}

type log_entry = {
  timestamp : Ptime.t;
  raw : string;
  source : source_id;
  terms : string list;
  metadata : (string * string) list;
}

type query = {
  terms : string list;
  time_range : time_range option;
}

type segment = {
  id : string;
  origin : string;
  local_path : string;
  time_range : time_range;
  sealed : bool;
  content_hash : string option;
  size_bytes : int64;
  fetched_at : Ptime.t;
  ttl_hours : int;
  joined_index_built : bool;
}

type archive_info = {
  remote_path : string;
  mtime : Ptime.t option;
  size_bytes : int64;
  matches_segment : string option;
}

type source_status =
  | Connected
  | Reconnecting
  | Failed of string
  | Disconnected

type transport_kind =
  | Ssh of string   (* command e.g. "ssh app-server-1" *)
  | Tsh of string   (* command e.g. "tsh ssh bastion.prod" *)
  | Local

type source_type =
  | File
  | Directory
  | Remote
  | Loki
  | Journald

type rotation_style =
  | Rename
  | Copytruncate

type timestamp_strategy =
  | Prefix_auto
  | Prefix_format of string
  | Regex_capture of { regex : string; format : string }
  | Json_field of { field : string; format : string }

type multiline_config = {
  continuation : string; (* regex pattern *)
  max_lines : int;
}

type rotation_config = {
  style : rotation_style;
  drain_timeout_sec : int;
}

type middleware_step =
  | Strip_ansi
  | Regex_extract of { pattern : string; fields : string list option }
  | Grok of { pattern : string }
  | Json_field_extract of { fields : string list; source_field : string option }
  | Regex_filter of { include_ : string option; exclude : string option }
  | Field_rename of { mapping : (string * string) list }

type format_config = {
  name : string;
  timestamp : timestamp_strategy option;
  multiline : multiline_config option;
  rotation : rotation_config option;
  middleware : middleware_step list;
}

type auth_config =
  | Bearer of { token_env : string }
  | Basic of { user_env : string; pass_env : string }
  | None_

type source_config = {
  name : string;
  source_type : source_type;
  transport : string option;     (* SSH/tsh command *)
  path : string option;          (* file path *)
  glob : string option;          (* directory glob *)
  url : string option;           (* Loki URL *)
  auth : auth_config;
  default_labels : string option; (* Loki labels *)
  journal_unit : string option;  (* journald unit name *)
  journal_filter : string option; (* extra journalctl filter args *)
  format : string;               (* references format by name *)
}

type general_config = {
  default_time_range : string;
  reorder_window_ms : int;
  base_path : string option;
}

type limits_config = {
  max_ssh_connections : int;
  max_loki_concurrent : int;
  catch_up_timeout_sec : int;
}

type cache_config = {
  dir : string;
  max_mb_per_source : int;
  default_ttl_hours : int;
}

type sources_file_config = {
  general : general_config;
  limits : limits_config;
  cache : cache_config;
  sources : source_config list;
}

type formats_file_config = {
  formats : format_config list;
}

(* Term with display metadata *)
type search_term = {
  term : string;
  color_idx : int;
  enabled : bool;
}

(* Manifest for a cached source *)
type cache_manifest = {
  source : source_id;
  format : string;
  segments : segment list;
  known_archives : archive_info list;
}
