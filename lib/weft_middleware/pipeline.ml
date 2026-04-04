open Weft_types

(* A compiled middleware step *)
type compiled_step =
  | C_strip_ansi
  | C_regex_extract of Regex_extract.t
  | C_grok of Grok.t
  | C_json_field_extract of Json_field_extract.t
  | C_regex_filter of Regex_filter.t
  | C_field_rename of Field_rename.t

type t = {
  multiline : Multiline.t option;
  timestamp_parser : (string -> Ptime.t option);
  steps : compiled_step list;
}

let compile_step (step : middleware_step) : compiled_step =
  match step with
  | Strip_ansi -> C_strip_ansi
  | Regex_extract { pattern; fields } ->
    C_regex_extract (Regex_extract.create ~pattern ~fields)
  | Grok { pattern } ->
    C_grok (Grok.create pattern)
  | Json_field_extract { fields; source_field } ->
    C_json_field_extract (Json_field_extract.create ~fields ~source_field)
  | Regex_filter { include_; exclude } ->
    C_regex_filter (Regex_filter.create ~include_ ~exclude)
  | Field_rename { mapping } ->
    C_field_rename (Field_rename.create ~mapping)

let create (format : format_config) : t =
  let multiline = match format.multiline with
    | None -> None
    | Some mc -> Some (Multiline.create ~continuation:mc.continuation ~max_lines:mc.max_lines)
  in
  let timestamp_parser = match format.timestamp with
    | None -> Weft_time.auto_detect
    | Some strategy -> Weft_time.parser_of_strategy strategy
  in
  let steps = List.map compile_step format.middleware in
  { multiline; timestamp_parser; steps }

(* Apply a single step to a line + metadata. Returns None if filtered out. *)
let apply_step step line metadata =
  match step with
  | C_strip_ansi ->
    Some (Strip_ansi.apply line, metadata)
  | C_regex_extract t ->
    Some (line, Regex_extract.apply t line metadata)
  | C_grok t ->
    Some (line, Grok.apply t line metadata)
  | C_json_field_extract t ->
    Some (line, Json_field_extract.apply t line metadata)
  | C_regex_filter t ->
    if Regex_filter.should_keep t line then Some (line, metadata)
    else None
  | C_field_rename t ->
    Some (line, Field_rename.apply t metadata)

(* Process a single text block through the pipeline, producing a log_entry *)
let process_block (t : t) ~source (block : string) : log_entry option =
  let timestamp = match t.timestamp_parser block with
    | Some ts -> ts
    | None -> Ptime_clock.now ()
  in
  let rec apply_steps line metadata = function
    | [] -> Some (line, metadata)
    | step :: rest ->
      match apply_step step line metadata with
      | None -> None (* filtered out *)
      | Some (line', metadata') -> apply_steps line' metadata' rest
  in
  match apply_steps block [] t.steps with
  | None -> None
  | Some (raw, metadata) ->
    Some { timestamp; raw; source; terms = []; metadata }

(* Process raw lines: multiline join -> timestamp -> middleware -> log_entries *)
let process_lines (t : t) ~source (lines : string list) : log_entry list =
  let blocks = match t.multiline with
    | None -> lines
    | Some ml -> Multiline.join_lines ml lines
  in
  List.filter_map (process_block t ~source) blocks

(* Streaming: process one line at a time with multiline state *)
type stream_state = {
  pipeline : t;
  ml_state : Multiline.state option;
  source : source_id;
}

let create_stream_state pipeline ~source =
  let ml_state = match pipeline.multiline with
    | None -> None
    | Some ml -> Some (Multiline.create_state ml)
  in
  { pipeline; ml_state; source }

let feed_line state line =
  match state.ml_state with
  | None ->
    (* No multiline — each line is a block *)
    process_block state.pipeline ~source:state.source line
    |> Option.map (fun e -> [e])
    |> Option.value ~default:[]
  | Some ml_state ->
    match Multiline.feed_line ml_state line with
    | None -> [] (* still accumulating *)
    | Some block ->
      process_block state.pipeline ~source:state.source block
      |> Option.map (fun e -> [e])
      |> Option.value ~default:[]

let flush_stream state =
  match state.ml_state with
  | None -> []
  | Some ml_state ->
    match Multiline.flush ml_state with
    | None -> []
    | Some block ->
      process_block state.pipeline ~source:state.source block
      |> Option.map (fun e -> [e])
      |> Option.value ~default:[]
