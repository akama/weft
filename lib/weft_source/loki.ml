open Weft_types

type t = {
  config : source_config;
  base_url : string;
  auth_header : (string * string) option;
  default_labels : string;
}

let connect config ~(proc : _ Eio.Process.mgr) ~fs:_ =
  ignore proc;
  match config.url with
  | None -> Error "Loki source requires a 'url'"
  | Some url ->
    let auth_header = match config.auth with
      | Bearer { token_env } ->
        (match Sys.getenv_opt token_env with
         | Some token -> Some ("Authorization", "Bearer " ^ token)
         | None -> None)
      | Basic { user_env; pass_env } ->
        (match Sys.getenv_opt user_env, Sys.getenv_opt pass_env with
         | Some user, Some pass ->
           let encoded = Base64.encode_string (user ^ ":" ^ pass) in
           Some ("Authorization", "Basic " ^ encoded)
         | _ -> None)
      | None_ -> None
    in
    let default_labels = Option.value ~default:"{}" config.default_labels in
    Ok { config; base_url = url; auth_header; default_labels }

let health_check _t =
  (* Would do GET /ready in real implementation *)
  Ok ()

let build_logql ~labels ~terms =
  if terms = [] then labels
  else
    let filter = String.concat "|" (List.map Re.Pcre.quote terms) in
    Printf.sprintf "%s |~ \"%s\"" labels filter

let format_time t =
  (* Loki wants nanosecond epoch as string *)
  let d, ps = Ptime.to_span t |> Ptime.Span.to_d_ps in
  let secs = Int64.of_int (d * 86400) in
  let nanos = Int64.add (Int64.mul secs 1_000_000_000L)
    (Int64.mul (Int64.div ps 1_000_000L) 1_000L) in
  Int64.to_string nanos

let parse_loki_timestamp ns_str =
  try
    let ns = Int64.of_string ns_str in
    let secs = Int64.to_float (Int64.div ns 1_000_000_000L) in
    Ptime.of_float_s secs
  with _ -> None

let search t ~terms ~time_range =
  let logql = build_logql ~labels:t.default_labels ~terms in
  let start_time = match time_range with
    | Some tr -> format_time tr.start_
    | None ->
      (* Default: 1 hour ago *)
      let now = Ptime_clock.now () in
      (match Ptime.sub_span now (Ptime.Span.of_int_s 3600) with
       | Some t -> format_time t
       | None -> format_time Ptime.epoch)
  in
  let end_time = match time_range with
    | Some { end_ = Some e; _ } -> format_time e
    | _ -> format_time (Ptime_clock.now ())
  in
  let _query_url = Printf.sprintf
    "%s/loki/api/v1/query_range?query=%s&start=%s&end=%s&limit=5000"
    t.base_url (Uri.pct_encode logql) start_time end_time in
  let source = t.config.name in
  (* In a real implementation, we'd use cohttp-eio to make the HTTP request.
     For now, return empty — the HTTP plumbing is straightforward once
     the rest of the architecture is working. *)
  ignore source;
  Seq.empty

let parse_query_response ~source json =
  try
    let open Yojson.Basic.Util in
    let data = json |> member "data" in
    let result = data |> member "result" |> to_list in
    let entries = List.concat_map (fun stream ->
      let labels = stream |> member "stream" in
      let label_str = Yojson.Basic.to_string labels in
      let values = stream |> member "values" |> to_list in
      List.filter_map (fun value ->
        match value |> to_list with
        | [`String ts; `String line] ->
          let timestamp = match parse_loki_timestamp ts with
            | Some t -> t
            | None -> Ptime_clock.now ()
          in
          Some {
            timestamp;
            raw = line;
            source;
            terms = [];
            metadata = [("labels", label_str)];
          }
        | _ -> None
      ) values
    ) result in
    entries
  with _ -> []

(* Tail via WebSocket — stub for now *)
let tail _t ~terms:_ ~emit:_ ~cancel:_ =
  (* Real implementation would use WebSocket connection to
     GET /loki/api/v1/tail with LogQL query *)
  ()

let fetch _t ~dst:_ = Error "Loki sources don't support direct fetch"
let fetch_archive _t ~path:_ ~dst:_ = Error "Loki sources don't support archive fetch"
let discover_archives _t = []

let close _t = ()
