open Weft_types

type t = {
  config : source_config;
  base_url : string;
  auth_header : (string * string) option;
  default_labels : string;
  mutable make_client : (Eio.Switch.t -> Cohttp_eio.Client.t) option;
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
         | None ->
           Printf.eprintf "Warning: Loki bearer token env var '%s' not set\n" token_env;
           None)
      | Basic { user_env; pass_env } ->
        (match Sys.getenv_opt user_env, Sys.getenv_opt pass_env with
         | Some user, Some pass ->
           let credentials = user ^ ":" ^ pass in
           (* Simple base64 encode without external dep *)
           let encoded = Base64.encode_string credentials in
           Some ("Authorization", "Basic " ^ encoded)
         | _ ->
           Printf.eprintf "Warning: Loki basic auth env vars not set\n";
           None)
      | None_ -> None
    in
    let default_labels = Option.value ~default:"{}" config.default_labels in
    (* We need the Eio.Net handle to make HTTP calls — it gets set later *)
    Ok { config; base_url = url; auth_header; default_labels;
         make_client = None }

let set_net t (net : _ Eio.Net.t) =
  t.make_client <- Some (fun _sw ->
    Cohttp_eio.Client.make ~https:None net)

let health_check _t =
  (* Would do GET /ready *)
  Ok ()

let build_logql ~labels ~terms =
  if terms = [] then labels
  else
    let filter = String.concat "|" (List.map Re.Pcre.quote terms) in
    Printf.sprintf "%s |~ \"%s\"" labels filter

let format_time t =
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
  with Failure _ -> None

let read_body body =
  let buf = Buffer.create Weft_constants.default_read_buf_size in
  let br = Eio.Buf_read.of_flow ~max_size:Weft_constants.loki_max_body_bytes body in
  (try
     while true do
       let chunk = Eio.Buf_read.line br in
       Buffer.add_string buf chunk;
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  Buffer.contents buf

let make_headers t =
  let h = Http.Header.init () in
  let h = Http.Header.add h "Accept" "application/json" in
  match t.auth_header with
  | Some (k, v) -> Http.Header.add h k v
  | None -> h

let parse_query_response ~source body_str =
  try
    let json = Yojson.Basic.from_string body_str in
    let open Yojson.Basic.Util in
    let data = json |> member "data" in
    let result = data |> member "result" |> to_list in
    List.concat_map (fun stream ->
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
    ) result
  with
  | Yojson.Basic.Util.Type_error (msg, _) ->
    Printf.eprintf "Warning: Loki response parse error: %s\n" msg; []
  | Yojson.Json_error msg ->
    Printf.eprintf "Warning: Loki JSON error: %s\n" msg; []

let search t ~terms ~time_range =
  let logql = build_logql ~labels:t.default_labels ~terms in
  let start_time = match time_range with
    | Some tr -> format_time tr.start_
    | None ->
      let now = Ptime_clock.now () in
      (match Ptime.sub_span now (Ptime.Span.of_int_s 3600) with
       | Some t -> format_time t
       | None -> format_time Ptime.epoch)
  in
  let end_time = match time_range with
    | Some { end_ = Some e; _ } -> format_time e
    | _ -> format_time (Ptime_clock.now ())
  in
  let query_url = Printf.sprintf
    "%s/loki/api/v1/query_range?query=%s&start=%s&end=%s&limit=%d&direction=forward"
    t.base_url (Uri.pct_encode logql) start_time end_time
    Weft_constants.loki_search_limit in
  let source = t.config.name in
  let headers = make_headers t in
  match t.make_client with
  | None ->
    Printf.eprintf "Loki client not initialized (call set_net first)\n";
    Seq.empty
  | Some mk_client ->
  Eio.Switch.run @@ fun sw ->
  let client = mk_client sw in
  let uri = Uri.of_string query_url in
  try
    let (resp, body) = Cohttp_eio.Client.get client ~sw ~headers uri in
    let status = Http.Response.status resp in
    if Http.Status.to_int status >= 400 then begin
      let body_str = read_body body in
      Printf.eprintf "Loki query error (HTTP %d): %s\n"
        (Http.Status.to_int status) body_str;
      Seq.empty
    end else begin
      let body_str = read_body body in
      let entries = parse_query_response ~source body_str in
      List.to_seq entries
    end
  with
  | Eio.Io _ as e ->
    Printf.eprintf "Loki connection error for %s: %s\n"
      t.config.name (Printexc.to_string e);
    Seq.empty

(* Tail via long-polling query_range (WebSocket would be better but
   requires a WebSocket library). Polls periodically for new entries.
   ~sleep: pass Eio.Time.sleep when running in Eio context to avoid
   blocking the scheduler. Defaults to Unix.sleepf for CLI use. *)
let default_sleep secs = Unix.sleepf secs

let tail t ~terms ~emit ~cancel ?(sleep = default_sleep) () =
  let logql = build_logql ~labels:t.default_labels ~terms in
  let source = t.config.name in
  let headers = make_headers t in
  let last_ts = ref (Ptime_clock.now ()) in
  match t.make_client with
  | None ->
    Printf.eprintf "Loki client not initialized for tail\n"
  | Some mk_client ->
  while not (Atomic.get cancel) do
    let start_time = format_time !last_ts in
    let end_time = format_time (Ptime_clock.now ()) in
    let query_url = Printf.sprintf
      "%s/loki/api/v1/query_range?query=%s&start=%s&end=%s&limit=%d&direction=forward"
      t.base_url (Uri.pct_encode logql) start_time end_time
      Weft_constants.loki_tail_limit in
    (try
       Eio.Switch.run @@ fun sw ->
       let client = mk_client sw in
       let uri = Uri.of_string query_url in
       let (resp, body) = Cohttp_eio.Client.get client ~sw ~headers uri in
       let status = Http.Response.status resp in
       if Http.Status.to_int status < 400 then begin
         let body_str = read_body body in
         let entries = parse_query_response ~source body_str in
         List.iter (fun (entry : log_entry) ->
           if Ptime.is_later entry.timestamp ~than:!last_ts then begin
             last_ts := entry.timestamp;
             emit entry
           end
         ) entries
       end
     with Eio.Io _ as e ->
       Printf.eprintf "Loki tail poll error: %s\n" (Printexc.to_string e));
    if not (Atomic.get cancel) then
      sleep Weft_constants.loki_tail_poll_sec
  done

let fetch _t ~dst:_ = Error "Loki sources don't support direct fetch"
let fetch_archive _t ~path:_ ~dst:_ = Error "Loki sources don't support archive fetch"
let discover_archives _t = []

let close _t = ()
