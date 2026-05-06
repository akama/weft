(* Journald source — reads from systemd journal via journalctl.
   Supports both local and remote (SSH) journals. Uses JSON output
   for structured field extraction. *)

open Weft_types

type t = {
  config : source_config;
  ssh : Weft_connection.Ssh_control.t option;
}

let connect config ~(proc : _ Eio.Process.mgr) ~fs:_ =
  match config.journal_unit with
  | None -> Error "Journald source requires a 'unit' field"
  | Some _ ->
    let ssh = match config.transport with
      | None -> Ok None
      | Some transport ->
        let ssh = Weft_connection.Ssh_control.create ~transport_cmd:transport in
        (try
           Weft_connection.Ssh_control.establish proc ssh;
           Ok (Some ssh)
         with exn -> Error (Printexc.to_string exn))
    in
    match ssh with
    | Ok ssh -> Ok { config; ssh }
    | Error e -> Error e

(* Build journalctl args for a query *)
let build_args config ~time_range ~output_format =
  let unit_name = Option.value ~default:"" config.journal_unit in
  (* Use -u for systemd units (.service, .socket, etc.), -t for syslog identifiers *)
  let unit_args =
    if String.contains unit_name '.' then
      ["-u"; unit_name]
    else
      ["-t"; unit_name]
  in
  let base = ["journalctl"; "--no-pager"] @ unit_args
    @ ["-o"; output_format] in
  let time_args = match time_range with
    | Some tr ->
      let fmt t =
        let ((y, mo, d), ((hh, mm, ss), _tz)) = Ptime.to_date_time t in
        Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" y mo d hh mm ss
      in
      let since = ["--since"; fmt tr.start_] in
      let until_ = match tr.end_ with
        | Some e -> ["--until"; fmt e]
        | None -> []
      in
      since @ until_
    | None -> []
  in
  let filter_args = match config.journal_filter with
    | Some f -> String.split_on_char ' ' f
    | None -> []
  in
  base @ time_args @ filter_args

(* Run journalctl locally *)
let run_local args =
  let cmd = String.concat " " (List.map (fun s ->
    if String.contains s ' ' then "'" ^ s ^ "'" else s
  ) args) in
  let ic = Unix.open_process_in cmd in
  let lines = ref [] in
  (try while true do
     lines := input_line ic :: !lines
   done with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  List.rev !lines

(* Run journalctl via SSH *)
let run_remote ssh args =
  try
    Weft_connection.Ssh_control.run_command_lines ssh args
  with Failure msg ->
    Printf.eprintf "Warning: remote journalctl failed: %s\n" msg;
    []

let run_cmd t args =
  match t.ssh with
  | None -> run_local args
  | Some ssh -> run_remote ssh args

(* Parse a journalctl JSON line into a log_entry *)
let parse_json_entry ~source line =
  try
    let json = Yojson.Basic.from_string line in
    let open Yojson.Basic.Util in
    let message = json |> member "MESSAGE" |> to_string_option
      |> Option.value ~default:"" in
    let timestamp =
      (* __REALTIME_TIMESTAMP is microseconds since epoch *)
      let us_str = json |> member "__REALTIME_TIMESTAMP" |> to_string_option in
      match us_str with
      | Some s ->
        (try
           let us = Int64.of_string s in
           let secs = Int64.to_float (Int64.div us 1_000_000L) in
           match Ptime.of_float_s secs with
           | Some t -> t
           | None -> Ptime_clock.now ()
         with Failure _ -> Ptime_clock.now ())
      | None -> Ptime_clock.now ()
    in
    (* Extract useful fields *)
    let fields = [
      ("unit", json |> member "_SYSTEMD_UNIT" |> to_string_option);
      ("priority", json |> member "PRIORITY" |> to_string_option);
      ("pid", json |> member "_PID" |> to_string_option);
      ("exe", json |> member "_EXE" |> to_string_option);
      ("identifier", json |> member "SYSLOG_IDENTIFIER" |> to_string_option);
    ] in
    let metadata = List.filter_map (fun (k, v) ->
      match v with Some s -> Some (k, s) | None -> None
    ) fields in
    Some { timestamp; raw = message; source; terms = []; metadata }
  with
  | Yojson.Json_error _ -> None
  | Yojson.Basic.Util.Type_error _ -> None

let search t ~terms ~time_range =
  let args = build_args t.config ~time_range ~output_format:"json" in
  let lines = run_cmd t args in
  let source = t.config.name in
  let entries = List.filter_map (parse_json_entry ~source) lines in
  let entries = if terms = [] then entries
    else
      let term_res = List.map (fun term ->
        (term, Re.compile (Re.Pcre.re (Re.Pcre.quote term)))
      ) terms in
      List.filter_map (fun (entry : log_entry) ->
        let dominated = List.exists (fun (_t, re) ->
          Re.execp re entry.raw) term_res in
        if dominated then
          let matched = List.filter_map (fun (t, re) ->
            if Re.execp re entry.raw then Some t else None
          ) term_res in
          Some { entry with terms = matched }
        else None
      ) entries
  in
  List.to_seq entries

(* Cache journald output for search — prepend ISO timestamp to each message
   so the auto-detect timestamp parser can extract it from cached lines. *)
let fetch_to_cache t ~time_range =
  let args = build_args t.config ~time_range ~output_format:"json" in
  let lines = run_cmd t args in
  let source = t.config.name in
  let entries = List.filter_map (parse_json_entry ~source) lines in
  let formatted = List.map (fun (e : log_entry) ->
    let ((y, mo, d), ((hh, mm, ss), _tz)) = Ptime.to_date_time e.timestamp in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ %s"
      y mo d hh mm ss e.raw
  ) entries in
  String.concat "\n" formatted

(* Tail via journalctl -f with JSON output *)
let tail_local ~args ~emit ~cancel ~source ~sleep =
  let full_args = args @ ["-f"; "-n"; "0"] in
  let cmd = String.concat " " (List.map (fun s ->
    if String.contains s ' ' then "'" ^ s ^ "'" else s
  ) full_args) in
  let ic = Unix.open_process_in cmd in
  Fun.protect (fun () ->
    try
      while not (Atomic.get cancel) do
        (* Non-blocking check if data available *)
        let fd = Unix.descr_of_in_channel ic in
        let ready, _, _ = Unix.select [fd] [] [] 0.5 in
        if ready <> [] then begin
          let line = input_line ic in
          match parse_json_entry ~source line with
          | Some entry -> emit entry
          | None -> ()
        end;
        ignore sleep  (* available but not needed in select loop *)
      done
    with End_of_file -> ()
  ) ~finally:(fun () ->
    (try ignore (Unix.close_process_in ic) with Unix.Unix_error _ -> ()))

let tail_remote ~ssh ~args ~emit ~cancel ~source =
  let full_args = args @ ["-f"; "-n"; "0"] in
  Weft_connection.Ssh_control.run_streaming ssh full_args
    ~on_line:(fun line ->
      match parse_json_entry ~source line with
      | Some entry -> emit entry
      | None -> ())
    ~on_stderr:(fun line ->
      Printf.eprintf "journalctl stderr [%s]: %s\n%!" source line)
    ~cancel ()

let tail t ~emit ~cancel ?(sleep = fun _ -> Unix.sleepf 1.0) () =
  let args = build_args t.config ~time_range:None ~output_format:"json" in
  let source = t.config.name in
  match t.ssh with
  | None -> tail_local ~args ~emit ~cancel ~source ~sleep
  | Some ssh -> tail_remote ~ssh ~args ~emit ~cancel ~source

let close t =
  match t.ssh with
  | Some ssh -> Weft_connection.Ssh_control.close ssh
  | None -> ()
