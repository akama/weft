open Weft_types

(* Common timestamp format patterns *)

let iso8601_re =
  Re.compile (Re.Pcre.re
    {|(\d{4})-(\d{2})-(\d{2})[T ](\d{2}):(\d{2}):(\d{2})(?:\.(\d+))?(?:Z|([+-]\d{2}):?(\d{2}))?|})

let syslog_bsd_re =
  Re.compile (Re.Pcre.re
    {|^(\w{3})\s+(\d{1,2})\s+(\d{2}):(\d{2}):(\d{2})|})

let epoch_seconds_re =
  Re.compile (Re.Pcre.re {|^(\d{10})(?:\.(\d+))?|})

let epoch_millis_re =
  Re.compile (Re.Pcre.re {|^(\d{13})|})

let common_log_re =
  Re.compile (Re.Pcre.re
    {|\[?(\d{2})/(\w{3})/(\d{4}):(\d{2}):(\d{2}):(\d{2})\s*([+-]\d{4})?\]?|})

let month_of_abbrev = function
  | "Jan" -> Some 1 | "Feb" -> Some 2 | "Mar" -> Some 3
  | "Apr" -> Some 4 | "May" -> Some 5 | "Jun" -> Some 6
  | "Jul" -> Some 7 | "Aug" -> Some 8 | "Sep" -> Some 9
  | "Oct" -> Some 10 | "Nov" -> Some 11 | "Dec" -> Some 12
  | _ -> None

let int_of_group g i =
  try int_of_string (Re.Group.get g i)
  with Not_found | Failure _ -> 0

let opt_group g i =
  try Some (Re.Group.get g i)
  with Not_found -> None

let ptime_of_ymd_hms ~y ~m ~d ~hh ~mm ~ss =
  match Ptime.of_date_time ((y, m, d), ((hh, mm, ss), 0)) with
  | Some t -> Some t
  | None -> None

let parse_iso8601 s =
  match Re.exec_opt iso8601_re s with
  | None -> None
  | Some g ->
    let y = int_of_group g 1 in
    let m = int_of_group g 2 in
    let d = int_of_group g 3 in
    let hh = int_of_group g 4 in
    let mm = int_of_group g 5 in
    let ss = int_of_group g 6 in
    let frac = match opt_group g 7 with
      | Some f ->
        let f = if String.length f > 9 then String.sub f 0 9 else f in
        let padded = f ^ String.make (9 - String.length f) '0' in
        int_of_string padded
      | None -> 0
    in
    let tz_offset_s =
      match opt_group g 8, opt_group g 9 with
      | Some th, Some tm ->
        let h = int_of_string th in
        let m = int_of_string tm in
        let sign = if h < 0 then -1 else 1 in
        sign * (abs h * 3600 + m * 60)
      | _ -> 0
    in
    (match Ptime.of_date_time ((y, m, d), ((hh, mm, ss), tz_offset_s)) with
     | Some t ->
       (* Add fractional seconds as picoseconds *)
       let ps = Int64.mul (Int64.of_int frac) 1000L in
       let span = Ptime.Span.of_d_ps (0, ps) in
       (match span with
        | Some sp -> Some (Ptime.add_span t sp |> Option.value ~default:t)
        | None -> Some t)
     | None -> None)

let parse_syslog_bsd s =
  match Re.exec_opt syslog_bsd_re s with
  | None -> None
  | Some g ->
    let mon_str = Re.Group.get g 1 in
    let day = int_of_group g 2 in
    let hh = int_of_group g 3 in
    let mm = int_of_group g 4 in
    let ss = int_of_group g 5 in
    match month_of_abbrev mon_str with
    | None -> None
    | Some m ->
      (* Syslog BSD has no year — use current year *)
      let now = Ptime_clock.now () in
      let (y, _, _), _ = Ptime.to_date_time now in
      ptime_of_ymd_hms ~y ~m ~d:day ~hh ~mm ~ss

let parse_epoch_s s =
  match Re.exec_opt epoch_seconds_re s with
  | None -> None
  | Some g ->
    let secs = Re.Group.get g 1 in
    let frac = opt_group g 2 in
    (match Ptime.of_float_s (float_of_string (secs ^ (match frac with Some f -> "." ^ f | None -> ""))) with
     | Some t -> Some t
     | None -> None)

let parse_epoch_ms s =
  match Re.exec_opt epoch_millis_re s with
  | None -> None
  | Some g ->
    let ms_str = Re.Group.get g 1 in
    let ms = Int64.of_string ms_str in
    let secs = Int64.div ms 1000L in
    let frac = Int64.to_float (Int64.rem ms 1000L) /. 1000.0 in
    Ptime.of_float_s (Int64.to_float secs +. frac)

let parse_common_log s =
  match Re.exec_opt common_log_re s with
  | None -> None
  | Some g ->
    let d = int_of_group g 1 in
    let mon_str = Re.Group.get g 2 in
    let y = int_of_group g 3 in
    let hh = int_of_group g 4 in
    let mm = int_of_group g 5 in
    let ss = int_of_group g 6 in
    match month_of_abbrev mon_str with
    | None -> None
    | Some m -> ptime_of_ymd_hms ~y ~m ~d ~hh ~mm ~ss

(* Named format shorthands *)
let rec parse_by_format_name name s =
  match name with
  | "iso8601" | "rfc3339" -> parse_iso8601 s
  | "syslog_bsd" -> parse_syslog_bsd s
  | "epoch_s" -> parse_epoch_s s
  | "epoch_ms" -> parse_epoch_ms s
  | "common_log" -> parse_common_log s
  | _ ->
    (* Try as strptime-style format — basic support *)
    parse_with_strptime name s

and parse_with_strptime fmt s =
  (* Basic strptime-like parser for common format strings *)
  let buf = Buffer.create 64 in
  let i = ref 0 in
  let len = String.length fmt in
  while !i < len do
    if fmt.[!i] = '%' && !i + 1 < len then begin
      let c = fmt.[!i + 1] in
      (match c with
       | 'Y' -> Buffer.add_string buf {|(\d{4})|}
       | 'm' -> Buffer.add_string buf {|(\d{2})|}
       | 'd' -> Buffer.add_string buf {|(\d{2})|}
       | 'H' -> Buffer.add_string buf {|(\d{2})|}
       | 'M' -> Buffer.add_string buf {|(\d{2})|}
       | 'S' -> Buffer.add_string buf {|(\d{2})|}
       | 'b' -> Buffer.add_string buf {|(\w{3})|}
       | _ -> Buffer.add_char buf fmt.[!i]; Buffer.add_char buf c);
      i := !i + 2
    end else begin
      (* Escape regex special chars *)
      let c = fmt.[!i] in
      (match c with
       | '.' | '*' | '+' | '?' | '(' | ')' | '[' | ']' | '{' | '}' | '\\' | '^' | '$' | '|' ->
         Buffer.add_char buf '\\'; Buffer.add_char buf c
       | _ -> Buffer.add_char buf c);
      i := !i + 1
    end
  done;
  let re = Re.compile (Re.Pcre.re (Buffer.contents buf)) in
  match Re.exec_opt re s with
  | None -> None
  | Some g ->
    (* Extract groups based on format specifiers *)
    let specs = ref [] in
    let j = ref 0 in
    let flen = String.length fmt in
    while !j < flen do
      if fmt.[!j] = '%' && !j + 1 < flen then begin
        specs := fmt.[!j + 1] :: !specs;
        j := !j + 2
      end else
        j := !j + 1
    done;
    let specs = List.rev !specs in
    let y = ref 2026 and m = ref 1 and d = ref 1 in
    let hh = ref 0 and mm = ref 0 and ss = ref 0 in
    List.iteri (fun i spec ->
      let grp = i + 1 in
      match spec with
      | 'Y' -> y := int_of_group g grp
      | 'm' -> m := int_of_group g grp
      | 'd' -> d := int_of_group g grp
      | 'H' -> hh := int_of_group g grp
      | 'M' -> mm := int_of_group g grp
      | 'S' -> ss := int_of_group g grp
      | 'b' ->
        (match month_of_abbrev (try Re.Group.get g grp with Not_found -> "") with
         | Some mo -> m := mo | None -> ())
      | _ -> ()
    ) specs;
    ptime_of_ymd_hms ~y:!y ~m:!m ~d:!d ~hh:!hh ~mm:!mm ~ss:!ss

(* Auto-detect: try all known formats in ranked order *)
let auto_detect_parsers = [
  parse_iso8601;
  parse_syslog_bsd;
  parse_common_log;
  parse_epoch_ms;
  parse_epoch_s;
]

let auto_detect s =
  List.find_map (fun parser -> parser s) auto_detect_parsers

(* Build a parser from a timestamp_strategy *)
let parser_of_strategy (strategy : timestamp_strategy) : string -> Ptime.t option =
  match strategy with
  | Prefix_auto -> auto_detect
  | Prefix_format fmt -> parse_by_format_name fmt
  | Regex_capture { regex; format } ->
    let re = Re.compile (Re.Pcre.re regex) in
    (fun s ->
       match Re.exec_opt re s with
       | None -> None
       | Some g ->
         let captured = try Re.Group.get g 1 with Not_found -> s in
         parse_by_format_name format captured)
  | Json_field { field; format } ->
    (fun s ->
       try
         let json = Yojson.Basic.from_string s in
         match json with
         | `Assoc fields ->
           (match List.assoc_opt field fields with
            | Some (`String v) -> parse_by_format_name format v
            | Some (`Float f) ->
              (match format with
               | "epoch_ms" -> Ptime.of_float_s (f /. 1000.0)
               | "epoch_s" -> Ptime.of_float_s f
               | _ -> parse_by_format_name format (string_of_float f))
            | Some (`Int i) ->
              (match format with
               | "epoch_ms" -> Ptime.of_float_s (float_of_int i /. 1000.0)
               | "epoch_s" -> Ptime.of_float_s (float_of_int i)
               | _ -> parse_by_format_name format (string_of_int i))
            | _ -> None)
         | _ -> None
       with Yojson.Json_error _ | Not_found -> None)

(* Cached auto-detection: try first N lines, remember which parser worked *)
type cached_parser = {
  mutable parser_ : (string -> Ptime.t option) option;
}

let create_cached_auto_parser () =
  { parser_ = None }

let parse_with_cache cache s =
  match cache.parser_ with
  | Some p -> p s
  | None ->
    (* Try each parser; remember the first that works *)
    let rec try_parsers = function
      | [] -> auto_detect s
      | p :: rest ->
        match p s with
        | Some t ->
          cache.parser_ <- Some p;
          Some t
        | None -> try_parsers rest
    in
    try_parsers auto_detect_parsers
