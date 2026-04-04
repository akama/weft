open Weft_types

let format_entry (entry : log_entry) =
  let (_, ((hh, mm, ss), _)) = Ptime.to_date_time entry.timestamp in
  let _, ps = Ptime.to_span entry.timestamp |> Ptime.Span.to_d_ps in
  let ms = Int64.to_int (Int64.rem (Int64.div ps 1_000_000_000L) 1000L) in
  let ts = Printf.sprintf "%02d:%02d:%02d.%03d" hh mm ss ms in
  let src = if String.length entry.source > 12 then
    String.sub entry.source 0 12
  else entry.source in
  let terms_str = match entry.terms with
    | [] -> ""
    | terms -> " [" ^ String.concat "," terms ^ "]"
  in
  let meta_str = match entry.metadata with
    | [] -> ""
    | pairs ->
      let shown = List.filteri (fun i _ -> i < 4) pairs in
      " {" ^ String.concat ", " (List.map (fun (k, v) ->
        let v_short = if String.length v > 30 then String.sub v 0 30 ^ "..." else v in
        k ^ "=" ^ v_short
      ) shown) ^ "}"
  in
  Printf.sprintf "%s [%-12s] %s%s%s" ts src entry.raw terms_str meta_str

let entry_to_json (entry : log_entry) =
  let fields = [
    ("timestamp", `String (Ptime.to_rfc3339 entry.timestamp));
    ("source", `String entry.source);
    ("raw", `String entry.raw);
    ("terms", `List (List.map (fun t -> `String t) entry.terms));
    ("metadata", `Assoc (List.map (fun (k, v) -> (k, `String v)) entry.metadata));
  ] in
  Yojson.Basic.to_string (`Assoc fields)

let format_cache_stats cache =
  let (total_size, total_segments, (earliest, latest)) =
    Weft_cache.cache_stats cache in
  let size_str = if total_size > Int64.of_int (1024 * 1024) then
    Printf.sprintf "%.1f MB" (Int64.to_float total_size /. 1048576.0)
  else if total_size > 1024L then
    Printf.sprintf "%.1f KB" (Int64.to_float total_size /. 1024.0)
  else
    Printf.sprintf "%Ld B" total_size
  in
  let range_str = match earliest, latest with
    | Some s, Some e ->
      let (sd, _) = Ptime.to_date_time s in
      let (ed, _) = Ptime.to_date_time e in
      let fmt (y, m, d) = Printf.sprintf "%04d-%02d-%02d" y m d in
      if sd = ed then fmt sd
      else Printf.sprintf "%s to %s" (fmt sd) (fmt ed)
    | Some s, None ->
      let (sd, _) = Ptime.to_date_time s in
      Printf.sprintf "%04d-%02d-%02d+" (let (y,_,_) = sd in y)
        (let (_,m,_) = sd in m) (let (_,_,d) = sd in d)
    | _ -> "none"
  in
  Printf.sprintf "%s, %d segments, %s" size_str total_segments range_str
