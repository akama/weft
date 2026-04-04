open Weft_types

let ptime_to_json t =
  `String (Ptime.to_rfc3339 t)

let ptime_of_json = function
  | `String s ->
    (match Ptime.of_rfc3339 s with
     | Ok (t, _, _) -> Some t
     | Error _ -> None)
  | _ -> None

let segment_to_json (s : segment) : Yojson.Basic.t =
  `Assoc [
    "id", `String s.id;
    "origin", `String s.origin;
    "local_path", `String s.local_path;
    "time_range", `Assoc [
      "start", ptime_to_json s.time_range.start_;
      "end", (match s.time_range.end_ with
              | Some t -> ptime_to_json t
              | None -> `Null);
    ];
    "sealed", `Bool s.sealed;
    "content_hash", (match s.content_hash with
                     | Some h -> `String h
                     | None -> `Null);
    "size_bytes", `String (Int64.to_string s.size_bytes);
    "fetched_at", ptime_to_json s.fetched_at;
    "ttl_hours", `Int s.ttl_hours;
    "joined_index_built", `Bool s.joined_index_built;
  ]

let segment_of_json (json : Yojson.Basic.t) : segment option =
  try
    let open Yojson.Basic.Util in
    let id = json |> member "id" |> to_string in
    let origin = json |> member "origin" |> to_string in
    let local_path = json |> member "local_path" |> to_string in
    let tr = json |> member "time_range" in
    let start_ = tr |> member "start" |> ptime_of_json |> Option.get in
    let end_ = ptime_of_json (tr |> member "end") in
    let sealed = json |> member "sealed" |> to_bool in
    let content_hash =
      match json |> member "content_hash" with
      | `Null -> None
      | `String s -> Some s
      | _ -> None
    in
    let size_bytes =
      Int64.of_string (json |> member "size_bytes" |> to_string) in
    let fetched_at =
      ptime_of_json (json |> member "fetched_at") |> Option.get in
    let ttl_hours = json |> member "ttl_hours" |> to_int in
    let joined_index_built =
      json |> member "joined_index_built" |> to_bool in
    Some {
      id; origin; local_path;
      time_range = { start_; end_ };
      sealed; content_hash; size_bytes; fetched_at;
      ttl_hours; joined_index_built;
    }
  with _ -> None

let archive_to_json (a : archive_info) : Yojson.Basic.t =
  `Assoc [
    "remote_path", `String a.remote_path;
    "mtime", (match a.mtime with Some t -> ptime_to_json t | None -> `Null);
    "size_bytes", `String (Int64.to_string a.size_bytes);
    "matches_segment", (match a.matches_segment with
                        | Some s -> `String s
                        | None -> `Null);
  ]

let archive_of_json (json : Yojson.Basic.t) : archive_info option =
  try
    let open Yojson.Basic.Util in
    let remote_path = json |> member "remote_path" |> to_string in
    let mtime = ptime_of_json (json |> member "mtime") in
    let size_bytes =
      Int64.of_string (json |> member "size_bytes" |> to_string) in
    let matches_segment =
      match json |> member "matches_segment" with
      | `Null -> None
      | `String s -> Some s
      | _ -> None
    in
    Some { remote_path; mtime; size_bytes; matches_segment }
  with _ -> None

let manifest_to_json (m : cache_manifest) : Yojson.Basic.t =
  `Assoc [
    "source", `String m.source;
    "format", `String m.format;
    "segments", `List (List.map segment_to_json m.segments);
    "known_archives", `List (List.map archive_to_json m.known_archives);
  ]

let manifest_of_json (json : Yojson.Basic.t) : cache_manifest option =
  try
    let open Yojson.Basic.Util in
    let source = json |> member "source" |> to_string in
    let format = json |> member "format" |> to_string in
    let segments =
      json |> member "segments" |> to_list
      |> List.filter_map segment_of_json
    in
    let known_archives =
      json |> member "known_archives" |> to_list
      |> List.filter_map archive_of_json
    in
    Some { source; format; segments; known_archives }
  with _ -> None

let save_manifest ~fs ~dir (manifest : cache_manifest) =
  let path = Eio.Path.(fs / dir / "manifest.json") in
  let json = manifest_to_json manifest in
  let data = Yojson.Basic.pretty_to_string json in
  Eio.Path.save ~create:(`Or_truncate 0o644) path data

let load_manifest ~fs ~dir : cache_manifest option =
  let path = Eio.Path.(fs / dir / "manifest.json") in
  try
    let data = Eio.Path.load path in
    let json = Yojson.Basic.from_string data in
    manifest_of_json json
  with _ -> None
