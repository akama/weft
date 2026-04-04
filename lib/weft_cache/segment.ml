open Weft_types

let generate_id () =
  Printf.sprintf "seg_%06d" (Random.bits () mod 1000000)

let create ~origin ~local_path ~ttl_hours =
  let now = Ptime_clock.now () in
  {
    id = generate_id ();
    origin;
    local_path;
    time_range = { start_ = now; end_ = None };
    sealed = false;
    content_hash = None;
    size_bytes = 0L;
    fetched_at = now;
    ttl_hours;
    joined_index_built = false;
  }

let compute_hash (fs : Eio.Fs.dir_ty Eio.Path.t) path =
  let data = Eio.Path.load Eio.Path.(fs / path) in
  let hash = Digestif.SHA256.digest_string data in
  Digestif.SHA256.to_hex hash

let seal (seg : segment) ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~full_path ~end_time =
  let content_hash =
    try Some (compute_hash fs full_path)
    with Eio.Io _ as e ->
      Printf.eprintf "Warning: could not hash segment %s: %s\n"
        full_path (Printexc.to_string e);
      None
  in
  let size_bytes =
    try
      let stat = Eio.Path.stat ~follow:true Eio.Path.(fs / full_path) in
      Int64.of_int (Optint.Int63.to_int stat.size)
    with Eio.Io _ -> seg.size_bytes
  in
  { seg with
    sealed = true;
    time_range = { seg.time_range with end_ = Some end_time };
    content_hash;
    size_bytes;
  }

let append_data ~fs ~dir seg data =
  let path = Eio.Path.(fs / dir / seg.local_path) in
  let existing =
    try Eio.Path.load path
    with Eio.Io (Eio.Fs.E (Not_found _), _) -> ""
  in
  Eio.Path.save ~create:(`Or_truncate 0o644) path (existing ^ data);
  let new_size = Int64.add seg.size_bytes (Int64.of_int (String.length data)) in
  { seg with size_bytes = new_size }

let is_expired seg =
  let now = Ptime_clock.now () in
  let ttl_span = Ptime.Span.of_int_s (seg.ttl_hours * 3600) in
  match Ptime.add_span seg.fetched_at ttl_span with
  | Some expiry -> Ptime.is_later now ~than:expiry
  | None -> false

let read_lines ~fs ~dir seg =
  let path = Eio.Path.(fs / dir / seg.local_path) in
  try
    let data = Eio.Path.load path in
    String.split_on_char '\n' data
    |> List.filter (fun s -> String.length s > 0)
  with Eio.Io _ as e ->
    Printf.eprintf "Warning: could not read segment %s: %s\n"
      seg.local_path (Printexc.to_string e);
    []

(* Stdlib-based read — works outside Eio runtime (e.g. from a Domain) *)
let read_lines_stdlib ~dir seg =
  let path = Filename.concat dir seg.local_path in
  try
    let ic = open_in path in
    let len = in_channel_length ic in
    let data = Bytes.create len in
    really_input ic data 0 len;
    close_in ic;
    String.split_on_char '\n' (Bytes.to_string data)
    |> List.filter (fun s -> String.length s > 0)
  with Sys_error msg ->
    Printf.eprintf "Warning: could not read segment %s: %s\n"
      seg.local_path msg;
    []
