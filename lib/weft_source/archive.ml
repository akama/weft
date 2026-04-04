open Weft_types

(* Archive discovery: find rotated/compressed siblings of a log file *)

(* Common rotation naming patterns *)
let numeric_suffix_re = Re.compile (Re.Pcre.re {|\.(\d+)(?:\.(gz|bz2|xz|zst))?$|})
let date_suffix_re = Re.compile (Re.Pcre.re {|[-.](\d{4}-?\d{2}-?\d{2})(?:\.(gz|bz2|xz|zst))?\.log$|})

let is_compressed path =
  List.exists (fun ext -> Filename.check_suffix path ext)
    [".gz"; ".bz2"; ".xz"; ".zst"]

let decompressor_for path =
  if Filename.check_suffix path ".gz" then Some "zcat"
  else if Filename.check_suffix path ".bz2" then Some "bzcat"
  else if Filename.check_suffix path ".xz" then Some "xzcat"
  else if Filename.check_suffix path ".zst" then Some "zstdcat"
  else None

(* Discover archives for a local file *)
let discover_local ~path =
  let dir = Filename.dirname path in
  let base = Filename.basename path in
  let pattern = Filename.concat dir (base ^ "*") in
  (* Use glob expansion *)
  let entries = try
    let ic = Unix.open_process_in (Printf.sprintf "ls -1 %s 2>/dev/null" pattern) in
    let lines = ref [] in
    (try while true do
       lines := input_line ic :: !lines
     done with End_of_file -> ());
    ignore (Unix.close_process_in ic);
    List.rev !lines
  with _ -> []
  in
  (* Filter out the active log file itself *)
  List.filter_map (fun filepath ->
    if filepath = path then None
    else if Re.execp numeric_suffix_re filepath ||
            Re.execp date_suffix_re filepath ||
            is_compressed filepath then
      let stat = try
        let s = Unix.stat filepath in
        Some s
      with _ -> None
      in
      let mtime = match stat with
        | Some s -> Ptime.of_float_s s.Unix.st_mtime
        | None -> None
      in
      let size_bytes = match stat with
        | Some s -> Int64.of_int s.Unix.st_size
        | None -> 0L
      in
      Some { remote_path = filepath; mtime; size_bytes; matches_segment = None }
    else None
  ) entries

(* Discover archives on a remote host via SSH *)
let discover_remote ~ssh ~path =
  let dir = Filename.dirname path in
  let base = Filename.basename path in
  let pattern = Printf.sprintf "%s/%s*" dir base in
  try
    let lines = Weft_connection.Ssh_control.run_command_lines ssh
      ["ls"; "-1"; "--time-style=+%s"; "-l"; pattern] in
    List.filter_map (fun line ->
      (* Parse ls -l output *)
      let parts = String.split_on_char ' ' line
        |> List.filter (fun s -> String.length s > 0) in
      match List.rev parts with
      | filepath :: _ when filepath <> path ->
        if Re.execp numeric_suffix_re filepath ||
           Re.execp date_suffix_re filepath ||
           is_compressed filepath then
          Some { remote_path = filepath; mtime = None;
                 size_bytes = 0L; matches_segment = None }
        else None
      | _ -> None
    ) lines
  with _ -> []

(* Sort archives by mtime, most recent first *)
let sort_by_mtime archives =
  List.sort (fun (a : archive_info) (b : archive_info) ->
    match a.mtime, b.mtime with
    | Some ta, Some tb -> Ptime.compare tb ta (* descending *)
    | Some _, None -> -1
    | None, Some _ -> 1
    | None, None -> 0
  ) archives
