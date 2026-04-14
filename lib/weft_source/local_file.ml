open Weft_types

type t = {
  config : source_config;
  path : string;
  fs : Eio.Fs.dir_ty Eio.Path.t;
}

let connect (config : source_config) ~proc:(_ : _ Eio.Process.mgr) ~fs =
  match config.path with
  | None -> Error "Local file source requires a 'path'"
  | Some path ->
    if Sys.file_exists path then
      Ok { config; path; fs }
    else
      Error (Printf.sprintf "File not found: %s" path)

let health_check t =
  if Sys.file_exists t.path then Ok ()
  else Error (Printf.sprintf "File not found: %s" t.path)

let fetch t ~dst =
  try
    let data = Eio.Path.load Eio.Path.(t.fs / t.path) in
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(t.fs / dst) data;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let fetch_archive _t ~path ~dst =
  match Archive.decompressor_for path with
  | None ->
    (try
       let ic = open_in_bin path in
       let len = in_channel_length ic in
       let data = Bytes.create len in
       really_input ic data 0 len;
       close_in ic;
       let oc = open_out_bin dst in
       output_bytes oc data;
       close_out oc;
       Ok ()
     with exn -> Error (Printexc.to_string exn))
  | Some decomp_cmd ->
    (try
       (* Use shell quoting to prevent injection via file paths *)
       let shell_quote s =
         "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'" in
       let cmd = Printf.sprintf "%s %s > %s"
         decomp_cmd (shell_quote path) (shell_quote dst) in
       let ret = Sys.command cmd in
       if ret = 0 then Ok ()
       else Error (Printf.sprintf "%s failed with exit code %d" decomp_cmd ret)
     with exn -> Error (Printexc.to_string exn))

let discover_archives t =
  Archive.discover_local ~path:t.path
  |> Archive.sort_by_mtime

let build_grep_pattern terms =
  String.concat "\\|" terms

let search t ~terms ~time_range:_ =
  if terms = [] then Seq.empty
  else
    let pattern = build_grep_pattern terms in
    (* Use array-based process creation to avoid shell injection *)
    let args = [| "grep"; "-n"; pattern; t.path |] in
    let ic = Unix.open_process_args_in "grep" args in
    let source = t.config.name in
    let rec read_entries () =
      match (try Some (input_line ic) with End_of_file -> None) with
      | None ->
        ignore (Unix.close_process_in ic);
        Seq.Nil
      | Some line ->
        let raw = match String.index_opt line ':' with
          | Some idx -> String.sub line (idx + 1) (String.length line - idx - 1)
          | None -> line
        in
        let matched_terms = List.filter (fun term ->
          let re = Re.compile (Re.Pcre.re (Re.Pcre.quote term)) in
          Re.execp re raw
        ) terms in
        let entry = {
          timestamp = Ptime_clock.now ();
          raw;
          source;
          terms = matched_terms;
          metadata = [];
        } in
        Seq.Cons (entry, read_entries)
    in
    read_entries

(* Rotation lifecycle:
   1. Detect rename (inotify Move_self) or truncation (size < offset)
   2. Drain remaining data from old fd (up to drain_timeout_sec)
   3. Call on_rotation callback (seals segment, opens new)
   4. Reopen the path for continued tailing *)
type rotation_callbacks = {
  on_seal : unit -> unit;
  on_new : unit -> unit;
}

(* Read new lines from a file descriptor starting at offset *)
let read_new_lines ic =
  let lines = ref [] in
  (try
     while true do
       lines := input_line ic :: !lines
     done
   with End_of_file -> ());
  List.rev !lines

(* Default wait: Unix.select (blocks OS thread — use only outside Eio) *)
let default_wait_readable fd timeout =
  let ready, _, _ = Unix.select [fd] [] [] timeout in
  ready <> []

(* Default sleep: Unix.sleepf (blocks OS thread — use only outside Eio) *)
let default_sleep secs = Unix.sleepf secs

(* Tail a file for new lines. Uses inotify on Linux, polling on macOS.
   wait_readable: function to poll fd readability. Pass an Eio-aware
   version when running inside Eio to avoid blocking the scheduler. *)
let tail t ~terms ~emit ~cancel
    ?(on_rotation : rotation_callbacks option)
    ?(drain_timeout = Weft_constants.default_drain_timeout)
    ?(wait_readable = default_wait_readable)
    ?(sleep = default_sleep)
    () =
  let pattern = if terms = [] then None
    else Some (Re.compile (Re.Pcre.re (String.concat "|"
      (List.map Re.Pcre.quote terms)))) in
  let source = t.config.name in

  let drain_timeout = match on_rotation with
    | Some _ -> drain_timeout
    | None -> 0.0
  in

  let emit_line line =
    let matches = match pattern with
      | None -> true
      | Some re -> Re.execp re line
    in
    if matches then begin
      let matched_terms = match pattern with
        | None -> terms
        | Some _ ->
          List.filter (fun term ->
            Re.execp (Re.compile (Re.Pcre.re (Re.Pcre.quote term))) line
          ) terms
      in
      emit {
        timestamp = Ptime_clock.now ();
        raw = line;
        source;
        terms = matched_terms;
        metadata = [];
      }
    end
  in

  (* Convert rotation_callbacks to the tuple form used by File_watcher *)
  let rotation_fns = match on_rotation with
    | Some cb -> Some (cb.on_seal, cb.on_new)
    | None -> None
  in
  File_watcher.tail ~path:t.path ~read_new_lines ~emit_line ~cancel
    ~on_rotation:rotation_fns ~drain_timeout ~wait_readable ~sleep ()

(* Simplified tail without rotation callbacks — backward compat *)
let tail_simple t ~terms ~emit ~cancel =
  tail t ~terms ~emit ~cancel ()

let close _t = ()
