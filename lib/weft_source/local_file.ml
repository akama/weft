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
       let cmd = Printf.sprintf "%s '%s' > '%s'" decomp_cmd path dst in
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
    let cmd = Printf.sprintf "grep -n '%s' '%s' 2>/dev/null" pattern t.path in
    let ic = Unix.open_process_in cmd in
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

let tail t ~terms ~emit ~cancel =
  let pattern = if terms = [] then None
    else Some (Re.compile (Re.Pcre.re (String.concat "|" (List.map Re.Pcre.quote terms)))) in
  let source = t.config.name in
  let ic = open_in t.path in
  seek_in ic (in_channel_length ic);
  let last_inode = (Unix.stat t.path).Unix.st_ino in
  let last_size = ref (Int64.of_int (in_channel_length ic)) in
  let last_inode = ref last_inode in
  (try
     while not (Atomic.get cancel) do
       (match Rotation.check_local_rotation ~path:t.path
                ~last_inode:!last_inode ~last_size:!last_size with
        | Some (_, new_inode, new_size) ->
          close_in_noerr ic;
          last_inode := new_inode;
          last_size := new_size;
          raise Exit
        | None -> ());
       (try
          let line = input_line ic in
          last_size := Int64.of_int (pos_in ic);
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
        with End_of_file ->
          Unix.sleepf 0.1);
     done
   with Exit -> ());
  close_in_noerr ic

let close _t = ()
