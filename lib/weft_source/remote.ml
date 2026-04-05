open Weft_types

type t = {
  config : source_config;
  ssh : Weft_connection.Ssh_control.t;
  path : string;
}

let connect config ~(proc : _ Eio.Process.mgr) ~fs:_ =
  match config.transport, config.path with
  | None, _ -> Error "Remote source requires a 'transport'"
  | _, None -> Error "Remote source requires a 'path'"
  | Some transport, Some path ->
    let ssh = Weft_connection.Ssh_control.create ~transport_cmd:transport in
    (try
       Weft_connection.Ssh_control.establish proc ssh;
       Ok { config; ssh; path }
     with exn -> Error (Printexc.to_string exn))

let health_check t =
  try
    let _ = Weft_connection.Ssh_control.run_command t.ssh
      ["test"; "-f"; t.path; "&&"; "echo"; "ok"] in
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let fetch t ~dst =
  try
    let data = Weft_connection.Ssh_control.run_command t.ssh
      ["cat"; t.path] in
    let oc = open_out_bin dst in
    output_string oc data;
    close_out oc;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let fetch_archive t ~path ~dst =
  let decomp = match Archive.decompressor_for path with
    | Some cmd -> cmd
    | None -> "cat"
  in
  try
    let data = Weft_connection.Ssh_control.run_command t.ssh
      [decomp; path] in
    let oc = open_out_bin dst in
    output_string oc data;
    close_out oc;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let discover_archives t =
  Archive.discover_remote ~ssh:t.ssh ~path:t.path
  |> Archive.sort_by_mtime

let build_grep_pattern terms =
  String.concat "\\|" terms

let search t ~terms ~time_range:_ =
  if terms = [] then Seq.empty
  else
    let pattern = build_grep_pattern terms in
    try
      let output = Weft_connection.Ssh_control.run_command t.ssh
        ["grep"; "-n"; pattern; t.path] in
      let lines = String.split_on_char '\n' output
        |> List.filter (fun s -> String.length s > 0) in
      let source = t.config.name in
      List.to_seq (List.map (fun line ->
        let raw = match String.index_opt line ':' with
          | Some idx -> String.sub line (idx + 1) (String.length line - idx - 1)
          | None -> line
        in
        let matched_terms = List.filter (fun term ->
          let re = Re.compile (Re.Pcre.re (Re.Pcre.quote term)) in
          Re.execp re raw
        ) terms in
        { timestamp = Ptime_clock.now ();
          raw; source; terms = matched_terms; metadata = [] }
      ) lines)
    with
    | Failure msg ->
      Printf.eprintf "Warning: remote search failed for %s: %s\n"
        t.config.name msg;
      Seq.empty
    | Eio.Io _ as e ->
      Printf.eprintf "Warning: remote search I/O error for %s: %s\n"
        t.config.name (Printexc.to_string e);
      Seq.empty

(* Shell-quote a string for safe use in remote shell commands *)
let shell_quote s =
  "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"

let tail t ~terms ~emit ~cancel =
  let filter_pattern = if terms = [] then ""
    else String.concat "|" (List.map Re.Pcre.quote terms) in
  let source = t.config.name in
  let pattern_re = if filter_pattern = "" then None
    else Some (Re.compile (Re.Pcre.re filter_pattern)) in
  let tail_cmd = if filter_pattern = "" then
    ["tail"; "-F"; t.path]
  else
    ["sh"; "-c";
     Printf.sprintf "tail -F %s | grep --line-buffered -E %s"
       (shell_quote t.path) (shell_quote filter_pattern)]
  in
  (try
     let output = Weft_connection.Ssh_control.run_command t.ssh tail_cmd in
     let lines = String.split_on_char '\n' output
       |> List.filter (fun s -> String.length s > 0) in
     List.iter (fun line ->
       if not (Atomic.get cancel) then begin
         let matches = match pattern_re with
           | None -> true
           | Some re -> Re.execp re line
         in
         if matches then begin
           let matched_terms = List.filter (fun term ->
             Re.execp (Re.compile (Re.Pcre.re (Re.Pcre.quote term))) line
           ) terms in
           emit {
             timestamp = Ptime_clock.now ();
             raw = line;
             source;
             terms = matched_terms;
             metadata = [];
           }
         end
       end
     ) lines
   with
   | Failure msg ->
     Printf.eprintf "Warning: remote tail failed for %s: %s\n"
       t.config.name msg
   | Eio.Io _ as e ->
     Printf.eprintf "Warning: remote tail I/O error for %s: %s\n"
       t.config.name (Printexc.to_string e))

let close t =
  Weft_connection.Ssh_control.close t.ssh
