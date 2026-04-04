open Weft_types

type t = {
  config : source_config;
  sub_sources : (string * Local_file.t) list;
}

let expand_glob pattern =
  let cmd = Printf.sprintf "ls -1 %s 2>/dev/null" pattern in
  let ic = Unix.open_process_in cmd in
  let files = ref [] in
  (try while true do
     files := input_line ic :: !files
   done with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  List.rev !files

let connect config ~(proc : _ Eio.Process.mgr) ~fs =
  match config.glob with
  | None -> Error "Directory source requires a 'glob'"
  | Some glob_pattern ->
    let files = expand_glob glob_pattern in
    if files = [] then
      Error (Printf.sprintf "No files matched glob: %s" glob_pattern)
    else
      let sub_sources = List.filter_map (fun path ->
        let basename = Filename.basename path in
        let sub_name = Printf.sprintf "%s:%s" config.name basename in
        let sub_config = { config with
          name = sub_name;
          source_type = File;
          path = Some path;
          glob = None;
        } in
        match Local_file.connect sub_config ~proc ~fs with
        | Ok src -> Some (sub_name, src)
        | Error _ -> None
      ) files in
      Ok { config; sub_sources }

let health_check t =
  let errors = List.filter_map (fun (name, src) ->
    match Local_file.health_check src with
    | Ok () -> None
    | Error e -> Some (Printf.sprintf "%s: %s" name e)
  ) t.sub_sources in
  if errors = [] then Ok ()
  else Error (String.concat "; " errors)

let search t ~terms ~time_range =
  let seqs = List.map (fun (_name, src) ->
    Local_file.search src ~terms ~time_range
  ) t.sub_sources in
  List.fold_left Seq.append Seq.empty seqs

let tail t ~terms ~emit ~cancel =
  List.iter (fun (_name, src) ->
    if not (Atomic.get cancel) then
      Local_file.tail_simple src ~terms ~emit ~cancel
  ) t.sub_sources

let discover_archives t =
  List.concat_map (fun (_name, src) ->
    Local_file.discover_archives src
  ) t.sub_sources

let sub_source_names t =
  List.map fst t.sub_sources

let close _t = ()
