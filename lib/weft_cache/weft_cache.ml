open Weft_types

type t = {
  fs : Eio.Fs.dir_ty Eio.Path.t;
  base_dir : string;
  config : cache_config;
  mutable manifests : (source_id * cache_manifest) list;
}

let expand_home path =
  if String.length path > 0 && path.[0] = '~' then
    let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
    home ^ String.sub path 1 (String.length path - 1)
  else path

let create ~fs config =
  let base_dir = expand_home config.dir in
  { fs; base_dir; config; manifests = [] }

let ensure_dir t source_name =
  let dir = Filename.concat t.base_dir source_name in
  (try
     Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 (Eio.Path.(t.fs / dir))
   with _ -> ());
  dir

let init_source t ~source_name ~format =
  let dir = ensure_dir t source_name in
  let manifest = match Manifest.load_manifest ~fs:t.fs ~dir with
    | Some m -> m
    | None ->
      { source = source_name; format; segments = []; known_archives = [] }
  in
  let manifest = Eviction.run_eviction manifest
    ~max_mb_per_source:t.config.max_mb_per_source in
  t.manifests <- (source_name, manifest) :: t.manifests;
  manifest

let get_manifest t source_name =
  List.assoc_opt source_name t.manifests

let update_manifest t source_name manifest =
  t.manifests <- (source_name, manifest) ::
    (List.filter (fun (k, _) -> k <> source_name) t.manifests);
  let dir = Filename.concat t.base_dir source_name in
  Manifest.save_manifest ~fs:t.fs ~dir manifest

let new_segment t ~source_name ~origin =
  let dir = ensure_dir t source_name in
  let seg = Segment.create ~origin
    ~local_path:(Filename.concat source_name (Segment.generate_id ()))
    ~ttl_hours:t.config.default_ttl_hours in
  match get_manifest t source_name with
  | None -> seg
  | Some manifest ->
    let manifest = { manifest with segments = manifest.segments @ [seg] } in
    update_manifest t source_name manifest;
    ignore dir;
    seg

let store_data t ~source_name seg data =
  let dir = Filename.concat t.base_dir source_name in
  let seg = Segment.append_data ~fs:t.fs ~dir seg data in
  (match get_manifest t source_name with
   | None -> ()
   | Some manifest ->
     let segments = List.map (fun (s : segment) ->
       if s.id = seg.id then seg else s
     ) manifest.segments in
     update_manifest t source_name { manifest with segments });
  seg

let seal_segment t ~source_name seg ~end_time =
  let sealed = Segment.seal seg ~fs:t.fs ~end_time in
  (match get_manifest t source_name with
   | None -> ()
   | Some manifest ->
     let segments = List.map (fun (s : segment) ->
       if s.id = sealed.id then sealed else s
     ) manifest.segments in
     update_manifest t source_name { manifest with segments });
  sealed

let read_cached_lines t ~source_name =
  let dir = Filename.concat t.base_dir source_name in
  match get_manifest t source_name with
  | None -> []
  | Some manifest ->
    List.concat_map (fun seg ->
      Segment.read_lines ~fs:t.fs ~dir seg
    ) manifest.segments

let time_coverage t source_name =
  match get_manifest t source_name with
  | None -> None
  | Some manifest ->
    let segments = manifest.segments in
    if segments = [] then None
    else
      let earliest = List.fold_left (fun acc (s : segment) ->
        match acc with
        | None -> Some s.time_range.start_
        | Some t -> Some (if Ptime.is_earlier s.time_range.start_ ~than:t then s.time_range.start_ else t)
      ) None segments in
      let latest = List.fold_left (fun acc (s : segment) ->
        match s.time_range.end_ with
        | None -> Some (Ptime_clock.now ())
        | Some e ->
          match acc with
          | None -> Some e
          | Some t -> Some (if Ptime.is_later e ~than:t then e else t)
      ) None segments in
      match earliest, latest with
      | Some s, Some e -> Some { start_ = s; end_ = Some e }
      | _ -> None

let update_archives t ~source_name archives =
  match get_manifest t source_name with
  | None -> ()
  | Some manifest ->
    update_manifest t source_name { manifest with known_archives = archives }

let run_eviction t source_name =
  match get_manifest t source_name with
  | None -> ()
  | Some manifest ->
    let manifest = Eviction.run_eviction manifest
      ~max_mb_per_source:t.config.max_mb_per_source in
    update_manifest t source_name manifest
