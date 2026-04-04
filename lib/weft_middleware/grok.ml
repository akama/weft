(* Grok pattern compiler: %{PATTERN:name} -> named regex groups *)

let grok_ref_re = Re.compile (Re.Pcre.re {|%\{(\w+)(?::(\w+))?\}|})

(* Expand %{PATTERN:name} references recursively, with depth limit *)
let expand_pattern pattern =
  let max_depth = 10 in
  let rec expand pat depth =
    if depth > max_depth then pat
    else
      let changed = ref false in
      let result = Re.replace grok_ref_re pat ~f:(fun g ->
        let pat_name = Re.Group.get g 1 in
        let field_name = try Some (Re.Group.get g 2) with _ -> None in
        match Grok_patterns.lookup pat_name with
        | None -> Re.Group.get g 0 (* leave as-is if unknown *)
        | Some replacement ->
          changed := true;
          match field_name with
          | Some name -> Printf.sprintf "(?<%s>%s)" name replacement
          | None -> Printf.sprintf "(?:%s)" replacement
      ) in
      if !changed then expand result (depth + 1)
      else result
  in
  expand pattern 0

type t = {
  re : Re.re;
  field_names : string list;
}

let create pattern =
  let expanded = expand_pattern pattern in
  let re = Re.compile (Re.Pcre.re expanded) in
  (* Extract field names from (?P<name>...) groups *)
  let name_re = Re.compile (Re.Pcre.re {|\(\?(?:P)?<(\w+)>|}) in
  let field_names =
    Re.all name_re expanded
    |> List.filter_map (fun g ->
      try Some (Re.Group.get g 1) with _ -> None)
  in
  { re; field_names }

let apply t s metadata =
  match Re.exec_opt t.re s with
  | None -> metadata
  | Some g ->
    let new_fields =
      List.filter_map (fun name ->
        (* Find the group index for this named group *)
        let n = Re.Group.nb_groups g in
        let found = ref None in
        for i = 1 to n - 1 do
          match !found with
          | Some _ -> ()
          | None ->
            (try
               let v = Re.Group.get g i in
               (* We map fields positionally since Re doesn't support
                  named group lookup directly — field_names are in order *)
               let idx = i - 1 in
               if idx < List.length t.field_names &&
                  List.nth t.field_names idx = name then
                 found := Some (name, v)
             with _ -> ())
        done;
        !found
      ) t.field_names
    in
    metadata @ new_fields
