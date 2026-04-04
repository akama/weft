type t = {
  re : Re.re;
  positional_fields : string list option;
}

let create ~pattern ~fields =
  let re = Re.compile (Re.Pcre.re pattern) in
  { re; positional_fields = fields }

let apply t s metadata =
  match Re.exec_opt t.re s with
  | None -> metadata
  | Some g ->
    let n = Re.Group.nb_groups g in
    let fields = match t.positional_fields with
      | Some names -> names
      | None ->
        List.init (n - 1) (fun i -> Printf.sprintf "field_%d" (i + 1))
    in
    let pairs = ref [] in
    List.iteri (fun i name ->
      let grp = i + 1 in
      if grp < n then
        match (try Some (Re.Group.get g grp) with Not_found -> None) with
        | Some v -> pairs := (name, v) :: !pairs
        | None -> ()
    ) fields;
    metadata @ List.rev !pairs
