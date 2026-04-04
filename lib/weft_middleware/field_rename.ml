type t = {
  mapping : (string * string) list; (* old_name -> new_name *)
}

let create ~mapping = { mapping }

let apply t metadata =
  List.map (fun (k, v) ->
    match List.assoc_opt k t.mapping with
    | Some new_name -> (new_name, v)
    | None -> (k, v)
  ) metadata
