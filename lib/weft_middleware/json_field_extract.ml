type t = {
  fields : string list;
  source_field : string option;
}

let create ~fields ~source_field =
  { fields; source_field }

let json_to_string = function
  | `String s -> s
  | `Int i -> string_of_int i
  | `Float f -> Printf.sprintf "%g" f
  | `Bool b -> string_of_bool b
  | `Null -> "null"
  | other -> Yojson.Basic.to_string other

let apply t s metadata =
  let json_str = match t.source_field with
    | None -> s
    | Some field_name ->
      (match List.assoc_opt field_name metadata with
       | Some v -> v
       | None -> s)
  in
  try
    let json = Yojson.Basic.from_string json_str in
    match json with
    | `Assoc fields ->
      let new_fields =
        List.filter_map (fun name ->
          match List.assoc_opt name fields with
          | Some v -> Some (name, json_to_string v)
          | None -> None
        ) t.fields
      in
      metadata @ new_fields
    | _ -> metadata
  with Yojson.Json_error _ -> metadata
