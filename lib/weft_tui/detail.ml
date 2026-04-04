open Notty
open Weft_types

type t = {
  mutable expanded : bool;
}

let create () = { expanded = false }

let toggle t = t.expanded <- not t.expanded
let is_expanded t = t.expanded

let render t ~entry ~width ~height =
  if not t.expanded then
    I.empty
  else
    match entry with
    | None ->
      I.string Theme.dim_attr "  No entry selected"
      |> I.hsnap ~align:`Left width
      |> I.vsnap ~align:`Top height
    | Some (entry : log_entry) ->
      let title = I.string Theme.title_attr "DETAIL" in
      let source_line = I.hcat [
        I.string Theme.detail_label_attr "Source: ";
        I.string A.empty entry.source;
      ] in
      let raw_label = I.string Theme.detail_label_attr "Raw: " in
      (* Wrap raw text to width *)
      let raw_lines =
        let max_line = width - 2 in
        let rec wrap s acc =
          if String.length s <= max_line then List.rev (s :: acc)
          else
            let chunk = String.sub s 0 max_line in
            wrap (String.sub s max_line (String.length s - max_line)) (chunk :: acc)
        in
        wrap entry.raw []
      in
      let raw_imgs = List.map (fun l -> I.string A.empty ("  " ^ l)) raw_lines in

      let metadata_label = I.string Theme.detail_label_attr "Fields: " in
      let metadata_lines = List.map (fun (k, v) ->
        I.string A.empty (Printf.sprintf "  %s=%s" k v)
      ) entry.metadata in

      let terms_label = I.string Theme.detail_label_attr "Matched: " in
      let terms_line = I.string A.empty ("  " ^ String.concat ", " entry.terms) in

      let content = I.vcat ([
        title;
        source_line;
        raw_label;
      ] @ raw_imgs @ [
        metadata_label;
      ] @ metadata_lines @ [
        terms_label;
        terms_line;
      ]) in
      content |> I.vsnap ~align:`Top height |> I.hsnap ~align:`Left width
