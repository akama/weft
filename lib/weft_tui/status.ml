open Notty

type log_entry = {
  time : float;
  message : string;
}

type t = {
  mutable message : string;
  mutable updated_at : float;
  mutable history : log_entry list;  (* newest first, capped at max_history *)
  max_history : int;
}

let create () =
  { message = ""; updated_at = 0.0; history = []; max_history = 200 }

let set t msg =
  t.message <- msg;
  t.updated_at <- Unix.gettimeofday ();
  t.history <- { time = t.updated_at; message = msg } :: t.history;
  if List.length t.history > t.max_history then
    t.history <- List.filteri (fun i _ -> i < t.max_history) t.history

let clear t =
  t.message <- "";
  t.updated_at <- 0.0

let render t ~width =
  let age = if t.message = "" then 999.0
    else Unix.gettimeofday () -. t.updated_at in
  let attr =
    if age < 5.0 then A.(fg lightyellow)
    else if age < 10.0 then A.(fg lightblack)
    else (t.message <- ""; A.(fg lightblack))
  in
  let text = if t.message = "" then
    String.make width ' '
  else
    let msg = if String.length t.message > width - 2 then
      String.sub t.message 0 (width - 2)
    else t.message in
    let line = " " ^ msg in
    let pad = max 0 (width - String.length line) in
    line ^ String.make pad ' '
  in
  I.string attr text

(* Render the log viewer — pageable history of all status messages *)
let render_log t ~width ~height ~scroll_offset =
  let title = I.string A.(st bold) "  Status Log (L to close, j/k to scroll)" in
  let n = List.length t.history in
  if n = 0 then
    I.vcat [title; I.string A.(fg lightblack) "  No messages yet"]
    |> I.vsnap ~align:`Top height
    |> I.hsnap ~align:`Left width
  else
    let avail = height - 2 in
    let offset = max 0 (min scroll_offset (n - avail)) in
    let visible = List.filteri (fun i _ -> i >= offset && i < offset + avail)
      t.history in
    let lines = List.map (fun entry ->
      let tm = Unix.gmtime entry.time in
      let ts = Printf.sprintf "%02d:%02d:%02d"
        tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec in
      let msg = if String.length entry.message > width - 12 then
        String.sub entry.message 0 (width - 12)
      else entry.message in
      let sanitized = String.map (fun c ->
        if Char.code c < 0x20 && c <> ' ' then ' ' else c
      ) msg in
      I.string A.(fg lightblack) (Printf.sprintf "  %s  %s" ts sanitized)
    ) visible in
    let scroll_info = I.string A.(fg lightblack)
      (Printf.sprintf "  [%d-%d of %d]" (offset + 1)
         (min n (offset + avail)) n) in
    I.vcat ([title] @ lines @ [scroll_info])
    |> I.vsnap ~align:`Top height
    |> I.hsnap ~align:`Left width
