open Notty
open Weft_types

(* Set to true when a search is in-flight to suppress auto-follow *)
let freeze_auto_follow = ref false

type sort_order = Asc | Desc

type t = {
  mutable entries : log_entry array;
  mutable selected : int;
  mutable scroll_offset : int;
  mutable visible_height : int;
  mutable order : sort_order;
}

let create () =
  { entries = [||]; selected = 0; scroll_offset = 0;
    visible_height = 20; order = Asc }

let set_entries t entries =
  (* Remember the currently selected entry so we can restore focus *)
  let prev_entry =
    let n = Array.length t.entries in
    if n = 0 then None
    else
      let idx = match t.order with
        | Asc -> t.selected
        | Desc -> n - 1 - t.selected in
      if idx >= 0 && idx < n then Some t.entries.(idx)
      else None
  in
  t.entries <- Array.of_list entries;
  (* Try to find the same entry in the new set *)
  let restored = match prev_entry with
    | None -> false
    | Some prev ->
      let n = Array.length t.entries in
      (* Match by source + raw prefix (more robust than exact timestamp
         since tail entries may have different timestamps than cached ones) *)
      let prev_raw_prefix =
        let len = min 80 (String.length prev.raw) in
        String.sub prev.raw 0 len in
      let found = ref false in
      (* First try exact timestamp + source match *)
      for i = 0 to n - 1 do
        if not !found then begin
          let e = t.entries.(i) in
          if Ptime.equal e.timestamp prev.timestamp
             && e.source = prev.source then begin
            (* Convert array index to display index *)
            let display_idx = match t.order with
              | Asc -> i
              | Desc -> n - 1 - i in
            t.selected <- display_idx;
            t.scroll_offset <- max 0 (display_idx - t.visible_height / 2);
            found := true
          end
        end
      done;
      (* Fallback: match by source + raw content prefix *)
      if not !found then begin
        for i = 0 to n - 1 do
          if not !found then begin
            let e = t.entries.(i) in
            if e.source = prev.source then begin
              let e_prefix = let len = min 80 (String.length e.raw) in
                String.sub e.raw 0 len in
              if e_prefix = prev_raw_prefix then begin
                let display_idx = match t.order with
                  | Asc -> i | Desc -> n - 1 - i in
                t.selected <- display_idx;
                t.scroll_offset <- max 0 (display_idx - t.visible_height / 2);
                found := true
              end
            end
          end
        done
      end;
      !found
  in
  if not restored then begin
    t.scroll_offset <- 0;
    t.selected <- 0
  end

let toggle_order t =
  t.order <- (match t.order with Asc -> Desc | Desc -> Asc);
  t.scroll_offset <- 0;
  t.selected <- 0

let order_label t =
  match t.order with Asc -> "oldest first" | Desc -> "newest first"

let append_entry t (entry : log_entry) =
  let old_len = Array.length t.entries in
  let new_arr = Array.make (old_len + 1) entry in
  Array.blit t.entries 0 new_arr 0 old_len;
  t.entries <- new_arr;
  (* When a search is pending, don't auto-follow — the user initiated a
     scope change and we need to preserve their position for set_entries.
     search_pending is set externally by the engine before appending. *)
  let search_pending = !freeze_auto_follow in
  match t.order with
  | Asc ->
    (* Auto-follow only when at live edge and not frozen *)
    let was_at_bottom = t.selected >= old_len - 1 in
    if (was_at_bottom || old_len = 0) && not search_pending then begin
      t.selected <- old_len;
      t.scroll_offset <- max 0 (old_len + 1 - t.visible_height)
    end
    (* Otherwise: Asc appends to end, display indices don't shift *)
  | Desc ->
    (* Desc: new entry at array end = display index 0. All existing
       display indices shift by +1. Must always compensate. *)
    if old_len > 0 then begin
      t.selected <- t.selected + 1;
      t.scroll_offset <- t.scroll_offset + 1
    end;
    (* Auto-follow to top only when at live edge and not frozen *)
    if t.selected = 1 && t.scroll_offset = 1 && not search_pending then begin
      t.selected <- 0;
      t.scroll_offset <- 0
    end

(* Map display index to array index based on sort order *)
let to_array_idx t display_idx =
  match t.order with
  | Asc -> display_idx
  | Desc -> Array.length t.entries - 1 - display_idx

let selected_entry t =
  let n = Array.length t.entries in
  if n = 0 then None
  else
    let idx = to_array_idx t t.selected in
    if idx >= 0 && idx < n then Some t.entries.(idx)
    else None

let scroll_up t =
  if t.selected > 0 then begin
    t.selected <- t.selected - 1;
    if t.selected < t.scroll_offset then
      t.scroll_offset <- t.selected
  end

let scroll_down t =
  if t.selected < Array.length t.entries - 1 then begin
    t.selected <- t.selected + 1;
    if t.selected >= t.scroll_offset + t.visible_height then
      t.scroll_offset <- t.selected - t.visible_height + 1
  end

let page_up t =
  let jump = max 1 (t.visible_height - 1) in
  t.selected <- max 0 (t.selected - jump);
  t.scroll_offset <- max 0 (t.scroll_offset - jump)

let page_down t =
  let n = Array.length t.entries in
  let jump = max 1 (t.visible_height - 1) in
  t.selected <- min (n - 1) (t.selected + jump);
  let max_offset = max 0 (n - t.visible_height) in
  t.scroll_offset <- min max_offset (t.scroll_offset + jump)

let goto_top t =
  t.selected <- 0;
  t.scroll_offset <- 0

let goto_bottom t =
  let n = Array.length t.entries in
  if n > 0 then begin
    t.selected <- n - 1;
    t.scroll_offset <- max 0 (n - t.visible_height)
  end

let set_visible_height t h =
  t.visible_height <- max 1 h

let format_timestamp ~show_date ts =
  let ((y, mo, d), ((hh, mm, ss), _)) = Ptime.to_date_time ts in
  let _d_span, ps = Ptime.to_span ts |> Ptime.Span.to_d_ps in
  let ms = Int64.to_int (Int64.rem (Int64.div ps 1_000_000_000L) 1000L) in
  if show_date then
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" y mo d hh mm ss
  else
    Printf.sprintf "%02d:%02d:%02d.%03d" hh mm ss ms

let truncate_source src max_len =
  if String.length src <= max_len then
    src ^ String.make (max_len - String.length src) ' '
  else
    String.sub src 0 max_len

let spans_multiple_days (entries : log_entry array) =
  if Array.length entries < 2 then false
  else
    let first = entries.(0) in
    let last = entries.(Array.length entries - 1) in
    let (first_date, _) = Ptime.to_date_time first.timestamp in
    let (last_date, _) = Ptime.to_date_time last.timestamp in
    first_date <> last_date

let render_entry (entry : log_entry) ~width ~is_selected ~term_list ~show_date =
  let ts_str = format_timestamp ~show_date entry.timestamp in
  let src_str = truncate_source entry.source 8 in
  let prefix = Printf.sprintf "%s [%s] " ts_str src_str in
  let prefix_len = String.length prefix in
  let raw_available = max 0 (width - prefix_len - 1) in
  (* Show only first line for multiline entries; replace control chars *)
  let first_line = match String.index_opt entry.raw '\n' with
    | Some i -> String.sub entry.raw 0 i ^ " ..."
    | None -> entry.raw
  in
  let sanitized = String.map (fun c ->
    if Char.code c < 0x20 && c <> ' ' then ' ' else c
  ) first_line in
  let raw_display = if String.length sanitized > raw_available then
    String.sub sanitized 0 raw_available
  else sanitized in

  let base_attr = if is_selected then Theme.selected_attr
    else Notty.A.empty in

  (* Color-code based on first matching term *)
  let term_attr = match entry.terms with
    | [] -> Notty.A.empty
    | first_term :: _ ->
      (match List.find_opt (fun (st : search_term) -> st.term = first_term) term_list with
       | Some st -> Theme.term_color st.color_idx
       | None -> Notty.A.empty)
  in

  let selector = if is_selected then "> " else "  " in
  let ts_img = I.string A.(Theme.timestamp_attr ++ base_attr) (selector ^ ts_str) in
  let src_img = I.string A.(Theme.source_tag_attr ++ base_attr)
    (Printf.sprintf " [%s] " src_str) in
  let raw_img = I.string A.(term_attr ++ base_attr) raw_display in
  I.hcat [ts_img; src_img; raw_img] |> I.hsnap ~align:`Left width

let render t ~width ~height ~term_list =
  t.visible_height <- height;
  let num_entries = Array.length t.entries in
  if num_entries = 0 then
    I.string Theme.dim_attr "  No log entries"
    |> I.vsnap ~align:`Top height
    |> I.hsnap ~align:`Left width
  else begin
    let show_date = spans_multiple_days t.entries in
    let visible_start = t.scroll_offset in
    let visible_end = min num_entries (visible_start + height) in
    let lines = List.init (max 0 (visible_end - visible_start)) (fun display_i ->
      let display_idx = visible_start + display_i in
      let array_idx = to_array_idx t display_idx in
      let entry = t.entries.(array_idx) in
      render_entry entry ~width ~is_selected:(display_idx = t.selected)
        ~term_list ~show_date
    ) in
    I.vcat lines |> I.vsnap ~align:`Top height
  end

let entry_count t = Array.length t.entries
