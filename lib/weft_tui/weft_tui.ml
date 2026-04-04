open Notty
open Weft_types

type focus = Sources | Terms | Timeline

type overlay = None_ | Help | Heatmap

type model = {
  search_bar : Search_bar.t;
  timeline : Timeline.t;
  sidebar : Sidebar.t;
  detail : Detail.t;
  progress : Progress.t;
  status : Status.t;
  search : Weft_search.t;
  mutable focus : focus;
  mutable quit : bool;
  mutable width : int;
  mutable height : int;
  mutable time_range : time_range option;
  mutable overlay : overlay;
}

let create ~search ~time_range =
  {
    search_bar = Search_bar.create ();
    timeline = Timeline.create ();
    sidebar = Sidebar.create ();
    detail = Detail.create ();
    progress = Progress.create ();
    status = Status.create ();
    search;
    focus = Timeline;
    quit = false;
    width = 80;
    height = 24;
    time_range;
    overlay = None_;
  }

let format_time_range = function
  | None -> "all time"
  | Some tr ->
    let date t = let ((_y, mo, d), _) = Ptime.to_date_time t in (mo, d) in
    let time t = let (_, ((hh, mm, ss), _)) = Ptime.to_date_time t in
      Printf.sprintf "%02d:%02d:%02d" hh mm ss in
    let fmt_date (mo, d) = Printf.sprintf "%02d-%02d" mo d in
    let start_t = time tr.start_ in
    match tr.end_ with
    | None ->
      Printf.sprintf "%s: %s -> now" (fmt_date (date tr.start_)) start_t
    | Some e ->
      let sd = date tr.start_ in
      let ed = date e in
      if sd = ed then
        Printf.sprintf "%s: %s -> %s" (fmt_date sd) start_t (time e)
      else
        Printf.sprintf "%s %s -> %s %s"
          (fmt_date sd) start_t (fmt_date ed) (time e)

(* Re-run search and update timeline *)
let refresh_search model =
  let terms = Weft_search.enabled_terms model.search in
  let term_desc = match terms with
    | [] -> "all entries"
    | [t] -> Printf.sprintf "'%s'" t
    | ts -> Printf.sprintf "%d terms" (List.length ts) in
  Status.set model.status (Printf.sprintf "Searching %s..." term_desc);
  let entries = if terms <> [] then
    Weft_search.search model.search ~time_range:model.time_range
  else
    Weft_search.load_all ?time_range:model.time_range model.search
  in
  let entry_list = List.of_seq (Seq.take 100000 entries) in
  let filtered = List.filter (fun (e : log_entry) ->
    Sidebar.is_source_enabled model.sidebar e.source
  ) entry_list in
  Timeline.set_entries model.timeline filtered;
  let range_desc = format_time_range model.time_range in
  Status.set model.status
    (Printf.sprintf "%d entries [%s]" (List.length filtered) range_desc)

(* Time range helpers *)
let window_seconds tr =
  match tr.end_ with
  | None ->
    let now = Ptime_clock.now () in
    (match Ptime.diff now tr.start_ |> Ptime.Span.to_float_s with
     | s -> int_of_float s
     | exception _ -> 3600)
  | Some e ->
    (match Ptime.diff e tr.start_ |> Ptime.Span.to_float_s with
     | s -> int_of_float s
     | exception _ -> 3600)

let shift_range tr seconds =
  let span = Ptime.Span.of_int_s (abs seconds) in
  if seconds >= 0 then
    let start_ = match Ptime.add_span tr.start_ span with
      | Some t -> t | None -> tr.start_ in
    let end_ = match tr.end_ with
      | None -> None
      | Some e -> Ptime.add_span e span in
    { start_; end_ }
  else
    let start_ = match Ptime.sub_span tr.start_ span with
      | Some t -> t | None -> tr.start_ in
    let end_ = match tr.end_ with
      | None -> None
      | Some e -> Ptime.sub_span e span in
    { start_; end_ }

let widen_range tr =
  let half = window_seconds tr / 2 in
  let span = Ptime.Span.of_int_s half in
  let start_ = match Ptime.sub_span tr.start_ span with
    | Some t -> t | None -> tr.start_ in
  let end_ = match tr.end_ with
    | None -> None
    | Some e -> Ptime.add_span e span
  in
  { start_; end_ }

let narrow_range tr =
  let quarter = window_seconds tr / 4 in
  let span = Ptime.Span.of_int_s quarter in
  let start_ = match Ptime.add_span tr.start_ span with
    | Some t -> t | None -> tr.start_ in
  let end_ = match tr.end_ with
    | None -> None
    | Some e -> Ptime.sub_span e span
  in
  { start_; end_ }

(* Handle keyboard input *)
let handle_key model key =
  (* Overlays consume all keys except their dismiss key *)
  if model.overlay <> None_ then begin
    (match key with
     | `Escape | `ASCII '?' ->
       model.overlay <- None_
     | `ASCII 'H' | `ASCII 'h' ->
       if model.overlay = Heatmap then model.overlay <- None_
       else model.overlay <- Heatmap
     | `ASCII 'q' -> model.quit <- true
     (* Allow time navigation while in heatmap *)
     | `ASCII '<' | `ASCII ',' when model.overlay = Heatmap ->
       let tr = match model.time_range with
         | Some tr -> tr
         | None ->
           let now = Ptime_clock.now () in
           { start_ = (match Ptime.sub_span now (Ptime.Span.of_int_s 3600) with
                        | Some t -> t | None -> now);
             end_ = Some now }
       in
       let half = window_seconds tr / 2 in
       model.time_range <- Some (shift_range tr (-half));
       refresh_search model
     | `ASCII '>' | `ASCII '.' when model.overlay = Heatmap ->
       (match model.time_range with
        | Some tr ->
          let half = window_seconds tr / 2 in
          model.time_range <- Some (shift_range tr half);
          refresh_search model
        | None -> ())
     | `ASCII '-' | `ASCII '_' when model.overlay = Heatmap ->
       (match model.time_range with
        | Some tr ->
          let narrowed = narrow_range tr in
          if window_seconds narrowed > 60 then begin
            model.time_range <- Some narrowed;
            refresh_search model
          end
        | None ->
          let now = Ptime_clock.now () in
          model.time_range <- Some {
            start_ = (match Ptime.sub_span now (Ptime.Span.of_int_s 1800) with
                       | Some t -> t | None -> now);
            end_ = Some now };
          refresh_search model)
     | `ASCII '+' | `ASCII '=' when model.overlay = Heatmap ->
       (match model.time_range with
        | Some tr ->
          model.time_range <- Some (widen_range tr);
          refresh_search model
        | None -> ())
     | `ASCII 'r' when model.overlay = Heatmap ->
       model.time_range <- None;
       refresh_search model
     | _ -> ())
  end else
  if Search_bar.is_active model.search_bar then begin
    match key with
    | `Escape ->
      Search_bar.deactivate model.search_bar
    | `Enter ->
      (match Search_bar.submit model.search_bar with
       | Some term ->
         ignore (Weft_search.add_term model.search term);
         refresh_search model
       | None -> ())
    | `Backspace ->
      Search_bar.handle_backspace model.search_bar
    | `Arrow `Left ->
      Search_bar.handle_left model.search_bar
    | `Arrow `Right ->
      Search_bar.handle_right model.search_bar
    | `ASCII c ->
      Search_bar.handle_char model.search_bar c
    | _ -> ()
  end else begin
    match key with
    | `ASCII '/' ->
      Search_bar.activate model.search_bar
    | `ASCII 'q' ->
      model.quit <- true
    | `ASCII 'j' | `Arrow `Down ->
      (match model.focus with
       | Timeline -> Timeline.scroll_down model.timeline
       | Sources ->
         Sidebar.move_source_selection model.sidebar 1
       | Terms ->
         let terms = Weft_search.all_terms model.search in
         Sidebar.move_term_selection model.sidebar ~terms 1)
    | `ASCII 'k' | `Arrow `Up ->
      (match model.focus with
       | Timeline -> Timeline.scroll_up model.timeline
       | Sources ->
         Sidebar.move_source_selection model.sidebar (-1)
       | Terms ->
         let terms = Weft_search.all_terms model.search in
         Sidebar.move_term_selection model.sidebar ~terms (-1))
    | `Enter ->
      Detail.toggle model.detail
    | `Tab | `ASCII '\t' ->
      model.focus <- (match model.focus with
        | Sources -> Terms
        | Terms -> Timeline
        | Timeline -> Sources)
    | `ASCII 's' ->
      (match Sidebar.toggle_selected_source model.sidebar with
       | Some _sid -> refresh_search model
       | None -> ())
    | `ASCII 't' ->
      let terms = Weft_search.all_terms model.search in
      (match Sidebar.selected_term_name model.sidebar ~terms with
       | Some term_name ->
         Weft_search.toggle_term model.search term_name;
         refresh_search model
       | None -> ())
    | `ASCII 'd' ->
      let terms = Weft_search.all_terms model.search in
      (match Sidebar.selected_term_name model.sidebar ~terms with
       | Some term_name ->
         Weft_search.remove_term model.search term_name;
         refresh_search model
       | None -> ())
    (* Time range controls *)
    | `ASCII '<' | `ASCII ',' ->
      (* Shift window earlier *)
      let tr = match model.time_range with
        | Some tr -> tr
        | None ->
          let now = Ptime_clock.now () in
          { start_ = (match Ptime.sub_span now (Ptime.Span.of_int_s 3600) with
                       | Some t -> t | None -> now);
            end_ = Some now }
      in
      let half = window_seconds tr / 2 in
      model.time_range <- Some (shift_range tr (-half));
      refresh_search model
    | `ASCII '>' | `ASCII '.' ->
      (* Shift window later *)
      (match model.time_range with
       | Some tr ->
         let half = window_seconds tr / 2 in
         model.time_range <- Some (shift_range tr half);
         refresh_search model
       | None -> ())
    | `ASCII '-' | `ASCII '_' ->
      (* Narrow window *)
      (match model.time_range with
       | Some tr ->
         let narrowed = narrow_range tr in
         if window_seconds narrowed > 60 then begin
           model.time_range <- Some narrowed;
           refresh_search model
         end
       | None ->
         let now = Ptime_clock.now () in
         model.time_range <- Some {
           start_ = (match Ptime.sub_span now (Ptime.Span.of_int_s 1800) with
                      | Some t -> t | None -> now);
           end_ = Some now };
         refresh_search model)
    | `ASCII '+' | `ASCII '=' ->
      (* Widen window *)
      (match model.time_range with
       | Some tr ->
         model.time_range <- Some (widen_range tr);
         refresh_search model
       | None -> ())
    | `ASCII 'r' ->
      (* Reset to full range *)
      model.time_range <- None;
      refresh_search model
    | `ASCII 'i' ->
      (* Isolate: disable all terms except one *)
      let term_to_isolate = match model.focus with
        | Terms ->
          (* Use the selected term in the sidebar *)
          let terms = Weft_search.all_terms model.search in
          Sidebar.selected_term_name model.sidebar ~terms
        | Timeline | Sources ->
          (* Use the first matched term of the selected entry *)
          (match Timeline.selected_entry model.timeline with
           | Some entry when entry.terms <> [] -> Some (List.hd entry.terms)
           | _ -> None)
      in
      (match term_to_isolate with
       | Some term_name ->
         Weft_search.isolate_term model.search term_name;
         refresh_search model
       | None -> ())
    | `ASCII 'I' ->
      (* Restore: enable all terms *)
      Weft_search.enable_all_terms model.search;
      refresh_search model
    | `ASCII '?' ->
      model.overlay <- Help
    | `ASCII 'H' ->
      model.overlay <- (if model.overlay = Heatmap then None_ else Heatmap)
    | _ -> ()
  end

(* Render the full TUI *)
let render model =
  let w = model.width in
  let h = model.height in

  let sidebar_width = min 15 (w / 5) in
  let main_width = w - sidebar_width - 1 in

  let status_height = 1 in
  let search_height = 1 in
  let time_bar_height = 1 in
  let detail_height = if Detail.is_expanded model.detail then min 10 (h / 3) else 0 in
  let progress_height = if model.progress.tasks = [] then 0 else 1 in
  let timeline_height = max 1 (h - status_height - search_height - time_bar_height
    - detail_height - progress_height - 2) in

  let terms = Weft_search.all_terms model.search in

  (* Status bar *)
  let status_img = Status.render model.status ~width:w in

  let search_img = Search_bar.render model.search_bar ~width:w in

  (* Time range bar *)
  let time_str = format_time_range model.time_range in
  let time_bar = I.string A.(fg lightcyan)
    (Printf.sprintf " [%s]  </>:shift  -/+:zoom  r:reset  ?:help" time_str) in
  let time_bar = I.hsnap ~align:`Left w time_bar in

  let sep = Theme.hline w in

  let sidebar_focus = match model.focus with
    | Sources -> `Sources | Terms -> `Terms | Timeline -> `None in
  let sidebar_img = Sidebar.render model.sidebar
    ~terms ~width:sidebar_width ~height:timeline_height ~focus:sidebar_focus in

  let vsep = Theme.vline timeline_height in

  let timeline_img = Timeline.render model.timeline
    ~width:main_width ~height:timeline_height ~term_list:terms in

  let main_row = I.hcat [sidebar_img; vsep; timeline_img] in

  let progress_img = Progress.render model.progress ~width:w in

  let detail_sep = if detail_height > 0 then Theme.hline w else I.empty in
  let detail_img = Detail.render model.detail
    ~entry:(Timeline.selected_entry model.timeline)
    ~width:w ~height:detail_height in

  let base = I.vcat ([
    status_img;
    search_img;
    time_bar;
    sep;
    main_row;
  ] @ (if progress_height > 0 then [progress_img] else [])
    @ (if detail_height > 0 then [detail_sep; detail_img] else []))
  in
  (* Render overlay on top if active *)
  match model.overlay with
  | None_ -> base
  | Help ->
    (* Help overlays the main content *)
    let help_img = Help.render ~width:w ~height:h in
    I.(help_img </> base)
  | Heatmap ->
    let sources = model.sidebar.sources in
    let heatmap_img = Heatmap.render
      ~entries:model.timeline.entries ~sources
      ~width:w ~height:h in
    heatmap_img

let run_ui model term =
  let img = ref (render model) in
  let rec loop () =
    Notty_unix.Term.image term !img;
    match Notty_unix.Term.event term with
    | `End | `Key (`ASCII 'C', [`Ctrl]) ->
      model.quit <- true
    | `Key (key, _mods) ->
      handle_key model key;
      let (w, h) = Notty_unix.Term.size term in
      model.width <- w;
      model.height <- h;
      img := render model;
      if not model.quit then loop ()
    | `Resize (w, h) ->
      model.width <- w;
      model.height <- h;
      img := render model;
      if not model.quit then loop ()
    | `Mouse _ | `Paste _ ->
      if not model.quit then loop ()
  in
  let (w, h) = Notty_unix.Term.size term in
  model.width <- w;
  model.height <- h;
  img := render model;
  loop ()

module Theme = Theme
module Search_bar = Search_bar
module Timeline = Timeline
module Sidebar = Sidebar
module Detail = Detail
module Progress = Progress
module Help = Help
module Heatmap = Heatmap
module Status = Status
