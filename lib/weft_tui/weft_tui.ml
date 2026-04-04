open Notty

type focus = Sources | Terms | Timeline

type model = {
  search_bar : Search_bar.t;
  timeline : Timeline.t;
  sidebar : Sidebar.t;
  detail : Detail.t;
  progress : Progress.t;
  search : Weft_search.t;
  mutable focus : focus;
  mutable quit : bool;
  mutable width : int;
  mutable height : int;
}

let create ~search =
  {
    search_bar = Search_bar.create ();
    timeline = Timeline.create ();
    sidebar = Sidebar.create ();
    detail = Detail.create ();
    progress = Progress.create ();
    search;
    focus = Timeline;
    quit = false;
    width = 80;
    height = 24;
  }

(* Handle keyboard input *)
let handle_key model key =
  if Search_bar.is_active model.search_bar then begin
    match key with
    | `Escape ->
      Search_bar.deactivate model.search_bar
    | `Enter ->
      (match Search_bar.submit model.search_bar with
       | Some term ->
         ignore (Weft_search.add_term model.search term)
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
      if model.focus = Timeline then
        Timeline.scroll_down model.timeline
    | `ASCII 'k' | `Arrow `Up ->
      if model.focus = Timeline then
        Timeline.scroll_up model.timeline
    | `Enter ->
      Detail.toggle model.detail
    | `ASCII '\t' ->
      model.focus <- (match model.focus with
        | Sources -> Terms
        | Terms -> Timeline
        | Timeline -> Sources)
    | `ASCII 's' ->
      () (* Toggle source — would need source selection *)
    | `ASCII 't' ->
      () (* Toggle term — would need term selection *)
    | `ASCII 'd' ->
      (* Delete selected term — simplified *)
      ()
    | _ -> ()
  end

(* Render the full TUI *)
let render model =
  let w = model.width in
  let h = model.height in

  (* Layout calculations *)
  let sidebar_width = min 15 (w / 5) in
  let main_width = w - sidebar_width - 1 in (* -1 for separator *)

  let search_height = 1 in
  let detail_height = if Detail.is_expanded model.detail then min 10 (h / 3) else 0 in
  let progress_height = if model.progress.tasks = [] then 0 else 1 in
  let timeline_height = h - search_height - detail_height - progress_height - 2 in

  let terms = Weft_search.all_terms model.search in

  (* Search bar *)
  let search_img = Search_bar.render model.search_bar ~width:w in

  (* Separator *)
  let sep = Theme.hline w in

  (* Sidebar *)
  let sidebar_img = Sidebar.render model.sidebar
    ~terms ~width:sidebar_width ~height:timeline_height in

  (* Vertical separator *)
  let vsep = Theme.vline timeline_height in

  (* Timeline *)
  let timeline_img = Timeline.render model.timeline
    ~width:main_width ~height:timeline_height ~term_list:terms in

  (* Main area: sidebar | vsep | timeline *)
  let main_row = I.hcat [sidebar_img; vsep; timeline_img] in

  (* Progress bar (if any active tasks) *)
  let progress_img = Progress.render model.progress ~width:w in

  (* Detail pane *)
  let detail_sep = if detail_height > 0 then Theme.hline w else I.empty in
  let detail_img = Detail.render model.detail
    ~entry:(Timeline.selected_entry model.timeline)
    ~width:w ~height:detail_height in

  (* Compose everything *)
  I.vcat ([
    search_img;
    sep;
    main_row;
  ] @ (if progress_height > 0 then [progress_img] else [])
    @ (if detail_height > 0 then [detail_sep; detail_img] else []))

(* Nottui integration: create an Lwd-reactive UI *)
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
