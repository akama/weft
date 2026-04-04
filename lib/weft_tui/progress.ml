open Notty

type task = {
  label : string;
  mutable progress : float; (* 0.0 to 1.0 *)
  mutable done_ : bool;
}

type t = {
  mutable tasks : task list;
}

let create () = { tasks = [] }

let add_task t ~label =
  let task = { label; progress = 0.0; done_ = false } in
  t.tasks <- t.tasks @ [task];
  task

let update_progress task progress =
  task.progress <- progress

let complete_task task =
  task.done_ <- true

let remove_completed t =
  t.tasks <- List.filter (fun task -> not task.done_) t.tasks

let render_task task ~width =
  let bar_width = max 0 (width - String.length task.label - 8) in
  let filled = int_of_float (task.progress *. float_of_int bar_width) in
  let empty = bar_width - filled in
  let pct = int_of_float (task.progress *. 100.0) in
  let bar = String.make filled '#' ^ String.make empty '-' in
  I.hcat [
    I.string A.empty ("── " ^ task.label ^ " ");
    I.string Theme.progress_bar_attr bar;
    I.string A.empty (Printf.sprintf " %d%% ──" pct);
  ]

let render t ~width =
  let active = List.filter (fun task -> not task.done_) t.tasks in
  if active = [] then I.empty
  else
    I.vcat (List.map (fun task -> render_task task ~width) active)
