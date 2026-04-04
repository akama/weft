open Notty

type t = {
  mutable input : string;
  mutable cursor_pos : int;
  mutable active : bool;
}

let create () =
  { input = ""; cursor_pos = 0; active = false }

let activate t =
  t.active <- true;
  t.input <- "";
  t.cursor_pos <- 0

let deactivate t =
  t.active <- false

let is_active t = t.active

let current_input t = t.input

let handle_char t c =
  let before = String.sub t.input 0 t.cursor_pos in
  let after = String.sub t.input t.cursor_pos (String.length t.input - t.cursor_pos) in
  t.input <- before ^ String.make 1 c ^ after;
  t.cursor_pos <- t.cursor_pos + 1

let handle_backspace t =
  if t.cursor_pos > 0 then begin
    let before = String.sub t.input 0 (t.cursor_pos - 1) in
    let after = String.sub t.input t.cursor_pos (String.length t.input - t.cursor_pos) in
    t.input <- before ^ after;
    t.cursor_pos <- t.cursor_pos - 1
  end

let handle_left t =
  if t.cursor_pos > 0 then
    t.cursor_pos <- t.cursor_pos - 1

let handle_right t =
  if t.cursor_pos < String.length t.input then
    t.cursor_pos <- t.cursor_pos + 1

let submit t =
  let term = String.trim t.input in
  t.input <- "";
  t.cursor_pos <- 0;
  t.active <- false;
  if String.length term > 0 then Some term else None

let render t ~width =
  let prefix = "/ search: " in
  let prefix_len = String.length prefix in
  let available = width - prefix_len - 6 in (* leave room for [+add] *)
  let display_input = if String.length t.input > available then
    String.sub t.input (String.length t.input - available) available
  else t.input in
  let suffix = String.make (max 0 (available - String.length display_input)) ' ' in
  let input_img = if t.active then
    I.string Theme.search_bar_attr (prefix ^ display_input ^ suffix ^ " [+add]")
  else
    I.string Theme.dim_attr (prefix ^ display_input ^ suffix ^ " [+add]")
  in
  I.hsnap ~align:`Left width input_img
