open Weft_types

(* Reorder buffer for tail mode.
   Holds entries for up to reorder_window before emitting. *)

type t = {
  buffer : Heap.t;
  reorder_window : Ptime.Span.t;
  dedup : Dedup.t;
}

let create ~reorder_window_ms =
  let span = Ptime.Span.of_int_s (reorder_window_ms / 1000) in
  {
    buffer = Heap.create ();
    reorder_window = span;
    dedup = Dedup.create ();
  }

let add t entry =
  if not (Dedup.check_and_mark t.dedup entry) then
    Heap.push t.buffer entry

(* Flush entries older than (now - reorder_window) *)
let flush t =
  let now = Ptime_clock.now () in
  let cutoff = match Ptime.sub_span now t.reorder_window with
    | Some t -> t
    | None -> Ptime.epoch
  in
  let entries = ref [] in
  let continue = ref true in
  while !continue do
    match Heap.peek t.buffer with
    | None -> continue := false
    | Some entry ->
      if Ptime.is_earlier entry.timestamp ~than:cutoff then begin
        ignore (Heap.pop t.buffer);
        entries := entry :: !entries
      end else
        continue := false
  done;
  List.rev !entries

(* Flush all remaining entries (e.g. on shutdown) *)
let flush_all t =
  let entries = ref [] in
  let continue = ref true in
  while !continue do
    match Heap.pop t.buffer with
    | None -> continue := false
    | Some entry -> entries := entry :: !entries
  done;
  List.rev !entries

let pending_count t = Heap.length t.buffer
