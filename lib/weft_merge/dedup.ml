open Weft_types

(* Bounded hash set for deduplication.
   Key = (timestamp, source_id, hash(raw)) *)

type entry_key = {
  ts : Ptime.t;
  source : source_id;
  raw_hash : string;
}

type t = {
  seen : (string, unit) Hashtbl.t;  (* serialized key -> unit *)
  max_size : int;
  mutable insertion_order : string list; (* for bounded eviction *)
}

let create ?(max_size=10000) () =
  { seen = Hashtbl.create (max_size / 2);
    max_size;
    insertion_order = [] }

let key_of_entry (entry : log_entry) =
  let raw_hash = Digestif.SHA256.(digest_string entry.raw |> to_hex) in
  Printf.sprintf "%s|%s|%s"
    (Ptime.to_rfc3339 entry.timestamp)
    entry.source
    raw_hash

let is_duplicate t entry =
  let key = key_of_entry entry in
  Hashtbl.mem t.seen key

let mark_seen t entry =
  let key = key_of_entry entry in
  if not (Hashtbl.mem t.seen key) then begin
    (* Evict oldest if at capacity *)
    if Hashtbl.length t.seen >= t.max_size then begin
      match List.rev t.insertion_order with
      | [] -> ()
      | oldest :: rest ->
        Hashtbl.remove t.seen oldest;
        t.insertion_order <- List.rev rest
    end;
    Hashtbl.replace t.seen key ();
    t.insertion_order <- key :: t.insertion_order
  end

let check_and_mark t entry =
  if is_duplicate t entry then true
  else begin
    mark_seen t entry;
    false
  end
