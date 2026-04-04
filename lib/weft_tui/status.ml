open Notty

(* Global status message — last operation shown in the status bar *)

type t = {
  mutable message : string;
  mutable updated_at : float;
}

let create () = { message = ""; updated_at = 0.0 }

let set t msg =
  t.message <- msg;
  t.updated_at <- Unix.gettimeofday ()

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

(* Global instance — modules can set status without passing the ref around *)
let global = create ()

let set_global msg = set global msg
