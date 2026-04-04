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
  if t.message = "" then I.empty
  else
    let age = Unix.gettimeofday () -. t.updated_at in
    (* Fade after 10 seconds *)
    let attr = if age < 5.0 then A.(fg lightyellow)
      else if age < 10.0 then A.(fg lightblack)
      else begin
        t.message <- "";
        A.empty
      end
    in
    if t.message = "" then I.empty
    else
      let msg = if String.length t.message > width - 2 then
        String.sub t.message 0 (width - 2)
      else t.message in
      I.string attr (" " ^ msg) |> I.hsnap ~align:`Left width

(* Global instance — modules can set status without passing the ref around *)
let global = create ()

let set_global msg = set global msg
