open Weft_types

(* Rotation detection *)

type rotation_event =
  | File_renamed
  | File_truncated

(* Parse tail -F stderr for rotation signals.
   Different tail implementations use different messages:
   - GNU coreutils: "has been renamed", "file truncated"
   - BusyBox/NixOS: "has become inaccessible", "has appeared" *)
let tail_renamed_re = Re.compile (Re.Pcre.re
  {|has been renamed|has become inaccessible|})
let tail_new_file_re = Re.compile (Re.Pcre.re
  {|has appeared|following new file|})
let tail_truncated_re = Re.compile (Re.Pcre.re
  {|file truncated|})

let detect_from_tail_stderr line =
  if Re.execp tail_renamed_re line then Some File_renamed
  else if Re.execp tail_new_file_re line then Some File_renamed
  else if Re.execp tail_truncated_re line then Some File_truncated
  else None

(* Rotation handler callback type *)
type rotation_handler = {
  on_rotation : rotation_event -> unit;
}

let create_handler ~(config : rotation_config) ~on_seal_segment ~on_new_segment =
  let _ = config in
  {
    on_rotation = (fun event ->
      match event with
      | File_renamed | File_truncated ->
        on_seal_segment ();
        on_new_segment ()
    );
  }

(* For local files: detect rotation by checking inode/size changes *)
let check_local_rotation ~path ~last_inode ~last_size =
  try
    let stat = Unix.stat path in
    let current_inode = stat.Unix.st_ino in
    let current_size = Int64.of_int stat.Unix.st_size in
    if current_inode <> last_inode then
      Some (File_renamed, current_inode, current_size)
    else if current_size < last_size then
      Some (File_truncated, current_inode, current_size)
    else
      None
  with Unix.Unix_error _ -> None
