(* File watcher using polling — portable fallback for macOS and other
   platforms without inotify. Checks file size periodically. *)

let tail ~path ~read_new_lines ~emit_line ~cancel
    ~on_rotation ~drain_timeout:_ ~wait_readable:_ ~sleep () =
  let ic = ref (open_in path) in
  seek_in !ic (in_channel_length !ic);
  let last_inode = ref (Unix.stat path).Unix.st_ino in
  let last_size = ref (Int64.of_int (in_channel_length !ic)) in
  let poll_interval_ms = Weft_constants.default_poll_interval_ms in
  Fun.protect (fun () ->
    while not (Atomic.get cancel) do
      (match Rotation.check_local_rotation ~path
               ~last_inode:!last_inode ~last_size:!last_size with
       | Some (event, new_inode, new_size) ->
         let lines = read_new_lines !ic in
         List.iter emit_line lines;
         (match on_rotation, event with
          | Some (on_seal, _), _ -> on_seal ()
          | None, _ -> ());
         close_in_noerr !ic;
         last_inode := new_inode;
         last_size := new_size;
         sleep Weft_constants.rotation_reopen_delay;
         if Sys.file_exists path then begin
           (match on_rotation with
            | Some (_, on_new) -> on_new () | None -> ());
           ic := open_in path
         end
       | None ->
         let lines = read_new_lines !ic in
         List.iter emit_line lines;
         last_size := Int64.of_int (pos_in !ic));
      sleep (float_of_int poll_interval_ms /. 1000.0)
    done
  ) ~finally:(fun () -> close_in_noerr !ic)
