(* File watcher using Linux inotify — efficient, event-driven.
   Selected at build time when the inotify library is available. *)

let tail ~path ~read_new_lines ~emit_line ~cancel
    ~on_rotation ~drain_timeout ~wait_readable ~sleep () =
  let inotify_fd = Inotify.create () in
  let ic = ref (open_in path) in
  seek_in !ic (in_channel_length !ic);
  Fun.protect (fun () ->
    let _watch = Inotify.add_watch inotify_fd path
      [Inotify.S_Modify; Inotify.S_Move_self; Inotify.S_Delete_self] in

    let reopen () =
      close_in_noerr !ic;
      sleep Weft_constants.rotation_reopen_delay;
      if Sys.file_exists path then begin
        ic := open_in path;
        ignore (Inotify.add_watch inotify_fd path
          [Inotify.S_Modify; Inotify.S_Move_self; Inotify.S_Delete_self])
      end
    in

    while not (Atomic.get cancel) do
      if wait_readable inotify_fd 0.5 then begin
        let events = Inotify.read inotify_fd in
        List.iter (fun (_wd, kinds, _cookie, _name) ->
          if List.mem Inotify.Modify kinds then begin
            let lines = read_new_lines !ic in
            List.iter emit_line lines
          end;
          if List.mem Inotify.Move_self kinds then begin
            Printf.eprintf "Rotation detected (rename) for %s\n%!" path;
            let drain_start = Unix.gettimeofday () in
            let drained = ref true in
            while !drained &&
                  Unix.gettimeofday () -. drain_start < drain_timeout do
              let lines = read_new_lines !ic in
              if lines = [] then drained := false
              else List.iter emit_line lines;
              if !drained then sleep Weft_constants.rotation_reopen_delay
            done;
            (match on_rotation with
             | Some (on_seal, on_new) -> on_seal (); on_new ()
             | None -> ());
            reopen ()
          end;
          if List.mem Inotify.Delete_self kinds then begin
            Printf.eprintf "File deleted: %s\n%!" path;
            (match on_rotation with
             | Some (on_seal, _on_new) -> on_seal ()
             | None -> ());
            reopen ()
          end
        ) events
      end
    done
  ) ~finally:(fun () ->
    close_in_noerr !ic;
    Unix.close inotify_fd
  )
