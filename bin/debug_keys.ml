(* Minimal key debug tool — prints Notty events to see what keys produce *)
let () =
  let term = Notty_unix.Term.create () in
  let running = ref true in
  let lines = ref ["Press keys to see events. Press 'Q' to quit."] in
  let add_line s = lines := s :: !lines; if List.length !lines > 20 then
    lines := List.filteri (fun i _ -> i < 20) !lines in
  Fun.protect (fun () ->
    while !running do
      let img = Notty.(I.vcat (List.rev_map (fun s ->
        I.string A.empty s) !lines)) in
      Notty_unix.Term.image term img;
      let (input_fd, _) = Notty_unix.Term.fds term in
      let ready, _, _ = Unix.select [input_fd] [] [] 1.0 in
      if ready <> [] then begin
        match Notty_unix.Term.event term with
        | `End -> running := false
        | `Key (`ASCII 'Q', _) -> running := false
        | `Key (key, mods) ->
          let key_str = match key with
            | `ASCII c -> Printf.sprintf "ASCII '%c' (0x%02x)" c (Char.code c)
            | `Tab -> "Tab"
            | `Enter -> "Enter"
            | `Escape -> "Escape"
            | `Backspace -> "Backspace"
            | `Arrow `Up -> "Arrow Up"
            | `Arrow `Down -> "Arrow Down"
            | `Arrow `Left -> "Arrow Left"
            | `Arrow `Right -> "Arrow Right"
            | `Delete -> "Delete"
            | `Home -> "Home"
            | `End -> "End"
            | `Page `Up -> "Page Up"
            | `Page `Down -> "Page Down"
            | `Insert -> "Insert"
            | `Function n -> Printf.sprintf "F%d" n
            | `Uchar u -> Printf.sprintf "Uchar U+%04X" (Uchar.to_int u)
          in
          let mods_str = match mods with
            | [] -> ""
            | ms -> " [" ^ String.concat "," (List.map (function
                | `Ctrl -> "Ctrl" | `Meta -> "Meta" | `Shift -> "Shift"
              ) ms) ^ "]"
          in
          add_line (Printf.sprintf "Key: %s%s" key_str mods_str)
        | `Resize (w, h) ->
          add_line (Printf.sprintf "Resize: %dx%d" w h)
        | `Mouse _ -> add_line "Mouse event"
        | `Paste _ -> add_line "Paste event"
      end
    done
  ) ~finally:(fun () ->
    Notty_unix.Term.release term)
