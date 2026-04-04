(* SSH ControlMaster lifecycle management *)

type run_cmd = string list -> string

type t = {
  transport_cmd : string;
  control_path : string;
  mutable active : bool;
  mutable run : run_cmd option;
}

let socket_dir =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat home ".cache/weft/sockets"

let create ~transport_cmd =
  let safe_name =
    String.map (fun c ->
      if Char.code c > 127 || c = '/' || c = ' ' then '_' else c
    ) transport_cmd
  in
  let control_path = Filename.concat socket_dir safe_name in
  { transport_cmd; control_path; active = false; run = None }

let parse_transport cmd =
  let parts = String.split_on_char ' ' cmd in
  match parts with
  | [] -> ("ssh", [])
  | base :: rest -> (base, rest)

let make_runner (proc : _ Eio.Process.mgr) : run_cmd =
  fun args ->
    Eio.Process.parse_out proc Eio.Buf_read.take_all args

let establish (proc : _ Eio.Process.mgr) t =
  let run = make_runner proc in
  t.run <- Some run;
  (* Ensure socket directory exists *)
  (try
     ignore (run ["mkdir"; "-p"; "-m"; "0700"; socket_dir])
   with Eio.Io _ as e ->
     Printf.eprintf "Warning: could not create socket dir: %s\n"
       (Printexc.to_string e));
  (* Ensure restrictive permissions on control socket dir *)
  (try Unix.chmod socket_dir 0o700
   with Unix.Unix_error _ -> ());
  let (base, args) = parse_transport t.transport_cmd in
  let is_tsh = base = "tsh" in
  if is_tsh then begin
    (try ignore (run ["tsh"; "status"])
     with Eio.Io _ ->
       failwith "Teleport session not active. Run 'tsh login' first.")
  end;
  let ssh_cmd = if is_tsh then begin
    t.active <- true;
    ignore args;
    []
  end else
    ["ssh";
     "-o"; "ControlMaster=auto";
     "-o"; Printf.sprintf "ControlPath=%s" t.control_path;
     "-o"; "ControlPersist=600";
     "-o"; "BatchMode=yes";
     "-N"; "-f"]
    @ args
  in
  if ssh_cmd <> [] then begin
    (try
       ignore (run ssh_cmd);
       t.active <- true
     with exn ->
       failwith (Printf.sprintf "Failed to establish SSH control: %s"
                   (Printexc.to_string exn)))
  end

let run_command t args =
  let (base, host_args) = parse_transport t.transport_cmd in
  let is_tsh = base = "tsh" in
  let cmd = if is_tsh then
    [base] @ host_args @ args
  else
    ["ssh";
     "-o"; Printf.sprintf "ControlPath=%s" t.control_path;
     "-o"; "ControlMaster=auto"]
    @ host_args @ args
  in
  match t.run with
  | Some run -> run cmd
  | None -> failwith "SSH control not established"

let run_command_lines t args =
  let output = run_command t args in
  String.split_on_char '\n' output
  |> List.filter (fun s -> String.length s > 0)

(* Run a long-lived SSH command, calling on_line for stdout and on_stderr
   for stderr. Uses Unix.open_process_full for separate channels.
   Multiplexes both with Unix.select. *)
let default_wait_fds fds timeout =
  let (ready, _, _) = Unix.select fds [] [] timeout in
  ready

let run_streaming t args ~on_line ~on_stderr ~cancel
    ?(wait_fds = default_wait_fds) () =
  let (base, host_args) = parse_transport t.transport_cmd in
  let is_tsh = base = "tsh" in
  let cmd_parts = if is_tsh then
    [base] @ host_args @ args
  else
    ["ssh";
     "-o"; Printf.sprintf "ControlPath=%s" t.control_path;
     "-o"; "ControlMaster=auto"]
    @ host_args @ args
  in
  let cmd_str = String.concat " " (List.map (fun s ->
    if String.contains s ' ' then "'" ^ s ^ "'" else s
  ) cmd_parts) in
  let (stdout_ic, _stdin_oc, stderr_ic) =
    Unix.open_process_full cmd_str (Unix.environment ()) in
  let stdout_fd = Unix.descr_of_in_channel stdout_ic in
  let stderr_fd = Unix.descr_of_in_channel stderr_ic in
  (* Set non-blocking so we can multiplex with select *)
  Unix.set_nonblock stdout_fd;
  Unix.set_nonblock stderr_fd;
  let stdout_buf = Buffer.create 4096 in
  let stderr_buf = Buffer.create 256 in
  let read_lines_from fd buf callback =
    let tmp = Bytes.create 4096 in
    (try
       let n = Unix.read fd tmp 0 4096 in
       if n = 0 then raise End_of_file;
       Buffer.add_subbytes buf tmp 0 n;
       (* Extract complete lines *)
       let content = Buffer.contents buf in
       let rec extract_lines start =
         match String.index_from_opt content start '\n' with
         | None ->
           (* Keep partial line in buffer *)
           Buffer.clear buf;
           if start < String.length content then
             Buffer.add_string buf (String.sub content start
               (String.length content - start))
         | Some nl_pos ->
           let line = String.sub content start (nl_pos - start) in
           callback line;
           extract_lines (nl_pos + 1)
       in
       extract_lines 0
     with
     | Unix.Unix_error (Unix.EAGAIN, _, _)
     | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> ()
     | End_of_file -> raise End_of_file)
  in
  Fun.protect (fun () ->
    try
      while not (Atomic.get cancel) do
        let ready = wait_fds [stdout_fd; stderr_fd] 0.5 in
        List.iter (fun fd ->
          if fd = stdout_fd then
            read_lines_from stdout_fd stdout_buf on_line
          else if fd = stderr_fd then
            read_lines_from stderr_fd stderr_buf on_stderr
        ) ready
      done
    with End_of_file -> ()
  ) ~finally:(fun () ->
    ignore (Unix.close_process_full (stdout_ic, _stdin_oc, stderr_ic)))

let close t =
  if t.active then begin
    let (base, _) = parse_transport t.transport_cmd in
    if base <> "tsh" then begin
      (try
         ignore (run_command t ["-O"; "exit"])
       with Eio.Io _ | Failure _ ->
         (* Best-effort cleanup — socket may already be gone *)
         ())
    end;
    t.active <- false
  end

let is_active t = t.active
