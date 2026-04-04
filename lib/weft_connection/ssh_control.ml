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

(* Run a long-lived SSH command, calling on_line for each line of output.
   Uses Unix.open_process_in for streaming reads. Blocks until process
   exits or cancel is set. *)
let run_streaming t args ~on_line ~cancel =
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
  let ic = Unix.open_process_in cmd_str in
  Fun.protect (fun () ->
    try
      while not (Atomic.get cancel) do
        let line = input_line ic in
        on_line line
      done
    with End_of_file -> ()
  ) ~finally:(fun () ->
    ignore (Unix.close_process_in ic))

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
