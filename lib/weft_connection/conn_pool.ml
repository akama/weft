open Weft_types

type source_conn = {
  config : source_config;
  ssh : Ssh_control.t option;
  mutable status : source_status;
  mutable filter_terms : string list;
  lock : Eio.Mutex.t;
}

type t = {
  connections : (source_id, source_conn) Hashtbl.t;
  ssh_semaphore : Eio.Semaphore.t;
  loki_semaphore : Eio.Semaphore.t;
}

let create ~(limits : limits_config) =
  {
    connections = Hashtbl.create 16;
    ssh_semaphore = Eio.Semaphore.make limits.max_ssh_connections;
    loki_semaphore = Eio.Semaphore.make limits.max_loki_concurrent;
  }

let add_source t (config : source_config) =
  let ssh = match config.source_type with
    | Remote | Journald ->
      (match config.transport with
       | Some cmd -> Some (Ssh_control.create ~transport_cmd:cmd)
       | None -> None)
    | _ -> None
  in
  let conn = {
    config;
    ssh;
    status = Disconnected;
    filter_terms = [];
    lock = Eio.Mutex.create ();
  } in
  Hashtbl.replace t.connections config.name conn

let get_connection t source_name =
  Hashtbl.find_opt t.connections source_name

let connect_source (proc : _ Eio.Process.mgr) t source_name =
  match get_connection t source_name with
  | None -> Error (Printf.sprintf "Unknown source: %s" source_name)
  | Some conn ->
    Eio.Mutex.use_rw ~protect:true conn.lock (fun () ->
      match conn.ssh with
      | Some ssh ->
        (try
           Ssh_control.establish proc ssh;
           conn.status <- Connected;
           Ok ()
         with exn ->
           conn.status <- Failed (Printexc.to_string exn);
           Error (Printexc.to_string exn))
      | None ->
        conn.status <- Connected;
        Ok ()
    )

let update_terms t source_name terms =
  match get_connection t source_name with
  | None -> ()
  | Some conn ->
    Eio.Mutex.use_rw ~protect:true conn.lock (fun () ->
      conn.filter_terms <- terms)

let get_status t source_name =
  match get_connection t source_name with
  | None -> Disconnected
  | Some conn -> conn.status

let set_status t source_name status =
  match get_connection t source_name with
  | None -> ()
  | Some conn -> conn.status <- status

let all_sources t =
  Hashtbl.fold (fun name conn acc -> (name, conn) :: acc) t.connections []

let close_all t =
  Hashtbl.iter (fun _name conn ->
    match conn.ssh with
    | Some ssh -> Ssh_control.close ssh
    | None -> ()
  ) t.connections

let with_ssh_semaphore t f =
  Eio.Semaphore.acquire t.ssh_semaphore;
  Fun.protect f ~finally:(fun () -> Eio.Semaphore.release t.ssh_semaphore)

let with_loki_semaphore t f =
  Eio.Semaphore.acquire t.loki_semaphore;
  Fun.protect f ~finally:(fun () -> Eio.Semaphore.release t.loki_semaphore)
