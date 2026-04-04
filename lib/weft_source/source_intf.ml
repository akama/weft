open Weft_types

module type Source = sig
  type t

  val connect : source_config -> proc:_ Eio.Process.mgr ->
    fs:Eio.Fs.dir_ty Eio.Path.t -> (t, string) result

  val health_check : t -> (unit, string) result

  val fetch : t -> path:string -> dst:string -> (unit, string) result

  val fetch_archive : t -> path:string -> dst:string -> (unit, string) result

  val discover_archives : t -> path:string -> archive_info list

  val search : t -> terms:string list -> time_range:time_range option ->
    log_entry Seq.t

  val tail : t -> terms:string list ->
    (log_entry -> unit) -> cancel:bool Atomic.t -> unit

  val close : t -> unit
end

(* Unified handle that wraps any source adapter *)
type source_handle = {
  name : source_id;
  config : source_config;
  format_config : format_config option;
  pipeline : Weft_middleware.Pipeline.t option;
  search : terms:string list -> time_range:time_range option -> log_entry Seq.t;
  fetch : dst:string -> (unit, string) result;
  discover_archives : unit -> archive_info list;
  tail_start : terms:string list -> (log_entry -> unit) -> cancel:bool Atomic.t -> unit;
  close : unit -> unit;
}
