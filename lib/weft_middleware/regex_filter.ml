type t = {
  include_re : Re.re option;
  exclude_re : Re.re option;
}

let create ~include_ ~exclude =
  let compile_opt = function
    | None -> None
    | Some pat -> Some (Re.compile (Re.Pcre.re pat))
  in
  { include_re = compile_opt include_; exclude_re = compile_opt exclude }

(* Returns true if the line should be kept *)
let should_keep t s =
  let dominated_by_exclude =
    match t.exclude_re with
    | None -> false
    | Some re -> Re.execp re s
  in
  if dominated_by_exclude then false
  else
    match t.include_re with
    | None -> true
    | Some re -> Re.execp re s
