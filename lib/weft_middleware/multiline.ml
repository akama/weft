type t = {
  continuation_re : Re.re;
  max_lines : int;
}

let create ~continuation ~max_lines =
  { continuation_re = Re.compile (Re.Pcre.re continuation); max_lines }

(* Join lines into multiline blocks.
   A line that matches the continuation regex is appended to the previous block.
   Returns a list of joined blocks (each block is the concatenated lines). *)
let join_lines t (lines : string list) : string list =
  let flush_block acc block =
    match block with
    | [] -> acc
    | _ -> (String.concat "\n" (List.rev block)) :: acc
  in
  let rec process acc current_block line_count = function
    | [] -> List.rev (flush_block acc current_block)
    | line :: rest ->
      if Re.execp t.continuation_re line && current_block <> [] then begin
        if line_count < t.max_lines then
          process acc (line :: current_block) (line_count + 1) rest
        else
          (* Max lines reached — start new block *)
          let acc = flush_block acc current_block in
          process acc [line] 1 rest
      end else begin
        let acc = flush_block acc current_block in
        process acc [line] 1 rest
      end
  in
  process [] [] 0 lines

(* Streaming version: feed lines one at a time, get back completed blocks *)
type state = {
  mutable current_block : string list;
  mutable line_count : int;
  config : t;
}

let create_state config =
  { current_block = []; line_count = 0; config }

let feed_line state line =
  if Re.execp state.config.continuation_re line && state.current_block <> [] then begin
    if state.line_count < state.config.max_lines then begin
      state.current_block <- line :: state.current_block;
      state.line_count <- state.line_count + 1;
      None
    end else begin
      let block = String.concat "\n" (List.rev state.current_block) in
      state.current_block <- [line];
      state.line_count <- 1;
      Some block
    end
  end else begin
    let result = match state.current_block with
      | [] -> None
      | _ -> Some (String.concat "\n" (List.rev state.current_block))
    in
    state.current_block <- [line];
    state.line_count <- 1;
    result
  end

let flush state =
  match state.current_block with
  | [] -> None
  | _ ->
    let block = String.concat "\n" (List.rev state.current_block) in
    state.current_block <- [];
    state.line_count <- 0;
    Some block
