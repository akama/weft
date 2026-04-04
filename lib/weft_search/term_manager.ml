open Weft_types

type t = {
  mutable terms : search_term list;
  mutable next_color : int;
}

let color_count = 8

let create () =
  { terms = []; next_color = 0 }

let add_term t term_str =
  if List.exists (fun (st : search_term) -> st.term = term_str) t.terms then
    None
  else begin
    let st = { term = term_str; color_idx = t.next_color; enabled = true } in
    t.next_color <- (t.next_color + 1) mod color_count;
    t.terms <- t.terms @ [st];
    Some st
  end

let remove_term t term_str =
  t.terms <- List.filter (fun (st : search_term) -> st.term <> term_str) t.terms

let toggle_term t term_str =
  t.terms <- List.map (fun (st : search_term) ->
    if st.term = term_str then { st with enabled = not st.enabled }
    else st
  ) t.terms

let enabled_terms t =
  List.filter_map (fun (st : search_term) ->
    if st.enabled then Some st.term else None
  ) t.terms

let all_terms t = t.terms

let find_term t term_str =
  List.find_opt (fun (st : search_term) -> st.term = term_str) t.terms

let isolate_term t term_str =
  t.terms <- List.map (fun (st : search_term) ->
    { st with enabled = (st.term = term_str) }
  ) t.terms

let enable_all_terms t =
  t.terms <- List.map (fun (st : search_term) ->
    { st with enabled = true }
  ) t.terms
