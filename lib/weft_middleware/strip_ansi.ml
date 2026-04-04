let ansi_re = Re.compile (Re.Pcre.re {|\x1b\[[0-9;]*[a-zA-Z]|})

let apply s =
  Re.replace_string ansi_re ~by:"" s
