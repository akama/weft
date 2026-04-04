You are autonomous and should make choices to the best of your ability. You are in a sandbox so install software as needed.

Write code according to best practices and commit code frequently by logical change.

We use jj for code management in this repo.
When writing code, check what change you are in, make a new change if needed.
To finish a commit, check `jj diff` and look for any issues, fix if needed, repeat until diff is good, than write a change message using `jj describe` and finish with `jj new`.

Write good tests, run the tests before checking code in to verify it.

Write helper tools if needed to call later.

Please write system docs to docs/systems/
Please write logs of your progress in docs/logs/

Please use CLAUDE.md to store relevent facts.

## Project Facts

- **Name**: weft — unified log search TUI
- **Language**: OCaml 5.2.1 (opam switch: `weft`)
- **Build**: `dune build` / `dune runtest`
- **TUI**: Nottui/Notty (not Minttea — Riot incompatible with OCaml 5.2+)
- **Concurrency**: Eio (structured concurrency, fiber-per-source)
- **Config**: TOML via otoml (formats.toml + sources.toml)
- **Tests**: Alcotest, 24 tests in test/ directory
- **Key libs**: eio, notty, otoml, ptime, re, yojson, cohttp-eio, digestif, camlzip
- **Re.Pcre note**: Named groups use `(?<name>...)` syntax, NOT `(?P<name>...)`
