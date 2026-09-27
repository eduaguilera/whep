# WHEP agent reference — build and verification gotchas

Read before debugging an env var or profile that "should" be set, or before trusting a green `devtools::test()` as a green CI.

Moved out of `AGENTS.md` on 2026-09-27 to keep that file under Codex's 32 KiB
default instruction-file read cap (`project_doc_max_bytes`). `AGENTS.md`
carries the pointer; this file carries the detail.

---

Gotchas worth knowing before losing an hour:

- `WHEP_*` paths belong in `~/.Renviron` and are read from there. Do **not**
  add a `.Renviron` at the repo root: R reads a working-directory `.Renviron`
  *instead of* `~/.Renviron`, never both, so one here silently hides every
  `WHEP_*` path an R session started at the root would otherwise see. That was
  #456, fixed by moving `_R_CHECK_SYSTEM_CLOCK_` out of a tracked `.Renviron`
  into the R-CMD-check workflow env and `.Rprofile`.
- `.Rprofile` has the same shape and the same trap: R reads the
  working-directory one *instead of* `~/.Rprofile`. The repo profile therefore
  **chain-sources the user profile first**, and must keep doing so. That line
  is not cosmetic: `r-lib/actions/setup-r` delivers `use-public-rspm: true` by
  writing the RSPM `repos` option into `~/.Rprofile`, so while it was shadowed
  every cold dependency install on ubuntu built all 141 packages from source
  instead of taking Linux binaries (#1102).
- Long pipeline builds are minutes-to-hours and read pins or multi-GB local
  rasters. Never put one in a test or an example; use the
  [`example = FALSE` fixture pattern](../AGENTS.md#documentation).
- `validation/` holds the ground-truth harness (`Rscript
  validation/validate_all.R`) — it compares real WHEP output against
  independent statistics. It **needs network and external data**, is
  `.Rbuildignore`d, and is not part of `R CMD check`. Run it when a change
  moves published numbers; see `validation/README.md` and
  `validation/SOURCES.md`.

### There are three verification surfaces and they disagree

A green `devtools::test()` does **not** mean a green CI. Three layouts exist:

1. **Source checkout** — what `devtools::test()` runs. `.Rprofile` calls
   `devtools::load_all()`, which pre-attaches every package data object, and
   `inst/` is present.
2. **`R CMD INSTALL .`** — installs `inst/` **wholesale**, ignoring
   `.Rbuildignore`.
3. **Built tarball** — what `R CMD check`, r-universe and CRAN run.
   `.Rbuildignore` applies, so `^inst/scripts$`, `^validation$` and `data-raw`
   are simply **absent**.

Only (3) is what CI runs, and each surface has shipped a real failure the
others could not see: a data fingerprint that hashed 55 of 100 objects because
`load_all()` had already attached them; a test that "passed" under `INSTALL`
while its script was `.Rbuildignore`d out of the tarball; an assertion that
failed only because CI resolves a newer dplyr. If a change touches
`utils::data()`, package data, a lazy-loaded object or anything under `inst/`,
verify on the tarball before claiming it is green.
