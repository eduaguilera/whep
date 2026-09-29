# WHEP agent reference — NSE globals and generated-file merges

Read before declaring a new NSE symbol, resolving a merge conflict in `R/utils.R`/`NAMESPACE`/`man/`/`data/*.rda`, or rebuilding harmonization package data.

Moved out of `AGENTS.md` on 2026-09-27 to keep that file under Codex's 32 KiB
default instruction-file read cap (`project_doc_max_bytes`). `AGENTS.md`
carries the pointer; this file carries the detail.

---

### NSE globals

Every NSE symbol must be declared in the `utils::globalVariables()` call at
the top of `R/utils.R` or `R CMD check` NOTEs — and the CI action fails on
warnings and above, never on notes, so an undeclared symbol merges green.
That is how #1114 shipped `predecessor` and `reporting_polity_code`
undeclared, and why #1135 had to be opened after it.
`tests/testthat/test_utils.R` now runs the same scan `R CMD check` runs, with
the same settings, and **fails** on it, naming the symbol, its file and its
lines.

The call is ~2000 entries long: **append** a small block at the end, above
the `NULL` sentinel, preceded by a comment naming the file and what the
symbols are for, following the existing pattern. Do not reorder or
alphabetise it — the file-grouped comments are the only thing making it
reviewable.

The file must contain **nothing but** that one call, and the list must keep
ending in `NULL`. Both are preconditions of the `merge=union` entry the next
section explains, and the same test file asserts them: without the sentinel,
two branches each appending a block merge into `"last of A"` followed by
`"first of B"` with no comma between — a syntax error, introduced silently.

### Generated and append-only files — regenerate, never hand-merge

`.gitattributes` writes down the deterministic resolution for the tracked
files that unrelated branches collide on by construction, so it is not
rediscovered per merge. Over the 30 days to 2026-09-16, of 608 commits on
`main`, **52** touched `R/utils.R`, **37** `NAMESPACE` and **22**
`data/whep_inputs.rda` — the last of those conflicts unconditionally, because
git cannot merge a binary at all.

- **`R/utils.R`** — `merge=union`, so two appended blocks both survive. That
  is always the right answer for an allowlist of strings: order carries no
  meaning, `globalVariables()` drops duplicates, and each side keeps its own
  comment header. Its two preconditions are in
  [NSE globals](#nse-globals).
- **`NAMESPACE` and `man/`** — roxygen output. Never hand-merge either: take
  one side and re-run `devtools::document()`, which regenerates both from
  `R/`. `NAMESPACE` also carries `merge=union` so the common case — two
  branches each adding an export — resolves itself, and the `document()` run
  that *Before committing* requires anyway puts the block back in sorted
  order.
- **`data/*.rda`** — marked `binary`, so git leaves the conflict for a human
  instead of half-merging it. Resolve the **source** — the CSV under
  `inst/extdata/harmonization/`, or `inst/extdata/whep_inputs.csv` — then
  re-run the builder from
  [Package data updates](#package-data-updates). Never pick a side of the
  `.rda` itself: `tests/testthat/test_data_raw_freshness.R` fails when a
  table stops matching its own source, and that gate is the only thing
  standing between a lazy resolution and a pin version going missing unseen.

## Package data updates

When modifying CSV files in `inst/extdata/harmonization/`:

1. Edit the CSV.
2. Run `Rscript data-raw/harmonization_tables.R` to rebuild `.rda` files.
3. Run `Rscript data-raw/table_mappings.R` if `regions.csv` or `items_*.csv`
   changed.
4. Run `Rscript data-raw/whep_inputs.R` if `whep_inputs.csv` changed.

Skipping the rebuild is a defect, not an omission: `data/*.rda` is a committed
build product, so an edited CSV that was never rebuilt ships a table
disagreeing with its own source, and it looks exactly like a fresh one
(#384 — that is how `regions_full` came to resolve eight areas to polities
upstream had retired). `tests/testthat/test_data_raw_freshness.R` is the gate.
It re-runs every builder whose inputs live inside the repo, with the
`usethis::use_data()` calls stripped so nothing is written, and compares each
rebuilt object with the committed `.rda` by content. It covers 49 of the 56
tables; the seven it cannot rebuild (the whep-polities GeoPackage, the Coello
CSV, the GLEAM workbook) are listed there with the input that blocks each one,
and the list is asserted to be exactly the complement, so a new dataset cannot
arrive both unchecked and unexcluded.
