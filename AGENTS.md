# AGENTS.md — WHEP Package

WHEP is an R package (~140 scripts in `R/`, ~70k lines, 290 documented topics)
that builds agro-environmental data: FAOSTAT/LUH2 primary production,
commodity balances, trade, livestock nutrient and emission flows, gridded
soil carbon/nitrogen/water balances, and FABIO-style footprints.

Its outputs are numbers that get published. A change that is beautifully
styled and quietly wrong is a failure; a plain change that is demonstrably
right is not.

## How a change is judged

In this order:

1. **Are the numbers right, and is that shown?** Evidence, not assertion.
2. **Do the checks pass?** See [CI Checks](#ci-checks). Run them; do not
   predict them.
3. **Is it readable and in-style?** See [Code style](#code-style).

Style nits are the cheapest thing to fix and the least important. Do not spend
a review on them while leaving 1 and 2 unexamined.

### Evidence a change must carry

- **Bug fix**: a test that fails before the fix and passes after it. Write it
  first, watch it fail, then fix.
- **Changed numbers**: state the before/after magnitude in the PR body.
  "No published value changes" is also a claim that must be checked, not
  assumed.
- **New code path**: a test that reaches it — including the branch that
  aborts or warns, not just the happy path.
- **Anything reading external data**: an offline fixture, so the test suite
  never depends on a host being up. See [Tests](#tests).
- **Prefer invariants over hand-picked expectations.** The package ships them:
  `check_supply_use_balance()`, `check_footprint_conservation()`,
  `assert_footprint_invariants()`, `check_series_jumps()`, plus pointblank
  column expectations in tests. An invariant catches the bug you did not
  imagine; an equality test on three rows does not.
- **An invariant that holds by construction cannot detect a missing input.**
  A pin shipped with **zero** inland water and ice — 533 Mha of lakes and
  glaciers booked as land — while `territory == land + water + ice` still
  held, because zero satisfies it. The layers were optional arguments that
  zero-filled silently. Assert that an input was *supplied* (a provenance
  column, or at least one non-missing non-zero value — a row count is not
  enough), not merely that the totals reconcile; the helpers for this are
  `check_inputs_supplied()` and `check_labels_supplied()`, and the rule is
  [Absent inputs must not become zeros](#absent-inputs-must-not-become-zeros).
  The same shape
  has appeared in a balance check with no unit dimension (head counts balanced
  against head counts), a global mean unchanged while every cell moved, and
  two Rest-of-World buckets matching by code while covering different
  countries.
- Report outcomes faithfully. If a check fails, say so and paste the output.
  If you skipped a step, say which.

### Before you start an issue, check it is still real

Issues go stale. A sweep of 86 pre-August issues found **12** that were already
fixed, superseded or factually wrong on `main` — and one issue was
independently re-derived by **four** separate investigations, none of which
found it because none searched. Both failures are cheap to avoid:

1. **Search first.** `gh issue list --state all --search "<key terms>"`. If
   another issue covers the same defect, work that one.
2. **Re-measure the premise on current `main`** before planning anything. Do
   not copy a figure out of an issue body — an audit-era number may predate a
   dozen merges. If the premise does not hold, say so with evidence and stop;
   that is a complete and valuable outcome, not a failed task.
3. **"Already fixed" is not always "close it".** Several fixes landed as side
   effects of unrelated work, with no test. A regression guard for a fix that
   nothing pins is worth writing.

### Classify every change: mechanical or science decision

Agents rank work by *engineering review effort* — small diff, green CI, one
isolated file. That axis is **orthogonal** to *scientific decision content*.
A one-line green diff can still embed a methodological choice. CI proves the
code runs and that regressions are locked; it proves nothing about whether the
embedded choice is the one the maintainer wants.

So whenever proposing, reviewing, or prioritising a change, classify it:

- **Mechanical** — objectively correct once implemented: a crash guard, a
  broken join, a dedup, a math identity that must hold, packaging/CI/docs.
  No defensible alternative. Reviewable on tests + CI.
- **Science decision** — defensible alternatives exist and results differ
  between them: a coefficient or reference value, an allocation rule, a
  conservation/mass-balance policy, a numeric cap or default, a
  fail-loud-vs-continue behaviour, a choice of estimation method or its
  default. Must not be merged on green CI alone.

State the classification and, for a science decision, **surface the choice
itself** — what the alternatives are and what changes numerically between them
— independent of how small the diff is. When in doubt, treat it as a science
decision.

### Never invent a result-affecting value

- **NEVER guess or hallucinate reference titles, authors, years, or DOIs.**
  Verify from the actual source (web search, PDF, DOI lookup) before writing
  any bibliographic information. If you cannot verify it, say so explicitly.
- The same rule extends past citations to every value that moves a result:
  coefficients, emission and extraction factors, allocation rules, A-matrix
  caps, tolerances, and defaults. Each needs a source or an explicit,
  visible "assumed, unverified" note — never a plausible-looking number.
- Cite in the roxygen `@description` or a code comment at the point of use,
  not only in the PR body, which nobody reads a year later.

### Labels

Issue/PR labelling conventions (triage axis, subsystem, type, priority,
plus the three contributor-facing labels) moved to
`agent-reference/issue-labels.md`. Read it before opening or labelling an
issue.

### Writing a PR or issue body

**Lead with what the change adds: the problem, then the result.** A reviewer
must be able to learn what they are being asked to merge from the opening
lines. Then the details, then the numbers, then the evidence.

**Process goes last.** Reconciliation with a branch that landed mid-flight, an
approach that turned out wrong, a claim that failed its own check, a figure
corrected on a second pass — all of it belongs in a closing section, never in
the opening and never woven through the substance. This is about placement,
not disclosure: a correction that changes the reviewer's decision is still
mandatory, and a wrong number is still fixed and stated. Do not tally your own
errors; state what is true now.

**Never use a bare number as a heading or a reference.** Every mention of a
pull request or an issue says which of the two it is and carries a short title
naming its subject — `PR #1042 — gridded livestock emissions restored`,
`issue #1043 — truncated aggregation factor`, never `#1042` alone. A reader
scanning headings, a release-notes assembler, and anyone reading `git log` a
year from now all need the subject without opening a link. The `Closes #N` /
`Refs #N` lines are the exception: those are machine-read and stay bare.

**State the base of every percentage**, and never quote a change in a
component as though it were the change in the total that contains it. Where
several figures come from different variants of one computation, either report
a single consistent series or label every variant — a figure taken from a
different variant than its neighbours is not a series.

**Ask open questions with the questioning tool, not in prose.** A science
decision surfaced under [Classify every change](#classify-every-change-mechanical-or-science-decision)
is put to the maintainer through the structured question interface, with the
alternatives and what changes numerically between them, not mentioned in a
paragraph and left to be noticed. A question buried in a PR body or a report is
a question that does not get answered: the work stalls, or proceeds on an
assumption nobody agreed to. Keep working on whatever does not depend on the
answer while it is outstanding.

## Running things

`.Rprofile` runs `devtools::load_all()` on session start, so a plain
`Rscript`/`R` session already has the package loaded.

```r
devtools::test(filter = "cbs")   # one test file — the cheap inner loop
devtools::test()                 # whole suite, must be 100% green
```

```bash
air format .                     # mandatory before committing
```

```r
devtools::document()             # after air, to refresh man/
rcmdcheck::rcmdcheck(
  build_args = "--no-build-vignettes",
  args = c("--no-tests", "--ignore-vignettes"),
  error_on = "error"
)
lintr::lint_package()   # linters and exclusions come from .lintr
```

Gotchas worth knowing before losing an hour
(`.Renviron`/`.Rprofile` shadowing, long pipeline builds, `validation/`)
moved to `agent-reference/build-verification-notes.md`. Read it before
debugging an env var or profile that "should" be set.

## Conventions that are easy to get wrong

### Area codes and polity columns

Why `reporting_polity_code`, `polity_area_code` and `area_code` are not
interchangeable, and the polity-period convention, moved to
`agent-reference/area-codes-and-polities.md`. Read it before grouping,
joining or reducing on any of them.

### Join on codes, never on names

Name-keyed joins have caused silent drops and double counts more than once.
Join and group on integer codes inside functions; attach human-readable names
only at the final output stage, with the lookup helpers that now exist:
`add_area_code()`, `add_area_name()`, `add_item_cbs_code()`,
`add_item_cbs_name()`, `add_item_prod_code()`, `add_item_prod_name()`,
`add_polity_code()`. Do not carry redundant name+code pairs through
intermediate computations.

### Multi-method functions

Estimation functions that admit more than one defensible method must expose a
`method =` (or `tier =`) argument selecting among them, validated with
`rlang::arg_match()`. The default is the most rigorous available method;
simpler methods stay selectable for the user's choice, sensitivity analysis,
and to quantify what the sophisticated method buys. Methods are alternatives,
**never silent fallbacks**: record the chosen method in an output column
(`method_<quantity>`, e.g. `method_soil_n2o`, `method_land`, `method_soc`),
and use a coarser method only when explicitly requested.

### Column contracts

Validate arguments with `rlang` (`rlang::has_name()`, `rlang::arg_match()`),
not base R, and abort with `cli::cli_abort()`. For completing a tibble to a
known schema, use the exported `ensure_columns()` with a zero-row prototype
rather than ad-hoc `if (!has_name(...)) mutate(x = NA)` chains.

### Absent inputs must not become zeros

Three times in one week an absent input silently became a zero and every check
downstream passed (#1010, #1016, #1034). **A guard must sit where the absence
is created, not where it is consumed**: once a zero is downstream it is
indistinguishable from a measurement, and no care at the consuming end
recovers the distinction.

Two exported helpers, in `R/absent_input.R`:

- `check_inputs_supplied(data, required)` — the column exists **and** holds at
  least one value that is neither missing nor zero. Use it at a boundary an
  external table crosses. It does not judge a zero-row frame (a filter that
  matched nothing is the caller's to answer for) and one non-zero value
  anywhere passes, so it cannot see a partial absence.
- `check_labels_supplied(data, column, labels)` — the labels a `filter()`
  selects on still occur in the column. This is #1016's mechanism, and it is
  invisible to any rule written around `coalesce()`, `replace_na()` or
  `na.rm`: none of those appear anywhere near it. The message names the labels
  that *are* there, because a rename is only obvious once you see the new
  spelling.

`stamp_inputs_supplied()` writes the provenance label the first reads: a
comma-separated list of the optional inputs a build consumed. A label cannot
be satisfied by arithmetic, which is the whole point — see `layers_supplied`
in `build_polycell_support()` and `read_polycell_support()`, the prior art
both helpers generalise, and `method_weed_npp` in
`calculate_npp_carbon_nitrogen()` for the per-row form.

Three states, and the middle one is where the completeness principle bites:

1. a bare zero or silent literal — forbidden, because it cannot be told from a
   measurement;
2. a refusal — also wrong wherever the quantity is known to exist, because
   excluding it biases the total just as silently;
3. a **declared assumption** — a named value, a citation or an explicit
   "assumed, unverified" note, and a `method_*` / `source` / `*_supplied`
   stamp on the row.

The line between (2) and (3) is not abort-versus-fill. It is whether the
absent thing is a **quantity** (a species with no published emission factor —
fill it and declare it) or a **contract** (a corrupted lookup, a label in an
unrecognised vocabulary — no defensible fill exists; abort). And before
filling anything: **run the lookup and look at what it returns.** A value the
code failed to reach is a defect, not an absent quantity, and a `method_*`
stamp on it makes a missed lookup read as a considered choice.

A structural zero — one where the right-hand side is a ledger or lattice WHEP
itself defines, so absence really is "did not happen" — is fine, but **say so
in a comment at the point of use**. There are ~170 silent sites in ~51 files;
they stay a documented backlog rather than a build-stopping gate, and turning
the readable ones into readable code is what makes any future gate possible.

Every guard ships with a test of the shape in
`tests/testthat/helper_absent_input.R`:

```r
expect_supplied_guard(
  identity = <the reconciliation, which HOLDS on the vacuous input>,
  guard = <the call, which must fire anyway>
)
```

Its subject is the inadequacy of the identity, not the identity. #1016's own
test is named *"enteric_ch4_kt conservation is exact"* and passes today while
the pin ships nothing, because zero distributes to zero. A row count is not
enough either: an Element whose rows exist while every `Value` is `NA`
collapses to a literal zero through `sum(na.rm = TRUE)` with a positive row
count.

### NSE globals

The `utils::globalVariables()` append procedure moved to
`agent-reference/merge-and-globals.md`. Read it before declaring a new
NSE symbol.

### data.table inside private helpers

This is a tidy-data project: exported functions accept and return `tibble`.
Private (`.`-prefixed) helpers may use `data.table` internally for
performance, and must convert back to tibble before returning. Never use bare
`data.frame`. `R/data_table_awareness.R` sets `.datatable.aware` for the
package — leave it alone. Always namespace-prefix (`data.table::`).

### Where input data comes from

Three distinct mechanisms, and picking the wrong one is a design error:

- **Pins** (`whep_inputs.csv` + the pins board) — for data WHEP itself
  produced or curated, which a user cannot otherwise obtain. Prepare with
  `inst/scripts/prepare_upload.R`.
- **Env-var-gated local rasters** — multi-GB third-party archives stay on
  local disk and are read via env vars: `WHEP_CRU_DIR`,
  `WHEP_LPJML_RUN_DIR`, `WHEP_HYDE_DIR`, `WHEP_HANI_DIR`, `WHEP_WIND_DIR`,
  `WHEP_LUH2_DIR`, `WHEP_HWSD_DIR`, `WHEP_CRITICAL_N_DIR`, plus the gridded
  land surfaces (`WHEP_TYPE_CROPLAND_PATH`, `WHEP_CROP_PATTERNS_PATH`,
  `WHEP_GRIDDED_PASTURE_PATH`). The readers
  **abort with an instruction** when unset. `WHEP_POLITY_FRACTION_PATH` is
  **not** one of them any more: the cell-polity crosswalk is WHEP-built, so it
  is the `spatialize-cell-polity-fraction` pin and the env var is only an
  override (#694). A WHEP-built artifact belongs in a pin, however small —
  gating one behind an env var means every user regenerates it, and then
  everyone reads a different vintage. Never hardcode an absolute path,
  and never invent a fallback that silently reads something else.
  Every one of these must be **reproducibly obtainable**: an
  `inst/scripts/download/download_*.R` fetches it from the official source into
  `<dest_dir>/<DATASET>/`, and the env var points there. A dataset that exists
  only because someone once downloaded it by hand is not a data source, it is a
  local accident — if you find one, add the download script rather than pointing
  an env var at wherever that copy happens to live. The script must also leave
  the data in the form its reader consumes: HaNi ships zipped but
  `read_n_deposition()` reads NetCDF, so `download_nitrogen.R` extracts; HYDE
  ships one archive containing per-year archives, so `download_hyde.R` unpacks
  the outer one only.
- **Verified on-demand download** — for third-party data already published
  with a stable DOI and checksum: download, verify the published MD5, cache
  under `rappdirs::user_cache_dir("whep")`, and treat the env var as an
  override. Prefer this over a pin, which adds an uncheckable second copy
  (#457). Current cases: the LUH2 `states.nc` (`read_luh2_landuse()`, Zenodo
  record 15556812) and the critical-nitrogen archive (`read_critical_n()`,
  Zenodo record 6395016).

#### Fixing the reader is half the job: the pin it feeds is now stale

Moved to `agent-reference/data-pipeline.md`. Read it before or after
changing a producer function — the fix does not move a published number
until the pin it feeds is regenerated.

### NEWS.md — do not edit it per PR

**Do not add a `NEWS.md` entry in a PR.** Every PR touching the same
"development version" heading conflicts with every other one, and with many
PRs in flight that is a steady stream of hand-resolved merges whose only
content is which bullet goes above which. Nothing is learned from resolving
them and the failure mode is real: a mis-resolution silently drops a bullet.

`NEWS.md` is assembled **at release time** from the conventional-commit
history, which is why the commit subject and body matter. Put the user-visible
consequence there and in the PR body:

- the subject says what changed, scoped (`fix(cbs): …`);
- the body states the before/after magnitude, or says plainly that no
  published value moves.

That is the same information the old per-PR entries carried, recorded where it
cannot conflict and where `git log` can find it. If a change is large enough
that a user needs prose beyond a commit message, write it in the PR body and
flag it for the release notes there.

### Generated and append-only files — regenerate, never hand-merge

The per-file merge-conflict rules for `R/utils.R`, `NAMESPACE`, `man/` and
`data/*.rda` moved to `agent-reference/merge-and-globals.md`. Read it
before resolving a conflict in any of them.

### File naming

New scripts in `R/` are `snake_case.R`, named after the subsystem
(`n_balance_losses.R`), never after a person. `tests/testthat/test_<script>.R`
mirrors the script. Some legacy files (`Typologies_Julia.R`,
`whep_typologies_spain.R`) break this; do not copy them.

## Code style

Load-bearing (a linter, `air`, or `R CMD check` enforces it):

- Maximum line width is 80 characters.
- **Always** run `air format .` before committing. Install the binary if it is
  not on PATH. Do not format manually — and note `air.toml` sets
  `skip = ["tribble"]`, so `tribble()` bodies keep the alignment you give
  them; align them yourself. Nothing gates this before merge any more (see
  [CI Checks](#ci-checks)): `main` reformats itself afterwards, so skipping it
  means the diff that was reviewed is not the diff that landed.
- Namespace-prefix every imported function (`dplyr::filter()`, and
  `stats::median()`, not `median()`). Do not use `@importFrom`.
- Variable and function names must not exceed 30 characters.
- Escaped characters in regex must be double-escaped in R strings (`\\.`, not
  `\.`).

Conventions of the codebase (follow them; they are how the code reads):

- Follow the workflow: <https://lbm364dl.github.io/follow-the-workflow/> and
  the tidyverse style guide: <https://style.tidyverse.org/>.
- Use `cli::cli_abort()` / `cli::cli_warn()` / `cli::cli_inform()` instead of
  `stop()` / `warning()` / `message()`, with cli's inline markup
  (`{.arg x}`, `{.val {v}}`) and pluralisation (`{?s}`).
- `snake_case` for column names in tibbles. Readable and self-explanatory —
  no cryptic abbreviations like `NEm`, `Bo`, `VS`, `GE`. Prefer
  `ne_maintenance`, `methane_potential`, `volatile_solids`, `gross_energy`.
- Extract complex logic into private helpers (`.` prefix) early. Helpers are
  stateless and receive all context via arguments.
- No functions inside functions — all definitions at top level. Exported
  functions first in the file, private helpers after them.
- Native pipes (`|>`); make functions read as piped expressions. Avoid long
  chains of intermediate assignments.
- Avoid `for` loops: vectorise, or use `purrr` / `dplyr` / `tidyr`.
  Exception: a data.table helper iterating a small fixed set of column names.
- Keep functions short. The codebase median is 16 lines and 72% are under 25 —
  that is the norm to match, not a limit to game. Split when a function does
  two things, not to hit a number; a 40-line function that reads top-to-bottom
  is better than four helpers that only exist to be short.
- Avoid signatures with more than ~5 arguments; group related ones into named
  lists.
- Column-name arguments are symbolic (unquoted), used with `{{ }}` inside and
  tunnelled with `{{ }}` when passed down.
- `tibble::tribble()` for small inline tibbles; `stringr` over base R for
  strings; `.by` for grouping.

## Documentation

- roxygen2, markdown enabled. Document exported functions only; private
  (`.`-prefixed) helpers may stay undocumented.
- First line = title, no `@title` tag, short, imperative verb. Then
  `@description`, one `@param` per parameter, `@return`, `@export`,
  `@examples`.
- One space after `#'`; indent continuation lines by two spaces. Finish all
  doc sentences with a full stop.
- Reuse shared descriptions with `@inheritParams` / `@inheritSection` (see
  `R/polity_columns_doc.R`) instead of re-describing the same columns.
- **Never** use `\dontrun{}` or `\donttest{}`. Every example runs during
  `R CMD check`. For functions depending on remote data or slow builds, use
  the `example = FALSE` pattern: an `example` argument that returns a small
  hardcoded `tibble::tribble()` from a `.example_*()` helper in
  `R/toy_examples.R` (run the real function once, then sample ~10 rows). The
  `@examples` block is then just `my_function(example = TRUE)`. ~50 functions
  already do this and `R/toy_examples.R` holds 46 such fixtures — copy the
  nearest one. Fast, self-contained functions get a plain inline example.
- Examples must not use a package from `Suggests` without guarding it
  (`requireNamespace()`), and must not need a `WHEP_*` env var.

## Tests

- `testthat` edition 3. One test file per `R/` script:
  `tests/testthat/test_scriptname.R`.
- **The suite must never reach the network or read a `WHEP_*` path.** A test
  that does turns an unrelated outage into a hard `R CMD check` ERROR (#490).
  Stub the reader with `testthat::local_mocked_bindings()` (43 call sites do
  this already) or use a fixture under `tests/testthat/fixtures/`.
  - `skip_on_ci()` does **not** enforce this: r-universe runs its check without
    `CI` set, so a `skip_on_ci()` test runs there for real. Use
    `skip_on_cran()`, which fires wherever `NOT_CRAN` is unset (r-universe,
    CRAN) while `r-lib/actions/setup-r` and `devtools::test()` both set it. A
    real-data test that genuinely cannot be rescoped onto a fixture needs
    **both**. Guarding on a local file or `WHEP_*` env var is equally fine —
    that is what the LPJmL/HWSD/LUH2 smoke tests do.
  - The `offline-tests` job is the enforcement, and it only sees these tests
    because it unsets `CI`. If it fails alone, add a fixture — do not skip the
    test and do not relax the job.
- Access exported objects via `whep::name` — never `:::` or
  `getFromNamespace()` for something exported. Private helpers are tested
  directly as `whep:::.helper()`, which is the established practice. For
  dynamic access in loops use `getExportedValue("whep", nm)`.
- Guard anything from `Suggests` with `testthat::skip_if_not_installed()`
  (pointblank, sf, terra, ncdf4, ggplot2 are all Suggests).
- Use `tibble::tribble()` fixtures, pipes, `dplyr::pull()`, and pointblank
  expectations (`expect_col_exists()`, `expect_col_vals_in_set()`,
  `expect_col_vals_not_null()`, `expect_col_vals_equal()`).
- Test edge cases and the failure branches: the abort message, the warning,
  the empty input, the missing column. `expect_error(class = ...)` against a
  condition class is better than matching message text.
- Factor repeated fixtures into helpers (`tests/testthat/helper_*.R`).
- `test_gapfilling.R` is the reference for style;
  `test_commodity_balance_sheet.R` is the reference for small self-contained
  fixtures with no pins and no network.

## CI Checks

The PR must pass these GitHub Actions checks:

1. **R-CMD-check** (4 platforms, 60-min job cap over a 40-min cap on the check
   step itself — setup is download and varies, the check step is what can
   hang): `rcmdcheck::rcmdcheck()`
   with no errors, warnings, or notes. Tests run here, which is why a
   network-dependent test breaks the build. **R-devel is not one of the four**:
   RSPM ships no R-devel binaries, so that leg source-builds the geo stack every
   time the R-devel snapshot rolls its cache key, and it told you nothing about
   your change while you waited. It runs as a separate weekly job
   (`R-CMD-check-devel`, Wednesdays 04:00 UTC, also `workflow_dispatch`) with
   `continue-on-error`, so a new R is still caught early without blocking a
   merge. Check it when R releases; do not expect it green on a PR.
2. **lint** (`lintr`): with `object_usage_linter`, `line_length_linter`,
   `indentation_linter` and `commas_linter` disabled (they conflict with
   `air`). `inst/scripts` and `inst/analysis` are excluded.
3. **pkgdown**: the site must build. **Every** documented topic (functions and
   documented datasets) must appear in `_pkgdown.yml` under `reference:` —
   every `man/*.Rd` except `whep-package.Rd`. Verify with the `comm` command
   below; it is currently clean, so any output is your change.
4. **test-coverage** (`covr` → Codecov): the suite runs again here and
   coverage is reported. New exported functions should arrive with tests, not
   after them.

Formatting is **not** among them: there is no pre-merge `air` gate. The
`format-main` workflow reformats `main` with `air format .` after every merge
and commits the result back. That is a safety net for the case where someone
forgets, not a licence to skip — a PR is not ready until you have run
`air format .` yourself, so that the diff under review is the diff that lands
and `main` does not fill up with formatting-only commits.

`cache-prune` is not a check and never runs on a pull request's check path. It
deletes the dependency caches a pull request leaves behind once that PR closes,
on close and on a six-hourly sweep. A cache written from `refs/pull/N/merge` is
readable only by that same PR and GitHub never deletes it on merge, so without
this the repository sat at 10.51 GB against the 10 GB quota with 6.14 GB of it
held by five already-merged PRs — bytes nothing could ever read, evicting
`main`'s shared caches and making later runs do cold installs (#1104). If a
cache seems to have vanished, read that workflow's log before suspecting a key
change.

## Before committing

```bash
# 1. Format (MANDATORY — do not skip, do not do manually)
air format .
```

```r
# 2. Document
devtools::document()

# 3. Check — WITH TESTS. Do not pass `--no-tests`; see below.
rcmdcheck::rcmdcheck(
  build_args = "--no-build-vignettes",
  args = c("--no-manual", "--as-cran", "--ignore-vignettes"),
  error_on = "error"
)

# 4. Test
devtools::test()
```

Why a green `devtools::test()` is not a green CI — the three build
layouts and how they disagree — moved to
`agent-reference/build-verification-notes.md`. Read it if a change
touches package data, a lazy-loaded object or anything under `inst/`.

```bash
# 5. Verify pkgdown — every man/*.Rd must be in _pkgdown.yml
# (compare outputs; empty = OK)
comm -23 \
  <(ls man/*.Rd | sed 's|man/||;s|\.Rd||' | grep -v whep-package | sort) \
  <(grep '^  - ' _pkgdown.yml | sed 's/^  - //' | sort)
```

Commits are conventional-commit style with a subsystem scope
(`fix(cbs): key source selection on area_code, not periodized name`), branches
are `fix/…`, `feat/…`, `perf/…` or `<user>/<topic>`.

**Write `Closes #N` (or `Refs #N`) in the PR body, always.** A backlog sweep
found issues that had been fixed weeks earlier by PRs that never named them —
they stayed open, and later work re-derived them from scratch. If a PR only
partly addresses an issue, say `Refs #N` and state in the body which part
survives, so the issue can be rescoped rather than closed by accident.

## Data pipeline

The function catalogue (primary production, CBS, soil water/carbon/
nitrogen, footprints, LPJmL pins) moved to
`agent-reference/data-pipeline.md`. Read it before touching a pipeline
stage or a pin.

## Package data updates

The CSV-edit / rebuild procedure for `inst/extdata/harmonization/` moved
to `agent-reference/data-pipeline.md`. Read it before or after editing a
harmonization CSV.

## This is the only agent instruction file

The repo used to carry per-tool copies of these rules
(`.github/copilot-instructions.md`, `.agent/rules/whep.md`). They drifted —
one still forbade `data.table`, which the package now Imports — so they were
deleted in favour of a single `CLAUDE.md` with a byte-identical `AGENTS.md`
copy, kept in step by a CI check. That copy existed only because not every
agent read `AGENTS.md`; now that they do, the copy is gone and `AGENTS.md`
is the one file.

`AGENTS.md` is the source of truth. Claude Code (>= 2.1.277), Codex and
Copilot all read `AGENTS.md` directly — no per-tool copy is needed for any of
them, and none should exist. Tools that look for another name by default can
usually be configured to read `AGENTS.md` (for example Gemini CLI's
`contextFileName` setting).

**Not a symlink, and not a copy either.** A symlinked or copied per-tool file
was the earlier failure mode: Git for Windows only materialises symlinks when
`core.symlinks` is true, which needs Developer Mode or admin rights, so a
symlink risks silently checking out as plain text containing the target path
instead of the rules; a copy risks silently drifting from the file it was
copied from. One canonical file that every tool reads natively removes both
risks at once.

This is enforced, not merely asked for:
`.github/workflows/agent-instructions.yaml` fails the build if `AGENTS.md` is
missing, or if any other known agent-instruction filename (`CLAUDE.md`,
`.github/copilot-instructions.md`, `.agent/rules/whep.md`, etc.) is present at
all. If your tool looks for a name that workflow does not list and reads
`AGENTS.md` natively, there is nothing to add. If your tool cannot be pointed
at `AGENTS.md`, raise it before adding a copy — do not start a second source
of truth.
