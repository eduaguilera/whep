# CLAUDE.md — WHEP Package

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
- **Changed numbers**: state the before/after magnitude (in the PR body, and
  in `NEWS.md` when it is user-visible). "No published value changes" is also
  a claim that must be checked, not assumed.
- **New code path**: a test that reaches it — including the branch that
  aborts or warns, not just the happy path.
- **Anything reading external data**: an offline fixture, so the test suite
  never depends on a host being up. See [Tests](#tests).
- **Prefer invariants over hand-picked expectations.** The package ships them:
  `check_supply_use_balance()`, `check_footprint_conservation()`,
  `assert_footprint_invariants()`, `check_series_jumps()`, plus pointblank
  column expectations in tests. An invariant catches the bug you did not
  imagine; an equality test on three rows does not.
- Report outcomes faithfully. If a check fails, say so and paste the output.
  If you skipped a step, say which.

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

### Labels — the triage surface

Labels are how the maintainer decides at a glance what can be batch-merged and
what needs a domain expert. Label **every** issue extensively:

- **Triage axis (required, exactly one)**: `mechanical` or `needs-expert`.
  Reference-value and coefficient changes are always `needs-expert`, even at
  one line. Infra, packaging, CRAN, docs, testing, and pure crash/identity
  fixes are `mechanical`. When in doubt, `needs-expert`.
- **Subsystem** (one or more, where applicable): `area:cbs`,
  `area:production`, `area:livestock`, `area:trade`, `area:footprint`,
  `area:nitrogen`, `area:soc`, `area:gapfilling`, `area:lmdi-decomp`,
  `area:data-io`, `area:spatialize`, `area:regions`, plus cross-cutting
  `fabio`, `lpjml`, `footprint-extension`. Cross-cutting infra/meta issues
  legitimately carry none — do not force one.
- **Type**: `bug` / `enhancement` / `documentation` / `dev-infra` /
  `testing` / `release`.
- **Priority**: a `priority:*` label.

Apply the full set when opening an issue; backfill missing labels when you
touch an old one. PRs do not need the labels duplicated when they close an
already-labelled issue.

#### Contributor-facing labels

Three further labels exist for people outside the team, who pick work by them.
Guidelines live in `.github/CONTRIBUTING.md` (that path keeps them out of the
package build — `^\.github$` is in `.Rbuildignore`).

- **`good first issue`** + **`help wanted`**: small, self-contained and
  precisely specified. **Verify the defect still exists in current code before
  applying it.** Several audit-era issues had already been fixed by a later
  commit while the issue stayed open; sending a newcomer to one of those is
  worse than leaving it unlabelled.
- **`no-data-needed`**: the issue can be reproduced, fixed **and verified**
  using only a clone. Package data (`data/*.rda`, `inst/extdata/`), hand-built
  `tribble()` fixtures and injected arguments all count as available; pins,
  `WHEP_*_DIR` rasters and any network read do not.

`no-data-needed` exists because the data barrier, not the science, is what
usually blocks an outside contributor: the test suite is fully offline, but a
real pipeline build needs inputs that cannot be handed out. Two rules keep it
worth having — apply it only after checking the verification path really is
offline, and never apply it to code that is not on `main` (work living on an
unmerged feature branch cannot be picked up from a fresh clone).

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

Gotchas worth knowing before losing an hour:

- `WHEP_*` paths belong in `~/.Renviron` and are read from there. Do **not**
  add a `.Renviron` at the repo root: R reads a working-directory `.Renviron`
  *instead of* `~/.Renviron`, never both, so one here silently hides every
  `WHEP_*` path an R session started at the root would otherwise see. That was
  #456, fixed by moving `_R_CHECK_SYSTEM_CLOCK_` out of a tracked `.Renviron`
  into the R-CMD-check workflow env and `.Rprofile`.
- Long pipeline builds are minutes-to-hours and read pins or multi-GB local
  rasters. Never put one in a test or an example; use the
  [`example = FALSE` fixture pattern](#documentation).
- `validation/` holds the ground-truth harness (`Rscript
  validation/validate_all.R`) — it compares real WHEP output against
  independent statistics. It **needs network and external data**, is
  `.Rbuildignore`d, and is not part of `R CMD check`. Run it when a change
  moves published numbers; see `validation/README.md` and
  `validation/SOURCES.md`.

## Conventions that are easy to get wrong

### Area codes and polity columns

There are **two code spaces**, and confusing them silently misattributes data:

- `area_code` — the FAOSTAT-style area key of the row.
- `polity_area_code` — the numeric key rows are **aggregated on** for the
  matrix workflows. It is a *bucket, not an identity*: 999 is Rest of World,
  206 is Sudan (former), and 62 of the 257 ISO3 codes in `regions_full` do not
  get their own code.
- `reporting_polity_code` — the polity itself (`"ESP-1846-1914"`), year-aware:
  the same `area_code` resolves to different polities in different years.
  Use this to say which territory a row belongs to.

`R/polity_columns_doc.R` documents these once; inherit that section instead of
writing a fresh, subtly different description. Rows that resolve to no polity
keep `NA` rather than being dropped, so gaps stay visible.

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

### NSE globals

Every NSE symbol must be declared in the `utils::globalVariables()` call at the
top of `R/utils.R` or `R CMD check` NOTEs. It is ~1700 entries long: **append**
a small block at the end, preceded by a comment naming the file and what the
symbols are for, following the existing pattern. Do not reorder or
alphabetise it — the file-grouped comments are the only thing making it
reviewable.

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
  `WHEP_GRIDDED_PASTURE_PATH`, `WHEP_POLITY_FRACTION_PATH`). The readers
  **abort with an instruction** when unset. Never hardcode an absolute path,
  and never invent a fallback that silently reads something else.
- **Verified on-demand download** — for third-party data already published
  with a stable DOI and checksum: download, verify the published MD5, cache
  under `rappdirs::user_cache_dir("whep")`, and treat the env var as an
  override. Prefer this over a pin, which adds an uncheckable second copy
  (#457). Current cases: the LUH2 `states.nc` (`read_luh2_landuse()`, Zenodo
  record 15556812) and the critical-nitrogen archive (`read_critical_n()`,
  Zenodo record 6395016).

### NEWS.md

`NEWS.md` carries user-visible behaviour changes under
`# whep (development version)`, in prose, naming what changed and what it does
to published values (including "no published value changes"). Roughly a fifth
of merges need an entry: add one whenever behaviour, an exported signature, or
a number a user could have relied on changes. Pure refactors, tests, and CI
work do not need one.

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
  them; align them yourself.
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

1. **R-CMD-check** (5 platforms, 30-min timeout): `rcmdcheck::rcmdcheck()`
   with no errors, warnings, or notes. Tests run here, which is why a
   network-dependent test breaks the build.
2. **lint** (`lintr`): with `object_usage_linter`, `line_length_linter`,
   `indentation_linter` and `commas_linter` disabled (they conflict with
   `air`). `inst/scripts` and `inst/analysis` are excluded.
3. **format-suggest** (`air`): code must be formatted with `air format .`.
   Mandatory, not optional. Air formats **all** `.R` files — `R/`, `tests/`,
   `data-raw/` — not just the ones you edited. Run `devtools::document()`
   afterwards.
4. **pkgdown**: the site must build. **Every** documented topic (functions and
   documented datasets) must appear in `_pkgdown.yml` under `reference:` —
   every `man/*.Rd` except `whep-package.Rd`. Verify with the `comm` command
   below; it is currently clean, so any output is your change.
5. **test-coverage** (`covr` → Codecov): the suite runs again here and
   coverage is reported. New exported functions should arrive with tests, not
   after them.

## Before committing

```bash
# 1. Format (MANDATORY — do not skip, do not do manually)
air format .
```

```r
# 2. Document
devtools::document()

# 3. Check
rcmdcheck::rcmdcheck(
  build_args = "--no-build-vignettes",
  args = c("--no-tests", "--ignore-vignettes"),
  error_on = "error"
)

# 4. Test
devtools::test()
```

```bash
# 5. Verify pkgdown — every man/*.Rd must be in _pkgdown.yml
# (compare outputs; empty = OK)
comm -23 \
  <(ls man/*.Rd | sed 's|man/||;s|\.Rd||' | grep -v whep-package | sort) \
  <(grep '^  - ' _pkgdown.yml | sed 's/^  - //' | sort)
```

Commits are conventional-commit style with a subsystem scope
(`fix(cbs): key source selection on area_code, not periodized name`), branches
are `fix/…`, `feat/…`, `perf/…` or `<user>/<topic>`, and the PR body references
the issue it closes.

## Data pipeline

- **Primary production**: `build_primary_production()` — FAOSTAT + LUH2
  extension (1850–2023).
- **CBS**: `build_commodity_balances()` — long format output with `source` and
  `fao_flag` columns.
- **Processing coefficients**: `build_processing_coefs()` — cascades from CBS.
- **Soil water balance**: `build_water_balance()` — gridded (0.5° cell ×
  polity) annual water budget from LPJmL hydrology (drainage for N leaching);
  `get_soc_climate_drivers()` emits the monthly SOC climate drivers.
- **Soil carbon (SOC)**: `build_carbon_balance()` — historical gridded SOC
  dynamics (equilibrium init + LUH2-driven march + LUC transfer), yielding
  ΔSOC → ΔSON. `calculate_soc_dynamics(model = c("hsoc","rothc","icbm","amg",
  "century"))` wraps the five SOC models (default `"hsoc"`);
  `build_soil_carbon_inputs()` assembles humified C inputs.
- **Soil nitrogen balance**: `build_nitrogen_balance()` — full gridded N
  balance (inputs − outputs, NUE indicators, GWP/CO2e). `build_n_inputs()`
  assembles the input terms; `calculate_nh3()` / `calculate_soil_n2o()` /
  `calculate_n_leaching()` are the selectable loss methods;
  `build_n_deposition()` / `build_urban_n()` read gridded deposition and
  urban/human N.
- **Footprints**: `build_footprint()` over the FABIO-style IO core
  (`build_io_model()`, `leontief.R`), with extensions wired per stressor
  (`crop_land_extension.R`, `grassland_land_extension.R`,
  `livestock_ghg_extension.R`, `energy_co2_extension.R`,
  `crop_soil_n2o_extension.R`, `n_exceedance_extension.R`). Conservation
  checks live in `R/conservation.R` — a footprint change should exercise them.
- **Source labels**: use dataset-specific names (`FAOSTAT_prod`,
  `FAOSTAT_FBS_New`, etc.).
- **New data sources**: see [Where input data comes
  from](#where-input-data-comes-from) for which of the three mechanisms to
  use.
- **LPJmL outputs are the one input a user cannot obtain**, so unlike the
  third-party rasters they are **pinned, and `WHEP_LPJML_RUN_DIR` is
  optional**. `build_grass_natural_carbon_inputs()` reads
  `lpjml-grass-natural-net-c` and `get_soc_climate_drivers()` reads
  `lpjml-soc-hydrology` by default; set the env var (or pass `run_dir`) only
  to derive those layers from a local run instead. Both artifacts hold **only**
  LPJmL-derived quantities — grazing excreta, humification fractions, CRU air
  temperature and the HWSD texture products are always computed locally, so
  the pinned and run-derived paths cannot silently disagree.
- **Regenerate the LPJmL-derived pins together, from one run.** Four pins carry
  LPJmL model *output* — `lpjml-grass-availability`,
  `lpjml-grass-productivity`, `lpjml-grass-natural-net-c` and
  `lpjml-soc-hydrology`. Use the single entry point
  `regenerate_whep_lpjml_pins()` in the `~/whep_inputs` project: dry-run by
  default (it prints a manifest plus the change against each pin it would
  replace), `upload = TRUE` to publish. Refreshing only some of them leaves
  WHEP mixing two LPJmL versions across its feed, soil-carbon and water chains
  at once — worse than consistently using either version, and invisible
  downstream because every pin still loads with the right schema. The six
  `lpjml-wind-*` / `lpjml-rsds-*` / `lpjml-rlds-*` pins are climate **forcing**
  (they feed *into* LPJmL, so they do not change with the model version) and
  must never go through that path; they come from
  `inst/scripts/prepare_spatialize_all.R`.
- **LPJmL output variable names are version-dependent.** 6.x renames some
  outputs to their CF short names — `mprec.nc` holds `pr` where 5.x held
  `prec`. Readers resolve the name against what the file actually contains
  (`.hydro_var_aliases()`), because a run directory carries no version stamp
  and both versions' output can sit side by side on one machine. Add the next
  rename there; the reader aborts listing the file's actual variables rather
  than failing on a `NULL` lookup.

## Package data updates

When modifying CSV files in `inst/extdata/harmonization/`:

1. Edit the CSV.
2. Run `Rscript data-raw/harmonization_tables.R` to rebuild `.rda` files.
3. Run `Rscript data-raw/table_mappings.R` if `regions.csv` or `items_*.csv`
   changed.
4. Run `Rscript data-raw/whep_inputs.R` if `whep_inputs.csv` changed.

## This is the only agent instruction file

The repo used to carry per-tool copies of these rules
(`.github/copilot-instructions.md`, `.agent/rules/whep.md`). They drifted —
one still forbade `data.table`, which the package now Imports — so they were
deleted. Do not reintroduce a copy: if a tool needs its own entry point, make
that file a pointer to this one.
