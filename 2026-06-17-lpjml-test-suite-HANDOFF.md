<!-- markdownlint-disable -->
# LPJmL Validation Suite — Implementation Handoff (whep)

**For:** Catalin (lbm364dl) · **From:** Eduardo · **Date:** 2026-06-17 · **Status:** spec complete, location + engine decided, 5 design decisions pending (to settle together)

## Why this exists

A whep-input LPJmL 5.9.7 run (2026-06-09, `…global_1901-2009_…_grassland_livestock_fix`) **exited cleanly, wrote all 136 outputs, and was scientifically dead**: 97% of land cells had `NPP=0`/`VegC=0` (Amazon/Congo/Europe flatlined) from a temperature-input grid/coverage mismatch, while precip + discharge looked global. Nothing flagged it. **A run completing is not a run being valid.** This suite makes that class of failure — and the input defects that cause it — impossible to pass silently.

## What it is

A tiered, source-anchored validation suite **in the whep R package**, extending the existing `inst/analysis/spatialize/validate_lpjml_compat.R` `.check()`/results-tibble pattern. One engine, organised on three axes:

- **3 stages** — `input` (a fail-fast gate *before* mpirun, zero compute), `output` (post-run), `paired` (whep vs stock). Selected by a registry `stage` column.
- **2 regimes** — `stock` (the control: must reproduce von Bloh 2018 / Schaphoff 2018 *tightly*) and `whep` (the experiment: *wider* plausibility bands + input-specific checks). Selected by a registry `regime` column; `both_modified` checks are two rows (stock-tight, whep-wide).
- **Tiers × severity** — structural → plausibility → mass-balance → benchmark; fatal / error / warning.

Entry points: `validate_lpjml_inputs(input_dir, input_regime, grid_file, config)` (pre-run gate), `validate_lpjml_run(run_dir, input_regime)` (post-run), `validate_lpjml_pair(whep_run, stock_run)` (cross-run). All thresholds live in a versioned check-registry CSV with inline source citations.

**Verified to do its job:** the paired `live-fraction-delta` catches the dead run (whep 0.03 vs stock 0.95); the input gate catches all four historical failures (A temp<precip coverage, B landuse sum>1, C lsuha=2174, D rejected unit) *before* the run; ~440 candidate checks adversarially reviewed.

## Four verified whep code bugs this work surfaced (fix at origin)

Independent of building the suite — found by reading the actual driver-writing code:

- **(a)** `.lsuha_nc_create` writes `units="LSU/ha"`, which the real run's LPJmL **rejected** (`WARNING404 … assume none`). Write the accepted unit string. `prepare_spatialize_all.R:159`
- **(b)** axis-name inconsistency: lsuha NC uses `lon`/`lat` (`:150-151`); all other whep drivers use `longitude`/`latitude`. Normalise.
- **(c)** ndep named `*_annual_*` but its writer builds a months-since 12-step axis (`:300`). Reconcile.
- **(d)** `whep:::.get_l_files_dir()` is called in 6 files but **defined nowhere** — stale refs after a refactor (so `validate_lpjml_compat.R` + 4 figure scripts + the methods qmd are currently broken). Replace with `Sys.getenv("WHEP_L_FILES_DIR")`. **This is a live breakage.**

## Pending decisions (Eduardo + Catalin)

Decided: **location = in whep**, **engine = R**. Open (detailed in Part 1 §"Pending decisions" and flagged at each point of use): (1) `R/` package rigor vs looser `inst/` script; (2) ship placeholder bands as non-blocking vs block first run; (3) crop DM→C fraction (read `pft.par` vs pin IPCC 0.47); (4) lsuha cap = 4 hard vs soft-warn; (5) NBP sign convention (read off first healthy run). Adversarial-review fixes to apply when building the registry are recorded at the end of Appendices B and C.

## Contents

- **Part 1 — Implementation plan** (Phases 0–5, task-by-task, TDD, exact file paths).
- **Appendix A — Verified threshold numbers** (136 source-attributed; verified / derived / consensus / placeholder).
- **Appendix B — Regime classification** (126 checks tagged stock/whep + the 5 review fixes).
- **Appendix C — Input-validation catalogue** (177 pre-run checks + the 5 review fixes + the code-bug list).
- **Appendix D — Codebase grounding** (how it extends the existing validators; exact line refs).

---


# Part 1 — Implementation plan

# Implementation Plan: WHEP LPJmL Run-Validation Test Suite

> **REQUIRED SUB-SKILL:** `superpowers:test-driven-development` — the corrupted-fixture tests in Phase 1 are written and made to fail/pass FIRST, before any check logic exists. Read it before starting Phase 1.
>
> All execution steps use checkbox syntax (`- [ ]`). Tick (`- [x]`) in the same edit that executes the step; never let the plan drift from reality.

## Location — IN WHEP (decided 2026-06-17)

The suite lives **in the whep R package**. whep both generates the LPJmL inputs (`prepare_spatialize_all.R`) and consumes the outputs (the livestock-intake availability read-back), so validation belongs where both ends already are; it reuses the existing `.check()` results-tibble, `lpjmlkit`/`calc_cellarea`, the grid/band conventions, and the `run_lpjml.R` hooks — no manifest-export contract needed. **Dependency hygiene:** keep `lpjmlkit`/`terra` in `Suggests` behind `rlang::is_installed()` guards, and the observational-benchmark tier optional + `skip_on_ci`, so downstream AFE repos that depend on whep are not forced to install them. The table below records the alternatives considered (standalone Python/R), kept only for reference:

| concern | in-whep (this plan) | standalone Python | standalone R |
|---|---|---|---|
| entry point | `whep::validate_lpjml_run()` | `lpjml_runqa` module / CLI | exported fn in new pkg |
| result object | `tibble(check, status, detail)` | list-of-dicts → pandas | same tibble |
| NetCDF/clm I/O | `lpjmlkit::read_io` | `xarray` + a `.clm` reader (the stable path this session) | `lpjmlkit::read_io` |
| grid/band contract | read directly from whep | whep exports a small grid/band/expected-vars manifest | depend on whep |
| check registry | `inst/extdata/.../check_registry.csv` | `registry.yaml`/`.csv` in repo | `inst/extdata` |
| fixtures/tests | `testthat` + `tests/testthat/fixtures/` | `pytest` + `tests/fixtures/` | `testthat` |
| post-run wiring | gate in `run_lpjml.R` | post-run hook in the run pipeline | gate in run pipeline |

If **standalone-Python** is chosen (my recommendation — runs live on the HPC and xarray was the stable engine all session), the only net-new work vs this plan is a `.clm` grid reader (≈ Task 0.1's `read_lpjml_grid` in Python) and a whep manifest exporter; the ~70-check catalogue, the numbers, and the phasing are unchanged.

## Goal

Build a tiered, source-anchored validation suite that scans actual LPJmL 5.9.7 simulation outputs (NetCDF + globalflux) after a `run_lpjml()` call and refuses to certify a run that exhibits the documented "completed but 97% of vegetated cells have NPP = 0" failure or any other physical-implausibility, mass-balance, or benchmark-divergence defect.

## Architecture

The suite extends the existing `inst/analysis/spatialize/` validators (which today only check the `gridded_landuse.parquet` *input*) with a new package-level `validate_lpjml_run()` whose checks are driven by a versioned check-registry CSV (`check_id`, variables, tolerance band, source citation, tier) and which emits the same tidy `tibble(check, status, detail)` schema the current `.check()` helper produces. Each check reads LPJmL output via `lpjmlkit::read_io()` behind a `rlang::is_installed()` guard (added to `Suggests`), aggregates per-cell/per-biome (gameable global scalars are hardened into spatial-coverage forms), and compares against verified thresholds embedded inline with their citations. Checks are organized into Tier-0 structural gates, Tier-1 liveness tripwires (the redundant catch for the NPP-collapse failure), Tier-2 mass-balance closure, Tier-3 physical-plausibility breadth, Tier-4 stock-control benchmark, and Tier-5 observational benchmarking. The suite spans **three stages** sharing this one engine — `input` (a fail-fast gate *before* the run), `output` (post-run), and `paired` (whep vs stock) — selected by a `stage` registry column (see Validation stages, below).

## Tech Stack

- R package `whep` (tidyverse style, `air format`, roxygen2, testthat 3e).
- `lpjmlkit` (`read_io`, `as_array`, `calc_cellarea`) for binary/NetCDF reading — added to `Suggests`, every call guarded by `rlang::is_installed("lpjmlkit")`.
- `nanoparquet`/`arrow` (already Imports) for the gridded CSV/parquet integral cross-checks.
- `cli` for the report surface; `data.table` permitted inside `.`-helpers; `readr` for the registry CSV.
- Fixtures: tiny synthetic NetCDF written with `lpjmlkit` writers (or hand-built arrays) under `tests/testthat/fixtures/lpjml_run/`.

---

## Regime model — stock vs whep (dual-run)

The suite validates TWO run regimes, and the registry encodes which checks apply to which:

- **Stock/canonical run** (`input_regime = "stock"`): LPJmL on its own published inputs (CRU climate, standard LUH2 land use, stock lsuha, 67,420-cell CRU mask). The **control** — must reproduce published LPJmL values (von Bloh 2018 Table 3: NPP 64.07 / GPP 131.8 / VegC 444 / SoilC 2049 PgC; Schaphoff 2018) within *tight* tolerance. A stock run that misses published values implicates the model build/config, not the inputs.
- **whep run** (`input_regime = "whep"`): LPJmL on whep inputs (LUH2 land use + grass band, capped lsuha, whep ndep, NaturalEarth ~58.8k-cell mask). The **experiment** — physically plausible but legitimately different; *wider* plausibility bands + input-specific checks.

Every check carries a `regime` tag across four categories, plus a paired mode:

| category | meaning | mechanism |
|---|---|---|
| `both_identical` | input-agnostic correctness law (closure, signs, no-NaN, liveness) — same threshold | one row, `regime = both` |
| `both_modified` | same check, **stock tight-to-published / whep wider** | **two rows**, `regime = stock` + `regime = whep` |
| `whep_only` | references whep input/grid (cftfrac==input, lsuha cap/unit, prescribe_lsuha, NaturalEarth grid) | one row, `regime = whep` |
| `stock_only` | reproduce-published control (von Bloh/Schaphoff, tight) | one row, `regime = stock` |
| `paired` | needs both runs (`bench.vs-stock-*`, `paired.live-fraction-delta`) | `validate_lpjml_pair()` |

Entry points: `validate_lpjml_run(run_dir, input_regime = c("whep","stock"))` runs checks where `regime in {both, input_regime}`; `validate_lpjml_pair(whep_run, stock_run)` runs the paired checks. The **paired** mode is the single most powerful diagnostic: `paired.live-fraction-delta` (whep live-cell fraction not catastrophically below stock's) would have screamed on the 97%-dead whep run while the stock run was alive. The exhaustive per-check regime tagging + the stock/whep bands live in the companion doc `2026-06-09-lpjml-regime-classification.md` and become the `regime`, `stock_lo/stock_hi`, `whep_lo/whep_hi` columns of the registry CSV (Task 1.3).

---

## Validation stages — input (pre-run) / output (post-run) / paired

One shared engine (the `.check`/registry/grid helpers), three stages selected by a registry `stage` column:

| stage | when | entry point | catches |
|---|---|---|---|
| `input` | **before** mpirun (fail-fast gate in `run_lpjml.R`) | `validate_lpjml_inputs(input_dir, input_regime, grid_file, config)` | A–D at ORIGIN, at zero compute cost |
| `output` | after the run | `validate_lpjml_run(run_dir, input_regime)` | physical / closure / benchmark defects in results |
| `paired` | after both runs | `validate_lpjml_pair(whep_run, stock_run)` | whep-vs-stock divergence |

The **input stage is the highest-ROI part of the suite**: the 97%-dead run (A) came from a temperature driver with fewer valid cells than precip — a pre-run `in.climate.temp-coverage >= precip-coverage` check aborts *before* the ~1 h run. B (landuse sum>1), C (lsuha=2174), D (bad unit string) are likewise input defects caught pre-run. It complements the existing upstream `validate_lpjml_compat.R` (which checks the whep *parquets*); the input stage checks the actual LPJmL-ready *driver files* (NetCDF/.clm), that each echoes its parquet source, and that all drivers share one grid + cover the simulation years. Input checks carry the same `regime` tag (whep drivers vs stock drivers) and their own tiers: structural → value_range → cross_consistency → provenance. The exhaustive per-driver input-check catalogue lives in companion doc `2026-06-09-lpjml-input-validation.md`; it becomes the `stage = input` rows of the registry CSV, and the pre-run gate is wired into `run_lpjml.R` (a new Phase 1 task, before the output gate).

---

## Phase 0 — PREREQS (canonical grid, log capture, output manifest)

Nothing in Phase 1+ can be trusted until the grid that the run *actually used* is the reference grid (not the hardcoded `67420L`), the mpirun output is captured to a scannable log, and the output config is known to emit the required variables.

### Task 0.1 — Resolve the canonical run grid and make `grid.cellcount` EXACT

**Files**
- Create: `R/lpjml_grid.R`
- Create: `tests/testthat/test_lpjml_grid.R`
- Modify: `R/utils.R` (add NSE globals)

- [ ] Create exported `read_lpjml_grid(grid_file)` that wraps `lpjmlkit::read_io(grid_file)`, guarded by `if (!rlang::is_installed("lpjmlkit")) cli::cli_abort("lpjmlkit required to read the run grid.")`.
- [ ] Return a tibble `cell` (1-based), `lon`, `lat` (`round(..., 2)` per `compare_lpjml_v6.R:69-70`), `cell_area_m2 = as.vector(lpjmlkit::calc_cellarea(grid_dat))` (the canonical cell-area source, `compare_lpjml_v6.R:71`), `land_area_m2 = cell_area_m2 * landfrac` where `landfrac` comes from `landfrac_gadm36.clm` if present else `rep(1, n)` (`compare_lpjml_v6.R:73-77`).
- [ ] Add `grid_ncell <- nrow(grid)` as an attribute; do NOT hardcode `67420L` — that constant is the *stock CRU mask* count and the whep run uses the ~58,765-cell NaturalEarth mask. The check `grid.cellcount` becomes EXACT against `nrow(read_lpjml_grid(<run grid>))`, not against a literal.
- [ ] Provide `.lpjml_global_cell_index(lon, lat)` private helper implementing the row-major N-to-S land index `(90 - lat - 0.25)/0.5 * 720 + (lon + 179.75)/0.5` (`LPJML_COMPATIBILITY.md:91`) for the parquet→CLM ordering cross-check used in Phase 2.
- [ ] Declare all new NSE names (`cell`, `lon`, `lat`, `cell_area_m2`, `land_area_m2`, `landfrac`) in `utils::globalVariables()` in `R/utils.R`.
- [ ] Add `check_id = "grid.cellcount"`: PASS iff `nrow(output_grid) == nrow(run_grid)` exactly (no tolerance — same file). Detail string reports both counts and, informationally, the delta vs the stock `67420L` (preserving the "informational, accept the ~12.8% NaturalEarth-vs-CRU gap" stance of current check 13, `validate_lpjml_compat.R:584`; `LPJML_COMPATIBILITY.md:192`).
- [ ] Test: build a 3-cell synthetic grid array, assert `read_lpjml_grid()` returns 3 rows, `cell_area_m2 > 0`, and `grid.cellcount` PASSes when output grid == run grid and FAILs when a cell is dropped.

### Task 0.2 — Add stdout/stderr log capture to `run_lpjml.R`

`run_lpjml.R:141-146` discards mpirun output; no log exists to scan. This task creates the canonical log path and tees output.

**Files**
- Modify: `inst/scripts/run_lpjml.R`

- [ ] Define canonical log path: `log_file <- file.path(sim_path, "output", "lpjml_run.log")`; `dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)`.
- [ ] Append a tee to the run command so the mpirun banner, the per-year progress, and any `ERROR`/`Warning` lines are persisted: `run_cmd = stringr::str_glue("mpirun -np {use_cores} 2>&1 | tee {log_file} && ")`. (Verify `lpjmlkit::run_lpjml`'s `run_cmd` is prepended to the executable invocation; if it instead `system2()`s without a shell, fall back to capturing via `withr::with_output_sink()` around the call, or `system(..., intern = TRUE)` writing to `log_file`.)
- [ ] Return `invisible(list(sim_path = sim_path, log_file = log_file, config = cfg))` instead of `invisible(NULL)` so the post-run gate (Task 1.9) can locate both log and outputs.
- [ ] Update the file header comment block (`run_lpjml.R:1-11`) to document the new return value and the log path. Per "always update docs after fixes", grep `inst/` + `documentation/` for any caller asserting `run_lpjml()` returns `NULL` and update.

### Task 0.3 — Output-config completeness manifest

The NOTE at `run_lpjml.R:128-131` is the contract: the output cjson must emit `pft_npp`, `cftfrac`, and the nine `*_mgrass` C+N terms. Today nothing verifies the config requested them.

**Files**
- Create: `inst/extdata/lpjml/required_outputs.csv`
- Create: `R/lpjml_output_manifest.R`
- Create: `tests/testthat/test_lpjml_output_manifest.R`

- [ ] Write `required_outputs.csv` with columns `variable`, `tier`, `purpose`: the liveness set (`npp`, `gpp`, `vegc`, `soilc`, `runoff`, `transp`, `evap`, `interc`, `nuptake`, `leaching`, `n2o_denit`, `n2o_nit`, `bnf`), the grazing set (`pft_npp`, `cftfrac`, `uptakec_mgrass`, `yieldc_mgrass`, `yieldn_mgrass`, `fecesc_mgrass`, `fecesn_mgrass`, `urinec_mgrass`, `urinen_mgrass`, `respc_mgrass`, `methanec_mgrass`), and the thermal/water set (`soiltemp`, `swc`, `discharge`).
- [ ] Create exported `check_lpjml_output_manifest(sim_path)` that parses the written `config_*.json` (or scans `<sim_path>/output/*.{nc,bin,clm}` filenames) and adds `check_id = "manifest.required-outputs-present"`: FAIL listing any `required_outputs.csv` row whose file/config entry is absent. This is a Tier-0 hard gate — a run missing `*_mgrass` cannot be grazing-validated.
- [ ] Test: a config manifest missing `methanec_mgrass` FAILs with that variable named; a complete one PASSes.

---

## Phase 1 — FOUNDATION + A–D TRIPWIRES (TDD: fixtures FIRST)

### Task 1.1 — Corrupted-fixture CI tests (WRITTEN AND FAILING FIRST)

Per `test-driven-development`: these tests are authored before `validate_lpjml_run()` exists, so they fail to even find the function; the skeleton in Task 1.5 makes them pass.

**Files**
- Create: `tests/testthat/test_validate_lpjml_run.R`
- Create: `tests/testthat/fixtures/lpjml_run/make_fixtures.R`
- Create (fixtures, written by the script above): `tests/testthat/fixtures/lpjml_run/{healthy,A_dead_npp,B_landuse_sum_gt1,C_lsuha_2174,D_bad_unit}/`

- [ ] Write `make_fixtures.R` that builds five tiny LPJmL output sets (a 12-cell grid × 3 years), each a folder of minimal NetCDFs readable by `lpjmlkit::read_io()` plus a matching `grid.clm` and the gridded-landuse CSV used for the integral cross-check:
  - `healthy/` — NPP ∈ [200, 900] gC/m²/yr on all 12 vegetated cells, ET/P ≈ 0.6, plant N uptake > 0, lsuha ≤ 4, landuse fractions sum ≤ 1.
  - `A_dead_npp/` — NPP = 0 on 11 of 12 cells (the 97% failure miniature); everything else healthy. Must trip `npp.valid-fraction`.
  - `B_landuse_sum_gt1/` — one cell's CFT fractions sum to 1.4. Must trip `lu.fraction-sum`.
  - `C_lsuha_2174/` — one cell's prescribed stocking = 2174 LSU/ha (the canonical bad-denominator value). Must trip `graze.lsuha-cap`.
  - `D_bad_unit/` — NPP stored in kgC/m² (×1000 off) so global NPP reads ~64,000 PgC. Must trip `npp.global-band`.
- [ ] In `test_validate_lpjml_run.R`, one `test_that()` per fixture:
  - healthy → `validate_lpjml_run(<healthy>)$status` has zero `"FAIL"` among Tier-0/Tier-1 checks.
  - A → result row `check == "npp.valid-fraction"` has `status == "FAIL"`.
  - B → `lu.fraction-sum` FAILs.
  - C → `graze.lsuha-cap` FAILs.
  - D → `npp.global-band` FAILs.
- [ ] Guard the whole file with `testthat::skip_if_not_installed("lpjmlkit")` so CI without lpjmlkit skips rather than errors (mirrors the `Suggests` guard).
- [ ] Run `devtools::test(filter = "validate_lpjml_run")` and confirm all five tests FAIL with "could not find function validate_lpjml_run" (the TDD red state). Record the failure in the step note.

### Task 1.2 — Add `lpjmlkit` to Suggests; fix the broken l_files resolution

Principle 0 / fix-at-origin: `validate_lpjml_compat.R:41` calls `whep:::.get_l_files_dir()`, which does not exist (grepped: zero matches; `utils.R` is only `globalVariables`). The script errors before any check runs.

**Files**
- Modify: `DESCRIPTION`
- Modify: `inst/analysis/spatialize/validate_lpjml_compat.R`
- Create: `R/lpjml_paths.R`

- [ ] Add `lpjmlkit` to `Suggests:` in `DESCRIPTION` (it is currently in neither Imports nor Suggests; `compare_lpjml_v6.R:28` and `run_lpjml.R` `library()` it from a user copy).
- [ ] Create exported `whep_l_files_dir()` in `R/lpjml_paths.R` consolidating the three drifted patterns into one: `dir <- Sys.getenv("WHEP_L_FILES_DIR", unset = ""); if (!nzchar(dir) || !dir.exists(dir)) cli::cli_abort("Set WHEP_L_FILES_DIR to the L_files root.")` (the working pattern from `compare_lpjml_v6.R:31-32`; `run_spatialize.R:338-345` passes it as an explicit arg, which remains valid).
- [ ] Replace `validate_lpjml_compat.R:41` `whep:::.get_l_files_dir()` with `whep::whep_l_files_dir()`. Grep `inst/`, `R/`, `tests/`, `documentation/` for any other `.get_l_files_dir` reference and update (expected: only line 41).
- [ ] Roxygen-document `whep_l_files_dir()`; add it to `_pkgdown.yml` under a new `LPJmL validation` reference section.

### Task 1.3 — Check-registry CSV + loader

**Files**
- Create: `inst/extdata/lpjml/check_registry.csv`
- Create: `R/lpjml_check_registry.R`
- Create: `tests/testthat/test_lpjml_check_registry.R`

- [ ] Define `check_registry.csv` columns: `check_id`, `tier` (0–5), `variables` (semicolon-separated output names), `aggregation` (`valid_fraction`/`global_total`/`per_biome_median`/`ratio`/`spatial_r`/`integral_eq`), `low`, `high`, `units`, `source`, `confidence` (`verified`/`derived`/`consensus`/`placeholder`), `calibrate` (`TRUE`/`FALSE`).
- [ ] Populate it with every threshold from the verified Number Registry (Phases 1–5 below cite the per-row `check_id` and band). Rows marked `confidence = placeholder` carry `calibrate = TRUE` and `low`/`high` left blank → loader sets them `NA` and the runner emits `status = NA` ("CALIBRATE") not PASS/FAIL.
- [ ] Create exported `load_lpjml_check_registry()` reading the CSV with `readr::read_csv()`, returning a tibble; validate with `pointblank` that `tier ∈ 0:5`, `confidence` ∈ the four levels, and `calibrate == TRUE ⟺ is.na(low)`.
- [ ] Test: registry loads, has ≥ 1 row per tier, every non-calibrate row has finite `low <= high`, every row's `source` is non-empty.

### Task 1.4 — The `.check()`/results-tibble contract, reused verbatim

**Files**
- Create: `R/lpjml_run_report.R`

- [ ] Port the `.check(name, passed, detail)` helper from `validate_lpjml_compat.R:80-88` as private `.lpjml_check()`, with the SKIP semantics fixed at origin: `status <- if (is.na(passed)) NA_character_ else if (passed) "PASS" else "FAIL"`, and emit `cli::cli_alert_info()` (not `cli_alert_success`) on `NA` — closing the cosmetic bug noted at `validate_lpjml_compat.R` (NA currently fires `cli_alert_success`).
- [ ] Keep the exact output schema `tibble(check = chr, status = chr ∈ {PASS,FAIL,NA}, detail = chr)` and the accumulation idiom `results[[length(results) + 1L]] <- .lpjml_check(...)`, bound with `dplyr::bind_rows()` (`validate_lpjml_compat.R:629`).
- [ ] Add `.summarise_lpjml(results_df)` returning `n_pass`/`n_fail`/`n_skip` via `sum(... , na.rm = TRUE)` (`validate_lpjml_compat.R:631-638`) and an overall `certified <- n_fail == 0L`.

### Task 1.5 — `validate_lpjml_run()` skeleton

**Files**
- Create: `R/validate_lpjml_run.R`
- Modify: `R/utils.R`
- Modify: `_pkgdown.yml`

- [ ] Create exported `validate_lpjml_run(sim_path, tiers = 0:3, benchmark_sim_path = NULL, obs_paths = NULL)`: reads the run grid (Task 0.1), reads each output via a private `.read_lpjml_output(sim_path, var)` (`lpjmlkit::read_io` + `as_array`, guarded), loads the registry (Task 1.3), dispatches each registry row whose `tier %in% tiers` to its aggregation handler, accumulates `.lpjml_check()` rows, and returns `list(results = results_df, summary = .summarise_lpjml(results_df), diag = diag)`.
- [ ] Group ≤ 5 args: collapse `benchmark_sim_path`/`obs_paths` into a single named `refs = list(stock = , obs = )` argument to respect the ≤ 5-arg rule.
- [ ] Keep the function ≤ 25 lines by delegating each tier to `.run_tier0()`, `.run_tier1()`, … private helpers (Tasks 1.6–1.8, Phases 2–5).
- [ ] Declare all NSE column names used across handlers in `utils::globalVariables()`.
- [ ] Roxygen-document `validate_lpjml_run()` with an `example = FALSE` toy fixture (per CLAUDE.md examples rule — real run needs lpjmlkit + L_files); add a `R/toy_examples.R` helper `.toy_lpjml_run_report()` returning ~10 sampled result rows.
- [ ] Run `devtools::test(filter = "validate_lpjml_run")`: the five Task 1.1 tests now reach the function (green for healthy/A/B/C/D once Tasks 1.6–1.8 land the actual checks).

### Task 1.6 — Tier-0 structural gates

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier0()`)

- [ ] `grid.cellcount` (Task 0.1): exact equality output grid vs run grid.
- [ ] `manifest.required-outputs-present` (Task 0.3): all `required_outputs.csv` variables emitted.
- [ ] `struct.no-nonfinite`: 0 NA/NaN/Inf across every loaded output array (extends current check 4, `validate_lpjml_compat.R`). FAIL with per-variable counts.
- [ ] `struct.temporal-complete`: `setdiff(seq(firstyear, lastyear), years_present)` empty for every time-varying output (extends check 2).
- [ ] `lu.fraction-sum` (fixture B): per cell-year, Σ(all CFT + grass + natural fractions) ≤ 1 + 1e-6. FAIL listing worst cell and its sum. Threshold 1.0 is a definitional invariant (fractional land use), not literature.
- [ ] `log.no-fatal-errors`: scan `lpjml_run.log` (Task 0.2) for `grep` of `^ERROR|Segmentation|not converged|NaN detected`. FAIL listing matched lines. This is the only check that reads the log.

### Task 1.7 — Tier-1 liveness tripwires (the redundant NPP-collapse catch)

Every threshold below is embedded inline; placeholders are written but emit `NA`/CALIBRATE, not invented bands.

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier1()`)
- Modify: `R/lpjml_biomes.R` (Task 1.8 biome mask, used here)

- [ ] `npp.valid-fraction` (fixture A — the single most diagnostic test): mask true desert/ice cells first (Köppen BW + ice, Task 1.8), then `frac_alive <- mean(npp_cell > 0)`. Provisional rule PASS iff `frac_alive >= 0.90` (failing run ≈ 0.03); `calibrate = TRUE` — **CALIBRATE the 0.90 floor from first healthy 5.9.7 run** (run-health statistic, no published value). Detail reports `frac_alive` and the dead-cell count.
- [ ] `npp.dead-fraction`: mirror, FAIL iff `mean(npp_cell == 0 on vegetated cells) > 0.15–0.20` (failing = 0.97). `calibrate = TRUE`.
- [ ] `npp.global-band` (fixture D): global NPP ∈ **[45, 82] PgC/yr** (von Bloh 2018 GMD 11:2789 Table 3: LPJmL5 64.07; no-N-limit 80.27; PNV 76.88; LPJmL3.5 57.12). `confidence = verified`. Catches the ×1000 unit error.
- [ ] `vegc.valid-fraction`: fraction of vegetated cells with VegC > 0 ≥ baseline; `calibrate = TRUE` from first healthy run.
- [ ] `ti.npp-alive-every-year`: for EVERY simulated year, `mean(npp_cell > 0 on vegetated cells) >= floor` — catches a mid-run collapse a single-year mean would mask. `calibrate = TRUE`.
- [ ] `lu.dead-cropland-fraction`: fraction of prescribed-cropland cells returning zero crop NPP/yield must be near 0; `calibrate = TRUE`. Sentinel that crops, not just natural veg, are alive.
- [ ] `n.uptake-alive-fraction`: fraction of land cells with positive plant N uptake ≥ 0.50 floor (suggest), healthy ≈ 0.90+; `calibrate = TRUE`. Decisive corroborator: global plant N uptake must be hundreds of Tg N/yr, never ~0.
- [ ] `t.cold-limitation-pattern`: land-mean 2 m air temperature ∈ **[6, 11] °C** (land-only SAT consensus ~8.5 °C; CRU TS/CRUTEM5, Osborn 2021; `confidence = consensus`); additionally flag all-zero/NaN soil-T field. An independent cross-check of the temperature-grid mismatch that zeroed NPP.
- [ ] `npp.major-biomes`: per major biome (tropical/temperate/boreal forest, grassland), assert BOTH (a) alive-fraction ≥ floor AND (b) area-weighted median NPP within the per-biome band — tropical evergreen median NPP ∈ **[400, 1300] gC/m²/yr** (Luyssaert 2007 GCB 13:2509 Table 3: 864 ± 96; `verified`), temperate evergreen **[400, 1200]** (783 ± 45), boreal evergreen **[100, 550]** (271 ± 17). Hardens the gameable global-mean into a per-biome form a dead-tropics + lush-temperate run cannot pass.
- [ ] `npp.vs-precip-coverage`: of cells with annual precip > 400 mm (i.e. should be vegetated), the fraction with NPP > 0 ≥ floor; `calibrate = TRUE`. Couples the NPP field to the precip forcing so a grid-misaligned NPP shows as wet-but-dead cells.
- [ ] `grid.integral-eq-csv`: Σ(gridded output × cell_area) reconciles to the WHEP gridded-landuse CSV totals within rel-error < 1e-4 (extends checks 6/7, `validate_lpjml_compat.R`; uses `.lpjml_global_cell_index` ordering from Task 0.1). Invariant tolerance, not literature.

### Task 1.8 — Biome / desert-ice mask helper

**Files**
- Create: `R/lpjml_biomes.R`
- Create: `tests/testthat/test_lpjml_biomes.R`

- [ ] Exported `lpjml_biome_mask(grid, clim_paths)` classifying each grid cell to a coarse biome (tropical/temperate/boreal forest, grassland, desert, ice) from the LPJmL climate forcing (temperature + precip) so liveness checks can legitimately exclude BW-desert/ice cells (where NPP = 0 is correct) before computing alive-fractions.
- [ ] Return tibble `cell`, `biome`, `is_vegetated` (logical). Document the rule (e.g. Köppen-style thresholds) inline.
- [ ] Test: a synthetic hot-dry cell → `desert`, `is_vegetated = FALSE`; a warm-wet cell → `tropical_forest`, `TRUE`.

### Task 1.9 — Wire as a post-run gate in `run_lpjml.R`

**Files**
- Modify: `inst/scripts/run_lpjml.R`

- [ ] After `lpjmlkit::run_lpjml(...)`, call `report <- whep::validate_lpjml_run(sim_path, tiers = 0:1)` (Tier-0/1 = the fast tripwires).
- [ ] If `!report$summary$certified`, `cli::cli_warn()` listing the FAILed `check_id`s and write `report$results` to `file.path(sim_path, "output", "validation_lpjml_run.csv")`; do NOT silently treat the run as good. Return the report inside the Task 0.2 return list.

---

## Phase 2 — MASS-BALANCE (per-cell AND global closure)

These are arithmetic identities that must hold regardless of band; bands are not literature thresholds but the closure tolerance is.

### Task 2.1 — Carbon closure

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier2_carbon()`)

- [ ] `c.npp-lt-gpp`: per cell AND global, NPP < GPP. FAIL listing violating cells.
- [ ] `c.cue-band`: CUE = NPP/GPP ∈ **[0.42, 0.58]** (derived from von Bloh 2018 Table 3, 0.44–0.49 across configs; `derived`), per-cell median and global.
- [ ] `c.ra-identity`: Ra = GPP − NPP ∈ **[55, 95] PgC/yr** (identity from von Bloh 2018 Table 3, LPJmL5 ≈ 68; `derived`).
- [ ] `c.nbp-closure`: NPP − Rh − fire − harvest ≈ NBP within closure tolerance (steady-state). Report residual; the **NBP magnitude/sign band is `calibrate = TRUE`** (`|NBP| < ~5 PgC/yr` sanity only) because the 5.9.7 sign convention differs from the real land-sink sign (von Bloh +1.21 = managed-grassland source vs GCB +3.3 sink) — **read the sign convention off the first healthy run before any sign test**.
- [ ] `c.rh-band`: Rh ∈ **[45, 75] PgC/yr** (steady-state closure; lit ~50–60; `consensus`).
- [ ] `c.harvest-global`: crop + grazing harvest C ∈ O(2–5) PgC/yr expectation; `calibrate = TRUE` (LPJmL papers report kcal or t DM/ha, never aggregate PgC).

### Task 2.2 — Water closure

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier2_water()`)

- [ ] `w.et-p-ratio`: ET/P ∈ **[0.50, 0.67]** (Oki & Kanae 2006 Science 313:1068 ">60% returns as ET"; `verified`); flag if < 0.45 or > 0.70. **Primary water failure signature: ET/P → 0 is the unmistakable mark of the temperature-grid mismatch.**
- [ ] `w.runoff-p-ratio`: runoff/P ∈ **[0.33, 0.50]** (Oki & Kanae 45.5/111 = 0.41; Haddeland 2011 JHM 12:869; `verified`).
- [ ] `w.closure`: ET/P + runoff/P ≈ 1 at steady state; report residual.
- [ ] `w.transp-fraction`: T/ET ∈ **[0.45, 0.90]** (deliberately wide: Jasechko 2013 Nature 80–90% vs GLEAM4/model 50–65%; `verified`) — a collapse-detector (catch T/ET → 0), NOT an arbiter of the isotope-vs-model debate; do not narrow.

### Task 2.3 — Nitrogen closure

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier2_nitrogen()`)

- [ ] `n.uptake-global`: plant N uptake ∈ **[300, 1300] Tg N/yr** (von Bloh 2018 Table 4: LPJmL5 618; lit 620–1130; `verified`). Must be hundreds, never ~0.
- [ ] `n.leaching-le-inputs`: leaching ≤ Ndep + BNF + fertilizer (mass-balance bound), per-cell and global.
- [ ] `n.leaching-global`: ∈ **[20, 150] Tg N/yr** (von Bloh 2018 Table 4: 62.83; `verified`).
- [ ] `n.bnf-global`: ∈ **[50, 290] Tg N/yr** (von Bloh 2018 Table 4: 128.9; Galloway 2004 107; note the 5.9.7 BNF scheme was revised in Sigurdsson 2024 GMD 17:7889; `verified`).

### Task 2.4 — Grazing closure

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier2_grazing()`)

- [ ] `graze.lsuha-cap` (fixture C): prescribed stocking ≤ **4.0 LSU/ha** (Heinke 2023 GMD 16:2455 sweep 0–4 LSU/ha; matches repo "cap at 4 LSU/ha"). **Record this cap as a flagged temporary safeguard** in the detail string, not a silent constant, and add an at-origin denominator check: FAIL with the offending value (2174 in fixture C) when a cell's intake-per-area denominator implies an absurd stocking, pointing to the upstream `grassland_lsuha` builder rather than masking it.
- [ ] `graze.mgrass-balance`: per grazed cell, `uptakec_mgrass ≈ yieldc_mgrass + fecesc_mgrass + urinec_mgrass + respc_mgrass + methanec_mgrass` (C closure of the nine `*_mgrass` terms) within tolerance; analogous N closure for `yieldn/fecesn/urinen`. Report residual. This is the closure the `run_lpjml.R:128-131` NOTE anticipates.
- [ ] `graze.mgrass-nonzero`: on cells with prescribed lsuha > 0, the `*_mgrass` terms must be non-zero (else `grazing="livestock"`/`prescribe_lsuha=TRUE` silently failed). `calibrate = TRUE` baseline; FAIL if fraction-grazed-but-zero ≫ healthy.
- [ ] `graze.enteric-ym`: enteric Ym ∈ **[5.5, 7.5] % of gross energy** (Heinke 2023 GMD 16:2455 Eq.46, 6.5 ± 1.0; IPCC Tier-2 forage default; `verified`).

---

## Phase 3 — PHYSICAL-PLAUSIBILITY breadth

All remaining magnitude/bounds/biome/temporal checks across the six cycles, each with its verified number embedded. Hardened to per-cell/per-biome where a global scalar is gameable.

### Task 3.1 — Carbon pools & per-cell fields

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier3_carbon()`)

- [ ] `c.gpp-global`: ∈ **[100, 175] PgC/yr** (von Bloh 2018 Table 3: 131.8; PNV 171.1; Schaphoff 2018 Pt2 GMD 11:1377 LPJmL4 123.7; `verified`). **Implement ONCE** — same quantity as the benchmark-metrics GPP row; use this union band, not two checks.
- [ ] `c.vegc-global`: ∈ **[350, 650] PgC** (von Bloh 2018 Table 3: 444; PNV 627–679; Schaphoff 2018 Pt2 507; `verified`).
- [ ] `c.soilc-global`: ∈ **[1500, 3400] PgC** (whole profile incl. permafrost; von Bloh 2018 Table 3: 2049; PNV 2344; `verified`).
- [ ] `c.fire-global`: ∈ **[1.5, 4.0] PgC/yr** (Schaphoff 2018 Pt2: GlobFIRM 2.33, SPITFIRE 2.7–2.8; GFED4 ~2.0–2.5; `verified`), only if fire enabled.
- [ ] `c.litter-global`: loose `0 < x < 300 PgC`; `calibrate = TRUE` (not tabulated in von Bloh/Schaphoff).
- [ ] `c.npp-percell-max`: productive cells reach **[~1200, 1300] gC/m²/yr**, min 0 valid (Schaphoff 2018 Pt1 GMD 11:1343 Fig 4b; `verified`).
- [ ] `c.gpp-percell-max`: reaches **> 2000 (up to ~3500) gC/m²/yr** (Schaphoff 2018 Pt1 Fig 4a; `verified`).
- [ ] `c.vegc-percell-forest`: forested cells reach **[~18, 22] kgC/m²** (Schaphoff 2018 Pt1, tropics ~20; `verified`).
- [ ] `c.soilc-percell-boreal`: boreal/permafrost cells reach **[50, 70] kgC/m²** (Schaphoff 2018 Pt1, > 60 in permafrost; `verified`).

### Task 3.2 — Water magnitudes & river discharge

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier3_water()`)

- [ ] `w.precip-global`: land P ∈ **[100000, 120000] km³/yr** (Oki & Kanae 2006 ~111k; Trenberth 2007 GPCP; `verified`).
- [ ] `w.et-global`: ∈ **[55000, 85000] km³/yr** (Haddeland 2011 WaterMIP incl. LPJmL 60–85k; GLEAM4 Miralles 2025 68.5k; `verified`).
- [ ] `w.runoff-global`: ∈ **[38000, 66000] km³/yr** (Haddeland 2011 42–66k incl. LPJmL; `verified`). **Do NOT conflate with the gauged-to-ocean 37,288 km³/yr** (Dai & Trenberth 2002 JHM 3:660) — that benchmark excludes ungauged + endorheic, so LPJmL total legitimately runs 15–25% higher; kept as a separate `w.discharge-gauged-obs` informational row.
- [ ] `w.et-depth` ∈ **[390, 590] mm/yr**, `w.runoff-depth` ∈ **[270, 460] mm/yr** (Haddeland 2011; `verified`).
- [ ] `w.discharge-amazon` ∈ **[5400, 7000] km³/yr** (Dai & Trenberth ~6642; GRDC ~6591), and per-river rows for Congo **[1250, 1400]**, Orinoco **[1100, 1250]**, Ganges–Brahmaputra **[1000, 1350]**, Yangtze **[900, 1010]**, Mississippi **[580, 680]**, Yenisei **[580, 640]**, Lena **[520, 590]** (all Dai & Trenberth 2002 / GRDC; `verified`). Locate the discharge cell by nearest grid cell to each mouth lon/lat.

### Task 3.3 — Nitrogen magnitudes

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier3_nitrogen()`)

- [ ] `n.nh3-global` ∈ **[8, 60] Tg N/yr** (von Bloh 2018 Table 4: 20.46; `verified`).
- [ ] `n.n2o-total` ∈ **[7, 22] Tg N/yr** (von Bloh 2018 Table 4: 14.57; Tian 2020 Nature ~17; band brackets both framings — do not tighten; `verified`), with `n.n2o-denit` ∈ **[2, 10]** (5.47) and `n.n2o-nit` ∈ **[3, 15]** (9.10).
- [ ] `n.denit-total` ∈ **[20, 120] Tg N/yr** (von Bloh 2018 Table 4: 49.71; Bouwman 2013 25–85; `verified`).
- [ ] `n.vegpool-global` ∈ **[1.2, 6.0] Pg N** (von Bloh 2018 Table 4: 1.78; PNV 2.69; `verified`).
- [ ] `n.veg-cn-ratio` ∈ **[150, 320] gC/gN** (von Bloh 2018 Fig 6b ~235–260; **vegetation-wide only — do NOT compare a leaf-C:N output here**; `verified`).

### Task 3.4 — Vegetation / biome per-cell bounds (NetCDF)

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier3_vegetation()`)

- [ ] `veg.tropevergreen-gpp` per-cell ∈ **[2000, 4000] gC/m²/yr** (Luyssaert 2007 Table 3: 3551 ± 160 confirmed verbatim; `verified`); `veg.tropevergreen-agb` ∈ **[5, 20] kgC/m²** (11.4 ± 5.8).
- [ ] `veg.lai-max-tropical` ∈ **[3, 7]** and `veg.lai-max-temp-boreal` ∈ **[2.5, 7] m²/m²** (Luyssaert 2007 Table 5; LPJmL caps LAI at 7, von Bloh 2018 Eq.2; `verified`).
- [ ] Leaf-C:N hard PFT bounds (von Bloh 2018 Table 2, TRY/Kattge 2011; `verified`), one check per PFT: tropical broadleaf evergreen **[15.6, 46.2]**, tropical raingreen **[15.4, 34.6]**, temperate/boreal needleleaf evergreen **[31.8, 63.8]**, temperate broadleaf summergreen **[15.4, 34.6]**, C3 grass **[10.5, 37.9]**, C4 grass **[17.4, 66.9]** gC/gN.
- [ ] `veg.fpc-coverage`: tropical-forest FPC > 0.6, all-cell FPC > 0; `calibrate = TRUE` (LPJmL publishes no global fAPAR/FPC table; MOD15 0.7–1.0 order-of-magnitude only).

### Task 3.5 — Crops (FAOSTAT-basis self-validation)

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier3_crops()`)

- [ ] `crop.maize-yield-global` ∈ **[5.0, 6.8] t/ha fresh** (FAOSTAT QCL via FAO AB79 ~6.0 in 2020; `verified`), `crop.wheat-yield-global` ∈ **[3.0, 4.2]**, `crop.rice-yield-global` ∈ **[4.0, 5.2] t/ha paddy fresh**.
- [ ] `crop.yield-collapse-floor`: global-mean simulated yield gC/m² with LOW = 0 to catch the zero-NPP collapse — maize `[0, 360]`, wheat `[0, 250]`, rice `[0, 280]` gC/m² (derived from FAOSTAT means × moisture × C-fraction; `derived`). **Per-cell hard caps (maize ~700, wheat ~400, paddy ~450 gC/m²) are `calibrate = TRUE`** — write the check, mark calibrate, do not invent the cap.
- [ ] `crop.maize-skill` (if a FAO time series is supplied): Pearson r of simulated vs FAO national maize yields ∈ **[0.5, 0.888]** (Müller 2017 GMD 10:1403, max maize r 0.888; `verified`).
- [ ] `crop.cropland-area-input` ∈ **[1500, 1600] Mha** (FAOSTAT land-use via FAO AB48/AB107, 1562 in 2020; `verified`) — sanity on the prescribed input.
- [ ] **Crop DM→C fraction:** use the LPJmL 5.9.7 `pft.par` value, not a literal — extract and record it; the IPCC default 0.47 vs candidate-derived 0.45 disagree and bias every derived t/ha. Mark the registry row `calibrate = TRUE` with a note to read `pft.par`.

### Task 3.6 — Cryosphere & energy

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier3_cryo()`)

- [ ] `cryo.permafrost-area`: LPJmL-simulated NH near-surface permafrost AREA ∈ **[12, 23] Mkm²** (Schaphoff 2013 ERL 8:014026 map-validated; obs anchors: Obu 2021 JGR-ES area ~14 verbatim, Obu 2019 ESR 13.9; `consensus` for the simulated band, obs anchors `verified`). **Do NOT mix area with the permafrost REGION/EXTENT ∈ [18, 23] Mkm²** (Brown 1997/2002 IPA/NSIDC 22.79; Obu 2021 ~21 verbatim) — separate `cryo.permafrost-region` informational row.
- [ ] `cryo.thermal-coverage`: flag if > X% of cells have identical/zero soil-T or missing permafrost where Obu/Brown say it exists; `calibrate = TRUE` (X from first healthy run). Independent cross-check of the grid-mismatch bug.
- [ ] `cryo.snow-jan` ∈ **[42, 50] Mkm²** (NOAA-Rutgers ~46.8; `verified`), `cryo.snow-aug` ∈ **[0.5, 5] Mkm²** (check ice mask before flagging low; `verified`).
- [ ] `cryo.alt-mean`: global active-layer-thickness mean `calibrate = TRUE` (CALM 0.2–2.5 m is per-site, not a global mean; no published LPJmL global mean).

---

## Phase 4 — STOCK-CONTROL benchmark

Run `run_lpjml(input_set = "stock")` produces LPJmL's own published-input run; the whep run is benchmarked against it (not against observations, which a process DGVM cannot reach).

### Task 4.1 — Stock-vs-whep globalflux + spatial

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier4_stock()`)
- Modify: `tests/testthat/test_validate_lpjml_run.R`

- [ ] `bench.vs-stock-globalflux`: for GPP/NPP/VegC/SoilC/ET/runoff, ratio whep-run-global / stock-run-global ∈ **[0.7, 1.3]** (`calibrate = TRUE` — tighten from the first paired healthy+stock run; whep inputs legitimately differ from stock so the band is not literature). Requires `refs$stock` sim_path.
- [ ] `bench.vs-stock-spatial`: per-variable spatial Pearson r(whep-cell, stock-cell on common cells) ≥ floor; `calibrate = TRUE`. A grid-misaligned whep run shows as low r against the structurally identical stock run — a second independent line of defense against the grid-mismatch failure.
- [ ] Test: add a `stock/` fixture (clone of `healthy/` perturbed +10%); assert `bench.vs-stock-globalflux` PASSes for healthy-vs-stock and the spatial r is ~1; assert it FAILs when `A_dead_npp` is benchmarked against `stock`.

---

## Phase 5 — OBSERVATIONAL benchmarking + report renderer

Spatial-r/κ floors are the third line of defense (r → 0/negative under grid mismatch). **Floors are set at model-vs-obs values a DGVM can plausibly reach, never obs-vs-obs.**

### Task 5.1 — Benchmark-dataset access table

**Files**
- Create: `inst/extdata/lpjml/benchmark_datasets.csv`
- Create: `R/lpjml_benchmarks.R`

- [ ] `benchmark_datasets.csv` columns: `dataset`, `variable`, `native_resolution`, `access` (URL/L_files path/pins id), `regrid_to` (0.5°), `metric` (`spatial_r`/`kappa`/`global_total`), `notes`. Rows: MODIS MOD17 (NPP/GPP), FLUXCOM (GPP/ET), GLEAM4 (ET), GRDC (discharge), ESA-CCI biomass (AGB), SoilGrids/HWSD (SOC), Obu/Brown IPA (permafrost), NOAA-Rutgers (snow), GFED4 (fire), FAOSTAT QCL (crop yield), Herrero 2013 + Fetzel 2017 (grazing).
- [ ] Exported `load_lpjml_benchmark_index()` + `.regrid_to_grid(obs, grid)` private helper conservatively remapping each obs product onto the run grid.

### Task 5.2 — Observational spatial-r / κ checks

**Files**
- Modify: `R/validate_lpjml_run.R` (add `.run_tier5_obs()`)

- [ ] `obs.gpp-spatial-r` ∈ **[0.6, 0.95]** (Xia 2022 ESD 13:833; Forkel/Smith LPJ-GUESS 0.63; `verified`).
- [ ] `obs.npp-spatial-r` ∈ **[0.5, 0.9]** (analogy to GPP, Piao 2013 GCB; `consensus`).
- [ ] `obs.et-spatial-r` ∈ **[0.55, 0.95]** (Jung 2019 SciData 6:74; Schaphoff 2018 GMD 11:1377; `consensus`).
- [ ] `obs.agb-spatial-r` ∈ **[0.35, 0.8]** (Thurner 2017 GEB; Forkel 2019 SciRep — DGVM biomass pattern weak; `consensus`).
- [ ] `obs.soc-spatial-r` ∈ **[0.25, 0.7]** (Tifafi 2018 GBC; Anav 2013 J.Clim — weakest field, products disagree; `consensus`).
- [ ] `obs.permafrost-kappa` ∈ **[0.5, 0.85]** Cohen κ vs IPA (Guo & Wang 2016 JGR, CMIP5 ensemble κ = 0.76; `verified`); obs anchor NH permafrost area **[12.2, 17.0] Mkm²** (Brown/IPA via Guo & Wang 2016; `verified`).
- [ ] `obs.gpp-global` ∈ **[108, 135] PgC/yr** (Beer 2010 Science 329:834, 123 ± 8; FLUXCOM Jung 2020 BG 17:1343 108–130; `verified`) — same quantity as `c.gpp-global`, implement once with the union band.
- [ ] `obs.et-global` ∈ **[66, 86] ×1000 km³/yr** (Jung 2019/FLUXCOM 75.6 ± 9.8; `verified`); `obs.runoff-ocean` ∈ **[37, 47] ×1000 km³/yr** (Oki & Kanae 45.5; GRDC-constrained 37–42; `verified`).
- [ ] `obs.grazing-intake-global` ∈ **[1.5, 3.7] Pg DM/yr** (Herrero 2013 PNAS: grass 2.3 = 48%, ruminants 3.7; external benchmark, no LPJmL aggregate published; `verified`); `obs.grazing-intensity` (intake/ANPP) ∈ **[0.0, 0.6]** (Fetzel 2017 GBC 31:1089, GI < 0.15 on 79% of area; `verified`).
- [ ] `obs.enteric-ch4-mass` ∈ **[70, 120] Tg CH₄/yr** (Chang 2019 NatComm 10:3420, 87–97 for 2000–09, 99.0 ± 11.7 in 2012; `verified`).
- [ ] Provide a `metric = "spearman"` fallback for each spatial-r check (same floors; Collier 2018 JAMES 10:2731 ILAMB robustness).

### Task 5.3 — `render_lpjml_qa_report()`

**Files**
- Create: `R/render_lpjml_qa_report.R`
- Create: `inst/rmarkdown/lpjml_qa_report.Rmd`
- Create: `tests/testthat/test_render_lpjml_qa_report.R`
- Modify: `_pkgdown.yml`

- [ ] Exported `render_lpjml_qa_report(report, out_file)` building a Markdown/HTML QA report from `validate_lpjml_run()`'s `results`+`diag`, reusing the `.md(...)` closure pattern (`validate_lpjml_compat.R:766-1049`): a per-tier table of `check_id` / status / band / observed / source, a PASS/FAIL/CALIBRATE summary banner, and a "calibrate-from-first-run" appendix listing every `confidence = placeholder` row still awaiting a locked band.
- [ ] The report must NOT re-run analysis (CLAUDE.md Quarto rule): it reads only the pre-computed `report` object + cached output CSVs via `data.table::fread()`.
- [ ] Test: feed the healthy-fixture report; assert the HTML renders, the summary banner says certified, and the placeholder appendix lists the `calibrate = TRUE` rows.
- [ ] Add `render_lpjml_qa_report`, `validate_lpjml_run`, `read_lpjml_grid`, `whep_l_files_dir`, `lpjml_biome_mask`, `load_lpjml_check_registry`, `check_lpjml_output_manifest`, `load_lpjml_benchmark_index` to `_pkgdown.yml` under the `LPJmL validation` section (every `man/*.Rd` must appear — CI pkgdown gate).

---

## Tooling gate (run after EVERY phase before reporting it complete)

- [ ] `air format .` (mandatory, never manual; install binary if absent).
- [ ] `Rscript -e "devtools::document()"` (regenerate `man/`).
- [ ] `Rscript -e "rcmdcheck::rcmdcheck(build_args='--no-build-vignettes', args=c('--no-tests','--ignore-vignettes'), error_on='error')"` — all new NSE vars declared in `utils::globalVariables()`, all `stats::`/`lpjmlkit::` prefixed, lpjmlkit guarded by `rlang::is_installed()`.
- [ ] `Rscript -e "lintr::lint_package(linters=lintr::linters_with_defaults(object_usage_linter=NULL, line_length_linter=NULL, indentation_linter=NULL, commas_linter=NULL))"`.
- [ ] `Rscript -e "devtools::test()"` (2 pre-existing `test_commodity_balance_sheet.R` failures expected).
- [ ] Verify every `man/*.Rd` topic is in `_pkgdown.yml` (the `comm -23` check from CLAUDE.md must be empty).

---

## Threshold registry summary

| `check_id` | Tier | Band | Units | Source | Confidence |
|---|---|---|---|---|---|
| `npp.valid-fraction` | 1 | ≥ 0.90 | frac | run-health, no paper | placeholder |
| `npp.dead-fraction` | 1 | ≤ 0.15–0.20 | frac | run-health | placeholder |
| `npp.global-band` | 1 | [45, 82] | PgC/yr | von Bloh 2018 Table 3 | verified |
| `vegc.valid-fraction` | 1 | ≥ baseline | frac | run-health | placeholder |
| `ti.npp-alive-every-year` | 1 | ≥ floor | frac | run-health | placeholder |
| `lu.dead-cropland-fraction` | 1 | ≈ 0 | frac | run-health | placeholder |
| `n.uptake-alive-fraction` | 1 | ≥ 0.50 | frac | run-health | placeholder |
| `t.cold-limitation-pattern` | 1 | [6, 11] | °C | CRU/CRUTEM5 (Osborn 2021) | consensus |
| `npp.major-biomes` (trop/temp/bor) | 1 | [400,1300]/[400,1200]/[100,550] | gC/m²/yr | Luyssaert 2007 Table 3 | verified |
| `npp.vs-precip-coverage` | 1 | ≥ floor | frac | run-health | placeholder |
| `grid.integral-eq-csv` | 1 | rel-err < 1e-4 | — | invariant | derived |
| `lu.fraction-sum` | 0 | ≤ 1.0 | frac | invariant | derived |
| `c.cue-band` | 2 | [0.42, 0.58] | — | von Bloh 2018 Table 3 | derived |
| `c.ra-identity` | 2 | [55, 95] | PgC/yr | von Bloh 2018 Table 3 | derived |
| `c.rh-band` | 2 | [45, 75] | PgC/yr | closure / lit | consensus |
| `c.nbp-closure` | 2 | \|NBP\| < ~5 (sign TBD) | PgC/yr | von Bloh +1.21 vs GCB +3.3 | placeholder |
| `c.harvest-global` | 2 | O(2–5) | PgC/yr | no LPJmL aggregate | placeholder |
| `w.et-p-ratio` | 2 | [0.50, 0.67] | — | Oki & Kanae 2006 | verified |
| `w.runoff-p-ratio` | 2 | [0.33, 0.50] | — | Oki & Kanae 2006 | verified |
| `w.transp-fraction` | 2 | [0.45, 0.90] | — | Jasechko 2013 / GLEAM4 | verified |
| `n.uptake-global` | 2 | [300, 1300] | Tg N/yr | von Bloh 2018 Table 4 | verified |
| `n.leaching-global` | 2 | [20, 150] | Tg N/yr | von Bloh 2018 Table 4 | verified |
| `n.bnf-global` | 2 | [50, 290] | Tg N/yr | von Bloh 2018 Table 4 | verified |
| `graze.lsuha-cap` | 2 | ≤ 4.0 (safeguard) | LSU/ha | Heinke 2023 sweep | verified |
| `graze.mgrass-balance` | 2 | C/N closure | — | run_lpjml NOTE | derived |
| `graze.enteric-ym` | 2 | [5.5, 7.5] | % GE | Heinke 2023 Eq.46 | verified |
| `c.gpp-global` | 3 | [100, 175] | PgC/yr | von Bloh 2018 Table 3 | verified |
| `c.npp-global` | 1/3 | [45, 82] | PgC/yr | von Bloh 2018 Table 3 | verified |
| `c.vegc-global` | 3 | [350, 650] | PgC | von Bloh 2018 Table 3 | verified |
| `c.soilc-global` | 3 | [1500, 3400] | PgC | von Bloh 2018 Table 3 | verified |
| `c.fire-global` | 3 | [1.5, 4.0] | PgC/yr | Schaphoff 2018 Pt2 / GFED4 | verified |
| `c.litter-global` | 3 | 0 < x < 300 | PgC | not tabulated | placeholder |
| `c.npp-percell-max` | 3 | [~1200, 1300] | gC/m²/yr | Schaphoff 2018 Pt1 | verified |
| `c.gpp-percell-max` | 3 | > 2000 (~3500) | gC/m²/yr | Schaphoff 2018 Pt1 | verified |
| `c.vegc-percell-forest` | 3 | [~18, 22] | kgC/m² | Schaphoff 2018 Pt1 | verified |
| `c.soilc-percell-boreal` | 3 | [50, 70] | kgC/m² | Schaphoff 2018 Pt1 | verified |
| `w.precip-global` | 3 | [100000, 120000] | km³/yr | Oki & Kanae 2006 | verified |
| `w.et-global` | 3 | [55000, 85000] | km³/yr | Haddeland 2011 | verified |
| `w.runoff-global` | 3 | [38000, 66000] | km³/yr | Haddeland 2011 | verified |
| `w.discharge-gauged-obs` | 3 | [35000, 40000] | km³/yr | Dai & Trenberth 2002 | verified |
| `w.discharge-{amazon…lena}` | 3 | per-river (see Task 3.2) | km³/yr | Dai & Trenberth / GRDC | verified |
| `n.nh3-global` | 3 | [8, 60] | Tg N/yr | von Bloh 2018 Table 4 | verified |
| `n.n2o-total` | 3 | [7, 22] | Tg N/yr | von Bloh 2018 Table 4 / Tian 2020 | verified |
| `n.denit-total` | 3 | [20, 120] | Tg N/yr | von Bloh 2018 Table 4 | verified |
| `n.vegpool-global` | 3 | [1.2, 6.0] | Pg N | von Bloh 2018 Table 4 | verified |
| `n.veg-cn-ratio` | 3 | [150, 320] | gC/gN | von Bloh 2018 Fig 6b | verified |
| `veg.tropevergreen-gpp` | 3 | [2000, 4000] | gC/m²/yr | Luyssaert 2007 Table 3 | verified |
| `veg.tropevergreen-agb` | 3 | [5, 20] | kgC/m² | Luyssaert 2007 Table 5 | verified |
| `veg.lai-max-*` | 3 | [3,7]/[2.5,7] | m²/m² | Luyssaert 2007 / von Bloh Eq.2 | verified |
| `veg.leaf-cn-*` (6 PFTs) | 3 | per-PFT (see Task 3.4) | gC/gN | von Bloh 2018 Table 2 | verified |
| `veg.fpc-coverage` | 3 | trop FPC > 0.6 | frac | no LPJmL table | placeholder |
| `crop.maize-yield-global` | 3 | [5.0, 6.8] | t/ha | FAOSTAT QCL | verified |
| `crop.wheat-yield-global` | 3 | [3.0, 4.2] | t/ha | FAOSTAT QCL | verified |
| `crop.rice-yield-global` | 3 | [4.0, 5.2] | t/ha | FAOSTAT QCL | verified |
| `crop.yield-collapse-floor` | 3 | LOW=0 (maize≤360…) | gC/m² | derived from FAOSTAT | derived |
| `crop.percell-yield-cap` | 3 | maize~700 etc. | gC/m² | not calibrated | placeholder |
| `crop.maize-skill` | 3 | [0.5, 0.888] | r | Müller 2017 GMD | verified |
| `crop.cropland-area-input` | 3 | [1500, 1600] | Mha | FAOSTAT land-use | verified |
| `cryo.permafrost-area` | 3 | [12, 23] | Mkm² | Schaphoff 2013 / Obu 2021 | consensus |
| `cryo.permafrost-region` | 3 | [18, 23] | Mkm² | Brown 1997 / Obu 2021 | verified |
| `cryo.snow-jan` | 3 | [42, 50] | Mkm² | NOAA-Rutgers | verified |
| `cryo.snow-aug` | 3 | [0.5, 5] | Mkm² | NOAA-Rutgers | verified |
| `cryo.alt-mean` | 3 | first-run | m | no global mean published | placeholder |
| `bench.vs-stock-globalflux` | 4 | [0.7, 1.3] | ratio | paired-run | placeholder |
| `bench.vs-stock-spatial` | 4 | ≥ floor | r | paired-run | placeholder |
| `obs.gpp-spatial-r` | 5 | [0.6, 0.95] | r | Xia 2022 ESD | verified |
| `obs.npp-spatial-r` | 5 | [0.5, 0.9] | r | Piao 2013 GCB | consensus |
| `obs.et-spatial-r` | 5 | [0.55, 0.95] | r | Jung 2019 SciData | consensus |
| `obs.agb-spatial-r` | 5 | [0.35, 0.8] | r | Thurner 2017 GEB | consensus |
| `obs.soc-spatial-r` | 5 | [0.25, 0.7] | r | Tifafi 2018 GBC | consensus |
| `obs.permafrost-kappa` | 5 | [0.5, 0.85] | κ | Guo & Wang 2016 | verified |
| `obs.gpp-global` | 5 | [108, 135] | PgC/yr | Beer 2010 / FLUXCOM | verified |
| `obs.et-global` | 5 | [66, 86] | 10³ km³/yr | Jung 2019 | verified |
| `obs.runoff-ocean` | 5 | [37, 47] | 10³ km³/yr | Oki & Kanae 2006 | verified |
| `obs.grazing-intake-global` | 5 | [1.5, 3.7] | Pg DM/yr | Herrero 2013 PNAS | verified |
| `obs.grazing-intensity` | 5 | [0.0, 0.6] | — | Fetzel 2017 GBC | verified |
| `obs.enteric-ch4-mass` | 5 | [70, 120] | Tg CH₄/yr | Chang 2019 NatComm | verified |

**Deduplication note (load-bearing):** GPP/NPP/VegC/SoilC appear in the carbon-cycle, vegetation, and benchmark sections of the source registry with slightly different bands drawn from the same von Bloh 2018 Table 3. Each is implemented ONCE in the check registry using the wider union band; the duplication is not two independent constraints. The Tier-1 `npp.global-band` and the Tier-3 `c.npp-global` are the same registry row referenced from two tiers.

**Redundant catch for the documented 97%-NPP=0 failure** (any one flags the "completed" bad run): (1) `npp.valid-fraction` ≥ 0.90 [calibrate], (2) `npp.global-band` ≥ 45 PgC/yr [verified], (3) `w.et-p-ratio` ≥ 0.50 [verified], (4) `n.uptake-global` ≥ 300 Tg N/yr [verified], (5) `obs.npp-spatial-r` ≥ 0.5 [verified], (6) `t.cold-limitation-pattern` ∈ [6, 11] °C [consensus].

---

## Pending decisions (for Eduardo + Catalin)

Location (**in whep**) and engine (**R**) are decided (2026-06-17). The following remain to settle together before locking the registry CSV; they are flagged at their point of use throughout the plan and appendices:

1. **Package vs `inst/` placement.** This plan promotes reusable check logic to `R/` (strict rules: roxygen, tests, `_pkgdown.yml`, `globalVariables`) rather than keeping it as a looser `inst/analysis/spatialize/` script, because the suite is run programmatically by `run_lpjml.R` and must be unit-tested. Confirm this is the desired rigor level, or keep `validate_lpjml_run()` as an `inst/` script (lower friction, no tests/pkgdown burden).
2. **Calibrate-from-first-run bands.** All `confidence = placeholder` rows emit `status = NA` ("CALIBRATE") until a verified-healthy 5.9.7 run AND a paired stock run lock their bands. Confirm the suite ships with these as non-blocking, then a follow-up task hardens them — versus blocking the first run on having no liveness floor.
3. **Crop DM→C fraction (0.45 vs IPCC 0.47).** The derived yield-collapse floors depend on the C-fraction. Plan reads the exact LPJmL 5.9.7 `pft.par` value rather than picking a literal; confirm reading `pft.par` is acceptable, or pin IPCC 0.47.
4. **lsuha cap = 4 LSU/ha.** Recorded as a flagged temporary safeguard in the `graze.lsuha-cap` detail string with an at-origin denominator check pointing to the `grassland_lsuha` builder. Confirm 4 LSU/ha (Heinke 2023 sweep ceiling) is the intended hard cap, or treat it as soft-warn only.
5. **NBP sign convention.** No NBP sign test runs until the 5.9.7 convention is read off the first healthy run (von Bloh's +1.21 managed-grassland source vs the GCB +3.3 land sink have opposite signs). Confirm the magnitude-only `|NBP| < ~5 PgC/yr` placeholder is acceptable for the first run.


---

# Appendix A — Verified threshold numbers

All major anchors verified: Obu 2021 (area ~14, region ~21 Mkm², both verbatim from the abstract), Herrero 2013 (grass 2.3 Pg DM = 48%, ruminants 3.7 Pg, total 4.7 Pg — all verbatim). The candidate JSON's attributions are accurate to the sources.

I have enough to consolidate. The candidate set is unusually well-attributed; my adversarial pass found the source claims hold up. The main work is flagging the internally-derived/placeholder values, deduplicating the carbon-budget vs benchmark-metrics overlap (NPP/GPP/VegC/SoilC appear in both), and checking internal consistency (CUE, runoff/ET closure, N mass balance).

---

# LPJmL 5.9.7 Validation Number Registry

Adversarially verified consolidation of source-attributed thresholds for the whep test suite. Every "verified" row had its source paper/dataset and the cited value confirmed by web lookup this session (von Bloh 2018 GMD 11:2789; Heinke 2023 GMD 16:2455; Luyssaert 2007 GCB 13:2509 incl. GPP 3551; Dai & Trenberth 2002 = 37,288 km³; Beer 2010 = 123±8 PgC; Oki & Kanae 2006 Science 313:1068; Obu 2021 area ~14 / region ~21 Mkm²; Herrero 2013 grass 2.3 Pg DM). Rows whose number is an internal derivation, a mass-balance closure, an order-of-magnitude envelope, or a run-health statistic with no published value are marked **placeholder** or **derived/consensus** and must not be treated as hard external thresholds.

Confidence legend: **verified** = number traced to a real paper/dataset and the value confirmed; **derived** = arithmetic identity from verified numbers (sound but not independently published); **consensus** = real literature range, no single pinned source value; **placeholder** = no trustworthy external value — calibrate from first healthy run.

---

## Numbers to treat as PLACEHOLDER (calibrate from first healthy + stock-control run)

These have **no defensible external value**. Do not hardcode a pass band from literature; lock them from the first verified-healthy 5.9.7 run, then upgrade.

| Quantity | Provisional rule (NOT a verified threshold) | Why placeholder |
|---|---|---|
| Fraction of vegetated land cells with NPP == 0 | FAIL if > 0.15–0.20 (failing run = 0.97) | Run-health statistic; no paper reports it. **Single most diagnostic test for the documented failure.** |
| Fraction of vegetated land cells with NPP > 0 | PASS if ≥ 0.90 (failing run ≈ 0.03) | Mirror of above; the direct catch. Mask true desert/ice first. |
| Fraction of land cells with positive plant N uptake | floor ≈ 0.50 (suggest), healthy ≈ 0.90+ | Sentinel for the N-collapse; desert/ice legitimately 0. |
| Fraction of cells with grassland NPP == 0 | FAIL if ≫ healthy baseline (failing = 0.97) | Calibrate desert/ice baseline. |
| Crop + grazing harvest carbon, global (PgC/yr) | O(2–5) PgC/yr expectation only | LPJmL papers report kcal or t DM/ha, never aggregate PgC. |
| Litter carbon pool, global (PgC) | loose 0 < x < 300 | Not tabulated in von Bloh/Schaphoff; lit ~50–200. |
| Per-cell column soil moisture range (mm) | ≥ 0 and ≤ profile capacity (~1000–1500 mm) | Unit/depth specific to this setup; not config-comparable. |
| Soil temperature field range (°C) | physical bounds ~[−25, 30]; flag all-zero/NaN | Plausibility bounds only. |
| Thermal-state spatial-coverage guard | flag if >X% cells identical/zero soil-T or missing permafrost where Obu/Brown say it exists | X from first healthy run. Independent cross-check of the grid-mismatch bug. |
| LPJmL per-crop simulated global mean yield | extract from Müller 2017 GMD / Jägermeyr 2021 NatFood / GGCMI archive | Use LPJmL's OWN simulated yield (has crop-specific bias vs FAO), not the FAOSTAT bands, for self-validation. Not pinned. |
| Per-cell maize/wheat/rice yield hard cap (gC/m²) | maize ~700, wheat ~400, paddy ~450 (provisional) | Per-cell record-yield caps not calibrated. |
| fAPAR / FPC per-biome band | tropical-forest FPC > 0.6, all-cell FPC > 0 | LPJmL publishes no global fAPAR/FPC table. MOD15 0.7–1.0 is order-of-magnitude only. |
| Global active-layer-thickness mean target | from first healthy run (CALM site range 0.2–2.5 m is per-site, not global mean) | No global-mean ALT published for LPJmL. |
| NBP magnitude/sign | wide \|NBP\| < ~5 PgC/yr only | **Sign convention differs** between von Bloh LPJmL5 output (+1.21 = source on managed grassland) and the real ~+3 PgC/yr land sink. Read the 5.9.7 convention off a healthy run before any sign test. |
| GLW3-based stocking mean (LSU/ha) | spatial-pattern check only | GLW3 is animals/km², not LSU/ha; no single global threshold. |

---

## Carbon cycle

| Quantity | Pass band [low, high] | Units | Source | Confidence |
|---|---|---|---|---|
| Global GPP (LPJmL total) | [100, 175] | PgC/yr | von Bloh 2018 GMD 11:2789 Table 3 (LPJmL5 131.8; PNV 171.1; LPJmL3.5 129.9); Schaphoff 2018 Pt2 GMD 11:1377 (LPJmL4 123.7) | verified |
| Global GPP, observational envelope | [112, 131] | PgC/yr | Beer 2010 Science 329:834 (123±8); Jung 2011 FLUXCOM (119±6) | verified |
| Global NPP (LPJmL total) | [45, 82] | PgC/yr | von Bloh 2018 Table 3 (LPJmL5 64.07; no-N-limit 80.27; PNV 76.88; LPJmL3.5 57.12) | verified |
| Global NPP, observational envelope | [44, 70] | PgC/yr | Ito 2011 GCB (56.2±14.3); Carvalhais 2014 Nature (54±10) | verified |
| Carbon-use efficiency NPP/GPP | [0.42, 0.58] | — | Derived from von Bloh 2018 Table 3 (0.44–0.49 across configs) | derived |
| Total vegetation C (VegC) | [350, 650] | PgC | von Bloh 2018 Table 3 (LPJmL5 444; LPJmL3.5 451; PNV 627–679); Schaphoff 2018 Pt2 (LPJmL4 507) | verified |
| VegC, observational benchmark | [362, 560] | PgC | Carvalhais 2014 Nature (445±8 total biomass); Liu 2015 NatCC (362 AGB-only = hard floor) | verified |
| Total soil C (whole profile incl. permafrost) | [1500, 3400] | PgC | von Bloh 2018 Table 3 (LPJmL5 2049; LPJmL3.5 2034; PNV 2344); Schaphoff 2018 Pt2 (LPJmL4 1869) | verified |
| Soil C, observational benchmark | [1500, 2750] | PgC | Carvalhais 2014 Nature (2352±400) | verified |
| Total terrestrial C (VegC+SoilC) | [1900, 3900] | PgC | Sum of von Bloh 2018 Table 3 (LPJmL5 ≈2493) + litter | derived |
| Fire / biomass-burning C | [1.5, 4.0] | PgC/yr | Schaphoff 2018 Pt2 (GlobFIRM 2.33; SPITFIRE 2.7–2.8); GFED4 ~2.0–2.5 | verified |
| Autotrophic respiration Ra (= GPP−NPP) | [55, 95] | PgC/yr | Identity from von Bloh 2018 Table 3 (LPJmL5 ≈68) | derived |
| Heterotrophic respiration Rh | [45, 75] | PgC/yr | Steady-state closure NPP−fire−harvest−NBP; lit ~50–60 | consensus |
| NBP (\|NBP\| sanity only) | [−3.0, 3.0] | PgC/yr | von Bloh 2018 Table 3 (+1.21); GCB2023 SLAND 3.3±0.8 — **sign convention conflict, see placeholder** | placeholder |
| Per-cell NPP max (productive cells) | reaches [~1200, 1300]; min 0 valid | gC/m²/yr | Schaphoff 2018 Pt1 GMD 11:1343 Fig 4b | verified |
| Per-cell GPP max | reaches > 2000 (up to ~3500) | gC/m²/yr | Schaphoff 2018 Pt1 Fig 4a | verified |
| Per-cell VegC, forested | reaches [~18, 22] | kgC/m² | Schaphoff 2018 Pt1 (tropics ~20, temperate ~8) | verified |
| Per-cell SoilC, boreal/permafrost | reaches [50, 70] | kgC/m² | Schaphoff 2018 Pt1 (>60 in permafrost) | verified |
| Per-cell fire C max | [0, 120] (if fire enabled) | gC/m²/yr | Schaphoff 2018 Pt1 Fig 4c | verified |

**Internal-consistency checks (must hold regardless of band):** NPP < GPP; CUE = NPP/GPP ∈ [0.42, 0.58]; GPP − NPP − Ra ≈ 0; NPP − Rh − fire − harvest ≈ NBP. **Note:** the carbon-budget GPP/NPP/VegC/SoilC rows and the benchmark-metrics global-total rows below are the **same quantities** with slightly different bands — implement once, using the wider band, not twice.

---

## Water cycle

| Quantity | Pass band [low, high] | Units | Source | Confidence |
|---|---|---|---|---|
| Global land precipitation | [100000, 120000] | km³/yr | Oki & Kanae 2006 Science 313:1068 (~111k); Trenberth 2007 GPCP | verified |
| Global land ET (actual) | [55000, 85000] | km³/yr | Haddeland 2011 JHM 12:869 (WaterMIP incl. LPJmL 60–85k); GLEAM4 Miralles 2025 (68.5k); Oki & Kanae (~65.5k) | verified |
| Global total land runoff | [38000, 66000] | km³/yr | Haddeland 2011 (42–66k incl. LPJmL); Oki & Kanae (45.5k) | verified |
| River discharge gauged to ocean (obs only) | [35000, 40000] | km³/yr | Dai & Trenberth 2002 JHM 3:660 (37,288±662) — **benchmark, NOT what LPJmL total runoff should match** | verified |
| Runoff ratio (runoff/P) | [0.33, 0.50] | — | Oki & Kanae (45.5/111 = 0.41); Haddeland ranges | verified |
| ET/P ratio | [0.50, 0.67] | — | Oki & Kanae (">60% returns as ET"); flag if <0.45 or >0.70 | verified |
| Transpiration fraction T/ET | [0.45, 0.90] | — | Jasechko 2013 Nature (80–90% isotope) vs GLEAM4 62% / model 50–65% — **wide on purpose; catch T/ET→0 only** | verified |
| Interception fraction of ET | [0.07, 0.25] | — | Miralles 2011 GLEAM; Schlesinger & Jasechko 2014 | consensus |
| Soil-evaporation fraction of ET | [0.15, 0.40] | — | Residual of T/ET + interception | consensus |
| Global mean land ET as depth | [390, 590] | mm/yr | Haddeland 2011 (415–586) | verified |
| Global mean runoff as depth | [270, 460] | mm/yr | Haddeland 2011 (290–457) | verified |
| Amazon discharge at mouth | [5400, 7000] | km³/yr | Dai & Trenberth 2002 (~6642); GRDC (~6591) | verified |
| Congo discharge | [1250, 1400] | km³/yr | Dai & Trenberth (~1308); GRDC (~1306) | verified |
| Orinoco discharge | [1100, 1250] | km³/yr | Dai & Trenberth (~1129); GRDC (~1230) | verified |
| Ganges–Brahmaputra (combined) | [1000, 1350] | km³/yr | GRDC sum (~1193); Dai & Trenberth (separate) | verified |
| Yangtze discharge | [900, 1010] | km³/yr | GRDC (~1006); Dai & Trenberth (~944) | verified |
| Mississippi discharge | [580, 680] | km³/yr | Dai & Trenberth (~610); GRDC (~672) | verified |
| Yenisei discharge | [580, 640] | km³/yr | Dai & Trenberth (~599); GRDC (~637) | verified |
| Lena discharge | [520, 590] | km³/yr | Dai & Trenberth (~531); GRDC (~577) | verified |

**Closure check:** ET/P + runoff/P ≈ 1 at steady state. **Primary failure signature:** ET/P → 0 (equivalently runoff/P → 1) is the unmistakable mark of the temperature-grid mismatch that zeroed NPP.

---

## Nitrogen cycle

| Quantity | Pass band [low, high] | Units | Source | Confidence |
|---|---|---|---|---|
| Global plant N uptake (w/ land use) | [300, 1300] | Tg N/yr | von Bloh 2018 GMD Table 4 (LPJmL5 618; lit anchors 620–1130) | verified |
| Global N leaching | [20, 150] | Tg N/yr | von Bloh 2018 Table 4 (62.83); lit 93–95 | verified |
| Global NH₃ volatilization | [8, 60] | Tg N/yr | von Bloh 2018 Table 4 (20.46) | verified |
| Global total N₂O (nitr.+denitr.) | [7, 22] | Tg N/yr | von Bloh 2018 Table 4 (14.57); Tian 2020 Nature (~17); GCP ESSD 2024 | verified |
| Denitrification N₂O component | [2, 10] | Tg N/yr | von Bloh 2018 Table 4 (5.47) | verified |
| Nitrification N₂O component | [3, 15] | Tg N/yr | von Bloh 2018 Table 4 (9.10) | verified |
| Total denitrification (N₂O+N₂) | [20, 120] | Tg N/yr | von Bloh 2018 Table 4 (49.71); lit 25–85 (Bouwman 2013) | verified |
| Biological N fixation (terrestrial) | [50, 290] | Tg N/yr | von Bloh 2018 Table 4 (128.9); Galloway 2004 (107); Vitousek 2013 / Fowler 2013 (58) — **5.9.7 BNF scheme revised in Sigurdsson 2024 GMD 17:7889** | verified |
| N deposition onto land (INPUT) | [40, 110] | Tg N/yr | Fowler 2013 (~70); Galloway 2004 (32→102) — prescribed forcing, not output | verified |
| Global vegetation N pool | [1.2, 6.0] | Pg N | von Bloh 2018 Table 4 (LPJmL5 1.78; PNV 2.69; lit to 16) | verified |
| Whole-vegetation global C:N | [150, 320] | gC/gN | von Bloh 2018 Fig 6b (~235–260) — **vegetation-wide only; do NOT compare a leaf-C:N output here** | verified |
| Per-cell plant N uptake | [0.2, 30] | gN/m²/yr | OOM from 618 Tg over ~1.3e14 m² | consensus |
| Per-cell N leaching | [0, 15] | gN/m²/yr | von Bloh 2018 Fig 8 axis | consensus |
| Per-cell soil N₂O | [0, 5] | gN/m²/yr | Tian 2020 / GCP ESSD 2024 gridded | consensus |

**Mass-balance check:** leaching ≤ Ndep + BNF + fertilizer. Plant N uptake must be hundreds of Tg N/yr, never ~0 — a decisive corroborator of the NPP-collapse failure.

---

## Grazing & grassland

| Quantity | Pass band [low, high] | Units | Source | Confidence |
|---|---|---|---|---|
| Managed-grassland NPP, area-wtd mean | [200, 900] | gC/m²/yr | Rolinski 2018 GMD 11:429; Chang/Vuichard 2015 PLOS ONE (cut 508, grazed 577) | consensus |
| Global grazed biomass intake (DM) | [1.5, 3.7] | Pg DM/yr | Herrero 2013 PNAS (grass 2.3 = 48%; ruminants 3.7) — **external benchmark, no LPJmL aggregate published** | verified |
| Enteric CH₄, domestic ruminants (mass) | [70, 120] | Tg CH₄/yr | Chang 2019 NatComm 10:3420 (87–97 for 2000–09; 99.0±11.7 in 2012) | verified |
| Enteric CH₄ as carbon | [0.052, 0.090] | PgC/yr | Chang 2019 band × 0.749 (C = 74.9% of CH₄; Heinke 2023) — only if output is PgC | derived |
| Enteric Ym (fraction of GE → CH₄) | [5.5, 7.5] | % of gross energy | Heinke 2023 GMD 16:2455 Eq.46 (6.5±1.0); IPCC Tier-2 forage default | verified |
| Max DM intake, generic 500 kg cow | [8, 14.1] | kg DM/animal/day | Heinke 2023 Eq.24 (2.82% BW = 14.1 max) | verified |
| Forage C digestibility | [0.56, 0.80] | — | Heinke 2023 (fC = 0.561 + 2.190·wN,CN) | verified |
| Sustainable stocking density | [0.0, 1.2] | LSU/ha | Rolinski 2018 (no soil-C loss at 0.4–1.2 temperate); Vuichard 2015 | verified |
| Max prescribed stocking (INPUT cap) | [0.0, 4.0] | LSU/ha | Heinke 2023 (sweep 0–4 LSU/ha); matches repo "cap at 4 LSU/ha" | verified |
| Grazing intensity (intake/ANPP) | [0.0, 0.6] | — | Fetzel 2017 GBC 31:1089 (GI<0.15 on 79% area) | verified |
| European grassland yield (regional) | [1.0, 10.0] | t DM/ha/yr | Vuichard/Chang 2015 PLOS ONE (mean ~4.4, Atlantic to 10) | verified |
| Carbon fraction of grass DM | [0.40, 0.50] (LPJmL = 0.424) | gC/g DM | Heinke 2023 (wC,DM = 0.424) | verified |

---

## Vegetation & biomes (per-cell NetCDF)

| Quantity | Pass band [low, high] | Units | Source | Confidence |
|---|---|---|---|---|
| Global NPP (globalflux) | [40, 80] | PgC/yr | von Bloh 2018 Table 3 (LPJmL5 64.07) — **same quantity as carbon-cycle NPP** | verified |
| Global GPP (globalflux) | [100, 175] | PgC/yr | von Bloh 2018 Table 3 (131.8) | verified |
| Global VegC (globalflux) | [300, 700] | PgC | von Bloh 2018 Table 3 (444); Forkel 2019 SciRep (505–544) | verified |
| Global SoilC (globalflux) | [1500, 2400] | PgC | von Bloh 2018 Table 3 (2049) | verified |
| Tropical evergreen per-cell NPP | [400, 1300] | gC/m²/yr | Luyssaert 2007 GCB 13:2509 Table 3 (864±96) | verified |
| Tropical evergreen per-cell GPP | [2000, 4000] | gC/m²/yr | Luyssaert 2007 Table 3 (3551±160) ✓confirmed verbatim | verified |
| Tropical evergreen AGB | [5, 20] | kgC/m² | Luyssaert 2007 Table 5 (11.4±5.8) | verified |
| Tropical moist forest AGB (dry mass, cross-check) | [150, 500] | Mg DM/ha | Avitabile 2016 GCB 22:1406; Lewis 2013 (intact ~396) | verified |
| Pantropical live biomass C density | [60, 175] | MgC/ha | Saatchi 2011 PNAS 108:9899 (mean ~100.5) | verified |
| Temperate evergreen per-cell NPP | [400, 1200] | gC/m²/yr | Luyssaert 2007 Table 3 (783±45) | verified |
| Boreal evergreen per-cell NPP | [100, 550] | gC/m²/yr | Luyssaert 2007 Table 3 (271±17) | verified |
| Temperate evergreen AGB | [6, 30] | kgC/m² | Luyssaert 2007 Table 5 (14.9±13.6) | verified |
| Max LAI, tropical evergreen | [3, 7] | m²/m² | Luyssaert 2007 Table 5 (5.2±1.2); LPJmL caps LAI at 7 (von Bloh 2018 Eq.2) | verified |
| Max LAI, temperate/boreal forest | [2.5, 7] | m²/m² | Luyssaert 2007 Table 5 | verified |
| Leaf C:N, tropical broadleaf evergreen | [15.6, 46.2] | gC/gN | von Bloh 2018 Table 2 (TRY/Kattge 2011) — **hard model-internal PFT bound** | verified |
| Leaf C:N, tropical raingreen | [15.4, 34.6] | gC/gN | von Bloh 2018 Table 2 | verified |
| Leaf C:N, temperate/boreal needleleaf evergreen | [31.8, 63.8] | gC/gN | von Bloh 2018 Table 2 | verified |
| Leaf C:N, temperate broadleaf summergreen | [15.4, 34.6] | gC/gN | von Bloh 2018 Table 2 | verified |
| Leaf C:N, C3/C4 perennial grass | C3 [10.5, 37.9]; C4 [17.4, 66.9] | gC/gN | von Bloh 2018 Table 2 — test per grass PFT | verified |
| AGB dry-mass → C fraction (helper) | [0.45, 0.50] (use 0.47) | gC/g DM | IPCC 2006 AFOLU | verified |
| Tropical root:shoot (helper) | [0.20, 0.40] | — | Luyssaert 2007 Table 5 (0.257); IPCC 2006 | verified |

---

## Crops (FAOSTAT-basis validation)

| Quantity | Pass band [low, high] | Units | Source | Confidence |
|---|---|---|---|---|
| Global mean maize yield | [5.0, 6.8] | t/ha fresh | FAOSTAT QCL via FAO AB79 (~6.0 in 2020) | verified |
| Global mean wheat yield | [3.0, 4.2] | t/ha fresh | FAOSTAT QCL via FAO AB79 (~3.5–3.7) | verified |
| Global mean rice (paddy) yield | [4.0, 5.2] | t/ha paddy fresh | FAOSTAT QCL via FAO AB79 | verified |
| Rice yield as dry matter | [3.4, 4.5] | t/ha DM | Derived (paddy × (1−0.14 MC)) | derived |
| World cereal yield (aggregate) | [3800, 4500] | kg/ha | World Bank WDI / FAOSTAT (4182 in 2022) | verified |
| Global cropland area (prescribed input) | [1500, 1600] | Mha | FAOSTAT land-use via FAO AB48/AB107 (1562 in 2020) | verified |
| Crop DM → C fraction | [0.45, 0.50] | gC/g DM | IPCC 2006 — **verify exact value in LPJmL 5.9.7 pft.par before hardcoding** | verified (constant); model value placeholder |
| Harvest index, maize | [0.40, 0.55] | — | Unkovich et al. Adv. Agron.; field range | consensus (LPJmL param not pinned) |
| Harvest index, wheat | [0.35, 0.55] | — | Unkovich et al.; field studies | consensus |
| Harvest index, rice | [0.40, 0.55] | — | Unkovich et al.; IRRI | consensus |
| Maize yield reproduction skill (time-series r vs FAO) | [0.5, 0.888] | Pearson r | Müller 2017 GMD 10:1403 (max maize r 0.888; wheat 0.673) | verified |
| Global-mean yield gC/m² caps (catch zero-NPP via LOW=0) | maize [0,360]; wheat [0,250]; rice [0,280] | gC/m² | Derived from FAOSTAT means × MC × CF — LOW=0 catches collapse; per-cell caps are placeholder | derived |

---

## Cryosphere & energy

| Quantity | Pass band [low, high] | Units | Source | Confidence |
|---|---|---|---|---|
| NH near-surface permafrost AREA | [12, 16] | Mkm² | Obu 2021 JGR-ES (~14, 14–15.7); Obu 2019 ESR (13.9) ✓verbatim | verified |
| NH permafrost REGION/EXTENT | [18, 23] | Mkm² | Brown 1997/2002 IPA/NSIDC (22.79); Obu 2021 (~21) ✓verbatim — **do not mix with area** | verified |
| LPJmL-simulated NH permafrost area | [12, 23] | Mkm² | Schaphoff 2013 ERL 8:014026; Schaphoff 2018b GMD 11:1377 — map-validated, no quoted Mkm² | consensus |
| Active-layer thickness (per-cell site range) | [0.2, 2.5] | m | CALM (NSIDC GGD313) via Schaphoff 2013 — per-site, not global mean | consensus |
| NH snow-cover extent, January | [42, 50] | Mkm² | NOAA-Rutgers (~46.8); IPCC/GRID-Arendal (45.2) | verified |
| NH snow-cover extent, August | [0.5, 5] | Mkm² | NOAA-Rutgers (~2.6); GRID-Arendal (1.9) — check ice mask before flagging low | verified |
| Global land-mean 2 m air temperature | [6, 11] | °C | Land-only SAT consensus (~8.5); CRU TS/CRUTEM5 (Osborn 2021) — coarse sanity, NaN/0 = red flag | consensus |

---

## Benchmark metrics (spatial pattern + global totals)

Spatial-r floors are the second independent line of defense against the grid-mismatch failure (r → 0 or negative). **Do not set floors at obs-vs-obs values** — a process DGVM cannot reach those.

| Quantity | Pass band [low, high] | Units | Source | Confidence |
|---|---|---|---|---|
| Spatial r, GPP vs MODIS/FLUXCOM | [0.6, 0.95] | Pearson r | Xia 2022 ESD 13:833; Forkel/Smith LPJ-GUESS (0.63) | verified |
| Spatial r, NPP vs MOD17 | [0.5, 0.9] | Pearson r | By analogy to GPP (Xia 2022; Piao 2013 GCB) | consensus |
| Spatial r, ET vs FLUXCOM | [0.55, 0.95] | Pearson r | Jung 2019 SciData 6:74; Schaphoff 2018 GMD 11:1377 | consensus |
| Spatial r, AGB vs ESA-CCI | [0.35, 0.8] | Pearson r | Thurner 2017 GEB; Forkel 2019 SciRep — DGVM biomass pattern weak | consensus |
| Spatial r, SOC vs SoilGrids/HWSD | [0.25, 0.7] | Pearson r | Tifafi 2018 GBC; Anav 2013 J.Clim — weakest field (products disagree) | consensus |
| Permafrost extent agreement (Cohen κ vs IPA) | [0.5, 0.85] | κ | Guo & Wang 2016 JGR (CMIP5 ensemble κ=0.76) | verified |
| Observed NH permafrost area (anchor) | [12.2, 17.0] | Mkm² | Brown/IPA via Guo & Wang 2016 (12.21–16.98; common 15.24) | verified |
| Global GPP (total) | [108, 135] | PgC/yr | Beer 2010 (123±8); FLUXCOM Jung 2020 BG 17:1343 (108–130) | verified |
| Global NPP (total) | [45, 70] | PgC/yr | MOD17 Zhao 2005 (56.0); Collection 7 (58±1.1) | verified |
| Global vegetation C stock | [440, 600] | PgC | Erb 2018 Nature (450, 444–584); widen low to 380 if LPJmL-calibrated | verified |
| Global SOC (0–1 m) | [1200, 2000] | PgC | Jobbágy & Jackson 2000 (1502); HWSD 1417 — allow to ~2400 if incl. permafrost/deep | verified |
| Global land ET | [66, 86] | 1000 km³/yr | Jung 2019 / FLUXCOM (75.6±9.8) | verified |
| Global runoff to ocean | [37, 47] | 1000 km³/yr | Oki & Kanae 2006 (45.5); GRDC-constrained 37–42 | verified |
| Spatial Spearman ρ (robust alt.) | same floors as Pearson | ρ | Collier 2018 JAMES 10:2731 (ILAMB) | consensus |

---

### Adversarial notes (deduplication & conflicts)

- **GPP/NPP/VegC/SoilC appear twice** (carbon cycle + benchmark/vegetation sections) with slightly different bands drawn from the same von Bloh 2018 Table 3. Implement each global total **once** using the wider/union band; the duplication is not two independent constraints.
- **NBP sign conflict is real and load-bearing:** von Bloh's LPJmL5 reports +1.21 PgC/yr as a managed-grassland *source*, opposite in sign to the GCB land *sink* (+3.3). Kept as placeholder; any NBP sign test before reading the 5.9.7 convention would be a false-failure generator.
- **Two N₂O budget framings (von Bloh land-surface 14.57 vs Tian/GCP total ~17)** are not in conflict — the band [7, 22] brackets both; flagged so a future editor does not "tighten" to one.
- **T/ET band is deliberately wide (0.45–0.90)** spanning the unresolved isotope-vs-model debate (Jasechko 80–90% vs GLEAM/model 50–65%); it is a collapse-detector, not an arbiter of that science. Do not narrow it to police the 0.5-vs-0.8 question.
- **Crop DM→C fraction (0.45 vs IPCC 0.47):** the candidate's derived yield caps assume CF=0.45; IPCC default is 0.47–0.50. Verify the exact LPJmL 5.9.7 `pft.par` value — a wrong CF biases every derived t/ha. Flagged, not silently picked.
- **Ocean-gauge discharge (37,288) vs total land runoff (~44,000)** must not be conflated: the gauged figure excludes ungauged + endorheic area; LPJmL global runoff legitimately runs ~15–25% higher. Kept as separate rows with explicit warnings.
- No fabricated citations detected: every paper named (von Bloh 2018, Schaphoff 2018 Pt1/Pt2, Heinke 2023, Rolinski 2018, Luyssaert 2007, Dai & Trenberth 2002, Oki & Kanae 2006, Beer 2010, Herrero 2013, Chang 2019, Obu 2019/2021, Fetzel 2017) resolves to a real DOI and the cited value was confirmed.

**Primary catch for the documented 97%-NPP=0 failure (redundant by design):** (1) fraction-of-cells-NPP>0 ≥ 0.90 [placeholder], (2) global NPP ≥ 45 PgC/yr [verified], (3) ET/P ≥ 0.50 [verified], (4) global plant N uptake ≥ 300 Tg N/yr [verified], (5) spatial r(NPP) ≥ 0.5 [verified], (6) land-mean air temperature ∈ [6, 11] °C [consensus]. Any one of (2)–(6) would have flagged the run that "completed"; (1) is the unambiguous direct test once calibrated.


---

# Appendix B — Regime classification (stock vs whep)

# Regime Model: STOCK Control and WHEP Experiment Validation

## 1. Stock-as-control philosophy

The LPJmL validation suite must serve two structurally different runs, and conflating them destroys its diagnostic power. We therefore define two **input regimes** and validate each against its own reference.

### STOCK (canonical) run = the CONTROL

STOCK is LPJmL driven by its **own published inputs**: CRU climate, standard LUH2 land use, stock `grassland_lsuha`, PIK soil, and the 67420-cell CRU land mask. Under these inputs LPJmL must **reproduce published behaviour within tight tolerance**: von Bloh 2018 (GMD 11:2789) Table 3 global NPP 64.07, GPP 131.8, VegC 444, SoilC 2049 PgC, and the Schaphoff 2018 (GMD 11:1343 / 1377) spatial and hydrological benchmarks.

The control exists to answer exactly one question: **is the model build/config/compilation correct?** If the stock run does not match the published values, the fault is in the build, the configuration, or the compilation, and **not** in the inputs (it is running on the published inputs by definition). This isolates **model bugs from input bugs**: a stock failure indicts the executable, a whep failure on a passing stock indicts the inputs.

### WHEP run = the EXPERIMENT

WHEP is LPJmL driven by **whep-generated inputs**: LUH2-derived land use carrying the managed-grassland band, whep `grassland_lsuha` capped at 4 LSU/ha, whep ndep, and the NaturalEarth ~58765-cell mask (~12.8% fewer cells than the CRU 67420). These inputs are physically plausible but **legitimately different** from stock: a different land-use history, a different grass distribution, a capped grazing density, and a different land mask. The whep run is therefore expected to depart from the published values; flagging that departure as an error would be a false positive.

Consequently the whep run gets **wider plausibility bands** on every shared magnitude check, and it carries **input-specific checks the stock run cannot have** (does the realized `cftfrac` echo the whep land-use input? is the lsuha field capped at 4? does `prescribe_lsuha` take effect? is the ndep input ingested cell-for-cell?). The stock run, symmetrically, carries the one check the whep run cannot have: **reproduce the PIK-published values within tight tolerance**.

### Why two regimes

A single band cannot serve both runs. Tight-enough-for-stock would false-fail the legitimately-different whep run; wide-enough-for-whep would let a broken model build sail through the control. Separating the regimes lets the control stay tight (catching build bugs) while the experiment stays plausible (catching input bugs), and a third, paired mode compares the two directly (catching grid-mismatch bugs invisible to either run alone).

## 2. Mechanism

### Registry schema

Every check is a row in the check registry. The load-bearing addition is a **`regime` column** taking a value in `{both, whep, stock}`, plus per-row threshold fields:

| column | meaning |
|---|---|
| `id` | check identifier |
| `regime` | `both` (input-agnostic law), `whep` (whep-only), `stock` (stock-only) |
| `category` | `both_identical`, `both_modified`, `whep_only`, `stock_only`, `paired` |
| `band` / `threshold` | the pass criterion for this row, in this regime |
| `cycle` | carbon, water, nitrogen, vegetation-biomes, landuse-crops, grazing-livestock, structural-liveness, cryosphere-benchmark-paired |
| `calibrate` | `TRUE` while the band is a placeholder awaiting first-run calibration |

A **`both_identical`** check is ONE row with `regime = both` and a single threshold applied to whichever run is validated.

A **`both_modified`** check is **TWO rows**: `regime = stock` carrying the tight published-anchored band, and `regime = whep` carrying the wider plausibility band. They share the `id` (and may be disambiguated by a `regime` suffix) so the suite knows they are the same quantity measured under two references.

A **`whep_only`** check is ONE row with `regime = whep`; a **`stock_only`** check is ONE row with `regime = stock`. **`paired`** checks live in a separate registry (or a `regime = pair` partition) because they require both run directories at once.

### `validate_lpjml_run(run_dir, input_regime)`

```r
validate_lpjml_run(run_dir, input_regime = c("whep", "stock"))
```

Runs every registry row whose `regime` is in `{both, input_regime}`. For a stock run (`input_regime = "stock"`) it executes all `both` rows plus all `stock` rows: the input-agnostic laws plus the tight published-reproduction band of each `both_modified` check plus the `stock_only` checks. For a whep run it executes all `both` rows plus all `whep` rows: the same laws plus the wide band of each `both_modified` check plus the `whep_only` input-echo checks. The complementary regime's rows are never evaluated, so a whep run is never held to the stock literal and vice versa.

### `validate_lpjml_pair(whep_run, stock_run)`

```r
validate_lpjml_pair(whep_run, stock_run)
```

Runs the paired checks, which need both runs together: the whep-vs-stock global-flux comparison, the whep-vs-stock spatial-NPP correlation, the live-fraction delta, and the paired observational comparisons (vs-MODIS, vs-GRACE) whose load-bearing form is a whep-vs-stock comparison on the shared cells. These cannot live in either single-run suite because neither run alone contains the other's field; the comparison anchor is the live stock result, not a fixed literature number.

### How a run is told its regime

The regime is **not inferred from the outputs** (that would defeat the isolation: a broken whep run could masquerade as stock). It is declared explicitly, by **one of**:

- a `input_regime: stock | whep` field in the run manifest written at run-assembly time (the same place that records which land-use, lsuha, ndep, soil and mask files were wired), OR
- an explicit `--input-regime {stock,whep}` CLI argument to the validation entry point.

The manifest field is preferred (it travels with the run and is set by the build that knows which inputs it wired); the CLI flag is the override for ad-hoc validation of a run whose manifest is missing or being re-checked.

## 3. Master table (all 126 checks, grouped by category)

### 3.1 `both_identical` — input-agnostic correctness laws (same check, same threshold, both runs)

| check id | stock band | whep band | why |
|---|---|---|---|
| grid.axis | 0.5° cell-centre alignment (lon −179.75..179.75, lat −89.75..89.75, modular to 1e-6) | identical | structural fact of any global half-degree grid, independent of mask |
| grid.cellorder | strict row-major N-to-S CLM ordering: `(90−lat−0.25)/0.5*720 + (lon+179.75)/0.5` | identical | ordering convention fixed by the CLM format, not the input set |
| fill.not-zero | masked fill value never read as a real 0 | identical | NetCDF/CLM fill-handling correctness law |
| var.no-all-nan | 0 entirely-NaN/NA/Inf output variables | identical | dead-variable / dead-run tripwire |
| var.coverage-mutual | outputs mutually co-present on the same cells | identical | output-stack self-consistency law |
| npp.vegc.copresence | 0 cells with NPP>0 ∧ VegC=0 (or vice versa) | identical | physical cross-variable invariant; catches wrong-grid reads |
| npp.no-interior-holes | no interior dead-block holes / tile-seam / checkerboard in NPP | identical | spatial-coherence law a global mean hides |
| grid.integral-eq-csv | `\|gridded·area / CSV-scalar − 1\| < 1e-4` | identical (each run vs its OWN CSV) | conservation/arithmetic identity |
| pft-npp.bands-sum | per-PFT NPP bands sum to total NPP within rel-err | identical | mass conserved across band decomposition |
| mb.no-nan-any-output | 0 NA/NaN/Inf across every output array | identical | numerical-health law |
| output-config-manifest | emitted variable set == config-requested set | identical (each vs its OWN config) | output-config completeness gate |
| c.csv-schema | all required CSV columns present, parseable, correct dtypes | identical | property of the output writer, not the forcing |
| c.gross-nonneg | GPP, gross fluxes, pools ≥ 0 | identical | sign law (gross quantities non-negative) |
| c.pools-positive | VegC, SoilC, litter ≥ 0 and finite per cell | identical | per-cell positivity/finiteness (distinct from magnitude bands) |
| c.gpp-gt-npp | NPP < GPP per cell and globally (Ra>0) | identical | conservation ordering law |
| c.nep-closure | `\|NEP − (NPP−Rh)\| / scale < 1e-4` | identical | mass-balance closure identity |
| c.nbp-closure | NBP = NPP−Rh−fire−harvest closes within tolerance | identical | budget identity (magnitude/sign band is separate, calibrate) |
| c.pool-delta-nbp | d(VegC+SoilC+litter)/dt == NBP within tolerance | identical | flux-to-stock-change conservation law |
| w.global-closure | `\|P−ET−runoff−dStorage\| / P < 1e-3` (~1e-4 steady-state) | identical | global water-balance closure (arithmetic identity) |
| w.cell-closure | per-cell `\|P−ET−runoff−dStorage−lateral\| / P < 1e-3` everywhere | identical | hardened non-gameable per-cell closure |
| w.et-deadzone | on every cell with precip>400mm, ET>0; wet-but-zero-ET fraction ~0 | identical | temperature-grid-mismatch tripwire (would scream on 97%-dead run) |
| w.snow-balance | dSWE = snowfall−melt−sublimation closes per cell-season (rel-err<1e-3); SWE≥0; no tropical snow | identical | cryosphere conservation law |
| w.stores-nonneg | SWC, SWE, groundwater, river, lake/reservoir storage ≥ 0 | identical | hard physical floor (negative water mass impossible) |
| w.soil-water-bounds | 0 ≤ SWC ≤ column water-holding capacity | identical (both use PIK soil → same capacity field) | soil-physics relational bound |
| n.no-all-zero | N uptake/loss fields not globally all-zero | identical | liveness tripwire; single-line catch for 97%-dead failure |
| n.pools-nonneg | all N pools ≥ 0 | identical | physical non-negativity |
| n.balance-closure | N-in == N-losses + dN-pool within closure tolerance; leaching ≤ ndep+BNF+fert | identical | conservation law (tolerance is numerical, not a magnitude) |
| n.losses-components | Σ loss components == total N loss; each component ≤ total | identical | additive identity among output fields |
| n.uptake-npp-coherence | positive N uptake on cells with NPP>0; uptake spatially coherent with NPP | identical | cross-variable coherence law; N-collapse corroborator |
| n.no-cell-spikes | no per-cell N-flux spikes beyond physical ceiling; no NaN/Inf | identical | structural outlier / no-corruption law |
| v.fpc-finite | 0 non-finite FPC cells | identical | no-corruption structural law |
| v.fpc-range(0-1) | 0 ≤ FPC_pft ≤ 1 and Σ_pft FPC ≤ 1+1e-6 per cell | identical | definitional FPC bound (von Bloh Eq.1-2) |
| v.fpc-heterogeneity | FPC field non-degenerate: sd>0, >1 distinct value | identical | dead-field liveness tripwire |
| v.live-coverage | ≥~0.90 of true-vegetated cells live; fatal if <0.60 | identical (each on its own vegetated subset) | flagship single-run liveness floor |
| v.vegc-csv-consistency | `\|gridded-integral / globalflux-scalar − 1\| < 1e-4` | identical | internal-accounting identity |
| v.fapar-range | 0 ≤ fAPAR ≤ 1 | identical | physical definitional bound |
| v.desert-bare | on Köppen-BW cells: FPC<0.05 ∧ NPP<~50 gC/m²/yr | identical | negative control tied to climate aridity |
| v.no-tree-in-ice | tree-PFT FPC == 0 on permanent-ice cells | identical | negative control (no tree on ice) |
| v.leaf-cn | per-PFT leaf C:N within von Bloh Table 2 hard bounds (e.g. C3 grass [10.5,37.9]) | identical (NOT widened for whep) | model-internal pft.par parameter constraint |
| lu.cftfrac-bands(32) | cftfrac carries exactly 32 bands (16 rainfed + 16 irrigated) | identical | structural band-count of the CFT output |
| lu.cftfrac-range | every cftfrac value in [0,1] | identical | land-use fraction bound |
| lu.grassland-band-idx(13) | managed-grass band at fixed index after the 13 crop CFTs | identical (content differs, index does not) | PFT-enumeration ordering law |
| cftfrac.output-sum-le-1 | Σ cftfrac bands ≤ 1 per cell (model output) | identical | area-closure invariant on realized output |
| lu.no-phantom-crops | no harvest/cftfrac for a crop PFT absent from that cell's input | identical (each vs its OWN input) | no-crop-from-nothing law |
| lu.harvest-where-cropped | harvest present wherever cropland supplied (live cell) | identical (each vs its OWN input) | converse-of-phantom consistency law |
| lu.yield-not-superphysical | no crop PFT exceeds a hard agronomic max yield | identical | absolute sanity ceiling |
| lu.harvest-le-agb | harvested C ≤ above-ground biomass per cell | identical | mass-conservation (cannot harvest more than grown) |
| g.only-on-grassland | mgrass uptakec == 0 on all cells with grassland cftfrac==0 | identical | stand-logic structural law (vs run's OWN grassland output) |
| g.carbon-closure | `\|uptakec − (yieldc+fecesc+urinec_C+respc+methanec)\| < small fp tol`; global residual ~0 | identical | grazing carbon mass-balance closure |
| g.nitrogen-closure | `\|ingested_N − (yieldn+fecesn+urinen)\| < small fp tol`; global residual ~0 | identical | grazing nitrogen mass-balance closure |
| g.methane-fraction(0.02-0.10) | methanec/uptakec ∈ [0.02,0.10] for the bulk of grazed cells | identical | enteric-CH4 physiological ratio law (module property) |
| g.offtake-le-npp | uptakec_mgrass ≤ grassland NPP per cell | identical | no-creation-of-energy ordering law |

### 3.2 `both_modified` — same check, STOCK tight (published-anchored), WHEP wide (plausibility)

| check id | stock band (tight) | whep band (wide) | why |
|---|---|---|---|
| grid.cellcount | == 67420 exact (CRU mask) | == nrow(whep grid.bin) (~58765 NaturalEarth) | reference grid differs: stock anchored to the published 67420 CRU mask, whep to its own NaturalEarth grid |
| npp.valid-fraction | ≥ 0.95 vegetated cells with NPP>0 | ≥ 0.90 | stock must reproduce von Bloh's near-complete live field; whep tolerates a slightly larger dead fraction but not collapse (0.03) |
| vegc.valid-fraction | ≥ 0.95 vegetated cells with VegC>0 | ≥ 0.90 | mirror of npp.valid-fraction for the dead-run failure |
| npp.major-biomes | per-biome median NPP tight on Luyssaert (trop-evergreen [600,1100], temp [650,950], boreal [180,380]) + alive-frac ≥0.95 | wider (trop [400,1300], temp [400,1200], boreal [100,550]) + alive-frac ≥0.90 | ungameable per-biome form of global-mean NPP |
| c.npp-mag | [58, 70] PgC/yr (~±10% of von Bloh 64.07) | [45, 82] PgC/yr (config spread) | stock reproduces published NPP; whep legitimately differs (land use, grid, cap) |
| c.gpp-mag | [120, 145] PgC/yr (von Bloh 131.8) | [100, 175] PgC/yr | as above for GPP |
| c.cue | [0.44, 0.50] (64.07/131.8 = 0.486) | [0.42, 0.58] | CUE band is input-sensitive (ordering is the separate input-agnostic law) |
| c.vegc-mag | [400, 510] PgC (von Bloh 444; excludes PNV 627-679) | [350, 650] PgC (full config spread incl. PNV) | managed grassland removes forest VegC under whep |
| c.soilc-mag | [1850, 2350] PgC (von Bloh 2049) | [1500, 3400] PgC | grid + land-use shift the permafrost-soil integral |
| c.rh-mag | [58, 75] PgC/yr (pinned by closure to reproduced NPP) | [45, 75] PgC/yr | Rh tied to the anchored NPP for stock; whep's NPP differs |
| c.fire-mag | [2.0, 3.0] PgC/yr (GlobFIRM 2.33 / SPITFIRE 2.7-2.8) | [1.5, 4.0] PgC/yr | burnable area shifts with whep land use/grass |
| c.harvest-mag | O(2-5) PgC/yr (calibrate from first stock run) | O(2-5) PgC/yr, wider (calibrate) | harvest driven directly by the differing land-use/stocking inputs (placeholder) |
| c.npp-percell-max | productive cells reach [1100,1350] gC/m²/yr | [1000, 1400] gC/m²/yr | stock reproduces Schaphoff Fig 4b productive tail |
| c.vegc-percell | tropical-forest cells [18,22] kgC/m² | [14, 24] kgC/m² | stock reproduces Schaphoff spatial map |
| v.vegc-mag | [400, 490] PgC (von Bloh 444) | [300, 650] PgC | same global VegC quantity, stock reproduces, whep widened |
| v.tropical-evergreen | NPP [600,1100], GPP [3000,4000] gC/m²/yr (Luyssaert 864/3551) | NPP [400,1300], GPP [2000,4000] | stock reproduces tropical productivity; whep catches dead/blown tropics |
| v.boreal-needleleaf | NPP [200,350] gC/m²/yr (Luyssaert 271) | [100, 550] | stock reproduces low-but-nonzero boreal NPP |
| v.agb-cap | tropics reach [9,20] kgC/m² (Luyssaert 11.4; Schaphoff ~20), ceiling ~22 | [5, 22] kgC/m² (under ceiling, nonzero tail) | stock must REACH published peak; whep only stay under ceiling |
| v.fapar-vs-modis | spatial r(fAPAR,MODIS MOD15) ≥ ~0.7 | ≥ ~0.5 (low value flagged, not a build failure) | per-run vs EXTERNAL obs (not paired); stock reproduces published skill |
| w.precip-mag | [105000,117000] km³/yr (Oki & Kanae ~111000, full CRU mask) | [85000,120000] km³/yr (smaller NaturalEarth land area) | precip is a forcing echo scaling with land mask/cell count |
| w.et-mag | [60000,80000] km³/yr (Haddeland WaterMIP 60-85k; GLEAM4 68.5k) | [45000,85000] km³/yr | ET scales with land area / land-use mix |
| w.discharge-mag | [35000,42000] km³/yr (Dai & Trenberth 37288±662) | [30000,45000] km³/yr | ocean-gauge total; whep's river network smaller/clipped |
| w.et-over-p | [0.55, 0.67] (Oki & Kanae ~0.6) | [0.45, 0.72] | partition ratio shifts with whep vegetation/land-use mix |
| w.transp-fraction | [0.50, 0.85] (GLEAM 0.50-0.65 vs Jasechko 0.80-0.90) | [0.40, 0.92] (collapse-detector) | T/ET depends on vegetation composition; science itself unresolved |
| w.amazon-mouth | [5500, 7000] km³/yr (Dai & Trenberth ~6642; GRDC ~6591) | [4500, 8000] km³/yr | named-river gauge; whep land use/grid shift routed total + mouth cell |
| w.river-panel | per-river tight on Dai & Trenberth/GRDC (Congo ~1308, Orinoco ~1129, Ganges-Brahm. ~1193, Yangtze ~1006, Mississippi ~610, Yenisei ~599, Lena ~531) | per-river ~±25-30% + liveness floor (each river >0) | multi-basin gauge panel; doubles as continental-liveness for whep |
| w.melt-timing | NH snowmelt peak month + snow-cover cycle (NOAA-Rutgers Jan ~46.8 / Aug ~2.6 Mkm²) reproduced; phase ≤~1 month | peak month ±1, amplitude widened; gross-wrong-phase still fails | stock reproduces observed seasonal timing; whep small phase shifts |
| n.uptake-mag | [560, 680] Tg N/yr (von Bloh Table 4 = 618) | [300, 1300] Tg N/yr (lit 620-1130; widened low for cap/cells) | stock reproduces published uptake; whep differs but never ~0 |
| n.leaching-mag | [55, 72] Tg N/yr (von Bloh 62.83) | [20, 150] Tg N/yr (lit 93-95) | differing fertiliser/land-use N, capped grazing, whep ndep |
| n.n2o-mag | [12.5, 16.5] Tg N/yr (von Bloh 14.57; denit 5.47 + nit 9.10) | [7, 22] Tg N/yr (brackets Tian 2020 ~17) | do not tighten whep to one framing |
| n.denit-mag | [44, 56] Tg N/yr (von Bloh 49.71) | [20, 120] Tg N/yr (Bouwman 2013 25-85) | differing N forcing / grass distribution |
| n.nh3-mag | [17.5, 23.5] Tg N/yr (von Bloh 20.46) | [8, 60] Tg N/yr | capped grazing density (manure NH3 source) + land use/ndep |
| n.bnf-mag | [112, 146] Tg N/yr (von Bloh 128.9) | [50, 290] Tg N/yr (Galloway 107 / Fowler 58) | differing land use/grass distribution |
| n.veg-cn(10-80) | [215, 275] gC/gN (von Bloh Fig 6b whole-veg C:N ~235-260) | [150, 320] gC/gN | whole-veg C:N shifts with PFT cover. **AMBIGUITY FLAG**: if this check actually reads per-PFT *leaf* C:N, the 10-80 bound is an input-agnostic pft.par parameter and the check is `both_identical` (like v.leaf-cn), not `both_modified`. Disambiguate with the registry author. |
| lu.dead-cropland-fraction | ≤ ~2% (published cropland establishes everywhere prescribed) | ≤ ~10% (reconstructed marginal + edge cells) | liveness diagnostic; would scream on 97%-dead run |
| lu.maize-yield(FAOSTAT) | within ±15% FAOSTAT (~5-6 t/ha) | within ±30-40% FAOSTAT | stock = the published crop-evaluation config; whep land use offset |
| lu.wheat-yield | within ±15% FAOSTAT (~3-3.5 t/ha) | within ±30-40% | as above for temperate cereals |
| lu.rice-yield | within ±15% FAOSTAT (~4-4.6 t/ha paddy) | within ±30-40% | as above for rice PFT |
| lu.cropland-area-faostat | within ±10% FAOSTAT/standard-LUH2 (~1.5-1.6 Gha) | within ±25% | aggregate area vs external anchor; whep reconstruction + fewer cells |
| g.magnitudes | global grazed C / enteric CH4 within ~±15% of published LPJmL grazing magnitudes | wide envelope (~2-4 Pg DM, ±40-50%) | stock reproduces published grazing behaviour; whep cap + grid + history |
| g.grazed-c-vs-herrero(2.3 Pg DM) | ~2.3 Pg DM ±20% (~1.8-2.8) (Herrero 2013 PNAS 110:20888) | ~1.3-3.5 Pg DM (~±50%) | external grazed-biomass anchor; whep cap/mask/history widen |
| g.enteric-ch4-inventory | within ~±20% of published LPJmL enteric CH4 (ruminant inventory ~90-100 Tg CH4/yr scaled to grazed fraction) | within ~±50% | global magnitude vs inventory (distinct from the per-cell ratio law) |
| g.grassland-npp | within ~±10-15% of published grassland-NPP partition of NPP 64.07 PgC | within ~±30% | grass-stand productivity vs published partition; whep grid/land-use |
| t.cold-limitation-pattern | cold-limited boreal/Arctic NPP fraction within ±5% of stock baseline; pattern monotonic poleward/upslope | same monotonic pattern enforced; fraction ±15-20% (NaturalEarth coverage) | direction is a law (both), magnitude/area-fraction is climate+grid-tied |
| t.permafrost-area | [12, 17] Mkm² (Obu ~14 NH; Schaphoff validation) | [8, 18] Mkm² (NaturalEarth under-resolves high-Arctic) | observational anchor; modelled area depends on mask coverage |
| t.permafrost-overlap | κ ≥ 0.6 vs Obu map (CRU 67420 cells) | κ ≥ 0.45 (reduced mask mechanically lowers achievable overlap) | spatial-skill ceiling depends on the cell set compared |
| t.thermal-npp-biomes | per-biome NPP shares within ±10% of published stock breakdown | shares ±20-25% (whep redistributes land among biomes) | ordering is a law (both); quantitative partition anchored to NPP 64.07 |
| t.snow-extent | peak NH snow extent within ±10% of stock baseline (Schaphoff snow validation) | ±20% (different snow-bearing cell set) | snow extent driven by climate + mask cell set |
| t.snow-seasonality | snow-season length / peak-SWE timing phase ≤~1 month vs stock baseline | phase ~2 months, length ±1-2 months; inverted/absent fails | PHASE is a law (both); precise length/timing anchored to CRU validation |
| t.soiltemp-range | per-latitude soil-temp amplitude within ±2-3 K (Schaphoff soil-thermal + PIK soil) | ±4-5 K; only physically impossible temps fail | quantitative amplitude reproduction anchored to published validation |

### 3.3 `whep_only` — meaningful only for the WHEP experiment (input echoes, lsuha cap, grazing presence, whep-grid provenance)

| check id | stock band | whep band | why |
|---|---|---|---|
| input.coverage-agreement | n/a (CRU climate co-registered on 67420; temp==precip coverage by construction) | temp input coverage ≥ precip input coverage on the run grid | direct detector of the temp-grid mismatch that zeroed 97% of whep NPP |
| input.temp-range | n/a (published CRU temperature, range guaranteed) | flag all-zero/NaN/missing soil-air-temp cells on the whep grid | plausibility/coverage of the whep-assembled temperature input |
| echo.precip-matches-input | n/a (no whep precip input to echo) | run precip output == whep precip input cell-for-cell | catches a forcing the model ignored/remapped |
| echo.climate-full-grid | n/a (CRU climate IS the grid → full coverage definitional) | climate forcing covers every NaturalEarth land cell | partner detector to the temp-grid mismatch |
| npp.vs-precip-coverage | n/a (precip and temp share one consistent grid) | of cells with precip>~400mm, fraction with NPP>0 above a floor | wet-but-dead detector for the whep input-assembly failure |
| n.ndep-echo | n/a (stock forced by LPJmL's own published ndep) | ingested ndep == whep ndep input (HaNi-derived) cell-by-cell | whep ndep input echo |
| n.grazing-balance | n/a (stock land use has no whep managed-grassland band) | per grazed cell, grass N uptake == yieldn+fecesn+urinen_mgrass within tol | depends on the whep managed-grassland band + prescribe_lsuha |
| n.input-units | n/a (no whep-generated N input to unit-check) | whep ndep unit ∈ {gN/m²/yr, kgN/ha/yr,...} whitelist; scaling consistent | guards the value/1e7 → kgN/ha scaling in prepare_spatialize_all.R |
| landuse.input-sum-le-1 | n/a (stock uses LPJmL's own validated LUH2 input) | per-cell Σ over 32 landuse-input bands ≤ 1.0 (tol ≤1.0001) | integrity of whep's own .write_lu_nc_chunk output |
| lu.output-eq-input | n/a (stock cftfrac echoes LUH2, a stock_only echo) | cftfrac output == whep landuse input per cell/band (abs ≤1e-4) | canonical whep-input echo |
| lu.input-output-coverage | n/a (stock coverage vs LUH2 on 67420 mask) | cells with non-zero whep input == cells with non-zero cftfrac output (≥~99%) | whep input→output plumbing |
| g.mgrass-present | n/a (stock not configured with whep prescribe_lsuha-from-NC) | mgrass vars present AND global \|uptakec_mgrass\| > 0 (not all-zero) | presence-of-the-experiment-output |
| g.nonzero-where-grass | n/a (stock grassland inputs are LPJmL's own) | every cell with whep lsuha>0 ∧ grass-frac>0 has mgrass uptakec>0 (<1-2% zero) | per-cell prescribe_lsuha-took-effect |
| g.coverage-floor | n/a (stock coverage floor is a different mask number) | active-grazing cells ≥ ~60-70% of non-zero whep-lsuha cells on NaturalEarth grid | whep-grid liveness floor (catches 97%-dead mode) |
| lsuha.bounded(<=4) | n/a (stock lsuha is exogenous published data, not whep-capped) | max(whep grassland_lsuha) ≤ 4.0 LSU/ha exactly; field non-negative | the whep .grassland_lsuha_chunk pmin(...,4) cap |
| lsuha.at-cap-tail | n/a (stock input never capped by whep) | fraction of grazed cells at lsuha==4.0 small (<~5-10%); warn if large | cap is a tail safeguard, not the modal value |
| lsuha.unit-string | n/a (stock does not consume a whep-written lsuha NC) | NC var name=='grassland_lsuha', units=='LSU/ha'; config var/fmt match | unit-string contract of the whep-generated input |
| g.density-realism | n/a (stock density is published input; realism via stock reproduction) | realised density bulk in ~0.1-4 LSU/ha, mean ~0.5-1.5; no pile-up at 0 or cap | whep lsuha-distribution realism (coupled to the cap/tail) |

### 3.4 `stock_only` — meaningful only for the STOCK control (reproduce-published = the "is the build correct" check)

| check id | stock band | whep band | why |
|---|---|---|---|
| bench.vs-pik-published | NPP [60.9,67.3] (±5% of 64.07); GPP [125.2,138.4] (±5% of 131.8); VegC [422,466] (±5% of 444); SoilC [1947,2151] (±5% of 2049) — von Bloh 2018 Table 3 / Schaphoff 2018 | n/a — does NOT apply to whep | the model-build-correctness check; reproducing PIK values is defined against LPJmL's OWN inputs. Applying it to whep would false-flag legitimate input-driven differences |

### 3.5 `paired` — needs BOTH run directories (validate_lpjml_pair); the most powerful diagnostics

| check id | comparison (whep vs stock, shared cells) | why it must be paired |
|---|---|---|
| paired.live-fraction-delta | whep live-cell fraction ≥ stock live-cell fraction − 0.10 (and not <50% of stock's) | **THE diagnostic that would have SCREAMED on the 97%-dead whep run while stock was alive.** Only evaluable by comparing the two live fractions; the reference is the live stock result, not a literature number |
| bench.vs-stock-globalflux | whep NPP/GPP/VegC/SoilC on shared cells within ±25% of stock's same-cell fluxes | neither run alone holds the other's value; anchor is the live stock result |
| bench.vs-stock-spatial-npp | cell-wise Pearson r(NPP_whep, NPP_stock) ≥ 0.9 on shared cells | intrinsically a two-field comparison; no single run holds both fields |
| c.npp-vs-modis | whep-vs-stock spatial r(NPP) ≥ 0.9 on shared cells; whep-vs-MODIS not weaker than stock-vs-MODIS (stock-vs-MODIS is the calibrated reference, r~0.5-0.9) | the grid-mismatch-catching form is the whep-vs-stock comparison; no fixed model-vs-obs r is input-agnostic |
| w.vs-grace | whep dTWS spatial r ≥ 0.9 vs stock on shared cells; global TWS-amplitude ratio within ±25% | GRACE constrains TWS anomalies weakly in absolute pattern; the input-isolating form is whep-vs-stock (same model, same routing) |

## 4. The paired mode is the most powerful diagnostic

The single most important check in the entire suite is **`paired.live-fraction-delta`**. The documented failure was a whep run with **97% of cells dead** (npp.valid-fraction = 0.03) caused by a temperature-grid/coverage mismatch in the whep-assembled climate inputs, while the stock run was fully alive. Several single-run checks would also have tripped on that run (`n.no-all-zero`, `v.live-coverage`, `w.et-deadzone`, `npp.vs-precip-coverage`, `g.coverage-floor`), but the **cleanest, unambiguous, false-positive-free signal** is the relative one: *the whep run's live fraction is catastrophically below the stock run's on the very same cells.*

That comparison is only possible with **both runs in hand**. A single-run liveness floor must be set low enough to admit a legitimately-sparser whep run, which blunts it; the paired delta needs no such slack because it measures whep **against the live stock reference**, isolating an input bug from any plausible input-driven sparsity. The other paired checks (`bench.vs-stock-globalflux`, `bench.vs-stock-spatial-npp`, and the obs comparisons recast as whep-vs-stock, `c.npp-vs-modis` and `w.vs-grace`) generalise the same principle from liveness to magnitude and to spatial pattern: a grid-misaligned whep run collapses its correlation against the structurally-correct stock field even when each field individually looks superficially plausible. Running `validate_lpjml_pair(whep_run, stock_run)` should therefore be **mandatory** before any whep run is accepted, not optional.

## 5. Placeholders awaiting first-run calibration

Several bands are intentionally provisional (`calibrate = TRUE`) until verified runs exist to anchor them:

- **Stock bands** are calibrated from the **first verified stock run**: the published von Bloh/Schaphoff anchors fix the central values, but the exact tight tolerances (and any stock band currently written as "calibrate from first healthy stock run", e.g. `c.harvest-mag`) are confirmed against the first build that demonstrably reproduces the PIK values. `c.nbp-closure`'s magnitude/sign band remains a placeholder pending resolution of the 5.9.7 sign convention (the closure-residual law itself is fixed and `both_identical`).
- **WHEP bands** are calibrated from the **first healthy whep run**: the wide plausibility envelopes (e.g. the `0.90` valid-fraction floors, `c.harvest-mag`, the crop-yield ±30-40% bands) are tightened or confirmed once a known-good whep run defines the legitimate spread of its inputs.
- **Paired tolerances** are calibrated from the **first good pair**: the ±25% global-flux window, the r≥0.9 spatial floors, and the −0.10 live-fraction delta are confirmed against the first matched (alive stock, healthy whep) pair, since the acceptable whep-vs-stock departure can only be measured once both halves are known-good.

One classification remains explicitly **flagged for the registry author**: `n.veg-cn(10-80)` is filed as `both_modified` (whole-vegetation C:N) but, if the check actually reads per-PFT *leaf* C:N, the 10-80 bound is a model-internal pft.par parameter and the check is instead `both_identical` (like `v.leaf-cn`). This must be disambiguated before the band is finalised.

---

## 4. Required fixes from adversarial review (apply when building the registry CSV)

Critique verdict: regime architecture sound; `paired.live-fraction-delta` confirmed to catch the 97%-dead-whep-vs-alive-stock failure (0.03 < 0.95 − 0.10). Five fixes before locking bands:

1. **Resolve `n.veg-cn` (self-contradictory).** The id literal (per-PFT leaf C:N 10–80, input-agnostic ⇒ `both_identical`) contradicts its band (whole-veg C:N 215–275, input-sensitive ⇒ `both_modified`). Read what the check actually computes and pick one classification before finalising.
2. **Add `paired.shared-cell-count` precondition (MISSING, important).** Gate ALL paired checks (`bench.vs-stock-globalflux`, `bench.vs-stock-spatial-npp`, `c.npp-vs-modis`, `w.vs-grace`, `paired.live-fraction-delta`) on shared-cell-count ≥ ~0.9·nrow(whep grid) (~55000). Without it a grid-misregistered whep run computes r≥0.9 / ±25% on a near-empty intersection and silently PASSES.
3. **Tighten WHEP N-flux upper bounds (mechanically backwards).** whep caps grazing at 4 LSU/ha (less manure N) and uses ~12.8% fewer cells (less total N), yet the whep upper bands balloon 2–3× above the stock anchors (leaching [20,150] vs 62.8; denit [20,120] vs 49.7; nh3 [8,60] vs 20.5; bnf [50,290] vs 128.9). A capped/clipped run emitting 2–3× published N should look suspicious. Anchor to the literature spread, not multiples of it.
4. **Document stock `both_modified` magnitude bands as SECONDARY guardrails.** The stock npp/gpp/vegc/soilc/rh bands are looser than `bench.vs-pik-published` (the real tight reproduction gate). State this so nobody mistakes the `both_modified` stock band for THE reproduction check. Re-anchor `c.rh-mag`(stock) to NPP_stock − fire − harvest closure (its upper 75 currently exceeds npp upper 70). Relabel `w.transp-fraction` as a collapse-detector (band spans the unresolved GLEAM-vs-Jasechko science), not a tight stock band. Fix the `w.soil-water-bounds` rationale: each run vs its OWN capacity field (whep does NOT use stock's 67420-cell soil).
5. **Add `whep_only` `grid.provenance`:** run grid cell-set == the manifest whep grid cell-set. Catches a whep run that silently fell back to the CRU 67420 grid (`grid.cellcount==nrow(grid)` alone cannot detect this). Also consider a `paired.n-vs-stock` sanity (whep N fluxes within a factor of stock on shared cells) given fix 3.



---

# Appendix C — Input-validation catalogue

# Input-validation stage

> Pre-run gate for the LPJmL run driver files (NetCDF/`.clm`/text), executed in `run_lpjml.R` **before** `mpirun`. Catches input defects at zero compute cost.

## 1. Why a pre-run input gate

LPJmL input validation is a **fail-fast gate**: it runs as a pure pre-run step, before any MPI process launches, so every check it performs costs no HPC compute. This matters because the most expensive failures in the documented history of this run were not crashes; they were runs that *completed* on defective input and produced silently dead or wrong results:

| Failure | Root cause | Input defect class |
|---|---|---|
| **A** | Temperature input valid on **fewer** cells than precipitation → 97% of cells ran with fill/0 temperature → NPP = 0 over a run that still "finished" after ~1h of wasted HPC | cross-input coverage divergence |
| **B** | Per-cell landuse band fractions summed to **> 1** → LPJmL silently rescaled the land mosaic | per-cell value-range (cross-band sum) |
| **C** | `grassland_lsuha` reached **2174 LSU/ha** (denominator collapse on near-zero mapped grass) → absurd grazing offtake | per-cell value-range (uncapped magnitude) |
| **D** | Unit string `"LSU/ha"` rejected by LPJmL's parser → "assume none" → grazing silently dropped | unit-string parse defect |

Every one of A/B/C/D is a property of a **driver file** that is fully knowable before the model runs. A gate that reads the NetCDF headers, coordinate axes, masked value arrays, and config flags can reject all four in milliseconds, so the ~1h run never starts.

### Relation to the upstream parquet check and the output stage

This stage sits in the middle of one validation chain, sharing a single engine across three stages:

- **`validate_lpjml_compat.R` (upstream, parquet stage)** validates the **whep input parquets** — the tidy source-of-truth tables — for 0.5° centre alignment, temporal completeness, physical ranges, and so on. It does **not** see the LPJmL-ready driver files.
- **Input stage (this document)** validates the **driver files that LPJmL actually reads** (`.nc`/`.nc4`/`.clm`/`.txt`), confirms each whep driver **echoes its parquet source** under the documented unit conversion, and confirms the whole driver set shares **one grid** and **covers the simulation years**. It is the bridge between "the parquet was valid" and "the driver faithfully carries that valid parquet onto the run grid."
- **Output stage (downstream)** validates the model results.
- **Paired stage** ties input cells to output cells 1:1.

All three stages run on **one shared engine** with a registry `stage` column in `{input, output, paired}`. The same primitives — grid-provenance resolution, value-range bounds, unit-string whitelisting, no-degenerate-field logic — are written once and reused across stages (see §4).

## 2. Mechanism

### Registry `stage` column

Every check is a registry row carrying a `stage` ∈ `{input, output, paired}`. The input stage selects `stage == "input"` (177 checks). The same registry, filter, and report machinery drive the output and paired stages.

### Entry point

```r
validate_lpjml_inputs(input_dir, input_regime, grid_file, config)
```

| Argument | Role |
|---|---|
| `input_dir` | Directory of the LPJmL-ready driver files named in the run config. |
| `input_regime` | `"whep"` or `"stock"`. Selects regime-specific thresholds and which checks apply. |
| `grid_file` | The run's own `grid.bin`/`grid.clm`. **The single authoritative cell list.** Every coverage/cross-input check resolves its reference cell set from this file, never from a hardcoded `ncell`. This is what makes one check correct for both the whep (~58,765-cell NaturalEarth) and stock (67,420-cell CRU) masks. |
| `config` | The resolved run config (cjson): simulation window, `grazing`, `prescribe_lsuha`, `fix_fertilization`, and the `input.*.name/.var/.fmt` wiring. |

### Tidy report

The function returns a tidy tibble, one row per check actually run:

```
id | stage | tier | files | regime | severity | status | detail | catches
```

`status` ∈ `{pass, warning, fail}`; `detail` carries the offending value (worst cell, coverage deficit, rejected unit string, etc.).

### Wiring as the pre-run gate

In `run_lpjml.R`, immediately before `mpirun`:

```r
report <- validate_lpjml_inputs(input_dir, input_regime, grid_file, config)
if (any(report$severity == "fatal" & report$status == "fail")) {
  cli::cli_abort("Pre-run input validation failed; aborting before mpirun.")
}
```

**Any fatal failure aborts before `mpirun`.** This extends the existing `.check_inputs()` guard (which only does `file.exists` on a subset of whep drivers) to a full structural + value-range + cross-consistency + provenance gate covering the grid, soil group, climate, CO₂, landuse, nitrogen, and grassland drivers that `.check_inputs()` does not currently touch.

### How the regime is supplied

`input_regime` is passed explicitly by `run_lpjml.R` from the run's `input_set` (`"whep"` vs `"stock"`). It governs three things: which thresholds apply (e.g. band count 32 vs 64; grid count NaturalEarth vs CRU), which checks are skipped (whep-only provenance round-trips, grass-band, lsuha checks do not apply to stock), and how a failure is attributed (a stock-file failure is a corrupted download/transfer, since stock inputs are valid-by-construction). Regime categories per check: `both_identical` (same threshold), `both_modified` (stock tighter/different), `whep_only`, `stock_only`.

## 3. Master input-check catalogue

Grouped by tier in execution order: **structural → value_range → cross_consistency → provenance**. Deduplicated across the climate, CO₂/time-span, soil, grid, landuse, nitrogen, grassland, and cross-input groups. The **failure-A coverage tripwire** and the **B/C/D origin checks** are called out explicitly.

> **Severity** ∈ `{fatal, error, warning}`. **Regime**: `both` / `whep` / `stock`. The `grid_file` is the resolved reference cell list throughout (never a hardcoded count).

### Tier 1 — structural (dims, bands, format, fill, axes)

| id | files | assertion + threshold | regime | sev | catches |
|---|---|---|---|---|---|
| `in.climate.files-exist-readable` | temp/prec/cloud.nc | Exist, > 1 KB, open cleanly via `nc_open`/`read_io` | both | fatal | Missing/truncated transfer |
| `in.climate.expected-variable-present` | temp/prec/cloud.nc | Each holds its CRU var (`tmp`/`pre`/`cld`) as sole data var | both | fatal | Wrong-file swap → silent zero field |
| `in.climate.dims-lon-lat-time` | temp/prec/cloud.nc | Exactly (lon=720, lat=360, time) or (cell, time); no transpose | both | fatal | Lat-flip / wrong-resolution grid |
| `in.climate.grid-axis-0p5deg-centres` | temp/prec/cloud.nc | lon ∈ −179.75…179.75, lat ∈ −89.75…89.75 on 0.5° centres, monotonic, unique | both | fatal | Off-by-¼°, 0–360 vs −180–180, S→N flip |
| `in.climate.fill-value-declared-not-zero` | temp/prec/cloud.nc | `_FillValue` declared and **≠ 0**; masked array read | both | error | Ocean fill read as literal 0 |
| `in.climate.time-axis-monthly-regular` | temp/prec/cloud.nc | Parseable CF units, 12 steps/yr, 28–31 d apart, no gaps/dupes | both | fatal | Daily/annual axis mis-fed; desynced forcing |
| `in.climate.all-vars-share-one-grid` | temp/prec/cloud.nc | Byte-identical lon/lat/time across the three | both | fatal | Cross-driver grid mismatch (A's structural root) |
| `in.co2.file-exists-and-readable` | CO₂ txt | Exists, > 0 B, ≥ 2 cols, ≥ 100 rows | both | fatal | Truncated CO₂ download |
| `in.co2.two-numeric-columns` | CO₂ txt | Each row = (int year ∈ [1500,2200], finite +ppm); 0 bad rows | both | fatal | Locale comma / stray header → coerced 0 |
| `in.co2.years-unique-no-duplicates` | CO₂ txt | `length(unique(year)) == length(year)` | both | fatal | Append bug → wrong-year CO₂ indexing |
| `in.co2.years-contiguous-annual` | CO₂ txt | Sorted, 1-yr step, no interior gap | both | fatal | Missing year distorts transient |
| `in.co2.driver-axis-annual-contiguous` | cft/fert/manure/ndep/lsuha | Each annual driver axis contiguous 1-yr, no gaps/dupes | whep | fatal | Regeneration dropped/repeated a year |
| `in.soil.type-file-readable-and-dims` | soil_30arcmin_13_types.nc | Opens; var present; 720×360 0.5° centres | both | fatal | Corrupt/wrong-resolution soil map |
| `in.soil.ph-file-readable-and-dims` | soil_pH.nc | Opens; same 720×360 grid as soil-type | both | fatal | Wrong-grid pH file |
| `in.soil.ph-unit-string-parseable` | soil_pH.nc | `units` ∈ {`1`,`pH`,`dimensionless`,`-`,absent} | both | warning | **D-class**: unparseable unit → "assume none" |
| `in.soil.landfrac-file-readable-and-dims` | landfrac_gadm_30arcmin.nc | Opens; same 720×360 grid | both | fatal | Corrupt/wrong-grid landfrac |
| `in.soil.landfrac-unit-string-parseable` | landfrac_gadm_30arcmin.nc | `units` ∈ {`1`,`fraction`,`dimensionless`,`-`,absent}; `%`→error | both | warning | **D-class**: percent encoding 100× wrong |
| `in.grid.file-exists-readable` | grid.bin/clm | Exists, non-empty (≥ ~235 KB), readable; `.clm` header parseable | both | fatal | Truncated/absent grid → "no cells" run |
| `in.grid.clm-header-magic-version` | grid.clm | id == `LPJGRID`, version ∈ {2,3,4}, nbands == 2, dtype LPJ_SHORT, scalar 0.01 | both | fatal | Wrong/byte-swapped file at `input.coord` |
| `in.grid.no-duplicate-cells` | grid.bin/clm | All (lon,lat) pairs unique | both | fatal | Double-emitted cells double-count area |
| `in.grid.lat-order-north-to-south` | grid.bin/clm | Lat non-increasing (row-major N→S, first = 83.75 whep) | both | fatal | Transposed order misaligns every positional join |
| `in.grid.no-fill-or-nan-coords` | grid.bin/clm | No sentinel/NaN/Inf in any coordinate | both | fatal | NA cells join to nothing, silently drop |
| `in.landuse.file-exists-readable` | cft_*.nc | Exists, > 0 B, `nc_open` succeeds | both | fatal | Truncated landuse transfer |
| `in.landuse.has-expected-variable` | cft_*.nc | Named fraction var present, dims (lon,lat,pft,time) | both | fatal | Renamed var → all-bare-soil run |
| `in.landuse.band-count-32-whep` | cft_*.nc | pft dim == 32 (16 rainfed + 16 irrigated) | whep | fatal | Band drift breaks grass-band index |
| `in.landuse.band-count-64-stock` | cft_*.nc | pft dim == 64 | stock | fatal | Wrong-vintage stock landuse |
| `in.landuse.fractions-finite` | cft_*.nc | No NaN/Inf/fill on any run-grid cell | both | fatal | Fill leak poisons a stand |
| `in.nitrogen.fert-manure-files-present` | fert_N/manure_N.nc | Both exist and parse via `nc_open` | both | fatal | Missing/truncated N driver |
| `in.nitrogen.ndep-both-species-present` | ndep_nhx/noy.nc4 | Both exist; var name matches species in filename | both | fatal | One species silently zero |
| `in.nitrogen.fert-manure-band-count-32` | fert/manure.nc | 32 (whep) / 64 (stock); equals paired landuse band count | both | fatal | Band-misaligned N → wrong crop |
| `in.nitrogen.fert-manure-irrigated-mirror-bands` | fert/manure.nc | band(k+16) == band(k), k=1..16, |Δ|<1e-6 | whep | error | Dropped irrigated duplication → 0 N irrigated |
| `in.nitrogen.fert-manure-fill-and-finite` | fert/manure.nc | Finite; fill == −1.175494e+38; no in-between values | both | fatal | NaN / fill read as huge negative N |
| `in.nitrogen.fert-manure-units-string` | fert/manure.nc | `units` == `g/m2`; **not** `kgN/ha`/blank | both | error | **D-class**: rejected unit → "assume none" |
| `in.nitrogen.ndep-units-string` | ndep nhx/noy.nc4 | `units` == `g/m2/day`; not blank/per-year token | both | error | **D-class**: 365× mis-scale |
| `in.nitrogen.ndep-fill-and-finite` | ndep nhx/noy.nc4 | Finite; fill == −1.175494e+38 | both | fatal | NaN from aggregation; fill as data |
| `in.nitrogen.ndep-months-broadcast-consistent` | ndep nhx/noy.nc4 | 12 steps/yr; 12 within-year slices identical (|Δ|<1e-9) | whep | error | Ragged month expansion → garbage months |
| `in.grassland.lsuha-file-exists-readable` | grassland_lsuha.nc | Exists, opens, sole var `grassland_lsuha` | whep | fatal | Missing/renamed → fallback `param.lsuha=0` |
| `in.grassland.lsuha-var-and-dims` | grassland_lsuha.nc | Var dims (lon,lat,time) float, match grid + export years | both | fatal | Transposed/missing-time raster |
| `in.grassland.lsuha-units-parseable` | grassland_lsuha.nc | `units` ∈ allowlist; **FAIL if == `LSU/ha`** (slash form) | whep | fatal | **Failure D exactly**: slash unit → "assume none" |
| `in.grassland.lsuha-fillvalue-declared` | grassland_lsuha.nc | fill == −1.175494e+38 declared | both | error | Undeclared fill read as −3.4e38 density |
| `in.grassland.lsuha-off-grass-zero-dense` | grassland_lsuha.nc | Dense-on-grid: off-grass cells = 0.0, no fill on grid | whep | error | Sparse/fill padding → "assume none" |
| `in.grassland.lsuha-stock-baseline-intact` | grassland_lsuha.nc (stock) | Range, no NaN, fill, dims well-formed on CRU grid | stock | fatal | Corrupted transfer of canonical lsuha |
| `in.landuse.units-fraction-unitless` | cft_*.nc | `units` dimensionless; **not** ha/m²/km² | whep | warning | **D-class**: area-unit mislabel mis-scales |
| `in.cross.grid-file-readable` | grid.bin/clm | `read_io` ok; ncell ≥ 1; bands == 2; regime count (whep 55–62k, stock == 67420) | both | fatal | Corrupt master cell list; guards hardcoded-67420 false-fatal |
| `in.cross.fill-value-consistent-not-zero` | all gridded drivers | `_FillValue` == −1.175494e+38 declared; sentinel absent on land cells | both | fatal | Undeclared fill → −1.17e38 ingested as data |
| `in.cross.no-duplicate-or-conflicting-cells` | grid.bin + cft/lsuha/fert | No dup cell; `coord_to_rowcol` maps 1:1 in-range (row ∈ 1..287, col ∈ 1..720) | both | error | Out-of-range cell silently dropped by writer |
| `in.cross.stock-baseline-valid-by-construction` | all stock drivers | Full structural+range+cross on stock thresholds (67420, 64-band, CRU) | stock | error | Corrupted transfer of LPJmL's own inputs |

### Tier 2 — value_range (physical bounds, no-degenerate)

| id | files | assertion + threshold | regime | sev | catches |
|---|---|---|---|---|---|
| `in.climate.no-all-fill-field` | temp/prec/cloud.nc | Valid cells > 0 every step; whole-file valid ≥ 0.20 land | both | fatal | All-fill field (extreme of **A**) |
| `in.climate.temp-physical-range` | temp.nc | Cell-month ∈ [−70,60] °C; land mean ∈ [6,11] °C | both | fatal | Kelvin mis-scale / 10× error |
| `in.climate.prec-physical-range` | prec.nc | Monthly ∈ [0,3000] mm; global land ∈ [100000,120000] km³/yr | both | fatal | mm/day vs mm/month (12×), negatives |
| `in.climate.cloud-percent-range` | cloud.nc | ∈ [0,100] %; land mean ∈ [30,75] %; reject 0–1 fraction | both | error | Fraction-vs-percent (100× radiation error) |
| `in.climate.no-degenerate-constant-field` | temp/prec/cloud.nc | sd(tmp)>3, sd(pre)>5, sd(cld)>3; > 1000 distinct values | both | error | Tiled-constant field with plausible mean |
| `in.climate.temp-spatial-gradient-realistic` | temp.nc | Tropics ∈ [22,30] °C, ≥ 25 °C warmer than poles | both | warning | Lat-flip / flattened temp (mean-invariant) |
| `in.co2.value-range-physical` | CO₂ txt | Every ppm ∈ [250,450] | both | fatal | ppb slip, −9999 fill, stray 0 (**C-class** magnitude) |
| `in.co2.preindustrial-start-value` | CO₂ txt | Spinup first-year ∈ [270,300] (fatal outside [250,350]) | both | error | Wrong baseline biases 200-yr equilibrium |
| `in.co2.monotone-trend-late-century` | CO₂ txt | 1959–2018 non-decreasing within 0.5 ppm; no ±3 ppm/yr jumps | both | error | Scrambled-row reorder (years intact, values shuffled) |
| `in.co2.endpoint-values-anchor` | CO₂ txt | 2018 ∈ [405,412]; 1959 ∈ [313,318] ppm | both | error | Wrong dataset / decade-shifted key |
| `in.soil.type-integer-no-fill-on-land` | soil-type + grid | Every land cell: non-fill integer; count fill == 0 | both | fatal | WARNING036 soilmap-undefined (A-shaped death) |
| `in.soil.type-in-range-1-13` | soil-type + grid | Every land cell ∈ [1,13]; 0 or > 13 count == 0 | both | fatal | Class LPJmL cannot map (**C-class** out-of-range) |
| `in.soil.type-exactly-13-classes-present` | soil-type | ≥ ~10 of 13 classes; no class > 70 % land | both | error | Degenerate/constant-filled soil map |
| `in.soil.ph-range-3-10` | pH + grid | Every land cell ∈ [3,10]; tight band [3.5,9.5]→warn | both | fatal | pH×10 scale / 99/−999 sentinel (**C-class**) |
| `in.soil.ph-no-fill-on-land` | pH + grid | Land-cell fill/NaN count == 0 | both | fatal | Missing pH → default rates (**A-class** soil) |
| `in.soil.ph-not-degenerate-constant` | pH + grid | sd > 0.3; no value > 60 % land | both | warning | Neutral-fill (all 7.0) meaningless field |
| `in.soil.landfrac-range-0-1` | landfrac + grid | Land cell ∈ [0, 1+1e-6]; out-of-range count == 0 | both | fatal | Percent/area encoding (**B-class** denominator) |
| `in.soil.landfrac-positive-on-land` | landfrac + grid | Every land cell > 0 (floor 1e-4) | both | fatal | landfrac 0 zeroes cell area (**A mechanism**) |
| `in.soil.landfrac-not-all-one` | landfrac + grid | ≥ 5 % of land cells < 1.0 | both | warning | `rep(1,ncell)` fallback: GADM coastal correction dropped |
| `in.grid.cell-count-positive-bounds` | grid.bin/clm | ncell ∈ [30000,70000]; not 0, not 207360 rectangle | both | fatal | 0-cell (**A enabler**) or ocean-included grid |
| `in.grid.cell-count-whep-natural-earth` | grid.bin/clm | ncell == frozen NaturalEarth count (nominal 58765) | whep | fatal | whep pointed at stock CRU grid / stale grid |
| `in.grid.cell-count-stock-cru` | grid.bin/clm | ncell == 67420 exactly | stock | fatal | Truncated stock grid download |
| `in.grid.resolution-half-degree` | grid.bin/clm | Centres on 0.5° lattice (frac .25/.75); modal spacing 0.5 | both | fatal | 1°/0.25°/half-cell-shifted grid |
| `in.grid.lon-axis-extent` | grid.bin/clm | All lon ∈ [−179.75, 179.75] | both | fatal | int16 overflow / 0–360 convention |
| `in.grid.lat-axis-extent-whep` | grid.bin/clm | lat ∈ [−59.25, 83.75] | whep | fatal | Wrong lat envelope vs nlat=288 inputs |
| `in.grid.lat-axis-extent-stock` | grid.bin/clm | lat ∈ [−55.75, 83.25] | stock | warning | Corrupted/truncated CRU lat axis |
| `in.grid.cell-area-all-positive` | grid.bin/clm | `calc_cellarea` ∈ (1000, 320000) ha, finite, > 0 | both | fatal | Lat out of range → zero/NaN-weighted aggregation |
| `in.landuse.fraction-range-0-1` | cft_*.nc | Every band ∈ [−1e-6, 1+1e-6] | both | fatal | Negative / > 1 single band |
| `in.landuse.cell-fraction-sum-le-1` | cft_*.nc | **Σ bands per cell-year ≤ 1+1e-4**; report worst cell | whep | fatal | **Failure B at origin**: per-cell sum unbounded by per-band clamp |
| `in.landuse.cell-fraction-sum-warn-098` | cft_*.nc | Warn when Σ ∈ (0.98, 1+1e-4] | whep | warning | Cells trending toward **B** |
| `in.landuse.grass-band-present-idx14` | cft_*.nc | Rainfed band 14 exists and not all-zero | whep | fatal | **Empty-grass run**: stand never establishes, `*_mgrass` = 0 |
| `in.landuse.irrigated-grass-band-zero` | cft_*.nc | band 30 ≤ 1e-9 everywhere | whep | error | Spurious irrigated grass inflates **B** sum |
| `in.landuse.no-empty-cropland-vs-pasture-conflict` | cft_*.nc | crop+grass cells still Σ ≤ 1+1e-4 jointly | whep | error | Crop/pasture double-allocation (mixed-cell **B**) |
| `in.nitrogen.fert-manure-nonnegative` | fert/manure.nc | All ≥ 0 g/m² | both | fatal | Sign flip → N removal |
| `in.nitrogen.fert-manure-upper-bound` | fert/manure.nc | Max < 100 g/m² (=1000 kgN/ha); p99.9 fert<50, manure<80 | both | error | Missing ×0.1 conversion (10×); **C-class** magnitude |
| `in.nitrogen.ndep-nonnegative` | ndep nhx/noy.nc4 | All ≥ 0 g/m²/day | both | fatal | Sign flip → canopy N export |
| `in.nitrogen.ndep-upper-bound` | ndep nhx/noy.nc4 | Per-species < 0.0055; nhx+noy < 0.0082 g/m²/day | both | error | Skipped ×0.1/365 (3650×); **C-class** |
| `in.nitrogen.ndep-global-budget` | ndep nhx/noy.nc4 | Global land total ∈ [40,90] TgN/yr; nhx frac ∈ [0.40,0.70] | both | error | Wholesale unit error / coverage gap per-cell caps miss |
| `in.nitrogen.ndep-nonuniform-hotspots` | ndep nhx/noy.nc4 | Spatial CV > 0.8; E-Asia/Europe/N-Am boxes > 2× land mean | whep | error | Flat/broadcast deposition lost spatial structure |
| `in.grassland.lsuha-no-nan-inf` | grassland_lsuha.nc | Run-grid NaN/Inf count == 0 | both | fatal | Division artefact NaN-poisons `*_mgrass` |
| `in.grassland.lsuha-range-0-4` | grassland_lsuha.nc | Every cell ∈ [0, 4.0]; > 4 count == 0 | both | fatal | **Failure C exactly**: uncapped 2174 LSU/ha |
| `in.grassland.lsuha-at-cap-tail-small` | grassland_lsuha.nc | Of nonzero cells, < 10 % exactly at 4.0 | whep | warning | Fat at-cap tail → upstream pasture-mask bug behind **C** |
| `in.grassland.lsuha-not-all-zero` | grassland_lsuha.nc | Nonzero cells > 0 every step, ≥ 1000 globally | whep | fatal | All-zero density: grass never grazed |
| `in.cross.no-all-fill-or-constant` | cft/fert/manure/ndep/lsuha/temp/prec/soil | > 0 valid cells, nonzero variance, not all-fill | both | fatal | All-fill/all-zero/constant broadcast driver |
| `in.cross.landuse-bandsum-le-1` | cft_*.nc | Σ 32 bands per cell-year ≤ 1+1e-6; name worst cell | whep | fatal | **Failure B** (stock → warning) |
| `in.cross.landuse-grass-band-written-nonzero` | cft_*.nc | Band 14 present and > 0 on ≥ 5 % land cells | whep | fatal | Silent-grazing: all `*_mgrass` zero |
| `in.cross.lsuha-bounded` | grassland_lsuha.nc | ∈ [0,4]; p99 ≤ 4; < 10 % at cap | whep | fatal | **Failure C**; at-cap tail surfaces denominator collapse |
| `in.cross.lsuha-unit-string` | grassland_lsuha.nc | `units` whitelisted; var == `grassland_lsuha` | whep | fatal | **Failure D** (slash unit → "assume none") |
| `in.cross.landfrac-bounds-and-aligns-grid` | landfrac + grid.bin | ∈ [0,1]; > 0 on every grid cell; nonzero set == grid set | both | error | Percent landfrac / area weighting 100× off |
| `in.cross.soil-type-domain-and-coverage` | soil-type + pH + grid | Soil ∈ [1,13] every land cell; pH ∈ [2.5,11]; shared axis | both | fatal | Undefined soil class → dead block (**A-shaped**) |

### Tier 3 — cross_consistency (shared grid + coverage agreement + time span)

| id | files | assertion + threshold | regime | sev | catches |
|---|---|---|---|---|---|
| **`in.climate.temp-coverage-ge-precip`** | temp.nc, prec.nc | **count(tmp valid ∧ pre valid)/count(pre valid) ≥ 0.999; count(pre valid ∧ tmp fill) == 0** | both | fatal | **FAILURE A EXACTLY** — the single most important check; saves the documented ~1h dead run |
| **`in.cross.temp-ge-precip-coverage`** | temp/prec/cloud.nc | C(temp_valid) ≥ C(precip_valid), ratio ≥ 0.999, on `grid_file`; same for cloud | both | fatal | **The failure-A pre-run tripwire this stage owns** (asymmetric ≥, not ==) |
| `in.climate.shared-valid-mask-across-vars` | temp/prec/cloud.nc | Pairwise mask symmetric-diff ≤ 0.02 land cells | both | error | One climate var masked on a different cell set (general A) |
| `in.climate.coverage-stable-over-time` | temp/prec/cloud.nc | Per-var valid count CV < 0.01; no month < 0.98 median | both | error | Time-localised coverage hole (A in time) |
| `in.climate.time-span-covers-simulation` | temp/prec/cloud.nc | Axis covers [sim_start, sim_end]; complete Jan–Dec ends | both | fatal | Forcing stops before sim end → dead tail |
| `in.climate.spinup-source-years-present` | temp/prec/cloud.nc | ≥ 30 complete climate years for spinup recycling | both | warning | Too few years → biased equilibrium |
| `in.climate.all-vars-share-one-grid` (xcons) | temp/prec/cloud.nc + grid | Identical lon/lat/time; match `grid_file` cells | both | fatal | Per-cell reads misalign across variables (A's root) |
| `in.climate.grid-matches-run-not-hardcoded` | temp/prec/cloud + grid.bin | Coverage vs **run `grid_file`**, not hardcoded ncell | both | fatal | **#1 false-fatal fix**: correct for both 58765 & 67420 |
| `in.co2.first-year-le-sim-start` | CO₂ txt | min(year) ≤ sim_start (1901) | both | fatal | WARNING001 hardened: spinup mis-forced with wrong CO₂ |
| `in.co2.last-year-ge-sim-end` | CO₂ txt | max(year) ≥ sim_end (2018) | both | fatal | Stale CO₂ on final years |
| `in.co2.span-covers-spinup-recycle-window` | CO₂ txt | min(year) ≤ sim_start (spinup recycles year-1; no 200-yr span needed) | both | warning | Prevents a spurious "must start 200 yr before" false-fatal |
| `in.co2.transient-coverage-all-timevarying` | CO₂ + cft/fert/manure/ndep/lsuha | Each axis: first ≤ 1901 ∧ last ≥ 2018 (axis metadata only) | whep | fatal | General A on time axis; any transient driver under-covering |
| `in.co2.end-years-agree-across-drivers` | cft/ndep/fert/manure/lsuha | All last-years equal (landuse 2023 vs ndep 2021 mismatch) | whep | error | Provenance drift: one driver regenerated, another not |
| `in.co2.start-years-agree-across-drivers` | cft/ndep/fert/manure/lsuha | All first-years equal (1851) | whep | error | Driver starts later → shifts transient pre-history |
| `in.co2.climate-series-covers-sim-window` | CRU tmp/pre/cld | First ≤ 1901 ∧ last ≥ 2018 (shipped CRU 3.10 ends 2009!) | both | fatal | **Temporal A surfaced**: 8-yr shortfall vs sim_end |
| `in.co2.climate-spinup-baseline-block-present` | CRU tmp/pre | ≥ 30 yr record; first ≤ sim_start | both | warning | Clipped climate too short for spinup baseline |
| `in.co2.stock-landuse-axis-covers-sim-window` | stock 64-band landuse + CO₂ | Stock transient axes cover [1901,2018] | stock | fatal | Corrupted canonical driver → meaningless whep-vs-stock |
| `in.soil.shared-grid-across-three-files` | soil-type + pH + landfrac + grid | One identical grid; land set ⊇ run grid | both | fatal | Cross-file soil grid mismatch (A class) |
| `in.soil.coverage-ge-forcing-grid` | soil-type + pH + landfrac + grid | Each field valid-count ≥ N_grid | both | fatal | **A generalised to soil**: any field under-covering grid |
| `in.soil.regime-grid-cellcount` | soil-type + grid | whep NaturalEarth ±1% / stock 67420 exact | both | error | Regime/grid swap (~13% cell-set difference) |
| `in.grid.coverage-superset-of-landuse` | grid.clm + cft.nc | Every nonzero-landuse cell (incl. band 14) ∈ grid | whep | fatal | Landuse on off-grid cells silently lost (ties **B**) |
| `in.grid.is-reference-for-all-inputs-cellcount` | grid.clm + all sibling drivers | Grid publishes canonical N + lattice; siblings share axis | both | fatal | **Makes temp≥precip well-defined** (A root enabler) |
| `in.grid.matches-output-cellcount` | grid.clm + output_grid.clm | Output grid ncell + (lon,lat) == input grid exactly | both | error | Output cells can't join back to inputs |
| `in.grid.landfrac-cells-subset` | grid.clm + landfrac.nc | landfrac ⊇ grid; every grid cell ∈ (0,1] | both | error | Land cell with 0/missing landfrac contributes 0 area |
| `in.landuse.grass-band-nonzero-on-pasture-cells` | cft.nc + gridded_pasture.parquet | ≥ 99 % of source-pasture cells carry band 14 > 0 | whep | fatal | Partial grass-band drop (global any-nonzero misses it) |
| `in.landuse.grid-matches-run-grid` | cft.nc + grid | 720×360 0.5°; populated cells ⊇ run cells (100%) | both | fatal | **A at landuse**: fewer valid cells than run grid |
| `in.landuse.cell-count-matches-regime` | cft_*.nc | whep ~58765 ±2% / stock 67420 ±1% | both | error | Grid/mask swap |
| `in.landuse.shared-grid-with-other-drivers` | cft/fert/manure/lsuha | Byte-identical axes across whep set | whep | fatal | Half-cell offset → fertiliser on wrong stand |
| `in.landuse.time-span-covers-simulation` | cft_*.nc | First ≤ 1901, last ≥ 2018, 1 record/yr, no gaps | both | fatal | Landuse clamps/recycles wrong year |
| `in.nitrogen.fert-manure-nonzero-where-cropped` | fert/manure + cft.nc | ≥ 60 % cropped cereal/maize bands carry N > 0 (yr ≥ 1961); mean ∈ [1,25] g/m² (2015) | both | error | All-zero fert field → N-starved crops |
| `in.nitrogen.fert-zero-where-no-cropland` | fert/manure + cft.nc | < 0.01 % no-cropland cell-bands carry nonzero N | both | warning | N on bare soil → spurious leaching (mis-join) |
| `in.nitrogen.shared-grid-one-definition` | fert/manure/ndep + grid | One grid (720×286, 83.75…−59.25), byte-identical, == run grid | both | fatal | N mapped to wrong cells (A class) |
| `in.nitrogen.coverage-vs-climate-grid` | fert/manure/ndep + grid + temp | N valid-count ≥ climate valid-count; fert/manure ≥ 99 % cropped-climate cells | both | fatal | **A mechanism on N**: driver covers fewer cells than climate |
| `in.nitrogen.time-span-covers-simulation` | fert/manure/ndep | Each covers [1901,2018]; filename year-range == content | both | fatal | Driver shorter than run → reuse/zero last slice |
| `in.grassland.lsuha-nonzero-where-grass` | lsuha + cft.nc | ≥ 95 % of band-14 > 0.01 cells have lsuha > 0 | whep | error | Grass established but no grazers mapped |
| `in.grassland.lsuha-zero-where-no-grass` | lsuha + cft.nc | Where band 14 == 0, lsuha == 0 | whep | error | Density on cells with no grassland stand |
| `in.grassland.lsuha-grid-matches-run` | lsuha + grid.bin/clm | 0.5° centres; cell set ⊆ run grid; one shared grid | both | fatal | Densities read for wrong cells (A/B class) |
| `in.grassland.lsuha-time-covers-run` | grassland_lsuha.nc | Contiguous years ⊇ [firstyear, lastyear]; no gaps/dupes | both | fatal | Uncovered years run un-grazed |
| `in.cross.all-drivers-same-grid-axis` | all gridded drivers | lon = seq(−179.75,179.75,0.5); lat descending (whep 83.75 top); identical axes; res 0.5° | both | fatal | Lat-flip/off-by-row/wrong-res (structural sibling of A) |
| `in.cross.landuse-cellset-covers-grid` | cft/fert/manure/lsuha/soil/pH | Each `all_land` driver covers 100 % grid cells; `coverage_scope` exempts lakes/cropland-only | both | fatal | Driver masked over part of land (A geography); **scope column prevents lakes false-fatal** |
| `in.cross.climate-covers-sim-years` | temp/prec/cloud (+wind/wetdays) | Axes span [1901,2018] no gap; sim-beyond-data fatal absent explicit recycle flag | both | fatal | **CRU ends 2009 vs sim 2018** — 9 unforced years (A on time axis) |
| `in.cross.co2-covers-sim-years` | CO₂ txt | Year col spans [1901,2018] monotone no gap; values ∈ [180,450] ppm | both | error | Truncated/wrong-era CO₂; units error |
| `in.cross.all-drivers-cover-export-window` | cft/fert/manure/ndep/lsuha | Each transient axis: min ≤ 1901 ∧ max ≥ 2018, gap-free | both | fatal | Transient ends before run → frozen/fill last years |

### Tier 4 — provenance (driver echoes its whep parquet; config flags match the input set)

| id | files | assertion + threshold | regime | sev | catches |
|---|---|---|---|---|---|
| `in.climate.units-attribute-parseable` | temp/prec/cloud.nc | tmp ∈ {`degC`,`Celsius`} not `K`; pre mm-family; cld percent-family not `1` | both | fatal | **D-class**: rejected/assumed climate unit → mis-scale |
| `in.climate.echoes-cru-source-metadata` | temp/prec/cloud.nc | Global attrs name CRU TS, CF-1.x, `stn` companion var; version matches sim span | stock | warning | Wrong-provenance file masquerading as CRU |
| `in.climate.config-flags-match-climate-set` | temp/prec/cloud + config.cjson | Config names these files, fmt `cdf`, var tmp/pre/cld; no constant-climate flag | both | error | Config ignores/points away from the climate NetCDFs |
| `in.co2.endpoint-values-anchor` (prov) | CO₂ txt | 2018 ∈ [405,412]; 1959 ∈ [313,318] ppm | both | error | Wrong dataset passing range+monotonicity |
| `in.co2.filename-span-echoes-data` | CO₂ txt | Filename `<A>_<B>` == min/max(year) | both | error | Stale filename after re-clip |
| `in.co2.config-name-matches-run` | CO₂ txt + run-config.cjson | `input.co2.name` exists at inpath, matches prepared mapping | both | fatal | Config names absent/old CO₂ file |
| `in.co2.filename-span-echoes-axis-each-driver` | cft/ndep/fert/manure/lsuha | Each filename `<start>-<end>` == its NetCDF axis bounds | whep | error | **Origin of 1851-2023 vs 1851-2021**: namer/writer disagree |
| `in.co2.config-flags-match-input-set` | run-config + lsuha.nc | whep: `prescribe_lsuha` needs lsuha file; `fix_fertilization==false` needs fert/manure | whep | fatal | **D-family in config**: flag promises a forcing whose file is absent |
| `in.soil.type-echoes-source-soilmap` | soil-type | Provenance records source soilmap + NaturalEarth build; footprint matches | whep | error | Stale/hand-edited soil file |
| `in.soil.landfrac-echoes-gadm-source` | landfrac | Provenance = GADM; field genuinely fractional (not all-ones) | whep | error | Filename claims GADM, content is `rep(1)` fallback |
| `in.soil.stock-files-baseline-integrity` | soil-type + pH + landfrac (stock) | Recorded checksum/size; all-checks-clean baseline | stock | fatal | Corrupted download of canonical soil/landfrac |
| `in.soil.config-references-these-files` | soil-type + pH + landfrac + grid | Config names all three, fmt `cdf`, files exist (closes `.check_inputs` gap) | both | fatal | Config points at missing/wrong-fmt soil input |
| `in.grid.echoes-country-grid-parquet` | grid.clm + country_grid.parquet | Decoded (lon,lat) set == parquet cells; Jaccard == 1.0 | whep | fatal | **Driver echoes its whep parquet** (B/C/D family) |
| `in.grid.config-resolution-flag-matches` | grid.clm + config.cjson | cellsize 0.5; ngridcell == ncell; `input.coord.name` == this grid; whep flags present | both | error | **D-family**: config resolution/flag vs grid mismatch |
| `in.landuse.no-phantom-crops` | cft.nc + gridded_landuse.parquet | Source-zero (cell,year,pft) → driver ≤ 1e-9 | whep | error | Fabricated cropland inflating **B** sum |
| `in.landuse.echoes-gridded-landuse-parquet` | cft.nc + gridded_landuse/cropland.parquet | Per-cell Σ bands == parquet (rainfed+irrigated)/cell_area, |Δ| ≤ 1e-3 | whep | fatal | Stale .nc / ha-vs-m² slip / remap drift |
| `in.landuse.cft-to-pft-band-mapping` | cft.nc + cft_mapping.csv | Every populated band maps to a defined `cft_lpjml` class; 0 unmapped | whep | error | Mapping drift routes crop to wrong band |
| `in.landuse.time-span-matches-filename` | cft_*.nc | Filename `<start>-<end>` == axis bounds | both | error | Stale rename / config span mismatch |
| `in.landuse.provenance-attrs-present` | cft_*.nc | `created_by` == WHEP writer; `created_date`; Conventions CF-1.8 | whep | warning | Hand-edited/foreign file masquerading as whep |
| `in.landuse.config-flags-match-landuse-set` | cft.nc + run-config.cjson | band 14 nonzero ⇒ `landuse==yes`, `grazing==livestock`, `prescribe_lsuha==true` | whep | fatal | Grass establishes but never grazed → `*_mgrass` zero |
| `in.nitrogen.ndep-50-50-fallback-not-used` | ndep nhx/noy.nc4 + n_deposition.parquet | Per-cell nhx/noy ratio spatially variable (CV > 0.05); not forced 1.0 | whep | warning | 50/50 fallback collapses NHx/NOy distinction |
| `in.nitrogen.fert-manure-echoes-parquet` | fert/manure + nitrogen_inputs.parquet | driver(g/m²) == parquet(kgN/ha) × 0.1 (×0.1 exactly once), ≤ 1 % | whep | error | Missing/doubled unit conversion; stale driver |
| `in.nitrogen.ndep-echoes-parquet` | ndep nhx/noy + n_deposition.parquet | value == parquet × 0.1/365 (exactly once), ≤ 1 % | whep | error | Per-year vs per-day confusion (365×) |
| `in.nitrogen.config-flags-match-input-set` | run-config + fert/manure/ndep | `fix_fertilization==false`; `input.*.name` == validated files | both | fatal | **Silent no-op**: fix_fertilization=TRUE ignores spatial N |
| `in.nitrogen.stock-baseline-valid-by-construction` | fert/manure/ndep (stock) | Structural+range on CRU 67420 / 64-band; whep-only checks skipped; checksum asserted | stock | error | Corrupted download of canonical N drivers |
| `in.grassland.lsuha-echoes-parquet` | lsuha + gridded_pasture.parquet | Per-yr global mean ±5 %, grazed-cell count ±1 % vs recomputed | whep | error | Stale/wrong-vintage lsuha driver |
| `in.grassland.config-prescribe-lsuha-true` | config.cjson | `prescribe_lsuha == true` whenever lsuha supplied | whep | fatal | **Silent regression**: FALSE → `param.lsuha=0`, `*_mgrass` zero |
| `in.grassland.config-grazing-livestock` | config.cjson | `grazing == 'livestock'` | whep | fatal | Grazing module off → no `*_mgrass` balance |
| `in.grassland.config-fix-fertilization-false` | config.cjson | `fix_fertilization == false` when whep fert/manure supplied | whep | error | N inputs silently replaced by global constants |
| `in.grassland.config-input-wiring-matches` | config.cjson + lsuha.nc | `input.grassland_lsuha`: file exists, fmt `cdf`, var matches NC | whep | fatal | **D-class wiring**: bad path/fmt/var → "assume none" |
| `in.cross.landuse-roundtrip-parquet` | cft.nc + gridded_landuse/pasture.parquet | ≥ 500 sampled points: band frac == parquet ha/cell_area, rel-err < 1e-3 | whep | error | Unit slip / row-col transposition / stale driver |
| `in.cross.lsuha-roundtrip-parquet` | lsuha + gridded_livestock/pasture.parquet | ≥ 300 grazed cell-yrs: == pmin(Σheads·lu/Σpasture, 4), rel-err < 1e-3; cap-count matches | whep | error | In-range but numerically wrong density |
| `in.cross.ndep-roundtrip-and-units` | ndep nhx/noy + n_deposition.parquet | == parquet × 0.1/365; units `g/m2/day`; same axis/grid; annual ≤ 100 kgN/ha | whep | error | Unit slip; 50/50 fallback; nhx/noy axis mismatch |
| `in.cross.fert-manure-roundtrip-units` | fert/manure + gridded_landuse.parquet | g/m², 32-band (17–32 == 1–16), ≥ 0, ∈ [0,1000] kgN/ha-eq, rel-err < 1e-3 | whep | error | kgN/ha left unconverted (10×); missing irrigated dup |
| `in.cross.config-prescribe-lsuha-coherence` | lsuha + config | `prescribe_lsuha==TRUE` ⇒ valid lsuha wired; FALSE while grazing intended → fail | whep | fatal | Documented silent-no-grazing trap |
| `in.cross.config-grazing-livestock-coherence` | cft.nc + config | `grazing=='livestock'` ⇒ band 14 nonzero ∧ `prescribe_lsuha==TRUE` | whep | fatal | Incoherent grazing triple → empty experiment |
| `in.cross.config-fix-fertilization-coherence` | fert/manure + config | `fix_fertilization==FALSE` ⇒ fert+manure present/valid; warn if TRUE while files exist | whep | error | Fertilisation-source contradiction |
| `in.cross.year-dependent-filenames-match-config` | cft/fert/manure/ndep/lsuha | Filename year-range == internal axis; config names resolve to existing files | both | error | Filename lies about content; missing configured file |

## 4. Shared with the output stage

The input and output stages are two `stage`-filtered views of one registry driven by one engine, so their shared primitives are written once and reused:

- **Grid-provenance resolution.** The rule "resolve the reference cell list from the run's own `grid_file`, never a hardcoded `ncell`" (the fix for the #1 false-fatal: comparing a whep ~58,765-cell run against a hardcoded 67,420) is a single helper. The input stage uses it for `temp-coverage ≥ precip-coverage` and every coverage check; the output stage uses the identical resolver so output cells join back to the same canonical grid (`in.grid.matches-output-cellcount` is literally the input↔output bridge), and the paired stage asserts the 1:1 cell correspondence.
- **Value-range bounds.** The physical-envelope machinery (land-mean SAT [6,11] °C, global land precip [100000,120000] km³/yr, CO₂ [250,450] ppm, lsuha [0,4], soil pH [3,10]) is the same bounds-table mechanism the output stage applies to fluxes and pools. Bounds differ by quantity; the comparator, the no-degenerate (spatial-sd + distinct-value) test, and the "fill masked, never read as 0" rule are shared code.
- **Unit-string whitelisting.** The whitelist-with-explicit-rejection primitive that catches **failure D** (`LSU/ha` slash form, Kelvin temp, `1`-as-percent cloud, `kgN/ha` for `g/m2`) is one function consulted by both stages; the output stage reuses it to validate output-variable units against their declared LPJmL unit strings.
- **No-degenerate logic.** "A field with a plausible mean can still be a tiled constant, a broadcast scalar, or all-fill" is detected by the same spatial-variance + distinct-value + valid-fraction test on both input drivers and output rasters.

One engine, three stages: the upstream `validate_lpjml_compat.R` parquet check, this pre-run input gate, and the post-run output/paired stages form a single chain in which the validated parquet is carried — provably, by the input stage's provenance tier — onto a single shared grid that the output stage can join back to 1:1.

---

## 5. Required fixes from adversarial review (apply when building the input registry)

Critique verdict: input-stage architecture sound; all of A/B/C/D fire pre-run at fatal severity. But one ship-blocking false-fatal + several others must be fixed, and the review surfaced real whep code bugs (below).

**Design fixes (false-fatals that would abort a HEALTHY whep run):**

1. **SHIP-BLOCKING — lsuha unit check is inverted.** `in.grassland.lsuha-units-parseable` / `in.cross.lsuha-unit-string` FAIL when `units == "LSU/ha"`, but whep writes exactly that string and the real run's LPJmL rejected it (`WARNING404 … assume none`). As written the gate aborts every healthy whep run. Fix: a POSITIVE whitelist of unit strings THIS LPJmL build accepts; fix the bad string at origin (see code bug a).
2. **Cross-driver grid checks compare axis NAMES, not VALUES.** Compare resolved coordinate VALUES against `grid_file`; whep names axes inconsistently (code bug b), so a byte-identical name compare false-aborts a healthy set. Add an axis-name echo at WARNING.
3. **Failure-A rule must be asymmetric.** Keep `temp-coverage >= precip-coverage` (ratio ≥ 0.999); demote symmetric mask-equality (soil/landfrac/lakes equal-cell-set) to WARNING. Apply a `coverage_scope` (all_land / cropland-only / lakes-exempt) to EVERY coverage check so legitimate fractional-landfrac, land-only-soil, wider-CRU, and lakes-excluded mask differences never trip a healthy run.
4. **De-hardcode the grid + fix the entry-point resolver.** Resolve the whep cell count from the manifest/`grid_file` (loose 55–62k fatal range; exact count only a WARNING); replace the non-existent `whep:::.get_l_files_dir()` resolver (code bug d) with `Sys.getenv("WHEP_L_FILES_DIR")`.
5. **Add missing checks; exempt ndep's monthly axis.** Add GLWD lakes driver checks (structural + grid-match + `var`/`fmt` wiring — same D-class trap), a pH-vs-soil-type mutual-mask check, and a `param.lsuha`-scalar-conflict check when `prescribe_lsuha=TRUE`. Exempt the monthly ndep axis (named `*_annual_*` but the writer uses a months-since 12-step axis) from the annual-driver contiguity checks.

**Real whep code bugs this design surfaced (verified; fix at origin, independent of building the suite):**

- **(a)** `.lsuha_nc_create` (`prepare_spatialize_all.R:159`) writes `units = "LSU/ha"`, which the run's LPJmL rejected (`WARNING404`, "assume none" — benign here only because LSU/ha is already the raw value). Write the unit string LPJmL accepts so the warning disappears and the check can whitelist it.
- **(b)** Axis-name inconsistency: `.lsuha_nc_create` uses `lon`/`lat` (`:150-151`) while every other whep NC creator uses `longitude`/`latitude` (`:100-101, :260-261, :296-297`). Normalise so all whep drivers share one axis-naming convention.
- **(c)** ndep is named `ndep_*_annual_*.nc4` (`run_lpjml.R:54`) but its writer builds a months-since, 12-step axis (`prepare_spatialize_all.R:300`). Reconcile the name with the actual temporal resolution.
- **(d)** `whep:::.get_l_files_dir()` is called in 6 files (`validate_lpjml_compat.R:41`, the 4 figure scripts, `methods_spatialize.qmd`) but is **defined nowhere in `R/`** — stale refs after a refactor; replace with `Sys.getenv("WHEP_L_FILES_DIR")`.



---

# Appendix D — Codebase grounding

`utils.R` is just `globalVariables`. The `.get_l_files_dir` is genuinely absent. The validation script `compare_lpjml_v6.R` uses `Sys.getenv("WHEP_L_FILES_DIR")` directly, which is the working pattern. I now have everything needed for a thorough brief.

---

# Brief: WHEP LPJmL Validation Code (for an EXTEND-not-restart plan)

## Files reviewed

| File | Path | Role |
|---|---|---|
| Compatibility validator | `inst/analysis/spatialize/validate_lpjml_compat.R` (1050 lines) | 15-check structural/numeric validation of `gridded_landuse.parquet` vs LPJmL v6 |
| v6 cell comparison | `inst/analysis/spatialize/compare_lpjml_v6.R` (230 lines) | Reads official v6 `.clm`, joins to WHEP output, computes per-cell diffs |
| Gap-analysis doc | `inst/analysis/spatialize/LPJML_COMPATIBILITY.md` (223 lines) | Format/CFT/irrigation/grid gap tables + conversion workflow |
| LPJmL runner | `inst/scripts/run_lpjml.R` (177 lines) | Builds config tibble, calls `lpjmlkit::run_lpjml` via mpirun |
| Deps | `DESCRIPTION` | Imports + Suggests |

---

## (1) Results-tibble / cli check pattern

The validator's core abstraction is a single helper at `validate_lpjml_compat.R:80-88`:

```r
.check <- function(name, passed, detail = "") {
  status <- if (passed) "PASS" else "FAIL"
  if (passed) {
    cli::cli_alert_success("{name}: {detail}")
  } else {
    cli::cli_alert_danger("{name}: {detail}")
  }
  tibble::tibble(check = name, status = status, detail = detail)
}
```

Recording pattern (any new check the plan adds must follow this exactly):
- **Columns:** `check` (chr), `status` (chr: `"PASS"`/`"FAIL"`/`NA`), `detail` (chr human-readable string with embedded numbers/tolerances).
- **Accumulation:** `results <- list()` (line 95), each check appended via `results[[length(results) + 1L]] <- .check(...)` (the idiom used 15×). Final bind: `results_df <- dplyr::bind_rows(results)` (line 629).
- **SKIP semantics:** pass `NA` as the `passed` arg → `status` becomes `NA` (the `if(passed)` short-circuits, so `cli::cli_alert_success` actually fires on `NA` — minor cosmetic bug, but `n_skip <- sum(is.na(results_df$status))` at line 633 counts them correctly). Used for checks 8/9/10 when `gridded_cropland.parquet` is absent (lines 427, 485, 517).
- **Informational (non-pass/fail) checks** pass hardcoded `TRUE` with a comment `# informational, not pass/fail` (checks 13 cell-count line 584, 14 year-range line 600).
- **Extra diagnostics** for the `.md` report go into a parallel `diag <- list()` (line 96), keyed by name (e.g. `diag$worst_conservation`, `diag$n_overcap`, `diag$cft_2020`). The `.md` report is built with a closure `.md <- function(...) md <<- c(md, paste0(...))` (line 775) writing Markdown tables, then `writeLines(md, report_file)`.

**Summary counting** (lines 631-638): `n_pass`/`n_fail`/`n_skip` with `na.rm = TRUE` on the status comparisons.

### The 15 checks already present (grouped by `cli::cli_h2` sections)

| # | Check (the `name` string) | Pass criterion |
|---|---|---|
| 1 | Grid alignment | all (lon,lat) on 0.5° centres (modular-arithmetic test, 1e-6 tol) |
| 2 | Temporal completeness | `setdiff(seq(first,last), years)` empty |
| 3 | CFT completeness | no missing CFTs **and** `min_cft == length(expected)` per year |
| 4 | No NA/NaN/Inf | 0 non-finite in `rainfed_ha`/`irrigated_ha` |
| 5 | No negative areas | 0 negatives |
| 6 | Country-level conservation | all country-year `rel_error < 1e-4` (gridded vs `country_areas.harvested_area_ha`) |
| 7 | Global totals match | `max_global_err < 1e-4` |
| 8 | Irrigated ≤ cropland irrigated | 0 cell-years exceed LUH2 irrigated by >1 ha (SKIP if no cropland) |
| 9 | Cell capacity (harvest ≤ cropland×3) | 0 cell-years over `cropland_ha * multicropping_max` (SKIP if no cropland) |
| 10 | Fraction range (harvest/cropland) | `max_frac <= multicropping_max + 0.01` (SKIP if no cropland) |
| 11 | No orphan cells | `anti_join(cells, country_grid)` empty |
| 12 | CFT-to-band mapping | all CFTs mapped + LPJmL crop bands 0–12 fully covered |
| 13 | Cell count vs LPJmL grid | informational (`TRUE`); reports delta vs 67,420 |
| 14 | Year range vs LPJmL v6 | informational (`TRUE`); reports start/end gaps vs 1700–2023 |
| 15 | Managed grassland (bands 13-14) | `has_pasture` (gridded_pasture.parquet exists) |

There is also a **conversion-readiness checklist** (lines 717-764, a separate `tibble::tribble` of `step`/`ready`/`notes`, rendered via `cli_alert_success`/`cli_alert_warning`) — not part of `results_df` but a second pass/fail surface the plan should be aware of.

**IMPORTANT GAP for the plan:** these checks validate the **Parquet** (`gridded_landuse.parquet`), not the actual LPJmL `.nc`/`.clm` inputs run_lpjml.R consumes, and **not** the LPJmL run outputs (the `*_mgrass` grazing balance, pft_npp, cftfrac). There is currently **no check that scans the mpirun log** and **no check on simulation outputs**. The doc lists optional checks 16-18 (v6 global totals, spatial correlation, top-N divergence) as future work; `compare_lpjml_v6.R` partially implements 16/18.

---

## (2) How it reads LPJmL output + grid handling

**`compare_lpjml_v6.R` is the file that touches real LPJmL binaries** (validate_lpjml_compat.R only reads Parquet via `nanoparquet::read_parquet`). Its reader stack (`library(lpjmlkit)` at line 28):

- **`read_io(grid_file)`** (line 67) — reads `grid.clm`; data is `[cell, time=1, band=2]`; lon = `data[,1,1]`, lat = `data[,1,2]`, both `round(..., 2)` (lines 69-70).
- **`calc_cellarea(grid_dat)`** (line 71) → `cell_area_m2` per cell. This is the canonical lpjmlkit cell-area source (the `.md` doc's alternative is `carea` from `staticData_quarterdeg.nc`, line 113).
- **`read_io(landfrac_file)$data[,1,1]`** (line 74) — `landfrac_gadm36.clm`, defaults to `rep(1, ...)` if absent.
- **`read_io(cft_file, subset = list(year = as.character(benchmark_years)))`** (line 82) then **`as_array(cft)`** → `[cell, year, band]`, asserted `dim[3] == 64L` (line 88).
- Hectare conversion (line 139): `rainfed_ha = frac * cell_area_m2 * landfrac / 1e4`.

**Band layout** (lines 108-126): 64 bands = 16 CFTs × 4 regimes; bands 1-16 rainfed, 17-32 surface, 33-48 sprinkler, 49-64 drip; irrigated = surface+sprinkler+drip summed. The 16 `cft_band_names` are hardcoded.

**Grid handling / the cell-count gap:**
- **`lpjml_ncell <- 67420L`** is hardcoded at `validate_lpjml_compat.R:48` (also referenced in the `.md` doc, lines 19/175).
- WHEP grid is **~58,765 cells** (NaturalEarth land mask) vs LPJmL's 67,420 (CRU mask) — a ~12.8% deficit treated as a **known acceptable difference**, not a failure (check 13 is informational; LPJML_COMPATIBILITY.md "Known acceptable differences" table line 192 and Action Item 4). The plan should preserve this "informational, accept the gap" stance rather than make cell-count a hard FAIL.
- **LPJmL global cell index** formula (LPJML_COMPATIBILITY.md:91): `cell_index = (90 - lat - 0.25)/0.5 * 720 + (lon + 179.75)/0.5` — row-major N-to-S, land cells only. This is the ordering any Parquet→CLM converter must reproduce.
- Constants block at `validate_lpjml_compat.R:47-53`: `lpjml_ncell=67420L`, `lpjml_first_year=1700L`, `lpjml_last_year=2023L`, `lpjml_nbands=64L`, `lpjml_n_cft=16L`, `multicropping_max=3.0`.

---

## (3) `run_lpjml.R` structure + log capture + output-config NOTE

**Structure:** single exported-style function `run_lpjml(model_path, l_files_dir="LPJmL_inputs", sim_path, export_start=1851, export_end=2021, dep_start, dep_end, simulation_start_year=1901, simulation_end_year=2018, nspinup=200, use_cores=24, input_set=c("whep","stock"))`. Helpers `.input_name()` (sprintf templating) and `.check_inputs()` (file.exists guard) at lines 153-166. Entry-point guard at line 171 (`if (sys.nframe() == 0L) stop(...)`).

Flow: `match.arg(input_set)` → build year-dependent input file names → `.check_inputs()` (whep set only) → build `simulation_params` tibble → `lpjmlkit::write_config(x, model_path, sim_path, debug=TRUE)` → **`lpjmlkit::run_lpjml(cfg, model_path, sim_path, run_cmd = stringr::str_glue("mpirun -np {use_cores} "))`** (lines 141-146).

**LOG CAPTURE — this is a gap the plan must fill.** `run_lpjml.R` does **NOT** capture, tee, or redirect the mpirun stdout/stderr to a log file. It calls `lpjmlkit::run_lpjml()` and discards its return; there is no `sink()`, no `2>&1 | tee`, no `file=` capture, no stored log path. **A log-scan check (e.g. for "Error", grazing-module warnings, non-convergence) has no log to scan today.** The plan needs to add stdout/stderr teeing — either wrap the `run_cmd` to append `| tee <logfile>`, or capture lpjmlkit's output — and define the canonical log path, before any log-scan validation check can exist.

**The output-config NOTE** (lines 128-131, verbatim):
```
# NOTE: the grazing outputs (pft_npp, cftfrac, and the *_mgrass C+N balance:
# uptakec/yieldc/yieldn/fecesc/fecesn/urinec/urinen/respc/methanec_mgrass) must
# be present in the model output config for the availability + validation step.
```
This is the contract: the LPJmL `output` cjson must emit `pft_npp`, `cftfrac`, and the nine `*_mgrass` C+N balance terms. `run_lpjml.R` sets `grazing = "livestock"` and `prescribe_lsuha = TRUE` (lines 96/102) to make the managed-grassland grazing module actually graze (otherwise every `*_mgrass` output is zero — see the inline comments at lines 97-101). **A validation step checking these outputs exist and are non-zero is anticipated by this NOTE but does not yet exist.** This is the natural extension point.

Relevant whep-set inputs the runner names (lines 38-68): `landuse/cft_default_cft_aggregation_30min_<s>-<e>.nc`, `fert_N_*`, `manure_N_*`, `nitrogen/ndep_nhx_whep_annual_*`, `ndep_noy_*`, `lakes_rivers/glwd_lakes_and_rivers_30arcmin.nc`, `landuse/grassland_lsuha_<s>-<e>.nc`. Note these are **`.nc`** files under `<l_files>/whep/lpjml_inputs/`, distinct from the `.parquet` the validator checks and the `.clm` the comparison reads — three different representations the plan should not conflate.

---

## (4) WHEP coding conventions the plan MUST follow

From `WHEP/CLAUDE.md` and confirmed by observed code:

- **Style:** tidyverse style guide; **max line width 80 chars**; `snake_case` column names that are readable (no cryptic abbrevs — prefer `methane_potential` over `Bo`); native pipe `|>`; `.by =` for grouping; `stringr` over base string ops; `tibble::tribble()` for small inline tables (used for `cft_to_lpjml_band` and `checklist`).
- **Functions:** exported funcs first, private `.`-prefixed helpers last; helpers stateless, all context via args; functions ≤ ~25 lines; no functions-inside-functions; ≤ 5 args (group extras into named lists); no for-loops except data.table private helpers over small fixed sets. **Note:** the existing scripts are `inst/` analysis scripts (top-level imperative code with for-loops over `seq_len(nrow(...))` for `.md` rendering) — they predate/relax the package-function rules. The plan should decide whether new validation logic becomes **package functions in `R/`** (strict rules apply, needs roxygen + tests + `_pkgdown.yml` entry) or stays as an **`inst/` analysis script** (looser, like the current ones). Given the existing validators live in `inst/analysis/spatialize/`, extending there is the lower-friction path; promoting reusable check helpers to `R/` is the more rigorous path.
- **Namespacing:** always prefix imported funcs (`dplyr::filter()`, `stats::median()`, `stats::quantile()` — the latter already used at lines 504/195); no bare imported names; no `@importFrom`.
- **Errors:** `cli::cli_abort()` not `stop()`; `cli::cli_warn()` for warnings. **Caveat:** the existing scripts use `stop()` (run_lpjml.R:162, compare_lpjml_v6.R uses `stopifnot`) and `validate_lpjml_compat.R` uses `stopifnot` (line 109) — so the repo's `inst/` scripts don't fully honor the `cli::cli_abort` rule. New package-level code must; new `inst/` script code should for consistency.
- **NSE globals:** every NSE column name must be declared in `utils::globalVariables()` in `R/utils.R` (R-CMD-check note enforcement). Only matters if logic moves to `R/`.
- **Tidy contract:** exported functions accept/return `tibble`; private `.` helpers may use `data.table` internally but must convert back to tibble before returning. Never bare `data.frame`.
- **Multi-method:** estimation functions with >1 defensible method expose `method=`/`tier=`, default = most rigorous, record choice in a `method_<quantity>` column (relevant if a check has alternative tolerances/methods).
- **Tests:** `testthat`, one file per script (`tests/testthat/test_<scriptname>.R`); access exported objects via `whep::name` or `getExportedValue("whep", nm)`; access private helpers in tests via `getFromNamespace(".helper", "whep")` (pattern in `test_run_spatialize.R:2`). Use `tribble`, pipes, `pull`, pointblank. **`inst/` scripts are not unit-tested** today (no `test_validate_lpjml_compat.R` exists) — if the plan promotes check logic to `R/`, it needs tests.
- **Tooling gate (mandatory before commit):** `air format .` (line ≤80, install binary if absent — never format manually) → `devtools::document()` → `rcmdcheck::rcmdcheck(build_args="--no-build-vignettes", args=c("--no-tests","--ignore-vignettes"), error_on="error")` → `devtools::test()` → **every `man/*.Rd` topic must be in `_pkgdown.yml`** under `reference:`. lint must pass with `object_usage_linter`, `line_length_linter`, `indentation_linter`, `commas_linter` disabled.
- **Excel:** never write `.xlsx` from R (`openxlsx` corrupts; openpyxl strips cached formulas) — irrelevant here (Parquet/NetCDF/CLM only), but flagged in CLAUDE.md.

**Dependencies available** (DESCRIPTION): Imports include `cli`, `dplyr`, `data.table`, `nanoparquet`, `readr`, `tibble`, `tidyr`, `purrr`, `stringr`, `rlang`, `arrow`, `fs`. **`lpjmlkit` is NOT in DESCRIPTION** (neither Imports nor Suggests) — `compare_lpjml_v6.R` and `run_lpjml.R` `library(lpjmlkit)` from a user-installed copy (`devtools::install_github("PIK-LPJmL/lpjmlkit")`). If the plan adds an LPJmL-output-reading check to the package, **`lpjmlkit` must be added to Suggests** and guarded with `rlang::is_installed()`/`requireNamespace`. `ggplot2`/`pointblank` are in Suggests.

---

## Latent bug the plan should fix (principle 0 — verified, not assumed)

`validate_lpjml_compat.R:41` calls **`whep:::.get_l_files_dir()`**, but **no function named `.get_l_files_dir` exists anywhere in the package** (grepped `R/`, all of `WHEP/`, and `utils.R` — zero matches; `utils.R` contains only `globalVariables`). The working pattern used by its sibling `compare_lpjml_v6.R:31` is `Sys.getenv("WHEP_L_FILES_DIR", unset="")` + `stopifnot(nzchar(...), dir.exists(...))`, and `run_spatialize.R` takes `l_files_dir` as an explicit `paths$l_files_dir` arg (required, `stopifnot(dir.exists(...))`). So `validate_lpjml_compat.R` as committed would **error on line 41 before any check runs**. The extension plan must replace that call with the `Sys.getenv("WHEP_L_FILES_DIR")` pattern (or a real helper if one is introduced) — and per CLAUDE.md "fix at origin" + "always update docs after fixes," reconcile the three different l_files-resolution patterns across these scripts.

## Key file/line references for the plan

- Check helper + record schema: `validate_lpjml_compat.R:80-88`, accumulation `:95,:629`, summary counts `:631-638`.
- `diag` list + `.md` closure report: `:96, :766-1049`.
- Constants (incl. `lpjml_ncell=67420L`, `multicropping_max=3.0`): `:47-53`.
- CFT→band tribble: `:57-76`.
- LPJmL binary reading (`read_io`, `calc_cellarea`, 64-band split): `compare_lpjml_v6.R:67-71, 108-142`.
- mpirun call (no log tee): `run_lpjml.R:141-146`; output-config NOTE: `:128-131`; grazing/lsuha activation: `:96-107`.
- l_files resolution patterns to reconcile: `validate_lpjml_compat.R:41` (broken), `compare_lpjml_v6.R:31` (env var, works), `run_spatialize.R:338-345` (arg).