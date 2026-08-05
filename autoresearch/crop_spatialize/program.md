# Autoresearch: run_crop_spatialize Optimization

You are an autonomous optimization agent for the `whep` R package. Your goal
is to reduce the wall-clock time of `run_crop_spatialize()` in
`inst/scripts/prepare_spatialize_all.R` without changing any outputs (files
written to `run_dir`).

## The loop

Each iteration you must:

1. **Read the current state.** Read `log.md` in this folder to see every prior
   experiment (kept and reverted). Do not re-try dead ends.
2. **Run the benchmark and read the profile.** The `PROFILE_START`/`PROFILE_END`
   block shows self-time hotspots. The `CALLERS_START`/`CALLERS_END` block
   shows which call sites trigger them.
3. **Form ONE hypothesis.** Pick a single targeted change. Write it down.
4. **Implement.** Modify `inst/scripts/prepare_spatialize_all.R` only.
   Follow all CLAUDE.md rules: tidyverse style, 80-char lines, no nested
   functions, namespace prefixes, native pipes.
5. **Verify correctness.** Run `devtools::test()` — all tests must pass.
   Pre-existing failures: 2 failures in `test_commodity_balance_sheet.R` are
   expected and can be ignored.
6. **Re-benchmark.** Run the benchmark again and compare `METRIC total_s`.
7. **Decide.**
   - **Keep** if gain ≥ 5% of total runtime OR the change simplifies the code.
   - **Revert** if gain is marginal and diff adds complexity, or if tests fail.
     Revert with `git checkout -- inst/scripts/prepare_spatialize_all.R`.
   - Log the result either way.
8. **Append to log.md.**

## Running the benchmark

```bash
Rscript autoresearch/crop_spatialize/benchmark.R 2>&1 | tee /tmp/bench_crop_spatialize.txt
```

The score is `METRIC total_s=XX.XX`. Lower is better.

The benchmark requires that Section 1–9 outputs already exist in
`LPJmL_inputs/whep/inputs/`. Run `test_crop_spatialize.R` (or the full
`prepare_spatialize/benchmark.R`) first if needed.

### Two-pass design

`parallel::mclapply` forks child processes. `Rprof` runs only in the parent
and cannot follow forks, so a naive single-pass benchmark produces an empty
or misleading profile (the parent just waits). `parallel`'s namespace is
locked, so `assignInNamespace` cannot patch it either.

The benchmark therefore runs two passes:

1. **METRIC pass** — full `run_crop_spatialize()` with real parallelism.
   This is the number to optimize.
2. **PROFILE pass** — loads the input parquets directly and calls
   `.run_spatialize_year()` in a plain `lapply` loop under `Rprof`.
   This bypasses `mclapply` entirely and gives Rprof a complete call stack
   through the data.table allocation, yield/N joins, and all helpers.
   Uses `profile_years` (3 years) to keep it fast (~15s).

Use the profile to find hotspots; use the metric to measure actual gains.

## Function anatomy

`run_crop_spatialize(run_dir, input_dir, year_range)` in
`inst/scripts/prepare_spatialize_all.R`:

1. **Reads parquets** — `country_grid`, `country_areas`, `crop_patterns`,
   `gridded_cropland`, `type_cropland` (optional), `country_yields`,
   `spatial_yield_index`, `nitrogen_inputs`, `spatial_n_index`.
2. **Chunks years** — splits `year_range` into chunks of 30 and iterates.
3. **Per-chunk**: calls `parallel::mclapply` with
   `min(2L, max(1L, parallel::detectCores() - 1L))` workers, each running
   `.run_spatialize_year(yr, shared_chunk)`.
4. **Per-chunk collect**: binds per-year crop rows, aggregates to CFT,
   writes `cft_NNN.parquet` to a temp dir. Does the same for yields and
   nitrogen if present.
5. **Combines chunks** via Arrow streaming — `arrow::open_dataset` +
   `arrow::write_parquet` for final `gridded_landuse.parquet`,
   `gridded_yields.parquet`, `gridded_nitrogen.parquet`.
6. **Cleans up** temp dir.

`.run_spatialize_year()` (also in the script) calls
`whep::build_gridded_landuse()` to do the actual per-year allocation, then
attaches yields and N rates.

## Known facts and prior work (from prepare_spatialize log)

- **s10 baseline**: ~54s for 1980–2010 (31 years), ~16% of total pipeline.
- `parallel::mclapply` is already used with up to 2 workers per chunk.
  Increasing `n_workers` beyond 2 is the first hypothesis to try.
- The per-year work in `.spatialize_year()` is already vectorised via
  data.table (all crops in one pass). Per-year time for 175 crops is ~1-2s.
- **Arrow combine step** (step 5) may have overhead for a small number of
  chunk files — investigate if streaming adds latency vs. a direct bind.
- **N join rewrite** (data.table inside `.run_spatialize_year`): run 6 in
  prepare_spatialize log tried this — 1.5s gain (0.4%), not worth it alone,
  but revisit if combined with other savings.

## Where to look for gains

- **`.run_spatialize_year` internals**: profile to see if the bottleneck is
  inside `build_gridded_landuse` (data.table allocation) or in the
  post-allocation steps (yield/N joins, normalization). Algorithmic wins
  here are free — no RAM risk.
- **Arrow combine**: does `open_dataset` + `group_by` + `summarise` +
  `compute` + `write_parquet` add meaningful overhead vs. just
  `bind_rows` + `write_parquet` for a handful of chunk files? Investigate
  if the streaming query has per-call overhead that's avoidable.
- **CFT aggregation**: done per-chunk in R, then re-aggregated in Arrow.
  Could skip the in-R step and let Arrow handle everything at the end —
  saves one pass over chunk data.
- **Chunk size**: 30 years per chunk. Smaller chunks → more write overhead;
  larger chunks → more RAM per chunk. Tune carefully; don't grow chunks
  beyond what fits comfortably in RAM.
- **Worker count**: already 2 workers. Increasing is possible but requires
  a RAM check first (see Constraints above). Only attempt after estimating
  `object.size(shared_chunk)` — each additional worker duplicates that
  footprint via fork. This is a last resort, not a first move.

## Constraints

- **RAM safety comes first.** Each `mclapply` worker forks the entire R
  process, including all pre-loaded parquet data (`country_areas`,
  `gridded_cropland`, `type_cropland`, etc.). With a full year range those
  objects can be several hundred MB each. Two workers already doubles peak
  RAM. **Never increase `n_workers` beyond 3 without first estimating peak
  memory per worker** (print `object.size` of `shared_chunk` before the
  `mclapply` call). If peak per-worker RAM × n_workers exceeds ~60–70% of
  available system RAM, do NOT keep the change — a crashing machine is
  worse than a slow run. Prefer algorithmic gains (fewer passes, cheaper
  joins) over raw parallelism.
- **Correctness is sacred.** Never change function outputs or file contents.
  Tests are the ground truth; `devtools::test()` must pass.
- **One change per iteration.** Don't bundle multiple optimizations.
- **Only modify `inst/scripts/prepare_spatialize_all.R`.** Do not change `R/`,
  tests, data, or `_pkgdown.yml`.
- **Respect CLAUDE.md.** Style rules apply throughout.
- **I/O variance is real.** Raster/parquet reads can swing ±5s between runs.
  If improvement is marginal, run twice before keeping.
- **Log everything.** Failed experiments stop future iterations from repeating
  the same dead ends.

## Log entry format

| Run | total_s | Status | Description |
|-----|---------|--------|-------------|
| 1   | 999.9   | kept   | Increase n_workers from 2 to 4; s10 54s→38s |
