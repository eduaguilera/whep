# Autoresearch: prepare_spatialize_all.R Pipeline Optimization

You are an autonomous optimization agent for the `whep` R package. Your goal
is to reduce the total wall-clock time of `inst/scripts/prepare_spatialize_all.R`
without changing any outputs (files written to `output_dir`).

## The loop

Each iteration you must:

1. **Read the current state.** Read `log.md` in this folder to see every prior
   experiment (kept and reverted). Do not re-try dead ends.
2. **Run the benchmark and read per-section timings.** The "Section breakdown"
   table shows which sections dominate total runtime. Focus on the largest.
   The `PROFILE_START`/`PROFILE_END` block shows self-time hotspots.
   The `CALLERS_START`/`CALLERS_END` block shows which call sites trigger them.
3. **Form ONE hypothesis.** Pick a single targeted change. Write it down.
4. **Implement.** Modify `inst/scripts/prepare_spatialize_all.R` only
   (this script is self-contained — all section functions live there).
   Follow all CLAUDE.md rules: tidyverse style, 80-char lines, no nested
   functions, namespace prefixes, native pipes.
5. **Verify correctness.** Run `devtools::test()` — all tests must pass.
   Pre-existing failures: 2 failures in `test_commodity_balance_sheet.R` are
   expected and can be ignored.
6. **Re-benchmark.** Run benchmark again and compare total_s and section timings.
7. **Decide.**
   - **Keep** if gain ≥ 5% of total runtime OR the change simplifies the code.
   - **Revert** if gain is marginal and diff adds complexity, or if tests fail.
     Revert with `git checkout -- inst/scripts/prepare_spatialize_all.R`.
   - Log the result either way.
8. **Append to log.md.**

## Running the benchmark

```bash
Rscript autoresearch/prepare_spatialize/benchmark.R 2>&1 | tee /tmp/bench_spatialize.txt
```

The score is `METRIC total_s=XX.XX`. Lower is better.

## What is already optimized — do not revisit

- **Section 5 (s5_mirca_irrigation)**: Already parallelized with `furrr::future_map`
  (26 crops processed concurrently) and uses data.table for aggregation.
  Baseline was ~260s; now ~25s. Do not touch this section.

- **Section 9a (s9a_hydrology) GLWD raster aggregation**: The GLWD v2 raster
  is 15 arc-seconds (33600×86400 = ~2.9 billion cells). Aggregating factor-120
  to 0.5° is irreducibly slow (~140s). Stacking layers did not help.
  The only real fix is pre-computing the aggregated raster and caching it next
  to the source data — but this is a one-time setup cost, not algorithmic.
  Do not attempt to speed up the terra calls inside `prepare_hydrology_inputs`.

## Where to look for gains

Start with the section breakdown table. Common sources of remaining slowness:

- **s2_country_areas / s4_gridded_cropland**: LUH2 NetCDF reads per year in a
  loop — consider batching layer reads or caching intermediate rasters.
- **s3_crop_patterns / s3b_crop_fert_patterns**: Serial EarthStat raster reads
  across 175 crops — could be parallelized with furrr like MIRCA was.
- **s6_yield_inputs / s7_nitrogen_inputs**: Check for repeated joins, redundant
  pivots, or multi-pass aggregations that could be merged.
- **s8_livestock_inputs**: Large section — profile to find inner bottleneck.
- **s10_crop_spatialize / s11_livestock_spatialize**: The actual spatialization
  loops; look for redundant rebalancing passes or repeated static joins.
- **Repeated data loads**: Some sections re-read the same parquet/raster that
  another section already computed in memory — pass through `main()` instead.

## Constraints

- **Correctness is sacred.** Never change function outputs or file contents.
  Tests are the ground truth; `devtools::test()` must pass.
- **One change per iteration.** Don't bundle multiple optimizations.
- **Only modify `inst/scripts/prepare_spatialize_all.R`.** Do not change `R/`,
  tests, data, or `_pkgdown.yml`.
- **Respect CLAUDE.md.** Style rules apply throughout.
- **I/O variance is real.** Raster reads can swing ±10s between runs.
  If the improvement is marginal, run twice to confirm before keeping.
- **Log everything.** Failed experiments stop future iterations from repeating
  the same dead ends.

## Log entry format

| Run | total_s | Status | Section | Description |
|-----|---------|--------|---------|-------------|
| 1   | 999.9   | kept   | s3      | Parallelize EarthStat crop reads with furrr |
